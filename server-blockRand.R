# server-blockRand.R

source("blockRand_20240516.R")
#------- TMT channels
tmt_channels <- c("126","127N","127C","128N","128C","129N","129C","130N","130C","131N",
                  "131C","132N","132C","133N","133C","134N","134C","135N")

df <- reactive({
  req(input$uploadData)
  data <- read.csv(input$uploadData$datapath, header = TRUE)
  data
})

output$inputData <- DT::renderDataTable({
  req(input$uploadData)
  DT::datatable(df(),
                extensions = c("Scroller", "RowReorder"),
                options = list(
                  rowReorder = TRUE,
                  deferRender = TRUE,
                  autoWidth = FALSE,
                  scrollY = 200,
                  scroller = TRUE,
                  scrollX = TRUE,
                  pageLength = 5,
                  searchHighlight = TRUE,
                  orderClasses = TRUE
                )
  )
})

output$factorDistribution <- renderUI({
  req(input$uploadData)
  if (!is.null(df()) && ncol(df()) > 1) {
    tabs <- lapply(seq_len(min(5, ncol(df()) - 1)), function(i) {
      if (is.factor(df()[[i + 1]]) || is.character(df()[[i + 1]])) {
        tabPanel(
          paste0("Factor ", i),
          fluidRow(column(12, plotlyOutput(paste0("factorDistPlot_", i), height = "267px")))
        )
      }
    })
    do.call(tabsetPanel, tabs)
  }
})

observe({
  req(input$uploadData)
  if (!is.null(df())) {
    lapply(seq_len(min(5, ncol(df()) - 1)), function(i) {
      if (is.factor(df()[[i + 1]]) || is.character(df()[[i + 1]])) {
        output[[paste0("factorDistPlot_", i)]] <- renderPlotly({
          counts <- table(df()[[i + 1]])
          counts_df <- as.data.frame(counts)
          counts_df$Var1 <- factor(counts_df$Var1, levels = unique(counts_df$Var1))
          
          # Default color palette
          defaultPalette <- RColorBrewer::brewer.pal(9, "Set1")
          
          plot_ly(
            x = counts_df$Var1,
            y = counts_df$Freq,
            type = "bar",
            marker = list(color = defaultPalette),
            hoverinfo = "text",
            text = paste("Category:", counts_df$Var1, "<br>Count:", counts_df$Freq),
            textposition = "none", # Remove text inside bars
            #width = 800, # Set the plot width
            height = 267 # Set the plot height
          ) %>%
            layout(
              title = paste("Distribution of ", colnames(df())[i + 1]),
              xaxis = list(title = "Categories"),
              yaxis = list(title = "Counts")
            )
        })
      }
    })
  }
})

batch_volume <- reactive(as.numeric(input$batch_volume))
experiment_type <- reactive(as.character(input$experiment_type))
n_IRs <- reactive(as.numeric(input$n_IRs))
nIterations <- reactive(as.numeric(input$n_iterations))

observeEvent({
  input$experiment_type
  input$batch_volume
}, {

  if (input$experiment_type == "TMT" && input$batch_volume > 18) {
    sendSweetAlert(
      session = session,
      title = "Warning",
      text = "Batch volume should not exceed 18 for TMT experiments.",
      type = "warning"
    )
    return()
  }
})

final_results <- eventReactive(
  input$run,
  final_outputs(df(), experiment_type(), batch_volume(), n_IRs(), nIterations())
)

# sample batch assign
output$sample_batch <- DT::renderDataTable({
  DT::datatable(final_results()[[1]], 
                extensions = c("Scroller","RowReorder"),
                option = list(
                  rowReorder = TRUE,
                  deferRender = TRUE,
                  autoWidth = F,
                  scrollY = 200,
                  scroller = TRUE,
                  scrollX = TRUE,
                  pageLength = 5,
                  searchHighlight = TRUE,
                  orderClasses = TRUE
                ))
})

# batch design matrix
output$batch_design <- renderUI({
  tabs <- lapply(seq_along(final_results()[[2]]), function(i) {
    tabPanel(
      paste0("Factor ", i),
      fluidRow(column(12, tableOutput(paste0("batch_design_", i)))))
  })
  do.call(tabsetPanel, tabs)
})

observe({
  lapply(seq_along(final_results()[[2]]), function(i) {
    output[[paste0("batch_design_", i)]] <- renderTable({
      final_results()[[2]][[i]]
    }, rownames = TRUE, spacing = "xs")
  })
})

# for download
output$download_all <- downloadHandler(
  filename = function() {
    "block_randomization_results.xlsx"
  },
  content = function(file) {
    # Create a list to hold data frames for each sheet
    sheets <- list(
      "sample batch assignation" = final_results()[[1]]
    )
    
    # Add batch design tables to the list
    batch_design_tables <- final_results()[[2]]
    for (i in seq_along(batch_design_tables)) {
      sheet_data <- batch_design_tables[[i]]
      sheets[[paste("batch design", i)]] <- cbind(Batches = rownames(sheet_data), sheet_data)
    }
    
    # Conditionally add channel design tables for TMT experiments
    if (experiment_type() == "TMT" && length(final_results()) > 2) {
      channel_design_tables <- final_results()[[3]]
      for (i in seq_along(channel_design_tables)) {
        sheet_data <- channel_design_tables[[i]]
        sheets[[paste("channel design", i)]] <- cbind(Channels = rownames(sheet_data), sheet_data)
      }
    }
    
    # Write all sheets to the Excel file
    write_xlsx(sheets, path = file)
  },
  contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
)
