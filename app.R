## app.R ##
library(dplyr)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(xlsx)

source("blockRand_20220804.R")
#------- TMT channels
tmt_channels <- c("126","127N","127C","128N","128C","129N","129C","130N","130C","131N",
                  "131C","132N","132C","133N","133C","134N","134C","135N")

ui <- dashboardPage(
  dashboardHeader(title = "JUMP-BREAD"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("fas fa-map-signs")),
      menuItem("Program", tabName = "program", icon = icon("far fa-folder-open"))
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "intro",
              fluidRow(
                box(
                  title = h1("JUMP-BREAD"), status = "primary", width=12,
                  "Author: Yingxue Fu",
                  h2("Background"), br(),
                  "Liquid chromatography−mass spectrometry (LC−MS)-based proteomics plays a pivotal role in biomedical research, enabling the identification and quantification of thousands of proteins across diverse biological conditions. To ensure reliable and reproducible proteome profiling, a robust experimental design is essential. A crucial aspect of such design is the organization of sample processing sequences and the creation of well-balanced sample sets when allocating them to different batches. This precautionary measure prevents the introduction of confounders that could bias data interpretation. ", br(), br(),
                  "JUMP-BREAD is designed as a tool that simplifies the Block Randomization in Experimental Analysis and Design, especially when dealing with multiple explanatory variables simultaneously.",br(),
                  h2("Usage"), br(),
                  "1) Make the input file. An example input file can be downloaded below. The file need to be saved in .csv format. The first column is the sampleID, and from the second column will be the factors that need to be considered.", br(),br(),
                  a(href="example_input_for_block_randomization.csv", "example input file", download=NA, target="_blank"),br(), br(),
                  "2) Upload the input file. Click the “Program” tab on the left. Upload your input file and choose how many factors you want the program to consider.",br(), br(),
                  "3) Set up parameters. In the parameter box, you can choose the TMT plex, whether to use IR sample and use which channels for IR. For the number of iterations, you may use larger number when your sample size is large. After setting up the parameters, click the “Run” button. ", br(), br(),
                  "4) Check and download the results. You may need to wait for seconds or up to minutes to see the results depending on your sample size and the number of iterations. All results can be downloaded as an excel file.", br(), br(),
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "program",
              fluidRow(
                column(width = 3,
                  box(title = "Input Data", status = "primary", width = NULL,
                      fileInput("uploadData","Upload sample infomation"),
                      numericInput(inputId = "nFactors", label = "No. of factors", value = 0)),
                  box(title = "Parameters", status = "primary", width = NULL,
                      selectInput("tmtPlex", "TMT plex", choices = c(5, 6, 8, 10, 11, 16, 18)),
                      radioGroupButtons(inputId = "useIR", label = "Use IR (Internal Reference) samples?",
                                        choices = c("Yes", "No"), justified = TRUE),
                      uiOutput("chooseIRchannel"),
                      sliderInput("n_iterations", "Number of iterations", value = 100, min = 50, max = 1000)
                      ),
                  box(title = "Run program", width = NULL, status = "primary",
                      actionButton("run", "Run", class = "btn btn-primary btn-block"))
                ),
                column(width = 9,
                  box(title = "Sample Information", status = "primary", width = NULL,
                      dataTableOutput("inputData")),
                  box(title = "Batch and channel assignation", 
                      status = "primary", width = NULL,
                      dataTableOutput("sample_batch")),
                  box(title = "Batch design matrix",
                      status = "primary", width = NULL,
                      fluidRow(column(2, tableOutput("batch_design_1")),
                               column(4, tableOutput("batch_design_2")),
                               column(3, tableOutput("batch_design_3"))),
                      downloadButton("download_all", label = "Download all results"))
                )
              )
      )

      )
    )
)

server <- function(input, output) {
  
  df <- reactive(read.csv(input$uploadData$datapath, header = T))
  
  output$inputData <- DT::renderDataTable({
    req(input$uploadData)
    DT::datatable(df(), options = list(pageLength = 5))
  })
  
  output$chooseIRchannel <- renderUI({
    pickerInput(inputId = "IR_channels",
      label = "If Yes, select channel(s) for IR sample", 
      choices = tmt_channels[1:input$tmtPlex],
      multiple = TRUE)
  })
  
  n_factors <- reactive(as.numeric(input$nFactors))
  tmt_plex <- reactive(as.numeric(input$tmtPlex))
  use_IR <- reactive(as.character(input$useIR))
  IRchannel <- reactive(as.character(input$IR_channels))
  nIterations <- reactive(as.numeric(input$n_iterations))
  
  final_results <- eventReactive(
    input$run,
    final_outputs(df(), n_factors(), tmt_plex(), use_IR(), IRchannel(), nIterations())
  )
  
  # sample batch assign
  output$sample_batch <- DT::renderDataTable({
    DT::datatable(final_results()[[1]], options = list(pageLength = 5))
  })
  
  # batch design
  output$batch_design_1 <- renderTable(final_results()[[2]][[1]], rownames = T, spacing = "xs")
  output$batch_design_2 <- renderTable(final_results()[[2]][[2]], rownames = T, spacing = "xs")
  output$batch_design_3 <- renderTable(final_results()[[2]][[3]], rownames = T, spacing = "xs")

  
  # for download
  output$download_all <- downloadHandler(
    filename = function() {
      "batch_channel_design.xlsx"
    },
    content = function(file) {
      
      wb = createWorkbook()
      
      sheet1 = createSheet(wb, "sample batch assignation")
      
      addDataFrame(final_results()[[1]], sheet=sheet1, row.names=F)
      
      # batch design tables
      sheet2 = createSheet(wb, "batch design")
      batch_design_tables <- final_results()[[2]]
      n_batches <- dim(batch_design_tables[[1]])[1]
      
      for (i in 1:n_factors()) {
        addDataFrame(batch_design_tables[[i]], sheet=sheet2, startRow=(2*i-1)+(i-1)*n_batches, startColumn=2, row.names=T)
      }
      
      # channel design tables
      sheet3 = createSheet(wb, "channel design")
      channel_design_tables <- final_results()[[3]]
      n_channels <- dim(channel_design_tables[[1]])[1]
      
      for (i in 1:n_factors()) {
        addDataFrame(channel_design_tables[[i]], sheet=sheet3, startRow=(2*i-1)+(i-1)*n_channels, startColumn=2, row.names=T)
      }
      
      saveWorkbook(wb, file)
    }
  )
  
}

shinyApp(ui, server)
