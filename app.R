## app.R ##
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
  dashboardHeader(title = "JUMP batch design"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("fas fa-map-signs")),
      menuItem("Imports", tabName = "inputdata", icon = icon("far fa-folder-open")),
      menuItem("Results", tabName = "outputdata", icon = icon("fas fa-chart-line")),
      menuItem("Help", tabName = "help", icon = icon("far fa-question-circle"))
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "intro",
              fluidRow(
                box(
                  title = h1("Usage:"), status = "primary",
                  h2("1) Make the input file."), br(),
                  "An example input file can be downloaded below. The file need to be saved in .csv format. The first column is the sampleID, and from the second column will be the factors that need to be considered.", br(),br(),
                  a(href="example_input_for_block_randomization.csv", "example input file", download=NA, target="_blank"),
                  h2("2) Upload the input file."), br(), 
                  "Click the “Imports” tab on the left. Upload your input file and choose how many factors you want the program to consider.",
                  h2("3) Set up parameters."), br(), 
                  "In the parameter box, you can choose the TMT plex, whether to use IR sample and use which channels for IR. For the number of iterations, you may use larger number when your sample size is large. After setting up the parameters, click the “Run” button. ", br(),
                  h2("4) Check and download the results."), br(),
                  "Click the “Results” tab on the left. You may need to wait for seconds or up to minutes to see the results depending on your sample size and the number of iterations. All results can be downloaded as an excel file.", br(), br(),
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "inputdata",
              fluidRow(
                column(width = 4,
                  box(title = "Input Data", status = "primary", width = NULL,
                      fileInput("uploadData","Upload sample infomation"),
                      numericInput(inputId = "nFactors", label = "No. of factors", value = 0)),
                  box(title = "Parameters", status = "primary", width = NULL,
                      selectInput("tmtPlex", "TMT plex", choices = c(5, 6, 8, 10, 11, 16, 18)),
                      radioGroupButtons(inputId = "useIR", label = "Use IR (Internal Reference) samples?",
                                        choices = c("Yes", "No"), justified = TRUE),
                      uiOutput("chooseIRchannel"),
                      sliderInput("n_iterations", "Number of iterations", value = 100, min = 50, max = 1000)
                      )
                ),
                column(width = 8,
                  box(title = "Sample Information", status = "primary", width = NULL,
                      dataTableOutput("inputData"))
                )
              ),
              
              fluidRow(box(title = "Run program", solidHeader = T, width = 4,
                           actionButton("run", "Run", class = "btn btn-primary btn-block")))
      ),
      
      tabItem(tabName = "outputdata",
              box(title = "Batch and channel assignation", 
                  status = "primary", width = 12,
                  dataTableOutput("sample_batch")),
              box(title = "Batch design matrix",
                  status = "primary", width = 12,
                  fluidRow(column(3, tableOutput("batch_design_1")),
                           column(4, tableOutput("batch_design_2")),
                           column(5, tableOutput("batch_design_3"))),
                            downloadButton("download_all", label = "Download all results"))
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
