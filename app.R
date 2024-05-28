library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(writexl)
library(DT)

options(shiny.maxRequestSize = 20 * 1024^2)

actionBttnParams <- list(
  size = "sm",
  color = "primary",
  style = "fill",
  block = TRUE
)

ui <- tagList(
  dashboardPage(
    dashboardHeader(
      title = "JUMP-BREAD",
      titleWidth = 200
    ),
    dashboardSidebar(
      sidebarMenu(
        id = "sidebar_menu",
        menuItem(
          "Introduction",
          tabName = "intro",
          icon = icon("fas fa-map-signs")
        ),
        menuItem(
          "Experiment Design",
          tabName = "BlockRand",
          icon = icon("pen-to-square")
        )
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "intro",
                fluidRow(
                  box(
                    title = h1("BREAD: Block Randomization in Experimental Analysis and Design"),
                    status = "primary",
                    width = 12,
                    h3("Background"),
                    br(),
                    h4("Liquid Chromatography with tandem mass spectrometry (LC-MS/MS)-based proteomics plays a pivotal role in biomedical research, enabling the identification and quantification of thousands of proteins across diverse biological conditions. To ensure reliable and reproducible proteome profiling, a robust experimental design is essential to eliminate systematic bias and facilitate appropriate statistical downstream analyses. A key aspect of experimental design involves organizing sample processing sequences and creating well-balanced sample sets when allocating them to different batches. JUMP-BREAD is designed as a tool that simplifies Block Randomization in Experimental Analysis and Design, especially when dealing with multiple explanatory variables simultaneously. It supports both label-free and labeling proteomics experiments, making it suitable for both small- and large-scale proteomics studies."),
                    br(),
                    div(
                      style = "text-align: center;",
                      img(src = "images/bread_background.png", height = "300px")
                    )
                  )
                )
        ),
        tabItem(tabName = "BlockRand", source("ui-blockRand.R", local = TRUE, verbose = FALSE)$value)
      )
    )
  )
)

server <- function(input, output, session) {
  source("server-blockRand.R", local = TRUE, verbose = FALSE)
}

shinyApp(ui, server)