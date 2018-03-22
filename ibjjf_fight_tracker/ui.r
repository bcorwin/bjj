library(shiny)

shinyUI(
  fluidPage(
    tabsetPanel(
      tabPanel("Selected Fights", dataTableOutput("selected_fights")),
      tabPanel("All Fights", dataTableOutput("all_fights"))
    ),
    textOutput("latest_update")
  )
)
