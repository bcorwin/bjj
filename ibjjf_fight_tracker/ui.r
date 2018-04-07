options(shiny.sanitize.errors = FALSE)
Y
library(shiny)
library(DT)

shinyUI(
  fluidPage(
    tabsetPanel(
      tabPanel("Selected Fights", DT::dataTableOutput("selected_fights")),
      tabPanel("All Fights", DT::dataTableOutput("all_fights"))
    ),
    uiOutput("links"),
    textOutput("latest_update")
  )
)
