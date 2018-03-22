library(shiny)
library(dplyr)
library(tidyr)
library(readr)
library(DT)

source("get_fight_order.R")

clean_table <- function(in_table) {
  in_table %>%
    separate(division, c("Age", "Gender", "Skill", "Weight"), "/") %>%
    mutate(
      fight_time = format(fight_time, format = "%m/%d %H:%M"),
      Competitors = paste(competitor1, "vs.", competitor2),
      mat = gsub("(?i) *mat *", "", mat)
    ) %>%
    select(
      `Fight Time` = fight_time,
      Mat = mat,
      Competitors,
      Age,
      Gender,
      Skill,
      Weight
    ) %>%
    datatable(
      rownames = FALSE,
      filter = 'top',
      list(order = list(list(0, 'asc'), list(1, 'asc')))
    )
}

shinyServer(function(input, output, session) {

  auto_refresh <- reactiveTimer(90000)

  # observe({
  #   auto_refresh()
  #
  #   refresh_data()
  # })

  output$all_fights <- renderDataTable({
    auto_refresh()

    read_csv("all_fights.csv") %>%
      clean_table()
  })

  output$selected_fights <- renderDataTable({
    auto_refresh()

    read_csv("important_fights.csv") %>%
      clean_table()
  })

  output$latest_update <- renderText({
    auto_refresh()

    latest_update <- read_lines("latest_update.txt")
    paste("Data last refreshed on", latest_update)
  })
})
