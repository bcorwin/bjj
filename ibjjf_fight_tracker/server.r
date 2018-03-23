options(shiny.sanitize.errors = FALSE)

library(shiny)
library(dplyr)
library(tidyr)
library(readr)
library(DT)
library(glue)

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
      Phase = phase,
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
  
  output$links <- renderUI({
    urls <- sort(get_urls())
    out <- lapply(seq_along(urls), function(d) {
      tags$li(tags$a(glue("Order of fights (Day {d})"), href=urls[d]))
    })
    out <- c(list(tags$h4("IBJJF Links:")), list(tags$ul(out)))
  })

  output$latest_update <- renderText({
    auto_refresh()

    latest_update <- as.POSIXct(read_lines("latest_update.txt"), tz = "UTC")
    data_lag <- round(difftime(Sys.time(), latest_update, units = "mins"), 1)
    
    attributes(latest_update)$tzone <- "America/Chicago"
    
    paste0("Data last refreshed ", data_lag, " min(s) ago (",
          format(latest_update, format="on %b %d @ %H:%M %Z"), ").")
  })
})
