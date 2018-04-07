library(rvest)
library(dplyr)
library(glue)
library(RMySQL)

# source("divisions_to_track.R")

get_all_comp_ids <- function() {
  all_comp_urls <- read_html("http://ibjjf.com/championships/calendar/") %>%
    html_nodes("#content > div > table > tbody > tr > td > a") %>%
    html_attr("href")

  sapply(all_comp_urls, get_comp_id)
}

get_comp_id <- function(comp_url) {
  comp_urls <- read_html(comp_url) %>%
    html_nodes("#sidebar-championships-menu > ul > li > a") %>%
    html_attr("href")
  comp_id <- comp_urls[grepl("ChampionshipResults/\\d+/PublicRegistrations", comp_urls)]
  comp_id <- gsub(".*ChampionshipResults/(\\d+)/PublicRegistrations.*", "\\1", comp_id)
  comp_id
}

get_competitors <- function(comp_id) {
  base_comp_url <- "https://www.ibjjfdb.com/ChampionshipResults/{comp_id}/PublicRegistrations?lang=en-US"
  comp_page <- read_html(glue(base_comp_url))

  comp_name <- comp_page %>%
    html_nodes("#content > h2") %>%
    html_text()

  divisions <- comp_page %>%
    html_nodes("#content > div")

  all_competitors <- lapply(divisions, function(div) {
    div_name <- div %>%
      html_nodes("h4") %>%
      html_text()
    if(length(div_name) == 0) return(NULL)

    competitors <- div %>%
      html_nodes("table") %>%
      html_table() %>%
      as.data.frame() %>%
      filter(!grepl("^Total\\: \\d+", X1)) %>%
      select(
        Competitor = X2,
        Team = X1
      ) %>%
      mutate(
        Division    = div_name,
        Competition = comp_name,
        comp_id     = comp_id,
        created_at  = Sys.time(),
        updated_at  = Sys.time()
      )

    return(competitors)
  }) %>%
    bind_rows()
}

get_results <- function(comp_id) {
  result_url <- "https://www.ibjjfdb.com/ChampionshipResults/{comp_id}/PublicResults"
  comp_page <- read_html(glue(result_url))

  comp_name <- comp_page %>%
    html_nodes("#content > div:nth-child(1) > h2") %>%
    html_text()
  comp_name <- gsub("(^[\r\n ]*)|([\r\n ]*$)", "", comp_name)

  divisions <- comp_page %>%
    html_nodes("#content > div:nth-child(3) > div") %>%
    html_nodes("div, h4")

  e <- environment()
  div_name <- NA

  all_results <- lapply(divisions, function(cur_node) {
    css_name <-  html_name(cur_node)
    if(css_name == "h4") {
      div_name <- html_text(cur_node)
      div_name <- gsub("\r\n", "", div_name)
      div_name <- gsub(" +", " ", div_name)
      assign("div_name", div_name, envir = e)
      out <- NULL
    } else {
      result <- html_text(cur_node)
      result <- gsub("\r\n\t\t -\r\n *", "|", result)
      result <- gsub("\r|\n|\t", "", result)
      result <- gsub("(^ *)|( *$)", "", result)
      result <- strsplit(result, "|", fixed = TRUE)[[1]]
      out <- data.frame(
        Place       = result[1],
        Competitor  = result[2],
        Team        = result[3],
        Division    = get("div_name", envir = e),
        Competition = comp_name,
        comp_id     = comp_id,
        created_at  = Sys.time(),
        updated_at  = Sys.time(),
        stringsAsFactors = FALSE
      )
    }
    return(out)
  }) %>% bind_rows()
}

# Get all ids and get results and competitors
all_comp_ids <- get_all_comp_ids()

all_comps <- lapply(all_comp_ids, get_competitors) %>%
  bind_rows()

all_results <- lapply(all_comp_ids, get_results) %>%
  bind_rows()

save(all_comps, all_results, file = "ibjjf_competitors.RData")
