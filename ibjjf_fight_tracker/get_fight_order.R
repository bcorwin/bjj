# To do:
# Create website to view results for a set of fighters
# Website should auto-refresh every few mins (include timestamp of latest refresh)
# Get set of fighters from division and name in a google sheet?

# Example:
# day1 <- get_fight_order("http://www.bjjcompsystem.com/tournaments/888/tournament_days/140")
# day2 <- get_fight_order("http://www.bjjcompsystem.com/tournaments/888/tournament_days/141")

get_urls <- function() {
  require(readr)
  url_list <- read_lines("list_urls.txt")
}

get_competitors <- function() {
  require(readr)
  competitors <- read_csv("list_competitors.csv")
}

get_fight_order <- function(order_url) {
  require(rvest)
  require(dplyr)

  order_selector <- "body > div.tournament-day > div.tournament-day__grid > div > div.sliding-columns__columns > li"
  date_selector <- "body > div.tournament-day > div.tournament-day__filters > div > ul > li.filter-column__tab.filter-column__tab--active > a > div > span"

  order_page <- read_html(order_url)

  all_nodes <- order_page %>%
    html_nodes(order_selector)

  # Tournament date
  tourney_date <- order_page %>%
    html_nodes(date_selector) %>%
    html_text()
  tourney_date <- gsub("(?i) *\\(\\d+ mats\\) *", "", tourney_date)
  tourney_date <- as.Date(tourney_date, format = "%A, %m/%d")

  all_mats <- lapply(seq_along(all_nodes), function(col_num) {
    # Mat number
    mat_num <- all_nodes[col_num] %>%
      html_nodes("div.grid-column__header") %>%
      html_text()

    # Division
    divisions <- all_nodes[col_num] %>%
      html_nodes("div.match-header__category-name") %>%
      html_text()

    # Time
    times <- all_nodes[col_num] %>%
      html_nodes("span.match-header__when") %>%
      html_text()

    # Phase
    phases <- all_nodes[col_num] %>%
      html_nodes("span.match-header__phase") %>%
      html_text()

    # Competitors
    fights <- all_nodes[col_num] %>%
      html_nodes("div.match-card.match-card--blue.panel.panel-default")

    competitors <- lapply(seq_along(fights), function(fight_num) {
      known <- fights[fight_num] %>%
        html_nodes("div.match-card__competitor-name") %>%
        html_text()
      unknown <- fights[fight_num] %>%
        html_nodes(".match-card__child-where") %>%
        html_text()
      unknown <- setdiff(unknown, "-")
      out <- c(known, unknown)
    })

    out <- data.frame(
      mat         = rep(mat_num, length(divisions)),
      time        = times,
      division    = divisions,
      phase       = phases,
      competitor1 = sapply(competitors, function(x) x[[1]]),
      competitor2 = sapply(competitors, function(x) x[[2]]),
      stringsAsFactors = FALSE
    ) %>%
      mutate(
        mat   = gsub("\\n", "", mat),
        phase = gsub("\\(|\\)", "", phase)
      )
  }) %>%
    bind_rows()

  out <- all_mats %>%
    mutate(
      fight_num  = as.numeric(gsub("^(?i).*\\: Fight (\\d+)$", "\\1", time)),
      fight_time = paste(tourney_date,
                         gsub("^(?i)(.*)\\: Fight \\d+$", "\\1", time)),
      fight_time = as.POSIXct(fight_time, format = "%Y-%m-%d %I:%M %p")
      ) %>%
    select(-time)
}

remove_file <- function(filename) {
  if(file.exists(filename)) {
    file.remove(filename)
  }
}

refresh_data <- function() {
  require(dplyr)

  url_list    <- get_urls()
  competitors <- get_competitors()

  latest_update <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")

  all_fights <- lapply(url_list, get_fight_order) %>%
    bind_rows()

  important_fights <- all_fights %>%
    inner_join(competitors, by = c("division")) %>%
    filter(competitor1 == name | competitor2 == name) %>%
    select(-name)

  remove_file("all_fights.csv")
  remove_file("important_fights.csv")
  remove_file("latest_update.txt")

  write_csv(all_fights, "all_fights.csv")
  write_csv(important_fights, "important_fights.csv")
  write_lines(latest_update, "latest_update.txt")
}
