# To do:
# Parse datetime out of times
# Get date of fight for above parsing
# Create website to view results for a set of fighters
# Website should auto-refresh every few mins (include timestamp of latest refresh)
# Get set of fighters from division and name in a google sheet?

# Example:
# day1 <- get_fight_order("http://www.bjjcompsystem.com/tournaments/888/tournament_days/140")
# day2 <- get_fight_order("http://www.bjjcompsystem.com/tournaments/888/tournament_days/141")

get_fight_order <- function(order_url) {
  require(rvest)
  require(dplyr)
  
  order_selector <- "body > div.tournament-day > div.tournament-day__grid > div > div.sliding-columns__columns > li"
  
  all_nodes <- read_html(order_url) %>%
    html_nodes(order_selector)
  
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
}