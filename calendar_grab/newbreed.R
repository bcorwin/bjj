get_newbreed_calendar <- function() {
  require(rvest)
  require(dplyr)

  CALENDR_URL <- "http://www.newbreedgear.com/NEWBREED_REGISTRATION_s/98.htm"
  EVENT_REGEX <- "(?i)^([[:alpha:]]+ \\d{1,2}, \\d{4}) - ([[:alpha:] ]+), ([[:alpha:]]{2})$"

  page <- read_html(CALENDR_URL)
  calendar_table <- html_nodes(x = page, xpath = '//*[@id="content_area"]/table[1]')

  calendar0 <- html_table(calendar_table, fill = TRUE)[[1]]

  calendar <- calendar0 %>%
    select(raw = X1) %>%
    mutate(is_event = grepl(EVENT_REGEX, raw)) %>%
    filter(is_event == TRUE) %>%
    mutate(
      federation = "New Breed",
      date  = as.Date(gsub(EVENT_REGEX, "\\1", raw), "%B %d, %Y"),
      city  = toupper(gsub(EVENT_REGEX, "\\2", raw)),
      state = toupper(gsub(EVENT_REGEX, "\\3", raw))
    ) %>%
    select(federation, date, city, state)
}
