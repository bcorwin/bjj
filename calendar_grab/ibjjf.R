get_ibjjf_calendar <- function() {
  require(rvest)
  require(dplyr)
  require(zoo)

  CALENDR_URL <- "http://ibjjf.com/championships/calendar/"


  page <- read_html(CALENDR_URL)
  calendar_table <- html_nodes(x = page, xpath = '//*[@id="content"]/div/table')

  calendar0 <- html_table(calendar_table)[[1]]
  calendar <- calendar0 %>%
    mutate(
      is_date  = toupper(X1) %in% toupper(month.name),
      is_head  = toupper(X1) == toupper("Federation"),
      is_note  = grepl("^ *\\*", X1),
      is_blank = X1 == "",

      month = ifelse(is_date, match(X1, toupper(month.name)), NA),
      month = na.locf(month),
      year  = format(Sys.Date(), "%Y"),

      federation = X1,
      championship = X3,
      city = X4,
      location = X5,

      raw_dates = gsub("\\*| ", "", X2),
      start_day = sapply(raw_dates, function(x) {
        out <- strsplit(x, "/")[[1]][1]
        suppressWarnings(as.numeric(out))
      }),
      end_day = sapply(raw_dates, function(x) {
        out <- strsplit(x, "/")[[1]][-1]
        ifelse(length(out) > 1,
               suppressWarnings(as.numeric(out)),
               NA)
      }),
      start_date = as.Date(paste(year, month, start_day), "%Y %m %d"),
      end_date   = as.Date(paste(year, month, end_day), "%Y %m %d"),
      end_date   = coalesce(end_date, start_date),

      notes = ifelse(is_note, X1, NA),
      notes = lead(notes)
    ) %>%
    filter(!is_date, !is_head, !is_note, !is_blank) %>%
    select(start_date, end_date, city, location, championship, federation, notes)
}
