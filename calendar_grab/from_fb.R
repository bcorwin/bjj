# NAGA = 368750393177848
# GI = 417708001597764
# GG = 437442429622674
# Fuji = 445765478821333

get_fb_calendar <- function(group_id, federation) {
  require(httr)
  require(dplyr)

  res <- GET(paste0("https://graph.facebook.com/v2.11/", group_id,
                    "/events?access_token=", Sys.getenv("ACCESS_TOKEN")))
  content(res)

  next_page <- content(res, as="parsed")$paging["next"]

  res_data <- content(res, as="parsed")$data

  calendar <- lapply(res_data, function(curr) {
    event <- data.frame(
      federation = federation,
      name       = curr$name,
      start_date = as.Date(as.POSIXct(curr$start_time)),
      end_date   = as.Date(as.POSIXct(ifelse(is.null(curr$end_time),
                                             NA, curr$end_time))),
      location   = ifelse(is.null(curr$place$name),
                          NA, curr$place$name),
      city       = ifelse(is.null(curr$place$location$city),
                          NA, curr$place$location$city),
      state      = ifelse(is.null(curr$place$location$state),
                          NA, curr$place$location$state),
      stringsAsFactors = FALSE
    )
  }) %>%
    bind_rows()
}


