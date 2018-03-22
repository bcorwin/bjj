setwd("/Users/ben.corwin/Documents/GitHub/personal/bjj/")

readRenviron(".Renviron")

source("calendar_grab/from_fb.R")
source("calendar_grab/ibjjf.R")
source("calendar_grab/newbreed.R")

naga_calendar <- get_fb_calendar("368750393177848", "NAGA")
gi_calendar   <- get_fb_calendar("417708001597764", "Grappling Industries")
gg_calendar   <- get_fb_calendar("437442429622674", "Grappling Games")
fuji_calendar <- get_fb_calendar("445765478821333", "Fuji")
ibjjf_cal     <- get_ibjjf_calendar()
nb_cal        <- get_newbreed_calendar()

fb_cals <- bind_rows(
  naga_calendar,
  gi_calendar,
  gg_calendar,
  fuji_calendar
)

all_cals <- bind_rows(
   fb_cals %>%
    select(
      federation,
      name = name,
      start_date,
      end_date,
      city,
      state),
  ibjjf_cal %>%
    mutate(state = NA) %>%
    select(
      federation,
      name = championship,
      start_date,
      end_date,
      city,
      state
    ),
  nb_cal %>%
    mutate(
      name = paste("New Breed", city),
      end_date = date
    ) %>%
    select(
      federation,
      name,
      start_date = date,
      end_date,
      city,
      state
    )
) %>%
  filter(end_date >= Sys.Date()) %>%
  arrange(start_date) %>%
  distinct()


# To do:
# Extract city and state when missing
# When possible, try to read website in addition to FB to get more info
# Clean up names
# Add links to FB event and/or registration
