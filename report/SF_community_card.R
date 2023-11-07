# Calculate comunity contribution statistic for a statistic card in SItefinity
# Hazim Timimi, November 2023


library(dplyr)
library(here)

# Load functions ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

source(here("import/load_gtb.R"))


# Set the report year ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
report_year <- 2023
snapshot_date <- latest_snapshot_date()

# Load functions ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

source(here("import/load_gtb.R"))
source(here("report/functions/NZ.R"))


# Load GTB data ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

strategy <- load_gtb("sty")

community <- strategy |>
  filter(year==report_year-1 & !is.na(notified_ref) & !is.na(notified_ref_community)) |>
  select(country, year, notified_ref, notified_ref_community)


community_stat <- community |>
  summarise(across(notified_ref:notified_ref_community, \(x) sum(x, na.rm = TRUE))) |>
  mutate(contribution = notified_ref_community * 100 / notified_ref)


# Print text for the SF card to the console
message(paste0(round(community_stat$contribution),
              "% of TB notifications in ",
              report_year - 1,
              " were attributed to community referrals in ",
              nrow(community),
              " countries reporting these data"))
