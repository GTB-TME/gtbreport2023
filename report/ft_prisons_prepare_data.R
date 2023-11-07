# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data preparation script for ft_prisons.rmd (TB in prisons)
# Hazim Timimi, July 2023
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load packages ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

library(tidyr)
library(dplyr)
library(stringr)
library(here)


# Set the report year ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

report_year <- 2023


# Load functions ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

source(here("report/functions/calculate_outcomes.R"))
source(here("report/functions/NZ.R"))



# Load data for the featured topic
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

AMR_pris_notif <- read.csv(here("report/ft_prisons_data/AMR_notifications_prisons_2023-07-23.csv"))
EUR_pris_notif <- read.csv(here("report/ft_prisons_data/EUR_notifications_prisons_2023-07-23.csv"))
EUR_pris_outc <- read.csv(here("report/ft_prisons_data/EUR_outcomes_prisons_2023-07-23.csv"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Create dataframe for figure 1 ----
# (Bar chart number of TB episodes in prisons in European region by year since 2014)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

EUR_prisoners_tb <- EUR_pris_notif |>
  # Identify the countries that have reported number of prisoners with TB for all years since 2014
  # and that have reported the total number of prisoners for at least one of the years
  # filter(!is.na(newrel_prisoners)) |>
  # group_by(iso2) |>
  # summarise(recs = n(),
  #           prisoners = sum(pris, na.rm = TRUE)) |>
  # ungroup() |>
  # filter(recs == report_year - 2014 & NZ(prisoners) > 0) |>
  # select(iso2) |>
  # Now we have the list of countries that reported TB for all years, and at least one year for
  # the total number of prisoners filter the original data frame
  # inner_join(EUR_pris_notif, by = "iso2") |>
  # group_by(iso2) |>
  # use locf via tidyr::fill() to replace missing numbers of total prisoners per country
  # fill(pris, .direction = "downup") |>
  # ungroup() |>
  select(iso2, year, pris, tb_pris = newrel_prisoners)

rm(EUR_pris_notif)

f1_data <- EUR_prisoners_tb |>
  # calculate the number of prisoners with TB for each year
  group_by(year) |>
  summarise_at(vars(pris,tb_pris), sum, na.rm = TRUE) |>
  mutate(pris_rate = tb_pris/pris*1e5) |>
  ungroup()

# A few totals for the text
countries <- unique(EUR_prisoners_tb$iso2) |> length()

f1_txt <- EUR_prisoners_tb |>
  # calculate the number of prisoners for each year
  group_by(year) |>
  summarise(prisoners = sum(pris, na.rm = TRUE)) |>
  ungroup() |>
  # calculate the average number of prisoners during this period
  summarise(pris_mean = mean(prisoners),
            pris_min = min(prisoners),
            pris_max = max(prisoners)) |>
  cbind(countries)

rm(countries)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Create dataframe for figure 2 ----
# (Bar chart number of TB episodes in prisons in region of the Americas by year since 2018)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

AMR_prisoners_tb <- AMR_pris_notif |>
  # Identify the countries that have reported number of prisoners with TB for all years since 2018
  # filter(!is.na(newrel_prisoners)) |>
  # group_by(iso2) |>
  # summarise(recs = n()) |>
  # ungroup() |>
  # filter(recs == report_year - 2018) |>
  # select(iso2) |>
  # # Now we have the list of countries that reported TB for all years
  # # filter the original data frame
  # inner_join(AMR_pris_notif, by = "iso2") |>
  select(iso2, year, tb_pris = newrel_prisoners)

rm(AMR_pris_notif)

f2_data <- AMR_prisoners_tb |>
  # calculate the number of prisoners with TB for each year
  group_by(year) |>
  summarise(tb_pris = sum(tb_pris, na.rm = TRUE)) |>
  ungroup()

# Number of countries for the text
f2_txt <- unique(AMR_prisoners_tb$iso2) |> length()



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Create dataframe for figure 3 ----
# (Horizontal bar chart of treatment outcomes in prisoners in the European region by year since 2012)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

EUR_prisoners_outc <- EUR_pris_outc |>
  # Identify the countries that reported outcomes for at least one new and relapse cases for all years since 2012
  # filter(NZ(pris_newrel_coh) > 1) |>
  # group_by(iso2) |>
  # summarise(recs = n()) |>
  # ungroup() |>
  # filter(recs == report_year - 1 - 2012)  |>
  # select(iso2) |>
  # # Now we have the list of countries that reported outcomes for all years
  # # filter the original data frame
  # inner_join(EUR_pris_outc, by = "iso2") |>
  select(iso2, year, pris_newrel_coh:pris_newrel_neval)

rm(EUR_pris_outc)

f3_data <- EUR_prisoners_outc |>
  # Calculate aggregates
  group_by(year) |>
  summarise(across(pris_newrel_coh:pris_newrel_neval, sum, na.rm=TRUE)) |>
  ungroup() |>

  # Rename the not evaluated variable to match what calculate_outcomes_pct() expects
  mutate(pris_newrel_c_neval = pris_newrel_neval) |>
  select(-pris_newrel_neval) |>

  # Calculate outcome proportions for plotting as stacked bars
  calculate_outcomes_pct("pris_newrel_") |>

  # Drop the actual numbers and keep percentages
  select(-coh, -succ, -fail, -died, -lost, -c_neval) |>

  # flip into long format
  pivot_longer(cols = `Treatment success`:`Not evaluated`,
               names_to = "outcome")



# Number of countries for the text
f3_txt <- unique(EUR_prisoners_outc$iso2) |> length()
