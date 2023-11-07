# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data preparation script for ch3.rmd (TB prevention and screening)
# Hazim Timimi, May-June 2023
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load packages ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

library(dplyr)
library(tidyr)
library(here)


# Set the report year ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

report_year <- 2023


# Load functions ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

source(here("import/load_gtb.R"))
source(here("report/functions/country_region_lists.R"))
source(here("report/functions/NZ.R"))


# Load GTB data ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

countries <- load_gtb("cty")
notification <- load_gtb("tb")
tpt <- load_gtb("tpt")
TBHIV_for_aggregates <- load_gtb("agg")
estimates_ltbi <- load_gtb("ltbi")
strategy <- load_gtb("sty")
estimates_population <- load_gtb("pop")

who_region_shortnames <- region_shortnames()
# 30 high TB countries
hbtb30 <- hb_list("tb")

# 30 high TB/HIV country list
hbtbhiv30 <- hb_list("tbhiv")

snapshot_date <- latest_snapshot_date()



# Calculate estimated number of HH contacts of all ages eligible for preventive therapy
# Use average cluster size of active TB per household = 1.06 (1.04-1.08) as per technical appendix
#
# Number of affected households = number notified / average cluster size
# Therefore number of contacts = number of households * (average household size - average cluster size)
# The - part in parentheses is to remove the TB cases because they are not contacts
#
# Eventually this will go into a database view


estimates_contacts <- filter(estimates_ltbi, year >= report_year - 2) |>

  mutate(e_hh_contacts = (c_notified_pulm_labconf/1.06) * (e_hh_size - 1.06),
         e_hh_contacts_lo  = (c_notified_pulm_labconf/1.08) * (e_hh_size - 1.08),
         e_hh_contacts_hi= (c_notified_pulm_labconf/1.04) * (e_hh_size - 1.04)) |>

  select(iso3, year, c_notified_pulm_labconf, e_hh_contacts, e_hh_contacts_lo, e_hh_contacts_hi)


# Get the incidence estimates
load(here("inc_mort/analysis/est.rda"))
est_date <- attr(est, which = "timestamp")




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Create dataframe for figure 3.1 ----
# (Bar chart showing numbers provided with TB preventive treatment each year since 2015)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f3.1_data <- filter(notification, year %in% seq(2015, report_year - 1)) |>
  select(iso2,
         year,
         hiv_ipt_reg_all,
         hiv_ipt,
         # These next ones introduced dcyear 2021 by GAM to replace the previous ones. The first two kept for dcyear 2022 onwards
         hiv_all_tpt,
         hiv_new_tpt,
         hiv_elig_all_tpt,
         hiv_elig_new_tpt) |>

  # Create calculated variables for TPT among all enrolled in HIV care
  # filling in gaps for missing data
  # The choice for 2020 GAM data is rather murky ...

  mutate(

    # TPT for all currently enrolled in HIV care (which in theory includes people newly enrolled in care)
    hiv_tpt = case_when(

      year < 2017 ~ hiv_ipt,

      year %in% 2017:2019 ~ coalesce(hiv_ipt_reg_all, hiv_ipt),

      year == 2020 ~ coalesce(hiv_all_tpt, hiv_elig_all_tpt, hiv_new_tpt, hiv_elig_new_tpt),

      year > 2020 ~ coalesce(hiv_all_tpt, hiv_new_tpt)

    )

  ) |>

  select(iso2, year, hiv_tpt) |>

  # Join to tpt data for household contacts in the contacts/tpt view
  inner_join( select(tpt,
                     iso2, year,
                     # next one added 2016 dcyear
                     newinc_con04_prevtx,
                     # next one used 2018 dcyear only
                     newinc_con5plus_prevtx,
                     # next one added 2019 dcyear
                     newinc_con_prevtx), by=c("iso2", "year")) |>

  # Calculate the "5 and over" fraction of tpt for household contacts
  mutate(prevtx_5plus = ifelse(NZ(newinc_con_prevtx) > 0 & NZ(newinc_con04_prevtx) > 0,
                               newinc_con_prevtx - newinc_con04_prevtx,
                               newinc_con_prevtx)) |>

  # Convert negative prevtx_5plus caused by weird combination of carry overs to zero
  mutate(prevtx_5plus = ifelse(NZ(prevtx_5plus) < 0 , 0, prevtx_5plus)) |>

  # deal with 2017 variable
  mutate(prevtx_5plus = ifelse(year == 2017 ,
                               newinc_con5plus_prevtx,
                               prevtx_5plus)) |>

  # Keep variables for HIV, contacts < 5 and contacts 5 plus
  select(iso2, year,
         hiv_tpt,
         house_con04_tpt = newinc_con04_prevtx,
         house_con5plus_tpt = prevtx_5plus) |>

  # Calculate the global totals by year ready for the plot
  group_by(year) |>
  summarise_at(vars(-iso2), sum, na.rm=TRUE) |>
  ungroup() |>

  # Finally, switch to a long format ready for plotting
  pivot_longer(cols = hiv_tpt:house_con5plus_tpt,
               names_to = "TPT_category",
               values_to = "how_many")

# Create summary stats for the text
f3.1_txt <- f3.1_data |>
  group_by(year) |>
  summarise(tot_tpt = sum(how_many)) |>
  pivot_wider(names_from = year,
              names_prefix = "tot_tpt_",
              values_from = tot_tpt) |>
  mutate(delta_19_20 = tot_tpt_2020 - tot_tpt_2019,
         pct_21_20 = abs(tot_tpt_2021 - tot_tpt_2020) * 100 / tot_tpt_2020,
         tot_tpt_18_21 = tot_tpt_2018 + tot_tpt_2019 + tot_tpt_2020 + tot_tpt_2021) |>
  mutate(pct_tpt_target = tot_tpt_18_21 * 100 / 30e6)

# Add stats just for household contacts aged 5 and over
f3.1_txt <- filter(f3.1_data, TPT_category == "house_con5plus_tpt" & year > 2019) |>
  group_by(year) |>
  summarise(tot_con_tpt = sum(how_many)) |>
  pivot_wider(names_from = year,
              names_prefix = "con_5plus_tpt_",
              values_from = tot_con_tpt) |>
  cbind(f3.1_txt)

# Add stats just for household contacts aged under 5 for last two years
f3.1_txt <- filter(f3.1_data, TPT_category == "house_con04_tpt" & year >= report_year - 2) |>
  group_by(year) |>
  summarise(tot_con_tpt = sum(how_many)) |>
  pivot_wider(names_from = year,
              names_prefix = "con04_tpt_",
              values_from = tot_con_tpt) |>
  mutate(con04_22_21_pct = abs(con04_tpt_2022 - con04_tpt_2021) * 100 / con04_tpt_2021) |>
  cbind(f3.1_txt)


# Add total HH contacts for last two years
f3.1_txt <- filter(f3.1_data, TPT_category != "hiv_tpt" & year >= report_year - 2) |>
  group_by(year) |>
  summarise(total_contacts_tpt = sum(how_many)) |>
  pivot_wider(names_from = year,
              names_prefix = "con_tpt_",
              values_from = total_contacts_tpt) |>
  cbind(f3.1_txt)

# Add PLHIV for last two years
f3.1_txt <- filter(f3.1_data, TPT_category == "hiv_tpt" & year >= report_year - 2) |>
  group_by(year) |>
  summarise(hiv_tpt = sum(how_many)) |>
  pivot_wider(names_from = year,
              names_prefix = "hiv_tpt_",
              values_from = hiv_tpt) |>
  cbind(f3.1_txt)


# Get list of high burden countries with big changes in TPT provision among contacts aged 5 and over
f3.1_txt_big_changes <- filter(tpt, year >= report_year - 2) |>
  # Calculate the "5 and over" fraction of tpt for household contacts
  mutate(prevtx_5plus = ifelse(NZ(newinc_con_prevtx) > 0 & NZ(newinc_con04_prevtx) > 0,
                               newinc_con_prevtx - newinc_con04_prevtx,
                               newinc_con_prevtx)) |>

  select(iso3, country, year, prevtx_5plus) |>
  arrange(country, year) |>
  mutate(pct_change = ifelse(lag(country) == country & NZ(lag(prevtx_5plus)) > 0,
                             (prevtx_5plus - lag(prevtx_5plus)) * 100 /lag(prevtx_5plus),
                               NA)) |>
  # Dennis wants to include countries that had not reported in the previous year but have now
  # So give them a fake 111%
  mutate(pct_change = ifelse(lag(country) == country & NZ(lag(prevtx_5plus)) == 0 & NZ(prevtx_5plus) > 0,
                             111,
                             pct_change)) |>
  filter(pct_change > 100 ) |>
  inner_join(hbtb30, by = "iso3") |>
  # Exclude Lesotho because their reported number is for age 0-14 but they couldn't separate out the
  # 5 plus age group
  filter(iso3 != "LSO") |>
  arrange(desc(prevtx_5plus))

f3.1_txt_big_changes_list <- f3.1_txt_big_changes |>
  filter(prevtx_5plus > 5e4) |>
  arrange(country)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 3.2 ----
# (Irwin's doughnuts -- % completion of UNHLM targets)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f3.2_data <- filter(f3.1_data, year>=2018) |>
  group_by(TPT_category) |>
  summarise(tot=sum(how_many)) |>
  ungroup()

# Add total for all categories
f3.2_data <- f3.2_data |>
  summarise(tot = sum(tot)) |>
  mutate(TPT_category = "all_tpt") |>
  rbind(f3.2_data)

# Calculate percent of targets reached
f3.2_targets <- data.frame(TPT_category = c('all_tpt', 'hiv_tpt', 'house_con04_tpt', 'house_con5plus_tpt'),
                           target = c(30e6, 6e6, 4e6, 20e6))

f3.2_data <- f3.2_data |>
  inner_join(f3.2_targets, by = "TPT_category") |>
  mutate(target_completion = tot * 100 / target)


f3.2_txt <- filter(f3.1_data, year>=2018) |>
  group_by(TPT_category) |>
  summarise(tot=sum(how_many)) |>
  pivot_wider(names_from = TPT_category,
              values_from = tot) |>
  mutate(all_con = house_con04_tpt + house_con5plus_tpt) |>
  # Calculate percent of target
  mutate(con04_target = house_con04_tpt * 100 / 4e6,
         con5plus_target = house_con5plus_tpt * 100 / 20e6,
         all_con_target = all_con * 100 / 24e6)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Create dataframe for figure 3.3 ----
# (Map showing countries using rifapentine for TB preventive treatment)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f3.3_data <- tpt |>
  filter(year == report_year -1) |>

  # Merge with data compiled by Tiziana from Sanofi, GDF and Lupin on countries supplied with rfp
  left_join(read.csv(here("report/ch3_data/rfp_supplied_2023-05-16.csv")), by = "iso3") |>
  select(iso3, tpt_1hp, tpt_3hp, rfp_supplied)  |>

  mutate(var = case_when(
                # Note ignoring Malaysia and Spain that had rfp_supplied == 2 meaning
                # used in clinical trials only
                tpt_1hp == 1 | tpt_3hp == 1 | rfp_supplied == 1 ~ "used",
                tpt_1hp == 0 | tpt_3hp == 0 ~ "not_used",
                tpt_1hp == 3 | tpt_3hp == 3 ~ NA,
                .default = NA)) |>

  mutate(var = factor(var,
                      levels = c("used",
                                 "not_used"),
                      labels = c("Used",
                                 "Not used")))

# Get list of mismatches between Tiziana's data and what countries reported to us
f3.3_mismatches <- f3.3_data |>
  filter( ((tpt_1hp == 1 | tpt_3hp == 1) & NZ(rfp_supplied) == 0 ) |
          (NZ(tpt_1hp) == 0 & NZ(tpt_3hp) == 0 & rfp_supplied == 1 )   ) |>
  inner_join(countries, by = "iso3") |>
  select(country, iso3, tpt_1hp, tpt_3hp, rfp_supplied)

# Save list of mismatches for later inspection
readr::write_csv(f3.3_mismatches, file=here("report/local/f3.3_mismatches.csv"), na = "")

# Summary for the text
f3.3_txt <- filter(f3.3_data, var=="Used") |>
  summarise(countries_used = n())

# Get number of countries and people treated from the GTB database
rif_nums <- tpt |>
  filter(year == report_year - 1 & tpt_short_regimens_used == 1) |>
  select(iso3, tpt_short_regimens_used,
         tpt_1hp,
         tpt_3hp,
         tpt_3hr,
         tpt_4r,
         prevtx_short_rifamycin)

# Stats for countries reporting numbers of people
f3.3_txt$people_short_rfp <- filter(rif_nums, prevtx_short_rifamycin > 0) |>
  select(prevtx_short_rifamycin) |>
  sum()

f3.3_txt$countries_short_rfp <- filter(rif_nums, prevtx_short_rifamycin > 0) |>
  nrow()


# Stats for countries reporting regimen type (some didn;t report numbers of people)
f3.3_txt$tpt_1hp <- filter(rif_nums, tpt_1hp == 1) |>
  nrow()

f3.3_txt$tpt_3hp <- filter(rif_nums, tpt_3hp == 1) |>
  nrow()

f3.3_txt$rifampicin <- filter(rif_nums, tpt_3hr == 1 | tpt_4r == 1) |>
  nrow()

f3.3_txt$tpt_3hr <- filter(rif_nums, tpt_3hr == 1) |>
  nrow()

f3.3_txt$tpt_4r <- filter(rif_nums, tpt_4r == 1) |>
  nrow()

# Get list of countries where rfp has been registed for use
f3.3_footnote <- countries |>
  # Merge with data compiled by Tiziana from Sanofi, GDF and Lupin on countries where
  # rfp has been registered
  inner_join(read.csv(here("report/ch3_data/rfp_registered_2023-05-16.csv")), by = "iso3") |>
  select(country) |>
  arrange(country)





# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Create dataframe for figure 3.4  ----
# (Map showing evaluation for TB disease and TB infection among household contacts of
# bacteriologically confirmed pulmonary TB cases)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f3.4_data <- filter(tpt, year == report_year - 1) |>
  select(iso3,
         country,
         newinc_con,
         newinc_con_screen,
         rev_newinc_con,
         rev_newinc_con_screen) |>

  # Calculate the proportion screened
  mutate(screened_pct = ifelse(!is.na(newinc_con) & newinc_con > 0,
                               newinc_con_screen * 100 / newinc_con,
                               NA)) |>

  # Add any countries that reported from review of patient records
  mutate(screened_pct = ifelse(is.na(screened_pct) & !is.na(rev_newinc_con) & NZ(rev_newinc_con) > 0,
                               rev_newinc_con_screen * 100 / rev_newinc_con,
                               screened_pct))  |>

  # Assign the categories for the map
  mutate(var = cut(screened_pct,
                   c(0, 25, 50, 75, Inf),
                   c('0-24', '25-49', '50-74', '\u226575'),
                   right = FALSE))


# Summary for the text
f3.4_txt <- filter(tpt, year >= report_year - 2 &
                     !is.na(newinc_con) & !is.na(newinc_con_screen)) |>
  select(iso3,
         year,
         newinc_con,
         newinc_con_screen) |>
  group_by(year) |>
  summarise(across(newinc_con:newinc_con_screen, ~ sum(.x, na.rm = TRUE))) |>
  ungroup() |>
  mutate(screened_pct = newinc_con_screen * 100 / newinc_con) |>
  pivot_wider(names_from = year,
              values_from = newinc_con:screened_pct) |>
  mutate(change_con_22_21_pct = abs(newinc_con_2022 - newinc_con_2021) * 100 / newinc_con_2021,
         change_screen_22_21_pct = abs(newinc_con_screen_2022 - newinc_con_screen_2021) * 100 / newinc_con_screen_2021)


# Add estimated number of contacts
f3.4_txt <- filter(estimates_contacts, year == report_year - 1) |>
  summarise(across(e_hh_contacts:e_hh_contacts_hi, ~sum(.x, na.rm = TRUE))) |>
  cbind(f3.4_txt)

# Add estimated number of contacts aged under 5
f3.4_txt <- filter(estimates_ltbi, year >= report_year - 1) |>
  summarise(across(e_prevtx_eligible, ~sum(.x, na.rm = TRUE))) |>
  cbind(f3.4_txt)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Create dataframe for figure 3.5  ----
# (Map showing percentage of household contacts provided with TB preventive treatment)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f3.5_data <- filter(tpt, year == report_year - 1) |>

  left_join(estimates_contacts, by = c("iso3", "year")) |>

  mutate(e_prevtx_pct = ifelse(NZ(e_hh_contacts) > 0,
                               newinc_con_prevtx * 100 / e_hh_contacts,
                               NA)) |>

  mutate(var = cut(e_prevtx_pct,
                   c(0, 25, 50,Inf),
                   c('0-24', '25-49', '\u226550'),
                   right = FALSE)) |>

  select(iso3, var)

f3.5_txt <- filter(tpt, year == report_year - 1) |>

  left_join(estimates_contacts, by = c("iso3", "year")) |>

  summarise(con_tpt = sum(newinc_con_prevtx, na.rm = TRUE),
            e_con = sum(e_hh_contacts, na.rm = TRUE)) |>

  mutate(tpt_pct = con_tpt * 100/e_con)




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Create dataframe for figure 3.6  ----
# (Panel plot showing percentage completion vs number contacts started TPT by WHO region)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f3.6_data <- filter(tpt, year == report_year - 2) |>

  select(country,
         g_whoregion,
         newinc_con_prevtx,
         newinc_con_prevtx_cmplt) |>

  # Calculate completion rate
  mutate(pct_completed = ifelse(!is.na(newinc_con_prevtx_cmplt) & NZ(newinc_con_prevtx) > 0,
                                newinc_con_prevtx_cmplt * 100 /newinc_con_prevtx ,
                                NA)) |>
  # Cap at 100%
  mutate(pct_completed = ifelse(pct_completed > 100, 100, pct_completed)) |>

  # merge with regional names
  inner_join(who_region_shortnames, by = "g_whoregion") |>
  select(-g_whoregion) |>

  # filter out empty lines
  filter(!is.na(pct_completed))


# Summary for the text
f3.6_txt <- f3.6_data |>
  summarise(countries = n(),
            median = median(pct_completed),
            q1 = unname(quantile(pct_completed, 0.25)),
            q3 = unname(quantile(pct_completed, 0.75)))





# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Create dataframe for figure 3.7  ----
# (Bar chart of TPT cascade of care for countries with data for all steps, 2 years before report rear)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f3.7_data <- filter(tpt, year == report_year - 2) |>

  # Get expected number contacts of all ages eligible for preventive therapy

  left_join(estimates_contacts, by = c("iso3", "year")) |>

  select(iso3, year, e_hh_contacts, newinc_con, newinc_con_screen, newinc_con_prevtx, newinc_con_prevtx_cmplt) |>

  # Restrict to countries that have estimated number of contacts, reported number of contacts, number screened, number started on TPT
  # and number that completed TPT
  filter(if_all(everything(), ~ !is.na(.))) |>


  # Calculate aggregates for the plot
  summarise(countries = n(),
            estimated_contacts = sum(e_hh_contacts),
            contacts = sum(newinc_con),
            screened = sum(newinc_con_screen),
            started_tpt = sum(newinc_con_prevtx),
            completed_tpt = sum(newinc_con_prevtx_cmplt))

# Capture the number of countries for the text
f3.7_txt <- select(f3.7_data, contacts_countries = countries)

# Switch data to a long format ready for plotting
f3.7_data <- f3.7_data |>
  select(-countries) |>
  pivot_longer(cols = estimated_contacts:completed_tpt,
               names_to = "stage",
               values_to = "how_many") |>

  # Calculate % of contacts for each stage
  mutate(pct = how_many * 100 /max(how_many)) |>

  mutate(stage = factor(stage,
                        levels = c(
                          "estimated_contacts",
                          "contacts",
                          "screened",
                          "started_tpt",
                          "completed_tpt"),
                        labels = c(
                          "Household contacts\n(estimated)",
                          "Household contacts\n(reported)",
                          "Screened for TB",
                          "Started on TPT",
                          "Completed TPT")))




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Create dataframe for figure 3.8  ----
# (Panel plots showing numbers of people living with HIV provided with TB preventive treatment each year since 2005 by WHO region and globally)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f3.8_data <- filter(notification, year %in% seq(2005, report_year - 1)) |>
  select(iso2,
         g_whoregion,
         year,
         # Next one added dcyear 2018 for TPT among all
         hiv_ipt_reg_all,
         # Next one in use since the early days
         hiv_ipt,
         # These next ones introduced dcyear 2021 by GAM to replace the previous ones. The first two kept for dcyear 2022 onwards
         hiv_all_tpt,
         hiv_new_tpt,
         hiv_elig_all_tpt,
         hiv_elig_new_tpt) |>

  # Create calculated variables for TPT among all enrolled and TPT among newly enrolled
  # filling in gaps for missing data
  # The choice for 2020 GAM data is rather murky ...

  mutate(

    # TPT for all currently enrolled in HIV care (which in theory includes people newly enrolled in care)
    hiv_tpt_all = case_when(

      year < 2017 ~ NA,

      year %in% 2017:2019 ~ coalesce(hiv_ipt_reg_all, hiv_ipt),

      year == 2020 ~ coalesce(hiv_all_tpt, hiv_elig_all_tpt, hiv_new_tpt, hiv_elig_new_tpt),

      year > 2020 ~ coalesce(hiv_all_tpt, hiv_new_tpt)

    ),

    # for people newly enrolled in HV care
    hiv_tpt_new =  case_when(

      year < 2020 ~  hiv_ipt,

      year == 2020 ~ coalesce(hiv_new_tpt, hiv_elig_new_tpt),

      year > 2020 ~ hiv_new_tpt

    )

  )

# Interlude to get the number of countries that reported between 2019 and 2022 to quote in the text
f3.8_txt_reported <- f3.8_data |>
  filter(year >= 2019 & NZ(hiv_tpt_all) > 0) |>
  group_by(year) |>
  summarise(reported = n(),
            provided = sum(hiv_tpt_all)) |>
  ungroup() |>
  pivot_wider(names_from = year,
              values_from = reported:provided)

# Another interlude to get info on the big hitters in the latest year
f3.8_txt_biggies <- f3.8_data |>
  filter(year == report_year - 1 & hiv_tpt_all > 1e5) |>
  inner_join(countries, by = "iso2") |>
  select(country, hiv_tpt_all) |>
  arrange(country)


# Calculate regional aggregates
f3.8_data <- f3.8_data |>
  group_by(year, g_whoregion) |>
  summarise_at(vars(hiv_tpt_all:hiv_tpt_new),
               sum,
               na.rm = TRUE) |>
  ungroup() |>

  # merge with regional names
  inner_join(who_region_shortnames, by = "g_whoregion") |>
  select(-g_whoregion)

# Create global aggregates
f3.8_data_global <- f3.8_data |>
  group_by(year) |>
  summarise_at(vars(hiv_tpt_all:hiv_tpt_new),
               sum,
               na.rm = TRUE) |>
  mutate(entity = 'Global')

# Add global to the regional aggregates
f3.8_data <- rbind(f3.8_data, f3.8_data_global)

# Only want hiv_tpt_all for years after 2016
f3.8_data <- f3.8_data |>
  mutate(hiv_tpt_all = ifelse(year < 2017,
                              NA,
                              hiv_tpt_all))

# Change the entity order
f3.8_data$entity <- factor(f3.8_data$entity,
                           levels = c("Global", "African Region", "Region of the Americas", "South-East Asia Region",
                                      "European Region", "Eastern Mediterranean Region", "Western Pacific Region"))

# Summary for quoting in the text
f3.8_txt <- filter(f3.8_data, entity=="Global" & year %in% c(2019, report_year -1) ) |>
  select(-hiv_tpt_new, -entity) |>
  pivot_wider(names_from = year,
              names_prefix = "hiv_tpt_",
              values_from = hiv_tpt_all)


# Get cumulative total provided with TPT since 2005
f3.8_txt_cumulative <- filter(f3.8_data, entity=="Global" & year %in% 2005:2017) |>
  summarise(hiv_tpt_05_17 = sum(hiv_tpt_new))

f3.8_txt_cumulative <- filter(f3.8_data, entity=="Global" & year >= 2018) |>
  summarise(hiv_tpt_18_now = sum(hiv_tpt_all)) |>
  cbind(f3.8_txt_cumulative) |>
  mutate(hiv_tpt_05_now = hiv_tpt_05_17 + hiv_tpt_18_now)

# Stats on reporting among newly enrolled in HIV care in the latest year
f3.8_txt_newly_enrolled <- filter(notification, year == report_year - 1) |>
  filter(NZ(hiv_new) > 0 & !is.na(hiv_new_tpt)) |>
  mutate(hiv_new_tpt_pct = hiv_new_tpt * 100 / hiv_new ) |>
  summarise(countries = n(),
            median = median(hiv_new_tpt_pct),
            q1 = unname(quantile(hiv_new_tpt_pct, 0.25)),
            q3 = unname(quantile(hiv_new_tpt_pct, 0.75)))





# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Create dataframe for figure 3.9  ----
# (Panel plot showing percentage completion vs number PLHIV started TPT by WHO region)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f3.9_data <- filter(notification, year == report_year - 2) |>

  select(country,
         g_whoregion,
         hiv_all_tpt_started,
         hiv_all_tpt_completed) |>

  # Calculate completion rate
  mutate(pct_completed = ifelse(!is.na(hiv_all_tpt_completed) & NZ(hiv_all_tpt_started) > 0,
                                hiv_all_tpt_completed * 100 /hiv_all_tpt_started ,
                                NA)) |>

  # merge with regional names
  inner_join(who_region_shortnames, by = "g_whoregion") |>
  select(-g_whoregion) |>

  # filter out empty lines
  filter(!is.na(pct_completed))

f3.9_txt <- f3.9_data |>
  summarise(countries = n(),
            median = median(pct_completed),
            q1 = unname(quantile(pct_completed, 0.25)),
            q3 = unname(quantile(pct_completed, 0.75)))




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Create dataframe for figure 3.10  ----
# (Panel plot showing percentage of household contacts had TB disease
# among those screened, by WHO region)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f3.10_data <- filter(tpt, year == report_year - 1) |>

  select(country,
         g_whoregion,
         newinc_con_screen,
         newinc_con_tb) |>

  # Calculate % with TB
  mutate(pct_tb = ifelse(!is.na(newinc_con_tb) & NZ(newinc_con_screen) > 0,
                         newinc_con_tb * 100 /newinc_con_screen ,
                         NA)) |>

  # merge with regional names
  inner_join(who_region_shortnames, by = "g_whoregion") |>
  select(-g_whoregion) |>

  # filter out empty lines
  filter(!is.na(pct_tb))

# Summary for the text
f3.10_txt <- f3.10_data |>
  summarise(countries = n(),
            overall = sum(newinc_con_tb) * 100 / sum(newinc_con_screen),
            median = median(pct_tb),
            q1 = unname(quantile(pct_tb, 0.25)),
            q3 = unname(quantile(pct_tb, 0.75)))

f3.10_txt_countries <- f3.10_data |>
  filter(pct_tb > 3.3 & pct_tb < 4) |>
  arrange(country)

f3.10_txt <- f3.10_txt_countries |>
  summarise(countries_in_range = n()) |>
  cbind(f3.10_txt)




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Create dataframe for figure 3.11 ----
# (Map showing active case finding activity)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f3.11_data <- read.csv(here("report/ch3_data/acf_2023-07-19.csv"))

# restrict to the most recent year of data
f3.11_data <- f3.11_data |>
  group_by(iso3) |>
  filter(year == max(year)) |>
  ungroup()

# To try to account for size of the burden, calculate the
# ratio of number screened to estimated TB incidence number


f3.11_data <- est |>
  filter(year >= report_year - 2 ) |>
  mutate(e_inc_num = inc * pop / 1e5) |>
  select(iso3, year, e_inc_num) |>
  inner_join(f3.11_data, by = c("iso3", "year")) |>
  mutate(screen_inc = ifelse(NZ(e_inc_num) > 0,
                            screened / e_inc_num,
                            NA)) |>
  mutate(var = cut(screen_inc,
                   c(0, 0.5, 5, 10, 100, Inf),
                   c('0-0.49', '0.5-4.9', '5.0-9.9', '10-99', '\u2265100'),
                   right=FALSE)) |>
  inner_join(countries, by = "iso3") |>
  select(iso3, country, year, screen_inc, var)




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Create dataframe for figure 3.12 ----
# (Map showing use of diagnostic tests for TB infection)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Dennis suggested
#
# TST only – there may still be interest to show this for the stragglers
# IGRA (+/-TST), no TBST – this would show where most countries should be expected to be (albeit they should also be thinking of TBST)
# TBST (+/-TST or IGRA) – all but one of them are actually doing both of the old tests too – these would be the trailblazers

f3.12_data <- strategy |>
  select(iso3, g_whoregion, year, igra_used, tst_used, tbst_used) |>
  filter(year == report_year -1) |>

  mutate(var = case_when(
                tst_used==1 & NZ(igra_used)!=1 & NZ(tbst_used)!=1 ~ "tst",
                igra_used==1 & NZ(tbst_used)!=1 ~ "igra_tst",
                tbst_used==1 ~ "tbst_igra_tst",
                tst_used==0 & igra_used==0 & tst_used==0 ~ "not_used",
                igra_used==3 | tst_used==3 | tbst_used==3 ~ NA,
                .default = NA
  )) |>

  mutate(var = factor(var,
                      levels = c("tst",
                                 "igra_tst",
                                 "tbst_igra_tst",
                                 "not_used"),
                      labels = c("Tuberculin Skin Tests (TST) only",
                                 "Interferon Gamma Release Assays (IGRA) +/- TST",
                                 "Antigen-based Skin Tests (TBST) +/-TST +/-IGRA",
                                 "Not used")))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Create dataframe for table 3.1 ----
# (Time to start of treatment)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

t3.1_data <- read.csv(here("report/ch3_data/time_start_tx_2023-07-19.csv"))

# format the numerical data
t3.1_data <- t3.1_data |>
  mutate(across(dr:median, gtbreport::ftb)) |>
  # convert dashes from ftb() to em dashes
  # otherwise knitr interprets the dash as the start of a bulleted list
  mutate(across(dr:median, ~ sub("-", "&mdash;", .x))) |>
  mutate(source = ifelse(source==2,
                         "Special research",
                         "Health information system"))






# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Create dataframe for figure 3.13 ----
# (Map showing ratio of TB notification rates among health care workers to those among the adult population)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f3.13_data <- filter(strategy, year == report_year - 1) |>

  select(iso3,
         country,
         hcw_tb_infected,
         hcw_tot)

# Get the total adult population aged between 15 and 64 (exclude those aged 65 and above)
pop_adults <- filter(estimates_population, year == report_year - 1) |>

  select(iso3,
         e_pop_f15plus,
         e_pop_f65,
         e_pop_m15plus,
         e_pop_m65) |>

  mutate(e_pop_adult = e_pop_f15plus + e_pop_m15plus - e_pop_f65 -  e_pop_m65 ) |>

  select(iso3,
         e_pop_adult)

# Get the total notifications among adults aged between 15 and 64 (exclude those aged 65 and above)
notif_adults <- filter(notification, year == report_year - 1) |>

  select(iso3,
         newrel_f15plus,
         newrel_f65,
         newrel_m15plus,
         newrel_m65) |>

  mutate(newrel_adult = newrel_f15plus + newrel_m15plus - NZ(newrel_f65) -  NZ(newrel_m65) ) |>

  select(iso3,
         newrel_adult) |>

  # Join to the adult population
  inner_join(pop_adults, by = "iso3")

f3.13_data <- f3.13_data |>

  inner_join(notif_adults, by = "iso3") |>

  # Calculate notification rate ratio
  # Use as.numeric() to avoid integer overflow
  mutate(nrr = ifelse(NZ(hcw_tot) > 0 & NZ(newrel_adult) > 0,
                      (as.numeric(hcw_tb_infected) * as.numeric(e_pop_adult))
                      /
                        (as.numeric(hcw_tot) * as.numeric(newrel_adult)),
                      NA)) |>

  # in previous years I had filtered out countries with fewer than 100 health care workers
  # as the rate ratios jumped around a lot but because these are very small countries they
  # don;t show up in the maps so won't bother anymore

  # Assign the categories for the map
  mutate(var = cut(nrr,
                   c(0, 1, 2, 3, Inf),
                   c('0-0.9', '1-1.9', '2-2.9', '\u22653'),
                   right=FALSE))

# Summary for the text
f3.13_txt <- filter(f3.13_data, hcw_tb_infected>0) |>
  summarise(tot_hcw_tb = sum(hcw_tb_infected, na.rm=TRUE),
            countries_hcw_tb = n())

# Add number with nrr more than one when number of TB among hcw is 5 or more
f3.13_txt <- filter(f3.13_data, nrr > 1 & hcw_tb_infected >= 5) |>
  summarise(countries_nrr = n()) |>
  cbind(f3.13_txt)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Create dataframe for figure 3.14 ----
# (BCG immunisation coverage)
# Next update to be published 18 July 2023
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Look at aggregate values
load(here("data/bcg/bcg_gho_agg.rda"))

f3.14_data <- bcg_gho_agg |>
  left_join(who_region_shortnames, by = c("group_name" = "g_whoregion"))

# Add a global entity factor
levels(f3.14_data$entity) = c(levels(f3.14_data$entity), "Global")
f3.14_data[f3.14_data$group_type=="global", "entity"] <- "Global"

f3.14_data <- f3.14_data |>
  select(entity,
         year,
         bcg_coverage)


# Change the entity order
f3.14_data$entity <- factor(f3.14_data$entity,
                            levels = c("Global", "African Region", "Region of the Americas", "South-East Asia Region",
                                       "European Region", "Eastern Mediterranean Region", "Western Pacific Region"))

f3.14_txt <- bcg_gho_agg |>
  filter(year %in% 2019:(report_year-1) & group_type=="global") |>
  pivot_wider(names_from = year,
              names_prefix = "bcg_cov_",
              values_from = bcg_coverage)


# Show number of reporting countries

# Load the country names
member_states <- countries |>
  filter(g_whostatus == "M") |>
  select(iso3, g_whostatus, g_whoregion)

# Load BCG by country
load(here("data/bcg/bcg_gho_country.rda"))

f3.14_txt_reps <- bcg_gho_country  |>
  filter(year==report_year - 1) |>
  right_join(member_states, by="iso3") |>
  mutate(reported = ifelse(is.na(bcg_coverage), 0, 1)) |>
  select(g_whoregion, iso3, reported)

f3.14_txt_rep_glob <- f3.14_txt_reps |>
  summarise(rep = sum(reported),
            all =  n())

f3.14_txt_rep_reg <- f3.14_txt_reps |>
  group_by(g_whoregion) |>
  summarise(rep = sum(reported),
            all =  n()) |>
  ungroup()

# Clear up
rm(bcg_gho_agg, bcg_gho_country)


