# - - - - - - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - -
# Data preparation script for ch1.3.rmd (Drug-resistant TB)
# Hazim Timimi, June-July 2023
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Load data packages ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

library(stringr)
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



# Load TB data  ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

drnew <- load_gtb("drnew") |>
  # We only use data from the year 2000
  filter(year_new >= 2000)

pop <- load_gtb("pop")


# Get global and regional DR-TB estimates directly from the files that get imported into the database
load(here("drtb/dboutput/db_dr_country.rda"))
load(here("drtb/dboutput/db_dr_group.rda"))
load(here("drtb/dboutput/FQR_in_RR_global.rda"))
# Global mortality estimates:
load(here("drtb/dboutput/global.mort.rr.rda"))
# Global isoniazid resistance estimates:
load(here("drtb/dboutput/HR_global.rda"))

# Get incidence estimates
load(here("inc_mort/analysis/est.rda"))

est <- est |>
  filter(year == report_year -1) |>
  select(iso3, inc.num)


# Find out when these data had been created
snapshot_date <- latest_snapshot_date()
estimates_date <- attr(db_dr_country, "timestamp")


# Create a set of WHO region short names to use in figures and tables
who_region_shortnames <- region_shortnames()


# Get list of the 30 high MDR-TB burden countries (used to filter records for some figures)
hbmdr30 <- hb_list("mdr")
hbtb <- hb_list("tb")
hbtb_hbmdr <- rbind(hbmdr30, hbtb) |>
  select(iso3) |>
  unique()

# Load the country names
country_names <- load_gtb("cty") |>
  select(iso3, country)




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Create dataframe for figure 1.3.1 ----
# (Line chart of RR-TB incidence estimates globally since 2015)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f1.3_01_data <- db_dr_group |>
  filter(group_type == 'global' & year>=2015) |>
  mutate(entity = 'Global') |>
  select(year,
         entity,
         e_inc_rr_num,
         e_inc_rr_num_lo,
         e_inc_rr_num_hi)

# Summary dataset for simple quoting of numbers in the text
f1.3_01_txt <- f1.3_01_data |>
  arrange(year) |>
  filter(year >= report_year - 2) |>
  # Calculate % change between the last two years
  mutate(previous = lag(e_inc_rr_num)) |>
  mutate(pct_diff = abs(e_inc_rr_num - previous)*100/previous)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Create dataframe for figure 1.3.2 ----
# (Panel plot of global proportion of TB cases with MDR/RR-TB)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f1.3_02_data <- db_dr_group |>
  filter(group_name=="global") |>
  mutate(pct_new_best    = e_rr_prop_new * 100,
         pct_new_lo = e_rr_prop_new_lo * 100,
         pct_new_hi = e_rr_prop_new_hi * 100,
         pct_ret_best    = e_rr_prop_ret * 100,
         pct_ret_lo = e_rr_prop_ret_lo * 100,
         pct_ret_hi = e_rr_prop_ret_hi * 100) |>
  select(year, starts_with("pct_")) |>

  # Switch to a long format
  pivot_longer(cols = starts_with("pct_"),
               names_to = c("pct", "case_type", "val"),
               names_sep = "_") |>
  select(-pct) |>

  # Switch back to wide but keep case type as identifier
  pivot_wider(names_from = val,
              values_from = value) |>

  # Change case type to a factor with descriptive names
  mutate(case_type = factor(case_type,
                            levels = c("new", "ret"),
                            labels = c("People with no previous history of TB treatment", "People previously treated for TB")))


# Summary dataset for simple quoting of numbers in the text
f1.3_02_txt <- f1.3_02_data |>
  filter(year %in% c(2015, report_year - 1)) |>
  arrange(year, case_type)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Create dataframe for figure 1.3.3 ----
# (Line chart of RR-TB incidence estimates by region since 2015)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f1.3_03_data <- db_dr_group |>
  filter(group_type == 'g_whoregion' & year>=2015) |>
  select(year,
         g_whoregion = group_name,
         e_inc_rr_num,
         e_inc_rr_num_lo,
         e_inc_rr_num_hi) |>

  # merge with regional names
  inner_join(who_region_shortnames, by = "g_whoregion") |>
  select(-g_whoregion) |>

  # Set the entity order for plotting
  mutate(entity = factor(entity,
                         levels = c("African Region", "Region of the Americas", "South-East Asia Region",
                                    "European Region", "Eastern Mediterranean Region", "Western Pacific Region")))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Create dataframe for figure 1.3.4 ----
# (Line chart of RR-TB incidence estimates, 30 high MDR burden countries, since 2015)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  f1.3_04_data <- db_dr_country |>
    inner_join(hbmdr30, by = "iso3")  |>
    inner_join(country_names, by = "iso3") |>
    select(country,
           year,
           e_inc_rr_num,
           e_inc_rr_num_lo,
           e_inc_rr_num_hi) |>
    arrange(country)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Create dataframe for figure 1.3.5 ----
# (Bubble map of estimated incidence of MDR/RR-TB for countries with at least 1000 incident cases)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f1.3_05_data <- db_dr_country |>
  filter(year == report_year - 1 & e_inc_rr_num >= 1000) |>
  select(iso3,
         size = e_inc_rr_num
  )

# Summary dataset for simple quoting of numbers in the text
f1.3_05_txt <- f1.3_05_data |>
  arrange(desc(size)) |>
  inner_join(country_names, by="iso3") |>
  # pick the top 3
  head(3) |>
  # Calculate proportion of 2021 global incidence
  mutate(pct = size * 100 / f1.3_01_data[f1.3_01_data$year==report_year - 1, "e_inc_rr_num"]) |>
  mutate(country = ifelse(country == "Russian Federation", "the Russian Federation", country)) |>
  select(-size)

f1.3_05_top  <- f1.3_05_data |>
  arrange(desc(size)) |>
  inner_join(country_names, by="iso3") |>
  # pick the top two thirds
  head(10) |>
  # Calculate proportion of global incidence
  mutate(pct = size * 100 / f1.3_01_data[f1.3_01_data$year==report_year - 1, "e_inc_rr_num"]) |>
  mutate(pct_cumsum = cumsum(pct)) |>
  filter(pct_cumsum < 67) |>
  select(country, pct_cumsum)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Create dataframe for figure 1.3.6 ----
# (Map showing percentage of new TB cases with MDR/RR-TB )
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f1.3_06_data <- db_dr_country |>
  filter(year == report_year - 1) |>
  # Convert proportion to %
  mutate(var = e_rr_prop_new * 100) |>
  select(iso3, var) |>

  # Assign the categories for the map
  mutate(var = cut(
    var,
    c(0, 3.0, 6.0, 12.0, 20, Inf),
    c('0\u20132.9', '3\u20135.9', '6\u201311.9', '12\u201319.9','\u226520'),
    right = FALSE
  ))


# Summary dataset for simple quoting of numbers in the text
# Text before 2.3.6 refers to regional averages

f1.3_06_txt <- db_dr_group |>
  filter(year==report_year - 1) |>
  arrange(desc(e_rr_prop_new)) |>
  inner_join(who_region_shortnames, by = c("group_name" = "g_whoregion")) |>
  select(g_whoregion = group_name, entity,  year, e_rr_prop_new)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Create dataframe for figure 1.3.7 ----
# (Map showing percentage of previously treated TB cases with MDR/RR-TB )
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f1.3_07_data <- db_dr_country |>
  filter(year == report_year - 1) |>
  # Convert proportion to %
  mutate(var = e_rr_prop_ret * 100) |>
  select(iso3, var) |>

  # Assign the categories for the map
  mutate(var = cut(
    var,
    c(0, 6.0, 12.0, 30.0, 50, Inf),
    c('0\u20135.9', '6\u201311.9', '12\u201329.9', '30\u201349.9','\u226550'),
    right = FALSE
  ))


# Summary dataset for simple quoting of numbers in the text
# Text before 2.3.7 refers to regional averages

f1.3_07_txt <- db_dr_group |>
  filter(year==report_year - 1) |>
  arrange(desc(e_rr_prop_ret)) |>
  inner_join(who_region_shortnames, by = c("group_name" = "g_whoregion")) |>
  select(g_whoregion = group_name, entity,  year, e_rr_prop_ret)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Create dataframe for figure 1.3.8 ----
# (Map showing source of data to estimate RR among new)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f1.3_08_data <- db_dr_country |>
  filter(year == report_year - 1) |>
  select(iso3, var = source_new) |>
  # Set a source of "Model" to NA as technically thisis not a source of data!
  mutate(var = ifelse(var == "Model", NA, var)) |>
  mutate(var = factor(var,
                      levels = c("Survey", "Survey & Surveillance", "Surveillance")))


f1.3_08_txt <- f1.3_08_data |>
  filter(var %in% c("Survey", "Survey & Surveillance", "Surveillance")) |>
  group_by(var) |>
  summarise(countries = n())



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Create dataframe for figure 1.3.9 ----
# (Map showing most recent year of data on RR among new)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f1.3_09_data <- drnew |>
  select(iso3, year_new) |>
  group_by(iso3) |>
  summarise(most_recent_year = max(year_new)) |>
  ungroup() |>
  # Find out whether the most recent year was for national or sub-national data
  inner_join(select(drnew, iso3, year_new, all_areas_covered_new, source_new), by=c("iso3", "most_recent_year" = "year_new")) |>
  mutate(var = case_when(
                  between(most_recent_year, 2000, 2010) ~ "2000\u20132010",
                  between(most_recent_year, 2011, 2020) ~ "2011\u20132020",
                  # There is one country with a 2023 survey result but was mainly conducted in 2022 so show it as a 2022 data point
                  between(most_recent_year, 2021, 2023) ~ "2021\u20132022",
                  .default = NA)) |>
  mutate(var = factor(var,
                      levels = c("2000\u20132010", "2011\u20132020", "2021\u20132022")))

f1.3_09_data_subnational <- f1.3_09_data |>
  filter(all_areas_covered_new == 0) |>
  # SCG is Serbia & Montenegro that no longer exists, so exclude it
  filter(iso3 != "SCG") |>
  select(iso3)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Create dataframe for figure 1.3.910 ----
# (Map showing number of data points on RR among new)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f1.3_10_data <- drnew |>
  select(iso3, year_new) |>
  # SCG is Serbia & Montenegro that no longer exists, so exclude it
  filter(iso3 != "SCG") |>
  group_by(iso3) |>
  summarise(datapoints = n()) |>
  ungroup() |>
  mutate(var = case_when(
                datapoints == 1 ~ "1",
                datapoints == 2 ~ "2",
                between(datapoints, 3, 5) ~ "3\u20135",
                between(datapoints, 6, 10) ~ "6\u201310",
                between(datapoints, 11, 15) ~ "11\u201315",
                datapoints > 15 ~ "\u226516",
                .default = NA)) |>
  mutate(var = factor(var,
                      levels = c("1", "2", "3\u20135", "6\u201310", "11\u201315", "\u226516")))



# Proportion of population accounted for by these countries
pop_drs <- pop |>
  filter(year == report_year - 1) |>
  inner_join(f1.3_10_data, by = "iso3") |>
  summarise_at("e_pop_num", sum, na.rm=TRUE)

pop_tot <-pop |>
  filter(year == report_year - 1) |>
  summarise_at("e_pop_num", sum, na.rm=TRUE)

pop_drs_pct = pop_drs * 100 /pop_tot

pop_drs_pct <- pop_drs_pct |>
  select(pct = e_pop_num)
rm(pop_drs, pop_tot, pop)

# Proportion of TB incidence for by these countries

inc_drs <- est |>
  inner_join(f1.3_10_data, by = "iso3") |>
  summarise_at("inc.num", sum, na.rm=TRUE)

inc_tot <- est |>
  summarise_at("inc.num", sum, na.rm=TRUE)

inc_drs_pct = inc_drs * 100 / inc_tot
inc_drs_pct <- inc_drs_pct |> select(pct = inc.num)
rm(inc_drs, inc_tot)



