# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Common data preparation script for chapters (sections) 2
# Takuya Yamanaka, June 2023
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load data packages ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

library(stringr)
library(dplyr)
library(tidyr)
library(here)
library(readr) # to save csv
library(magrittr) # to use tee pipe
library(data.table)
library(jsonlite) # for provisional monthly notification in India

# Load functions ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

source(here("import/load_gtb.R"))
source(here("report/functions/country_region_lists.R"))
source(here("report/functions/NZ.R"))
source(here("report/functions/calculate_outcomes.R"))


# Set the report year ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
report_year <- 2023
snapshot_date <- latest_snapshot_date()
csv_datestamp <- "2023-07-24"
lsaved_year <- 2010

# Set whether or not to include objects with estimates in the output ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

show_estimates <- T
datacoll <- T

# Load functions ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

source(here("import/load_gtb.R"))
source(here("report/functions/country_region_lists.R"))
source(here("report/functions/NZ.R"))


# Load GTB data ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

notification <- load_gtb("tb")
tpt <- load_gtb("tpt")
TBHIV_for_aggregates <- load_gtb("agg")
estimates_ltbi <- load_gtb("ltbi")
strategy <- load_gtb("sty")
estimates_population <- load_gtb("pop")
grpmbr <- load_gtb("grpmbr")
grp <- load_gtb("grp")
data_collection <- load_gtb("datacoll")
dr_surveillance <- load_gtb("drroutine")
outcomes <- load_gtb("tx")

who_region_shortnames <- region_shortnames()

# Fix lists of the three sets of 30 high burden countries (used to filter records for some figures)
hbc30 <- hb_list("tb")

hbmdr30 <- grpmbr %>%
  filter(group_type == "g_hb_mdr" & group_name == 1) %>%
  select(iso3,group_type)

hbtbhiv30 <- grpmbr %>%
  filter(group_type == "g_hb_tbhiv" & group_name == 1) %>%
  select(iso3,group_type)


# list iso3 - country
list_iso3_country <-
  grpmbr %>% filter(group_type == 'g_whoregion') %>%
  select(iso3,country)

list_watch_list <-
  list_iso3_country %>%
  filter(iso3 %in% c('KHM','RUS','ZWE'))

list_hbcs <-
  grpmbr %>%
  filter(group_type == "g_hb_tb" & group_name == 1) %>%
  select(iso3,country) %>%
  arrange(country)

list_hbcs_plus_wl <- list_hbcs %>% add_row(list_watch_list) %>%   arrange(country)

iso3_hbc <- list_hbcs$iso3
iso3_hbc_plus_wl <- list_hbcs_plus_wl$iso3
iso3_hmdrc <- hbmdr30$iso3


iso3_income <- grpmbr %>%
  filter(group_type == "g_income") %>% select(iso3,group_name) %>% rename(g_income=2)

list_hbcs_income <-
  list_hbcs %>%
  left_join(iso3_income)

list_hbcs_plus_wl_income <-
  list_hbcs_plus_wl %>%
  left_join(iso3_income)

# WB income group list
wb_incomelist <- grpmbr %>%
  filter(group_type == "g_income") %>% select(iso3,group_name) %>% rename(income=2)

# Get global and regional estimates directly from the files that get imported into the database
if(show_estimates) {

  load(here::here("inc_mort/analysis/est.rda"))
  est_country <- est
  load(here::here("inc_mort/analysis/global.rda"))
  est_global <- global
  load(here::here("inc_mort/analysis/regional.rda"))
  est_regional <- regional
  
  load(here::here('drtb/dboutput/db_dr_country.rda'))
  est_dr_country <- db_dr_country
  
  load(here::here('drtb/dboutput/db_dr_group.rda'))
  est_dr_group <- db_dr_group

  load(here::here(paste0('lives_saved/output/regional_saved_',lsaved_year,'.rda')))
  lives_saved <- saved.regional.print

  load(here::here(paste0('lives_saved/output/regional_saved_',2005,'.rda')))
  lives_saved_hiv <- saved.regional.print
    
  # Get Pete's aggregate incidence estimates by age group and sex
  load(here('disaggregation/dboutput/db_estimates_country.Rdata'))
  load(here('disaggregation/dboutput/db_estimates_group.Rdata'))

}

# Kill any attempt at using factors, unless we explicitly want them!
options(stringsAsFactors=FALSE)






