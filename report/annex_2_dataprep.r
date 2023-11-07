# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data preparation script for annex_2.Rmd
# Hazim Timimi, July 2023
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load data packages ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
library(dplyr)
library(stringr)
library(here)

# Set the report year ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

report_year <- 2023


# Load functions ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

source(here("import/load_gtb.R"))
source(here("report/functions/country_region_lists.R"))


# Hard code some dates and values
# (when SDG indicators were updated
# and the high income group definition)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
sdg_update_date <- "4 July 2023"
hic_threshold <- "13 206" # threshold 2021
hic_threshold_yr <- "2021"

snapshot_date <- format(latest_snapshot_date(), "%d %B %Y")



# Load GTB data ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

notification <- load_gtb("tb")
data_collection  <- load_gtb("datacoll")
external_indicator_defs <- load_gtb("sdgdef")

who_region_shortnames <- region_shortnames()


# Create a data frame for table E2_1 (reporting rates) ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

reporting_countries <- data_collection |>
  filter(datcol_year == report_year & dc_form_description != 'Not requested') |>
  select(iso3, g_whostatus)

reporting_rates <- notification |>
  filter(year == report_year - 1) |>

  # define reported as having a non-empty value for notified cases of TB, RR-TB or HIV-positive TB
  mutate(reported = ifelse(is.na(c_notified) & is.na(conf_rrmdr) & is.na(newrel_hivpos),
                           0,
                           1)) |>
  select(iso3, g_whoregion, reported) |>
  inner_join(reporting_countries, by = "iso3") |>

  # ignore the distinction between associate members and non members of WHO
  mutate(g_whostatus = ifelse(g_whostatus == "M",
                              "M",
                              NA)) |>

  # calculate summaries by region and membership of WHO
  group_by(g_whoregion, g_whostatus) |>
  summarise(total = n(),
            reported = sum(reported, na.rm = TRUE) )


# Create summary for all countries and areas by region
reporting_rates_tot <- reporting_rates |>
  group_by(g_whoregion) |>
  summarise_at(vars(total, reported),
               sum,
               na.rm = TRUE)

# Create a summary for WHO member states by region
reporting_rates_ms <- reporting_rates |>
  filter(g_whostatus == "M") |>
  select(g_whoregion,
         total_ms = total,
         reported_ms = reported)


# Combine the two summaries into one summary table
reporting_rates_tab <- reporting_rates_tot |>
  inner_join(reporting_rates_ms,
             by="g_whoregion") |>

  # Add the short WHO region names
  inner_join(who_region_shortnames,
             by="g_whoregion") |>
  select(-g_whoregion) |>
  arrange(entity)


# Calculate a global summary
reporting_rates_glob <- reporting_rates_tab |>
  select(-entity) |>
  summarise_all(sum) |>
  mutate(entity = "Global")

# Add the global summary to the summary table
ta2.1_data <- rbind(reporting_rates_tab, reporting_rates_glob)

# Clean up
rm(list=ls(pattern = "^reporting_"))


# Create a data frame for table E2_2 (external indicators)  ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# To ensure consistency of sorting of indicators from one report to the next, force it manually here

external_indicator_order <- data.frame(indicator_id = c("SI_POV_DAY1",
                                                        "per_allsp.cov_pop_tot",
                                                        "SN.ITK.DEFC.ZS",
                                                        "MDG_0000000029",
                                                        "NCD_GLUC_04",
                                                        "SA_0000001462",
                                                        "M_Est_smk_curr_std",
                                                        "UHC_INDEX_REPORTED",
                                                        "FINPROTECTION_CATA_TOT_10_POP",
                                                        "SH.XPD.CHEX.PP.CD",
                                                        "EG.CFT.ACCS.ZS",
                                                        "NY.GDP.PCAP.PP.KD",
                                                        "SI.POV.GINI",
                                                        "EN_LND_SLUM"),
                                       sort_order = seq(1:14))

ta2.2_data <- external_indicator_defs |>
  inner_join(external_indicator_order, by = "indicator_id") |>
  arrange(sort_order) |>

  # There seems to be a weird character in one of the definition fields that kable doesn't like,
  # Or maybe it interprets int$ as something special. So fudge by hand
  mutate(name_at_source = ifelse(indicator_id == "GHED_CHE_pc_PPP_SHA2011",
                                 "Current health expenditure (CHE) per capita in PPP int $",
                                 name_at_source)) |>

  # Replace ">=" with a greater than or equal to entity code in indicator names
  mutate(display_name_in_profile = str_replace_all(display_name_in_profile, ">=", "&ge;"),
         name_at_source = str_replace_all(name_at_source, ">=", "&ge;")) |>

  # Minor tweaks needed to the SDG number fields to match what was published previously
  mutate(sdg_number = ifelse(indicator_id %in% c("MDG_0000000029",
                                                 "NCD_GLUC_04",
                                                 "M_Est_smk_curr_std",
                                                 "NY.GDP.PCAP.PP.KD",
                                                 "SI.POV.GINI"),
                             paste(sdg_number,
                                   "<i>(alternative)</i>"),
                             sdg_number)) |>
  mutate(sdg_number = ifelse(indicator_id == "SA_0000001462", "3.5.2 <i>(alternative)</i>", sdg_number)) |>
  mutate(sdg_number = ifelse(indicator_id == "SH.XPD.CHEX.PP.CD", "3.8.2 <i>(alternative)</i>", sdg_number))


