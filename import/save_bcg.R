#' ---
#' title: Download BCG indicator from the WHO Global Health Observatory
#' author: Hazim Timimi
#' date: 2023-05-31
#' output:
#'    html_document:
#'      mode: selfcontained
#'      toc: true
#'      toc_depth: 3
#'      toc_float: true
#'      number_sections: true
#'      theme: flatly
#'      highlight: zenburn
#'      df_print: paged
#'      code_folding: hide
#' ---

#' (Last updated: `r Sys.Date()`)
#'
#' # Download the BCG indicator for 2015 onwards from the GHO at both country and regional/global levels
#'
#' indicator code WHS4_543 is the BCG immunisation coverage among 1-year-olds
#' https://www.who.int/data/gho/data/indicators/indicator-details/GHO/bcg-immunization-coverage-among-1-year-olds-(-)
#'
#' Dependences:
#'  - libraries dplyr, ghost and here
#'
#' Output:
#'  - bcg_country_YYYY-MM-DD.csv file in the ~/data/bcg folder
#'  - bcg_agg_YYYY-MM-DD.csv file in the ~/data/bcg folder


# Set the location of the output folder
#
bcg_folder <- here::here("data/bcg")


# Use the ghost package to get the GHO indicator data into a dataframe
# (Both country-level and aggregate)
#
bcg_gho_country <- ghost::gho_data("WHS4_543") |>
  dplyr::filter(TimeDim >= 2015 & SpatialDimType == "COUNTRY") |>
  dplyr::mutate(bcg_coverage = as.integer(Value)) |>
  dplyr::select(iso3 = SpatialDim,
                year = TimeDim,
                bcg_coverage)

bcg_gho_agg <- ghost::gho_data("WHS4_543") |>
  dplyr::filter(TimeDim >= 2015 & SpatialDimType == "REGION") |>
  dplyr::mutate(
    bcg_coverage = as.integer(Value),
    group_type = ifelse(SpatialDim == "GLOBAL", "global", "g_whoregion"),
    group_name = ifelse(SpatialDim == "GLOBAL", "global", substr(SpatialDim, 1, 3))
  ) |>
  dplyr::select(group_type,
                group_name,
                year = TimeDim,
                bcg_coverage)


# Add timestamps to both data sets
attr(bcg_gho_country, "timestamp") <- Sys.Date()
attr(bcg_gho_agg, "timestamp") <- Sys.Date()

# Save as .rda files
#
save(bcg_gho_country, file = paste0(bcg_folder, "/bcg_gho_country", ".rda"))
save(bcg_gho_agg, file = paste0(bcg_folder, "/bcg_gho_agg", ".rda"))


