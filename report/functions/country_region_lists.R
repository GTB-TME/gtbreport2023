
region_shortnames <- function() {

  # Return a dataframe of WHO region codes and short names to use in figures and tables
  # Sorted in the order specified by WHO Press

  if (!exists("load_gtb")) {
    source(here::here("import/load_gtb.R"))
  }

  # Get the GTB list of groups
  regions <- load_gtb("grp") |>
    dplyr::filter(group_type == "g_whoregion") |>
    dplyr::select(g_whoregion = group_name, entity = group_description) |>

    # Remove the WHO bit at the beginning of regional names
    dplyr::mutate(entity = gsub("(WHO )|(WHO/PAHO )", "", entity)) |>

    # Change the order based on the bizarre method chosen by WHO Press ...
    dplyr::mutate(entity = factor(
      entity,
      levels = c(
        "African Region",
        "Region of the Americas",
        "South-East Asia Region",
        "European Region",
        "Eastern Mediterranean Region",
        "Western Pacific Region"
      )
    ))

  return(regions)

}

hb_list <- function(hbc_type = "tb") {

  # Convenience function to return a dataframe of the iso3 codes of one of the 30 high TB burden countries
  #
  # Input: hbc_type, one of:
  #       tb: for high TB burden country
  #       mdr: for high MDR-TB burden country
  #       tbhiv: for high TB/HIV burden country
  #


  # Use input parameter to create the group type code used in the GTB database
  hbc_type_gtb = paste0("g_hb_", hbc_type)


  if (!exists("load_gtb")) {
    source(here::here("import/load_gtb.R"))
  }

  # Get the GTB list of groups members
  isolist <- load_gtb("grpmbr") |>
    dplyr::filter(group_type == hbc_type_gtb & group_name == 1) |>
    dplyr::select(iso3, group_type)

  return(isolist)

}
