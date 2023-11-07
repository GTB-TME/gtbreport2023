# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data preparation script for ch2-2.rmd
# Takuya Yamanaka, June 2023
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Load chapter 2 packages, settings and data
source(here::here('report/ch2_load_data.r'))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: bac confirmation ----
# Determine numerators and denominators for bacteriological confirmation and
# then use this dataframe for figures 2.2.1, 2.2.2 and 2.3.3
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


bacconf_data <- notification %>%
  filter(year >= 2010) %>%
  select(iso3,
         country,
         year,
         g_whoregion,
         # old variables pre-2013
         new_sp,
         new_sn,
         new_su,
         # new variables
         new_labconf, new_clindx,
         ret_rel_labconf, ret_rel_clindx) %>%

  #calculate % of pulmonary cases with bac confirmation
  rowwise() %>%
  # a bit tricky for years before 2013, so do for new only by smear only
  mutate(bacconf_pct_numerator = ifelse(year < 2013 & g_whoregion != 'EUR',
                                        # old variables, do for new only outside EUR
                                        new_sp,
                                        # new variables
                                        sum(c_across(contains("labconf")), na.rm = TRUE)),
         bacconf_pct_denominator = ifelse(year < 2013 & g_whoregion != 'EUR',
                                          # old variables, do for new only outside EUR
                                          sum(c_across(new_sp:new_su), na.rm = TRUE),
                                          # new variables
                                          sum(c_across(new_labconf:ret_rel_clindx), na.rm = TRUE))) %>%

  # Adjust calculation for EUR pre-2013 (applies to years 2010 - 2012)
  mutate(bacconf_pct_numerator = ifelse(between(year, 2010, 2012) & g_whoregion == 'EUR',
                                        # old variables, but using new_labconf
                                        new_labconf,
                                        # otherwise keep calculation from previous step
                                        bacconf_pct_numerator),
         bacconf_pct_denominator = ifelse(between(year, 2010, 2012) & g_whoregion == 'EUR',
                                          # old variables
                                          sum(c_across(new_sp:new_su), na.rm = TRUE),
                                          # otherwise keep calculation from previous step
                                          bacconf_pct_denominator)) %>%

  ungroup() %>%

  # reduce to needed variables
  select(country,
         iso3,
         year,
         g_whoregion,
         bacconf_pct_numerator,
         bacconf_pct_denominator)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.2.1 ----
# (Panel plot of TB cases with bacteriological confirmation by WHO region and globally since 2010)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# by income group for main texts
bacconf_data_income <- bacconf_data %>%
  mutate(bacconf_pct = bacconf_pct_numerator * 100 / bacconf_pct_denominator) %>%
  inner_join(wb_incomelist, by = "iso3") %>%
  filter(year==report_year-1) %>%
  group_by(income) %>%
  summarise(across(bacconf_pct, median, na.rm = TRUE)) 
  

# Calculate aggregates
bacconf_data_regional <- bacconf_data %>%
  group_by(year, g_whoregion) %>%
  summarise(across(bacconf_pct_numerator:bacconf_pct_denominator, sum, na.rm = TRUE)) %>%

  # merge with regional names
  inner_join(who_region_shortnames, by = "g_whoregion") %>%
  select(-g_whoregion) %>%
  ungroup()

bacconf_data_global <- bacconf_data %>%
  group_by(year) %>%
  summarise(across(bacconf_pct_numerator:bacconf_pct_denominator, sum, na.rm = TRUE)) %>%
  mutate(entity = 'Global')

# Add global to the regional aggregates
f2.2.1_data <- rbind(bacconf_data_regional, bacconf_data_global) %>%

  # Calculate the percentages
  mutate(bacconf_pct = bacconf_pct_numerator * 100 / bacconf_pct_denominator) %>%

  # Change the order of the entities
  mutate(entity = factor(entity,
                         levels = c("Global", "African Region", "Region of the Americas", "South-East Asia Region",
                                    "European Region", "Eastern Mediterranean Region", "Western Pacific Region")))



# summary dataset for quoting numbers in the text on bac confirmation in pulmonary TB
f2.2.1_txt <- filter(notification, year == (report_year - 1)) %>%
  select(c_newinc,
         new_labconf, new_clindx,
         ret_rel_labconf, ret_rel_clindx) %>%
  summarise(across(c_newinc:ret_rel_clindx, sum, na.rm=TRUE)) %>%
  mutate(pulm = new_labconf + new_clindx + ret_rel_labconf + ret_rel_clindx) %>%
  mutate(pulm_pct = pulm * 100/ c_newinc) %>%
  select(c_newinc, pulm, pulm_pct)

# Calculate global % bac conf for the last two years and percent change and add to the summary
f2.2.1_txt <- filter(f2.2.1_data, entity == "Global" & year >= report_year-5) %>%
  select(year, bacconf_pct) %>%
  pivot_wider(names_from = year,
              names_prefix = "bc_pct_",
              values_from = bacconf_pct) %>%
  cbind(f2.2.1_txt)

# Add regional max and min values for 2020
f2.2.1_txt <- filter(f2.2.1_data, year==report_year-1 & entity %in% c("Region of the Americas", "Western Pacific Region")) %>%
  select(entity, bacconf_pct) %>%
  pivot_wider(names_from = entity,
              names_prefix = "bc_pct_",
              values_from = bacconf_pct) %>%
  # handle spaces in column names
  select(bc_pct_AMR = `bc_pct_Region of the Americas`,
         bc_pct_WPR = `bc_pct_Western Pacific Region`) %>%
  cbind(f2.2.1_txt)

# extract absolute number of TB case notifications for the past 3 years
f2.2.1_txt <- filter(notification, year >= (report_year - 3)) %>%
  select(year,c_newinc) %>%
  group_by(year) %>%
  summarise(across(c_newinc, sum, na.rm=TRUE)) %>%
  pivot_wider(names_from = year,
              names_prefix = "c_newinc_",
              values_from = c_newinc) %>%
  cbind(f2.2.1_txt)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.2.2 ----
# (World map showing percent of TB cases with bacteriological confirmation)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f2.2.2_data <- filter(bacconf_data, year>=report_year-2) %>%
  
  # Calculate the percentages
  mutate(bacconf_pct = ifelse(bacconf_pct_denominator > 0,
                              bacconf_pct_numerator * 100 / bacconf_pct_denominator,
                              NA)) %>%
  
  # Assign the categories for the map
  mutate(var = cut(bacconf_pct,
                   c(0, 50, 65, 80, Inf),
                   c('<49', '50\u201364', '65\u201379', '\u226580'),
                   right=FALSE))

# Find the countries with empty data for latest year and see if there are data for the previous year
bacconf_prev_year_data <- f2.2.2_data %>%
  filter(year == report_year - 1 & is.na(bacconf_pct)) %>%
  select(iso3) %>%
  inner_join(filter(f2.2.2_data, year == report_year - 2), by = "iso3") %>%
  filter(!is.na(bacconf_pct))

# Now combine into one data frame, with previous data used if latest year's data are not available
f2.2.2_data <- f2.2.2_data %>%
  filter(year == report_year - 1) %>%
  anti_join(bacconf_prev_year_data, by= "iso3") %>%
  rbind(bacconf_prev_year_data)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.2.3 ----
# (Box plot showing percent of TB cases tested with rapid diagnostics)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Calculate aggregates
f2.2.3_data <- bacconf_data %>%
  mutate(bacconf_pct=bacconf_pct_numerator/bacconf_pct_denominator) %>%
  left_join(wb_incomelist, by = "iso3") %>%  #NA: (no TB) Anguilla, Cook Islands, Montserrat, Niue, Tokelau, Wallis and Futuna, (with TB cases) Venezuela 
  mutate(income=factor(income,labels=c("High-income","Low-income","Lower-middle-income","Upper-middle-income"))) %>%
  mutate(income=factor(income,levels=c("Low-income","Lower-middle-income","Upper-middle-income","High-income")))


## fig 2.2.3 **need to find summarized data to draw boxplot with Kendo UI!
### function to find outliers
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

f2.2.3_data_outliers <- f2.2.3_data %>% filter(year == report_year-1 & !is.na(income) & bacconf_pct_denominator>=100) %>% mutate(bacconf_pct = bacconf_pct*100) %>% select(year,country,income,bacconf_pct) %>%
  group_by(income) %>% mutate(outlier = ifelse(is_outlier(bacconf_pct), bacconf_pct, as.numeric(NA))) %>% filter(!is.na(outlier))

f2.2.3_data_in <- f2.2.3_data %>% filter(year == report_year-1 & !is.na(income) & bacconf_pct_denominator>=100) %>% mutate(bacconf_pct = bacconf_pct*100) %>% select(year,country,income,bacconf_pct) %>%
  group_by(income) %>% mutate(outlier = ifelse(is_outlier(bacconf_pct), bacconf_pct, as.numeric(NA))) %>% filter(is.na(outlier))

### produce json data without outliers
f2.2.3_data_in %>%
  summarise(lower = min (bacconf_pct), q1 = quantile(bacconf_pct, 0.25), median = quantile(bacconf_pct, 0.5), q3 = quantile(bacconf_pct, 0.75), upper = max(bacconf_pct)) %>%
  mutate(color = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3")) %>% toJSON("rows") 

f2.2.3_txt <- f2.2.3_data_in %>%
  summarise(lower = min (bacconf_pct), q1 = quantile(bacconf_pct, 0.25), median = quantile(bacconf_pct, 0.5), q3 = quantile(bacconf_pct, 0.75), upper = max(bacconf_pct)) %>%
  select(income,median) %>%
  filter(income == "High-income" | income == "Low-income") %>%
  pivot_wider(names_from = "income", values_from = "median") %>%
  rename(lic=1, hic=2)

### write json data for outliers manually
f2.2.3_data_outliers %>% select(income,outlier) %>% arrange(income)%>% ungroup() %>% select(outlier) %>% toJSON("rows") 

# [{"income":"Low-income","lower":42.9353,"q1":63.1672,"median":70.8954,"q3":81.4344,"upper":96.4299,"color":"#BDD7E7"},{"income":"Lower-middle-income","lower":38.1871,"q1":63.3816,"median":76.4921,"q3":86.0583,"upper":94.316,"color":"#6BAED6"},{"income":"Upper-middle-income","lower":51.8219,"q1":72.4573,"median":77.9326,"q3":85.5078,"upper":100,"color":"#3182BD","outliers": [49.5028,36.8298]},{"income":"High-income","lower":74.422,"q1":85.9127,"median":90.5028,"q3":93.8272,"upper":98.1073,"color":"#08519C"},,"outliers": [62.4161,67.8322,47.2351,57.7586]] 

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.2.4 ----
# (Panel plot of TB cases with bacteriological confirmation for 30 countries since 2010)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f2.2.4_data <- bacconf_data %>%
  inner_join(hbc30, by = "iso3") %>%

  # Calculate the percentages
  mutate(bacconf_pct = ifelse(bacconf_pct_denominator > 0,
                              bacconf_pct_numerator * 100 / bacconf_pct_denominator,
                              NA)) %>%

  # get rid of extra variables
  select(country,
         year,
         bacconf_pct)

# summary datasets for quoting numbers in the text
f2.2.4_txt_MOZ <- filter(f2.2.4_data, year==report_year-1 & country=="Mozambique")
f2.2.4_txt_list_hi <- filter(f2.2.4_data, year==report_year-1 & bacconf_pct > 74) %>%
  select(country)
f2.2.4_txt_list_lo <- filter(f2.2.4_data, year==report_year-1 & bacconf_pct < 53) %>%
  select(country) %>%
  arrange(country)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.2.5 ----
# (Global trend of percent of TB cases tested with rapid diagnostics)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Calculate regional aggregates
f2.2.5_data <- filter(notification, year  >= 2015) %>%
  select(year,
         g_whoregion,
         c_newinc,
         rdx_data_available,
         newinc_rdx,
         newinc_pulm_labconf_rdx,
         newinc_pulm_clindx_rdx,
         newinc_ep_rdx,
         rdxsurvey_newinc,
         rdxsurvey_newinc_rdx) %>%
  rowwise() %>%
  mutate(newinc_rdx = ifelse(rdx_data_available==61, sum(across(rdxsurvey_newinc:rdxsurvey_newinc_rdx), na.rm = T), newinc_rdx)) %>%
  rowwise() %>%
  mutate(newinc_rdx = ifelse(rdx_data_available==62, sum(across(newinc_pulm_labconf_rdx:newinc_ep_rdx), na.rm = T), newinc_rdx)) %>%
  
  group_by(year, g_whoregion) %>%
  summarise(across(c_newinc:newinc_rdx, sum, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(wrd_pcnt = newinc_rdx * 100/c_newinc) %>%
  # merge with regional names
  inner_join(who_region_shortnames, by = "g_whoregion") %>%
  select(-c_newinc, -g_whoregion) %>%
  ungroup()


# global trend
f2.2.5_data_global <- filter(notification, year  >= 2015) %>%
  select(year,
         g_whoregion,
         c_newinc,
         rdx_data_available,
         newinc_rdx,
         newinc_pulm_labconf_rdx,
         newinc_pulm_clindx_rdx,
         newinc_ep_rdx,
         rdxsurvey_newinc,
         rdxsurvey_newinc_rdx) %>%
  rowwise() %>%
  mutate(newinc_rdx = ifelse(rdx_data_available==61, sum(across(rdxsurvey_newinc:rdxsurvey_newinc_rdx), na.rm = T), newinc_rdx)) %>%
  rowwise() %>%
  mutate(newinc_rdx = ifelse(rdx_data_available==62, sum(across(newinc_pulm_labconf_rdx:newinc_ep_rdx), na.rm = T), newinc_rdx)) %>%
  
  group_by(year) %>%
  summarise(across(c_newinc:newinc_rdx, sum, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(wrd_pcnt = newinc_rdx * 100/c_newinc) %>%
  select(-c_newinc) %>%
  mutate(entity = "Global")

# merge global and regional trends
f2.2.5_data <- rbind(f2.2.5_data, f2.2.5_data_global) %>%
  arrange(entity,year) %>%
# Change the order of the entities
  mutate(entity = factor(entity,
                       levels = c("Global", "African Region", "Region of the Americas", "South-East Asia Region",
                                  "European Region", "Eastern Mediterranean Region", "Western Pacific Region")))


f2.2.5_txt <- filter(notification, year  >= report_year - 3) %>%
  select(year,
         g_whoregion,
         c_newinc,
         rdx_data_available,
         newinc_rdx,
         newinc_pulm_labconf_rdx,
         newinc_pulm_clindx_rdx,
         newinc_ep_rdx,
         rdxsurvey_newinc,
         rdxsurvey_newinc_rdx) %>%
  rowwise() %>%
  mutate(newinc_rdx = ifelse(rdx_data_available==61, sum(across(rdxsurvey_newinc:rdxsurvey_newinc_rdx), na.rm = T), newinc_rdx)) %>%
  rowwise() %>%
  mutate(newinc_rdx = ifelse(rdx_data_available==62, sum(across(newinc_pulm_labconf_rdx:newinc_ep_rdx), na.rm = T), newinc_rdx)) %>%
  
  group_by(year) %>%
  summarise(across(c_newinc:newinc_rdx, sum, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(wrd_pcnt = newinc_rdx * 100/c_newinc) %>%
  select(-c_newinc, -rdx_data_available ) %>%
  pivot_wider(names_from = year,
              values_from = c(newinc_rdx, wrd_pcnt))

# Add info about testing at least half of new cases in the high burden countries
f2.2.5_txt <- notification %>%

  rowwise() %>%
  mutate(newinc_rdx = ifelse(rdx_data_available==62, sum(across(newinc_pulm_labconf_rdx:newinc_ep_rdx), na.rm = T), newinc_rdx)) %>%
  
  filter(year  >= report_year - 3 &
           newinc_rdx >= 0.5 * c_newinc &
                       (iso3 %in% hbc30$iso3 | iso3 %in% hbtbhiv30$iso3 | iso3 %in% hbmdr30$iso3)) %>%
  select(year, country) %>%
  group_by(year) %>%
  summarise(hbcs = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = year,
              names_prefix = "hbcs_",
              values_from = hbcs) %>%
  cbind(f2.2.5_txt) 

f2.2.5_txt <- f2.2.5_data %>%
  filter(year == report_year-1) %>%
  arrange(wrd_pcnt) %>%
  slice(1,7) %>%
  select(entity,wrd_pcnt) %>%
  pivot_wider(names_from = entity, values_from = wrd_pcnt) %>%
  rename(searo=1,euro=2) %>%
  cbind(f2.2.5_txt)


# country level for texts in the main report
f2.2.5_data_country <- filter(notification, year  == report_year-1) %>%
  select(iso3, country,
         g_whoregion,
         c_newinc,
         rdx_data_available,
         newinc_rdx,
         newinc_pulm_labconf_rdx,
         newinc_pulm_clindx_rdx,
         newinc_ep_rdx,
         rdxsurvey_newinc,
         rdxsurvey_newinc_rdx) %>%
  rowwise() %>%
  mutate(newinc_rdx = ifelse(rdx_data_available==61, sum(across(rdxsurvey_newinc:rdxsurvey_newinc_rdx), na.rm = T), newinc_rdx)) %>%
  rowwise() %>%
  mutate(newinc_rdx = ifelse(rdx_data_available==62, sum(across(newinc_pulm_labconf_rdx:newinc_ep_rdx), na.rm = T), newinc_rdx)) %>%
  
  group_by(iso3) %>%
  summarise(across(c_newinc:newinc_rdx, sum, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(wrd_pcnt = newinc_rdx * 100/c_newinc) %>%
  ungroup()

f2.2.5_data_country %>%
  filter(c_newinc>0, wrd_pcnt>=80) %>% nrow()
f2.2.5_data_country %>%
  filter(c_newinc>0, wrd_pcnt<20) %>% nrow()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.2.6 ----
# (map showing percent of TB cases tested with rapid diagnostics)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
f2.2.6_data <- notification %>%
  filter(year  >= report_year - 2) %>%
  select(iso3,
         country,
         year,
         c_newinc,
         rdx_data_available,
         newinc_rdx,
         newinc_pulm_labconf_rdx,
         newinc_pulm_clindx_rdx,
         newinc_ep_rdx,
         rdxsurvey_newinc,
         rdxsurvey_newinc_rdx) %>%
  
  rowwise() %>%
  mutate(newinc_rdx_type = ifelse(rdx_data_available==62, sum(across(newinc_pulm_labconf_rdx:newinc_ep_rdx), na.rm = T), NA)) %>%
  
  # calculate the percentage for each country depending on data availability
  mutate(wrd_pcnt_nu = ifelse(rdx_data_available == 60 & NZ(c_newinc) > 0,
                              newinc_rdx,
                              ifelse(rdx_data_available == 62 & NZ(c_newinc) > 0,
                                     newinc_rdx_type,
                                     ifelse(rdx_data_available == 61 & NZ(rdxsurvey_newinc) > 0,
                                            rdxsurvey_newinc_rdx,
                                            NA))),
         wrd_pcnt_de = ifelse((rdx_data_available == 60|rdx_data_available == 62) & NZ(c_newinc) > 0,
                              c_newinc,
                              ifelse(rdx_data_available == 61 & NZ(rdxsurvey_newinc) > 0,
                                     rdxsurvey_newinc,
                                     NA)),
         wrd_pcnt =ifelse(rdx_data_available == 60 & NZ(c_newinc) > 0,
                          newinc_rdx * 100 / c_newinc,
                          ifelse(rdx_data_available == 62 & NZ(c_newinc) > 0,
                                 newinc_rdx_type * 100 / c_newinc,
                                 ifelse(rdx_data_available == 61 & NZ(rdxsurvey_newinc) > 0,
                                        rdxsurvey_newinc_rdx * 100 / rdxsurvey_newinc,
                                        NA)))) %>%
  
  # Assign the categories for the map
  mutate(var = cut(wrd_pcnt,
                   c(0, 20, 40, 60, 80, Inf),
                   c('<20', '20\u201339', '40\u201359', '60\u201379','\u226580'),
                   right=FALSE)) %>%

  # get rid of extra variables
  select(country,
         iso3,
         year,
         wrd_pcnt_nu,
         wrd_pcnt_de,
         wrd_pcnt,
         var)


# Find the countries with empty data for latest year and see if there are data for the previous year
wrd_prev_year_data <- f2.2.6_data %>%
  filter(year == report_year - 1 & is.na(wrd_pcnt)) %>%
  select(iso3) %>%
  inner_join(filter(f2.2.6_data, year == report_year - 2), by = "iso3") %>%
  filter(!is.na(wrd_pcnt))

# Now combine into one dataframe, with previous data used if latest year's data are not available
  
f2.2.6_data <- f2.2.6_data %>%
  filter(year == report_year - 1) %>%
  anti_join(wrd_prev_year_data, by= "iso3") %>%
  rbind(wrd_prev_year_data) %>%
  left_join(hbc30, by= "iso3") %>%
  left_join(hbtbhiv30, by= "iso3") %>%
  left_join(hbmdr30, by= "iso3") %>%
  rename(hbc30=group_type.x,hbtbhiv30=group_type.y,hbmdr30=group_type)

f2.2.6_txt_list <- f2.2.6_data %>%
  filter(!is.na(hbc30), wrd_pcnt>80) 

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.2.7 ----
# (30HBCs trend of percent of TB cases tested with rapid diagnostics)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# additional to see trend in HBCs
f2.2.7_data <- notification %>%
  filter(year  >= 2015 & iso3 %in% iso3_hbc) %>%
  select(iso3,
         country,
         year,
         c_newinc,
         rdx_data_available,
         newinc_rdx,
         newinc_pulm_labconf_rdx,
         newinc_pulm_clindx_rdx,
         newinc_ep_rdx,
         rdxsurvey_newinc,
         rdxsurvey_newinc_rdx) %>%
  
  rowwise() %>%
  mutate(newinc_rdx_type = ifelse(rdx_data_available==62, sum(across(newinc_pulm_labconf_rdx:newinc_ep_rdx), na.rm = T), NA)) %>%
  
  # calculate the percentage for each country depending on data availability
  mutate(wrd_pcnt_nu = ifelse(rdx_data_available == 60 & NZ(c_newinc) > 0,
                              newinc_rdx,
                              ifelse(rdx_data_available == 62 & NZ(c_newinc) > 0,
                                     newinc_rdx_type,
                                     ifelse(rdx_data_available == 61 & NZ(rdxsurvey_newinc) > 0,
                                            rdxsurvey_newinc_rdx,
                                            NA))),
         wrd_pcnt_de = ifelse((rdx_data_available == 60|rdx_data_available == 62) & NZ(c_newinc) > 0,
                              c_newinc,
                              ifelse(rdx_data_available == 61 & NZ(rdxsurvey_newinc) > 0,
                                     rdxsurvey_newinc,
                                     NA)),
         wrd_pcnt =ifelse(rdx_data_available == 60 & NZ(c_newinc) > 0,
                          newinc_rdx * 100 / c_newinc,
                          ifelse(rdx_data_available == 62 & NZ(c_newinc) > 0,
                                 newinc_rdx_type * 100 / c_newinc,
                          ifelse(rdx_data_available == 61 & NZ(rdxsurvey_newinc) > 0,
                                 rdxsurvey_newinc_rdx * 100 / rdxsurvey_newinc,
                                 NA)))) %>%

  # get rid of extra variables
  select(entity = country,
         iso3,
         year,
         wrd_pcnt_nu,
         wrd_pcnt_de,
         wrd_pcnt)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.2.8 ----
# (World map showing proportion of TB diagnostic sites with WRDs)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f2.2.8_data <- strategy %>%
  filter(year == report_year - 1) %>%
  select(iso3,
         country,
         year,
         m_wrd,
         dx_test_sites) %>%
  # Calculate the proportion
  mutate(wrd_pct = ifelse(dx_test_sites > 0,
                          m_wrd * 100 / dx_test_sites,
                              NA)) %>%
  # Assign the categories for the map
  mutate(var = cut(wrd_pct,
                   c(0, 25, 50, 75, 90, Inf),
                   c('<25', '25\u201349', '50\u201375', '76\u201390','\u226590'),
                   right=FALSE)) %>%
  # get rid of extra variables
  select(country,
         iso3,
         year,
         m_wrd,
         dx_test_sites,
         wrd_pct,
         var) %>%
  left_join(hbc30, by= "iso3") %>%
  left_join(hbtbhiv30, by= "iso3") %>%
  left_join(hbmdr30, by= "iso3") %>%
  rename(hbc30=group_type.x,hbtbhiv30=group_type.y,hbmdr30=group_type) 

  filter(f2.2.8_data, !is.na(hbc30), wrd_pct>50) %>%
  nrow()

f2.2.8_txt <- f2.2.8_data %>%
  summarise(median = median(wrd_pct, na.rm=TRUE),
            quantile = list(quantile(wrd_pct, probs = seq(.25, 0.75, by = .5), na.rm = TRUE))) %>%
  unnest_wider(quantile) %>%
  ungroup() %>%
  mutate(entity = "Global") %>%
  select(entity,
         median,
         q1=`25%`,
         q3=`75%`) %>%
  mutate(hbc_wrd50over = filter(f2.2.8_data, !is.na(hbc30), wrd_pct>=50) %>% nrow())

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.2.9 ----
# (Chart showing number of WRD tests per capita as an indicator of the case finding effort)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# wrd_percapita is also for 2.2.11
wrd_percapita <- strategy %>%
  filter(year >= report_year - 2) %>%
  select(country,iso3,year,g_whoregion,m_wrd_tests_performed) 

pop <- estimates_population %>%
  filter(year >= report_year - 2) %>%
  select(iso3, year, pop)


not <- filter(notification, year >= report_year - 2)  %>%
  select(iso3,
         year,
         c_newinc) 

wrd_percapita <- wrd_percapita %>%
  inner_join(.,pop,by=c("iso3","year")) %>%
  inner_join(.,not,by=c("iso3","year")) 
  
wrd_percapita <- wrd_percapita %>%
  # Calculate the proportion
  mutate(wrd_ppp           = ifelse(m_wrd_tests_performed > 0,
                          m_wrd_tests_performed *100000 / pop,
                          NA)) %>%

  mutate(wrd_per_notif     = ifelse(m_wrd_tests_performed > 0,
                                m_wrd_tests_performed / c_newinc,
                                NA)) 

# sample ploting
f2.2.9_country <- wrd_percapita %>%
  filter(iso3 %in% hbc30$iso3) %>%
  select(entity = country,
         year,
         median = wrd_ppp)%>%
  mutate(q1=NA,
         q3=NA) 

f2.2.9_txt <- f2.2.9_country %>% filter(year == report_year-1 & median>1000) %>% nrow()

f2.2.9_txt_list <- f2.2.9_country %>% filter(year == report_year-1 & median>1000) %>% arrange (entity)

# Create combined dataframe in order of countries then regional and global estimates
f2.2.9_data <- f2.2.9_country 

f2.2.9_sel_order <-
  f2.2.9_data %>%
  filter(year == report_year-1) %>%
  arrange(desc(median)) %>%
  mutate(entity, levels = rev(entity))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.2.10 ----
# (Chart showing proportion of positive WHO-recommended rapid diagnostic tests)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

wrd_country <- strategy %>%
  filter(year >= report_year - 2) %>%
  select(iso3,
         country,
         year,
         g_whoregion,
         m_wrd_tests_positive,
         m_wrd_tests_performed) %>%
  
  # Calculate the proportion
  mutate(pos_wrd_pct = ifelse(m_wrd_tests_performed > 0,
                          m_wrd_tests_positive * 100 / m_wrd_tests_performed,
                          NA))

# for HBC plotting
f2.2.10_country <- wrd_country %>%
  filter(iso3 %in% hbc30$iso3) %>%
  select(entity = country,
         year,
         median = pos_wrd_pct) %>%
  mutate(q1=NA,
         q3=NA) 

f2.2.10_region <- wrd_country %>%
  group_by(g_whoregion, year) %>%
  summarise(median = median(pos_wrd_pct, na.rm=TRUE),
            quantile = list(quantile(pos_wrd_pct, probs = seq(.25, 0.75, by = .5), na.rm = TRUE))) %>%
  unnest_wider(quantile) %>%
  ungroup() %>%

  # merge with regional names and simplify to match structure of country table
  inner_join(who_region_shortnames, by = "g_whoregion") %>%
  select(entity,
         year,
         median,
         q1=`25%`,
         q3=`75%`) 

f2.2.10_global <- wrd_country %>%
  group_by(year) %>%
  summarise(median = median(pos_wrd_pct, na.rm=TRUE),
            quantile = list(quantile(pos_wrd_pct, probs = seq(.25, 0.75, by = .5), na.rm = TRUE))) %>%
  unnest_wider(quantile) %>%
  ungroup() %>%
  mutate(entity = "Global") %>%
  # merge with regional names and simplify to match structure of country table
  select(entity,
         year,
         median,
         q1=`25%`,
         q3=`75%`) 
  
# Bring them all together
# - - - - - - - - - - - - -

# Create dummy records so can see a horizontal line in the output to separate countries, regions and global parts
wrd_dummy1 <- data.frame(entity = " ", year = report_year-1, median = NA, q1 = 0, q3 = 100)
wrd_dummy2 <- data.frame(entity = "  ", year = report_year-1, median = NA, q1 = 0, q3 = 100)


# Create combined dataframe in order of countries then regional and global estimates
f2.2.10_data <- rbind(f2.2.10_country,
                      wrd_dummy1,
                      f2.2.10_region,
                      wrd_dummy2,
                      f2.2.10_global) 

f2.2.10_sel_order <-
  f2.2.10_data %>%
  filter(year == report_year-1) %>%
  mutate(entity = factor(entity, levels = c("Myanmar",                             
                                            "Papua New Guinea",                    
                                            "Sierra Leone",                        
                                            "China",                               
                                            "Liberia",                             
                                            "Philippines",                         
                                            "Viet Nam",                            
                                            "Pakistan",                            
                                            "Gabon",                               
                                            "Angola",                              
                                            "Central African Republic",            
                                            "India",                               
                                            "Thailand",                            
                                            "Indonesia",                           
                                            "Mozambique",                          
                                            "United Republic of Tanzania",         
                                            "Namibia",                             
                                            "Democratic Republic of the Congo",    
                                            "Nigeria",                             
                                            "Kenya",                               
                                            "Brazil",                              
                                            "Bangladesh",                          
                                            "Ethiopia",                            
                                            "South Africa",                        
                                            "Mongolia",                            
                                            "Lesotho",                             
                                            "Zambia",                              
                                            "Uganda",                              
                                            "Congo",                               
                                            "Democratic People's Republic of Korea",
                                            " ",
                                            "African Region",
                                            "Region of the Americas",
                                            "South-East Asia Region",
                                            "European Region",
                                            "Eastern Mediterranean Region",
                                            "Western Pacific Region",
                                            "  ",
                                            "Global"
                                            ))) %>%
  arrange(entity)

f2.2.10_data <- f2.2.10_data %>%
  mutate(entity = factor(entity, levels = rev(f2.2.10_sel_order$entity), ordered = TRUE)) %>%
  mutate(year = factor(year, labels = c("2021","2022"))) %>%
  arrange(factor(entity))

f2.2.10_data_kendo <- f2.2.10_data %>%
  mutate(entity = factor(entity, levels = f2.2.10_sel_order$entity, ordered = TRUE)) %>%
  mutate(year = factor(year, labels = c("2021","2022"))) %>%
  arrange(factor(entity))

# texts
f2.2.10_txt <- f2.2.10_data %>%
  filter(entity == "Global" & year == report_year-1)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.2.11 ----
# (Chart showing Number of WRD per notification of TB cases as an indicator of all TB cases are detected and notified)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f2.2.11_country <- wrd_percapita %>%
  filter(iso3 %in% hbc30$iso3) %>%
  select(entity = country,
         year,
         median = wrd_per_notif)%>%
  mutate(q1=NA,
         q3=NA) 

f2.2.11_txt <- f2.2.11_country %>% filter(year == report_year-1 & median<2) %>% nrow()

f2.2.11_txt_list <- f2.2.11_country  %>% filter(year == report_year-1) %>% arrange(desc(median)) %>% slice(1)


# Create combined dataframe in order of countries then regional and global estimates
f2.2.11_data <- f2.2.11_country 

f2.2.11_sel_order <-
  f2.2.11_data %>%
  filter(year == report_year-1) %>%
  arrange(desc(median)) %>%
  mutate(entity, levels = rev(entity))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.2.12 ----
# (Panel plot of TB cases with known HIV status by WHO region and globally since 2004)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Calculate regional aggregates
f2.2.12_data <- TBHIV_for_aggregates %>%
  filter(year >= 2010) %>%
  select(g_whoregion,
         year,
         hivtest_pct_numerator,
         hivtest_pct_denominator) %>%
  group_by(year, g_whoregion) %>%
  summarise(across(hivtest_pct_numerator:hivtest_pct_denominator, sum, na.rm = TRUE)) %>%
  # merge with regional names
  inner_join(who_region_shortnames, by = "g_whoregion") %>%
  select(-g_whoregion) %>%
  ungroup()

# Calculate Global aggregates
hivstatus_global <- TBHIV_for_aggregates %>%
  filter(year >= 2010) %>%
  select(year,
         hivtest_pct_numerator,
         hivtest_pct_denominator) %>%
  group_by(year) %>%
  summarise(across(hivtest_pct_numerator:hivtest_pct_denominator, sum, na.rm = TRUE)) %>%
  mutate(entity = "Global")

# COmbine regional with global
f2.2.12_data <- rbind(f2.2.12_data, hivstatus_global) %>%

  # Calculate % with known HIV status
  mutate(hivstatus_pct = hivtest_pct_numerator * 100 / hivtest_pct_denominator) %>%

  # Change the order of the entities
  mutate(entity = factor(entity,
                         levels = c("Global", "African Region", "Region of the Americas", "South-East Asia Region",
                                    "European Region", "Eastern Mediterranean Region", "Western Pacific Region")))

# summary dataset for quoting numbers in the text
f2.2.12_txt <- filter(f2.2.12_data, year>=report_year-2 & entity %in% c("African Region", "European Region", "Global")) %>%
  select(year, entity, hivstatus_pct) %>%
  pivot_wider(names_from = c(entity, year),
              values_from = hivstatus_pct) %>%
  # handle spaces in column names
  mutate(AFR_2021 = `African Region_2021`,
         EUR_2021 = `European Region_2021`)

# Add the numbers that are HIV-positive
f2.2.12_txt <- filter(TBHIV_for_aggregates, year == report_year-1) %>%
  summarise(across(c(hivtest_pos_pct_numerator, hivtest_pos_pct_denominator), sum, na.rm = TRUE)) %>%
  mutate(hivtest_pos_pct = hivtest_pos_pct_numerator * 100 / hivtest_pos_pct_denominator) %>%
  cbind(f2.2.12_txt)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.2.13 ----
# (World map showing percent of TB cases with known HIV status)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f2.2.13_data <- TBHIV_for_aggregates %>%
  filter(year >= report_year - 2) %>%
  select(iso3,
         country,
         year,
         g_whoregion,
         hivtest_pct_denominator,
         hivtest_pct_numerator) %>%

  # Calculate % with known HIV status
  mutate(hivstatus_pct = ifelse(hivtest_pct_denominator > 0,
                                hivtest_pct_numerator * 100 / hivtest_pct_denominator,
                                NA))  %>%

  # Assign the categories for the map
  mutate(var = cut(hivstatus_pct,
                   c(0, 50, 76, 90, Inf),
                   c('0-49', '50-75', '76-89', "\u226590"),
                   right=FALSE)) %>%

  # get rid of extra variables
  select(country,
         iso3,
         year,
         g_whoregion,
         hivstatus_pct,
         var)

# Find the countries with empty data for latest year and see if there are data for the previous year
hivstatus_prev_year_data <- f2.2.13_data %>%
  filter(year == report_year - 1 & is.na(hivstatus_pct)) %>%
  select(iso3) %>%
  inner_join(filter(f2.2.13_data, year == report_year - 2)) %>%
  filter(!is.na(hivstatus_pct))

# Now combine into one dataframe, with previous data used if latest year's data are not available
f2.2.13_data <- f2.2.13_data %>%
  filter(year == report_year - 1) %>%
  anti_join(hivstatus_prev_year_data, by= "iso3") %>%
  rbind(hivstatus_prev_year_data)

# Summary data for text
f2.2.13_txt <- filter(f2.2.13_data, hivstatus_pct >= 90) %>%
  summarise(over_90 = n())

f2.2.13_txt_afr <- filter(f2.2.13_data, hivstatus_pct >= 90) %>%
  filter(g_whoregion == "AFR") %>% nrow()

f2.2.13_txt_lo_list <- filter(f2.2.13_data, hivstatus_pct <= 50) %>% arrange(country)
f2.2.13_txt_lo <- filter(f2.2.13_data, hivstatus_pct <= 50) %>% nrow()


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.2.14 ----
# (Panel plot of TB cases tested for susceptibility to rifampicin by WHO region and globally since 2009)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# The calculations here are a bit messy for years prior to 2018. It is much simpler starting in 2018 when
# we switched to using routine DR surveillance records only. Prior to that we mixed and matched between
# that and the DR-TB detection section of the data collection form. Keeping this for consistency with
# previously published reports

# 1. Get DR-TB detection data

dst_notif_data <- notification %>%
  filter(year >= 2010) %>%
  select(year,
         iso3,
         g_whoregion,
         new_labconf,
         c_ret,
         rdst_new,
         rdst_ret) %>%

  rowwise() %>%
  mutate(

    # numerator
    dst_notif_num = ifelse(is.na(rdst_new) & is.na(rdst_ret),
                           NA,
                           sum(c_across(rdst_new:rdst_ret), na.rm = TRUE)),

    # denominator is a bit of a fudge: new_labconf + c_ret
    dst_notif_denom = ifelse(is.na(new_labconf) & is.na(c_ret),
                             NA,
                             sum(c_across(new_labconf:c_ret), na.rm = TRUE))) %>%
  ungroup()


# 2. Get routine DR surveillance data
# (numerator and denominator variables are different according to the year, but
#  don't use 2015, 2016 numerator data)
dst_drs_data <- dr_surveillance %>%
  filter(year >= 2010) %>%
  select(year,
         iso3,
         dst_rlt_new,
         dst_rlt_ret,
         pulm_labconf_new,
         pulm_labconf_ret,
         r_rlt_new,
         r_rlt_ret) %>%

  rowwise() %>%
  mutate(
    # numerator
    dst_drs_num = ifelse(year < 2015,
                         ifelse(is.na(dst_rlt_new) & is.na(dst_rlt_ret),
                                NA,
                                sum(c_across(dst_rlt_new:dst_rlt_ret), na.rm = TRUE)),
                         ifelse(year >= 2017,
                                sum(c_across(r_rlt_new:r_rlt_ret), na.rm = TRUE),
                                NA)),
    # denominator
    dst_drs_denom = ifelse(year >= 2017,
                           sum(c_across(pulm_labconf_new:pulm_labconf_ret), na.rm = TRUE),
                            NA)
  ) %>%
  ungroup()


# Link the two data sets

dst_data <- dst_notif_data %>%
  left_join(dst_drs_data, by = c("year", "iso3")) %>%

  # To calculate the percentage DST coverage we need to identify the greater of the two numerators
  # Note the exception made for South Africa in 2017


  mutate(
    dst_num = ifelse(year == 2017 & iso3 == "ZAF",
                     dst_notif_num,
                     ifelse(year >= 2017,
                            dst_drs_num,
                           ifelse(NZ(dst_drs_num) >= NZ(dst_notif_num),
                                  dst_drs_num,
                                  dst_notif_num))),

    dst_denom = ifelse(year == 2017 & iso3 == "ZAF",
                       dst_notif_denom,
                       ifelse(year >= 2017,
                              dst_drs_denom,
                              dst_notif_denom))) %>%

  # Set numerator to NA if the denominator is NA for a country-year
  mutate(dst_num = ifelse(is.na(dst_denom), NA, dst_num)) %>%

  # Drop unwanted variables
  select(iso3,
         year,
         g_whoregion,
         dst_num,
         dst_denom) %>%

  # Drop rows with empty numerators and denominator
  filter(!is.na(dst_num) & !is.na(dst_denom))


f2.2.14_data <- dst_data %>%
  group_by(g_whoregion, year) %>%
  summarise(across(dst_num:dst_denom, sum, na.rm = TRUE)) %>%

  # merge with regional names
  inner_join(who_region_shortnames, by = "g_whoregion") %>%
  ungroup() %>%
  select(-g_whoregion)

dst_global <- dst_data %>%
  group_by(year) %>%
  summarise(across(dst_num:dst_denom, sum, na.rm = TRUE)) %>%
  mutate(entity = "Global") %>%
  ungroup()

# Phew! Bring it all together now
# COmbine regional with global
f2.2.14_data <- rbind(f2.2.14_data, dst_global) %>%

  # Calculate % tested for rifampicin resistance
  mutate(dst_pcnt = dst_num * 100 / dst_denom) %>%

  # Change the order of the entities
  mutate(entity = factor(entity,
                         levels = c("Global", "African Region", "Region of the Americas", "South-East Asia Region",
                                    "European Region", "Eastern Mediterranean Region", "Western Pacific Region")))


# summary data for text
f2.2.14_txt <- filter(f2.2.14_data, year >= report_year-3 & entity %in% c("European Region", "Global")) %>%
  select(year, entity, dst_pcnt) %>%
  pivot_wider(names_from = c(entity, year),
              names_prefix = "dst_pct_",
              values_from = dst_pcnt) %>%
  # Handle spaces in variable name
  mutate(dst_pct_EUR_2021 = `dst_pct_European Region_2021`)

# Add numbers of drug-resistant cases detected
f2.2.14_txt <- filter(notification, year >= report_year - 2) %>%
  select(year,
         c_newinc,
         conf_rr_nfqr,
         conf_rr_fqr,
         conf_rrmdr) %>%
  group_by(year) %>%
  summarise(across(c_newinc:conf_rrmdr, sum, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(dr_tb = conf_rr_nfqr + conf_rr_fqr + conf_rrmdr) %>%
  select(-conf_rrmdr) %>%
  pivot_wider(names_from = year,
              values_from = c(c_newinc,
                              conf_rr_nfqr,
                              conf_rr_fqr,
                              dr_tb)) %>%
  select(-conf_rr_nfqr_2021, -conf_rr_fqr_2021) %>%
  mutate(dr_tb_change_pct = abs(dr_tb_2022 - dr_tb_2021) * 100 / dr_tb_2021,
         c_newinc_change_pct= abs(c_newinc_2022 - c_newinc_2021) * 100 / c_newinc_2021 ) %>%
  select(-c_newinc_2022, -c_newinc_2021) %>%
  cbind(f2.2.14_txt)






# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.2.15 ----
# (World map showing percent of TB cases tested for susceptibility to rifampicin)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f2.2.15_data <- dr_surveillance %>%
  filter(year >= report_year - 2) %>%
  select(year,
         country,
         iso3,
         pulm_labconf_new,
         pulm_labconf_ret,
         r_rlt_new,
         r_rlt_ret) %>%

  # Calculate coverage of DST percentages
  mutate(
    dst_pct = ifelse((NZ(pulm_labconf_new) + NZ(pulm_labconf_ret)) == 0 |
                       is.na(r_rlt_new) & is.na(r_rlt_ret), NA,
                     (NZ(r_rlt_new) + NZ(r_rlt_ret)) * 100 /
                       (NZ(pulm_labconf_new) + NZ(pulm_labconf_ret)))
  ) %>%

  # Assign the categories for the map
  mutate(var = cut(dst_pct,
                   c(0, 20, 50, 80, Inf),
                   c('0\u201319', '20\u201349', '50\u201379', '\u226580'),
                   right=FALSE)) %>%

  # get rid of extra variables
  select(country,
         iso3,
         year,
         dst_pct,
         var)


# Find the countries with empty data for latest year and see if there are data for the previous year
dst_prev_year_data <- f2.2.15_data %>%
  filter(year == report_year - 1 & is.na(dst_pct)) %>%
  select(iso3) %>%
  inner_join(filter(f2.2.15_data, year == report_year - 2), by = "iso3") %>%
  filter(!is.na(dst_pct))

# Now combine into one dataframe, with previous data used if latest year's data are not available
f2.2.15_data <- f2.2.15_data %>%
  filter(year == report_year - 1) %>%
  anti_join(dst_prev_year_data, by= "iso3") %>%
  rbind(dst_prev_year_data)


# summary numbers for the text
f2.2.15_txt <- filter(f2.2.15_data, dst_pct >=80 & iso3 %in% hbmdr30$iso3) %>%
  arrange(country) %>%
  select(country)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.2.16 ----
# (Panel plot of RR-TB cases tested for susceptibility to fluoroquinolones by WHO region and globally since 2015)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f2.2.16_data <- dr_surveillance %>%
  filter(year >= 2015) %>%
  select(iso3,
         g_whoregion,
         year,
         # denominator changed in 2017 from mdr to rr
         mdr_new,
         mdr_ret,
         xpert_dr_r_new,
         xpert_dr_r_ret,
         rr_new,
         rr_ret,
         # numerator changed in 2017 from mdr to rr and in 2019 from sld to fq
         mdr_dst_rlt,
         rr_dst_rlt,
         rr_dst_rlt_fq) %>%

  group_by(year, g_whoregion) %>%
  summarise(across(mdr_new:rr_dst_rlt_fq, sum, na.rm = TRUE)) %>%
  ungroup() %>%

  # Calculate the numerators and denominators depending on the year
  mutate(fqdst_pct_denominator = ifelse(year < 2017,
                                        mdr_new + mdr_ret + xpert_dr_r_new + xpert_dr_r_ret,
                                        NA),
         fqdst_pct_numerator = ifelse(year < 2017,
                                      mdr_dst_rlt,
                                      NA)) %>%

  mutate(fqdst_pct_denominator = ifelse(year >= 2017,
                                        rr_new + rr_ret,
                                        fqdst_pct_denominator),
         fqdst_pct_numerator = ifelse(year %in% c(2017, 2018),
                                      rr_dst_rlt,
                                      fqdst_pct_numerator)) %>%

  mutate(fqdst_pct_numerator = ifelse(year >= 2019,
                                      rr_dst_rlt_fq,
                                      fqdst_pct_numerator)) %>%

  # merge with regional names
  inner_join(who_region_shortnames, by = "g_whoregion") %>%

  # get rid of extra variables
  select(entity,
         year,
         fqdst_pct_numerator,
         fqdst_pct_denominator)

# Calculate global aggregaes
fqdst_global <- f2.2.16_data %>%
  group_by(year) %>%
  summarise(across(fqdst_pct_numerator:fqdst_pct_denominator, sum, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(entity = 'Global')

# Add global to the regional aggregates
f2.2.16_data <- rbind(f2.2.16_data, fqdst_global) %>%

  # Calculate the percentages
  mutate(fqdst_pct = fqdst_pct_numerator * 100 / fqdst_pct_denominator) %>%

  # Change the order of the entities
  mutate(entity = factor(entity,
                         levels = c("Global", "African Region", "Region of the Americas", "South-East Asia Region",
                                    "European Region", "Eastern Mediterranean Region", "Western Pacific Region")))




f2.2.16_txt <- filter(notification, year >= (report_year - 2)) %>%
  select(year,
         g_whoregion,
         c_notified,
         c_newinc,
         new_labconf, new_clindx, new_ep,
         ret_rel_labconf, ret_rel_clindx, ret_rel_ep,
         newrel_hivpos,
         conf_rr_nfqr,
         conf_rr_fqr) %>%
  
  # calculate regional aggregates
  group_by(year, g_whoregion) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  
  # merge with regional names
  inner_join(who_region_shortnames, by = "g_whoregion") %>%
  arrange(entity) %>%
  ungroup() %>%
  select(-g_whoregion)


# Add global summary to the regional summary
f2.2.16_txt <- f2.2.16_txt %>%
  group_by(year) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  mutate(entity="Global") %>%
  mutate(rr_nfqr_fqr = conf_rr_nfqr+conf_rr_fqr) %>%
  mutate(rr_nfqr_fqr_p = lag(rr_nfqr_fqr))%>%
  mutate(rr_nfqr_pct_dif = (rr_nfqr_fqr - rr_nfqr_fqr_p)*100/rr_nfqr_fqr_p) %>%
  mutate(c_newinc_p   = lag(c_newinc)) %>%
  mutate(c_newinc_pct_dif = (c_newinc - c_newinc_p)*100/c_newinc_p) %>%
  select(entity, year, 
         conf_rr_nfqr,
         conf_rr_fqr,
         rr_nfqr_fqr, rr_nfqr_pct_dif, c_newinc_pct_dif) %>%
  pivot_wider(names_from = year,
              values_from = conf_rr_nfqr:c_newinc_pct_dif) 

f2.2.16_txt2 <- f2.2.16_data %>%
  filter(year == report_year-1 & (entity=="European Region"|entity=="Western Pacific Region")) %>%
  select(entity, fqdst_pct) %>%
  pivot_wider(names_from = entity, values_from = fqdst_pct) %>%
  rename(eur = 1, wpr = 2)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.2.17 ----
# (World map showing percent of MDR/RR-TB cases tested for susceptibility to fluoroquinolones)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f2.2.17_data <- dr_surveillance %>%
  filter(year >= report_year - 1) %>%
  select(iso3,
         country,
         year,
         rr_new,
         rr_ret,
         rr_dst_rlt_fq) %>%
  
  # Calculate percentage RR cases with 2nd line DST
  mutate(fqdst_pct = ifelse( (NZ(rr_new) + NZ(rr_ret)) > 0,
                             rr_dst_rlt_fq * 100 / (NZ(rr_new) + NZ(rr_ret)),
                             NA)) %>%
  
  # Assign the categories for the map
  mutate(var = cut(fqdst_pct,
                   c(0, 20, 50, 80, Inf),
                   c('<20', '20\u201349', '50\u201379', '\u226580'),
                   right=FALSE)) %>%
  
  # get rid of extra variables
  select(country,
         iso3,
         year,
         fqdst_pct,
         var)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.2.18 ----
# (World map showing percent of pre-XDR-TB cases tested for susceptibility to bedaquiline)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f2.2.18_data <- dr_surveillance %>%
  filter(year >= report_year - 1) %>%
  select(iso3,
         country,
         year,
         rr_fqr,
         rr_fqr_bdqr_lzdr,rr_fqr_bdqs_lzdr,rr_fqr_bdqu_lzdr,
         rr_fqr_bdqr_lzds,rr_fqr_bdqs_lzds,rr_fqr_bdqu_lzds,
         rr_fqr_bdqr_lzdu,rr_fqr_bdqs_lzdu,rr_fqr_bdqu_lzdu) %>%
  
  # Calculate percentage RR cases with 2nd line DST
  mutate(bddst_pct = ifelse( NZ(rr_fqr) > 0,
                             (NZ(rr_fqr_bdqr_lzdr)+NZ(rr_fqr_bdqs_lzdr)+NZ(rr_fqr_bdqr_lzds)+NZ(rr_fqr_bdqs_lzds)+NZ(rr_fqr_bdqr_lzdu)+NZ(rr_fqr_bdqs_lzdu)) * 100 / NZ(rr_fqr),
                             NA)) %>%
  
  # Assign the categories for the map
  mutate(var = cut(bddst_pct,
                   c(0, 20, 50, 80, Inf),
                   c('<20', '20\u201349', '50\u201379', '\u226580'),
                   right=FALSE)) %>%
  
  # get rid of extra variables
  select(country,
         iso3,
         year,
         bddst_pct,
         var)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.2.19 ----
# (World map showing percent of pre-XDR-TB cases tested for susceptibility to linezolid)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f2.2.19_data <- dr_surveillance %>%
  filter(year >= report_year - 1) %>%
  select(iso3,
         country,
         year,
         rr_fqr,
         rr_fqr_bdqr_lzdr,rr_fqr_bdqs_lzdr,rr_fqr_bdqu_lzdr,
         rr_fqr_bdqr_lzds,rr_fqr_bdqs_lzds,rr_fqr_bdqu_lzds,
         rr_fqr_bdqr_lzdu,rr_fqr_bdqs_lzdu,rr_fqr_bdqu_lzdu) %>%
  
  # Calculate percentage RR cases with 2nd line DST
  mutate(lzdst_pct = ifelse( NZ(rr_fqr) > 0,
                             (NZ(rr_fqr_bdqr_lzdr)+NZ(rr_fqr_bdqs_lzdr)+NZ(rr_fqr_bdqr_lzds)+NZ(rr_fqr_bdqs_lzds)+NZ(rr_fqr_bdqu_lzdr)+NZ(rr_fqr_bdqu_lzds)) * 100 / NZ(rr_fqr),
                             NA)) %>%
  
  # Assign the categories for the map
  mutate(var = cut(lzdst_pct,
                   c(0, 20, 50, 80, Inf),
                   c('<20', '20\u201349', '50\u201379', '\u226580'),
                   right=FALSE)) %>%
  
  # get rid of extra variables
  select(country,
         iso3,
         year,
         lzdst_pct,
         var)



