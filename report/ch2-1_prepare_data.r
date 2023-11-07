# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data preparation script for ch2-1.rmd
# Takuya Yamanaka, June 2023
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load chapter 2, settings and data
source(here::here('report/ch2_load_data.r'))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: table 2.1.1 ----
# (Notifications of TB, HIV-positive TB, and DR-TB cases, globally and for WHO regions)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

t2.1.1_data <- filter(notification, year == (report_year - 1)) %>%
  select(g_whoregion,
         c_notified,
         c_newinc,
         new_labconf, new_clindx, new_ep,
         ret_rel_labconf, ret_rel_clindx, ret_rel_ep,
         newrel_hivpos,
         conf_rr_nfqr,
         conf_rr_fqr) %>%

  # calculate regional aggregates
  group_by(g_whoregion) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%

  # merge with regional names
  inner_join(who_region_shortnames, by = "g_whoregion") %>%
  arrange(entity) %>%
  select(-g_whoregion)


# Add global summary to the regional summary
t2.1.1_global <- t2.1.1_data %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  mutate(entity="Global")


t2.1.1_data <- rbind(t2.1.1_data, t2.1.1_global)


# Calculate total pulmonary and %ages that are bac confirmed and that are extrapulmonary
t2.1.1_data <- t2.1.1_data %>%
  mutate( newrel_pulm = new_labconf + new_clindx + ret_rel_labconf + ret_rel_clindx,
          newrel_pulm_conf_pct = (new_labconf + ret_rel_labconf) * 100
          /
            (new_labconf + new_clindx + ret_rel_labconf + ret_rel_clindx),
          newrel_ep_pct = (new_ep + ret_rel_ep) * 100
          /
            (c_newinc)
  ) %>%
  # Restrict to variables needed in the final output
  select(entity,
         c_notified,
         c_newinc,
         newrel_pulm,
         newrel_pulm_conf_pct,
         newrel_ep_pct,
         newrel_hivpos,
         conf_rr_nfqr,
         conf_rr_fqr)

# summary dataset for the text
t2.1.1_region <- filter(t2.1.1_data, entity!="Global") %>%
  arrange(desc(c_notified)) %>%
  mutate(c_total_p = c_notified/t2.1.1_global$c_notified*100) %>%
  mutate(c_newinc_cum_p = cumsum(c_total_p))

t2.1.1_txt <- filter(t2.1.1_data, entity=="Global") %>%
  mutate(c_pulm_p = newrel_pulm/c_newinc *100)

t2.1.1_txt <- filter(t2.1.1_region, entity=="Western Pacific Region") %>%
  select(c_newinc_cum_p = c_newinc_cum_p) %>%
  cbind(t2.1.1_txt)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.1.1 and 2.1.2 ----
# (Global trend in case notifications of people newly diagnosed with TB, 2010–2021)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
f2.1.2_data <- filter(notification, year >= 2010) %>%
  select(year,
         g_whoregion,
         c_newinc) %>%
  
  # calculate regional aggregates
  group_by(g_whoregion,year) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  
  # merge with regional names
  inner_join(who_region_shortnames, by = "g_whoregion") %>%
  arrange(entity) %>%
  select(-g_whoregion)

# to adjust yaxis range in facet_wrap
f2.1.2_data <- data.table(f2.1.2_data)
f2.1.2_data[g_whoregion == "AFR",y_min := 1.25*1e6]
f2.1.2_data[g_whoregion == "AFR",y_max := 1.5*1e6]
f2.1.2_data[g_whoregion == "AMR",y_min := 0.19*1e6]
f2.1.2_data[g_whoregion == "AMR",y_max := 0.24*1e6]
f2.1.2_data[g_whoregion == "SEA",y_min := 2.5*1e6]
f2.1.2_data[g_whoregion == "SEA",y_max := 3.5*1e6]
f2.1.2_data[g_whoregion == "EUR",y_min := 0.15*1e6]
f2.1.2_data[g_whoregion == "EUR",y_max := 0.25*1e6]
f2.1.2_data[g_whoregion == "EMR",y_min := 0.39*1e6]
f2.1.2_data[g_whoregion == "EMR",y_max := 0.55*1e6]
f2.1.2_data[g_whoregion == "WPR",y_min := 1*1e6]
f2.1.2_data[g_whoregion == "WPR",y_max := 1.5*1e6]

# texts!
f2.1.2_text <- f2.1.2_data %>%
  filter(entity=="African Region", year==2019|year==2020) %>%
  pivot_wider(names_from = year,
              values_from = c_newinc) %>%
  mutate(pct_decline=(1-`2020`/`2019`)*100)

f2.1.2_txt <- filter(f2.1.2_data, year>=2019) %>%
  mutate(c_newinc_p = lag(c_newinc)) %>%
  mutate(pct_dif = (c_newinc - c_newinc_p)*100/c_newinc_p) %>%
  filter(year==2020, entity=="African Region")


# Add global summary to the regional summary
f2.1.1_data <- f2.1.2_data %>%
  group_by(year) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  mutate(entity="Global")

f2.1.1_txt <- filter(f2.1.1_data, year>=2019) %>%
  mutate(c_newinc_p = lag(c_newinc)) %>%
  mutate(pct_dif = (c_newinc - c_newinc_p)*100/c_newinc_p) %>%
  filter(year==2020)

f2.1.1_txt2 <- filter(f2.1.1_data, year>=2019) %>%
  select(year,c_newinc) %>%
  mutate(c_newinc = c_newinc/1e6) %>%
  pivot_wider(names_from = year, values_from = c_newinc) %>%
  rename(c_newinc_2019 = `2019`, c_newinc_2020 = `2020`, c_newinc_2021 = `2021`, c_newinc_2022 = `2022`)
  

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.1.3 ----
# (Trends in case notifications of people newly diagnosed with TB, 30 high TB burden countries, 2016-2021)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

pct_change <- notification %>%
  select(iso3,year,country,g_whoregion,c_newinc) %>%
  pivot_wider(names_from = year, values_from = c_newinc) %>%
  mutate(
    `2016_2` = round(`2016`/`2015`*100-100,2),
    `2017_2` = round(`2017`/`2016`*100-100,2),
    `2018_2` = round(`2018`/`2017`*100-100,2),
    `2019_2` = round(`2019`/`2018`*100-100,2),
    `2020_2` = round(`2020`/`2019`*100-100,2),
    `2021`   = round(`2021`/`2020`*100-100,2)) %>%
  select(iso3,country,g_whoregion,`2016_2`,`2017_2`,`2018_2`,`2019_2`,`2020_2`,`2021`) %>%
  rename(`2016`=`2016_2`,`2017`=`2017_2`,`2018`=`2018_2`,`2019`=`2019_2`,`2020`=`2020_2`) %>%
  pivot_longer(names_to = "year", cols = `2016`:`2021`, values_to = "pct_change") %>%
  mutate(year=as.numeric(year))

# grouping countries by changes in 2019-2020 and 2020-2021
segmentation <- notification %>%
  select(iso3,year,country,g_whoregion,c_newinc) %>%
  pivot_wider(names_from = year, values_from = c_newinc) %>%
  mutate(pct1920 = `2020`/`2019`-1,
         pct1921 = `2021`/`2019`-1,
         pct1922 = `2022`/`2019`-1,
         pct2021 = `2021`/`2020`-1,
         num1920 = `2019`-`2020`,
         num1921 = `2019`-`2021`,
         num1922 = `2019`-`2022`) %>%
  select(iso3,country,g_whoregion,`2019`:`2021`,pct1920:num1922) %>%
  mutate(col=ifelse(pct1920<(-0.05)&pct2021>=0&num1921>0&pct1920,"a", #impact in 2020 and some recovery in 2021
                    ifelse(pct1920<(-0.05)&pct2021>=0&num1921<=0,"b", #impact in 2020 and recovery in 2021 exceeding 2019
                           ifelse(pct1920>=(-0.05)&pct2021<0,"d", #impact only in 2021
                                  ifelse(pct1920<(-0.05)&pct2021<0,"c", #impact both in 2020 and 2021
                                         ifelse(pct1920>0&pct2021>0,"e", #increases in 2020 and 2021
                                                "f")))))) %>% # the rest
  left_join(list_hbcs_plus_wl,by="iso3") %>%
  rename(country=country.x, country_hbc=country.y) %>%
  mutate(hbc=ifelse(is.na(country_hbc),0,1))


segmentation_hbc <- notification %>%
  select(iso3,year,country,g_whoregion,c_newinc) %>%
  pivot_wider(names_from = year, values_from = c_newinc) %>%
  mutate(pct1920 = `2020`/`2019`-1,
         pct1921 = `2021`/`2019`-1,
         pct1922 = `2022`/`2019`-1,
         pct2122 = `2022`/`2021`-1) %>%
  select(iso3,country,g_whoregion,pct1920:pct2122) %>%
  filter(iso3 %in% list_hbcs_plus_wl$iso3) %>%
  mutate(col=ifelse(pct1920>0&pct1921>0,"c", # increases during COVID-19
                    ifelse(iso3=="CHN"|iso3=="PRK"|iso3=="RUS","d", # no impact or limited departure
                           ifelse(iso3=="NAM"|iso3=="ZAF"|iso3=="ETH","f", # no impact, but increase in 2022
                                  ifelse(pct2122<0,"e", # reduction in 2022
                                  ifelse(pct1922>=-0.001,"a", # recovery more than the level of 2019
                                                "b")))))) # the rest: partial recovery by 2022

f2.1.3_data <- notification %>%
  select(iso3,year,country,g_whoregion,c_newinc) %>%
  subset(year>=2015) %>%
  inner_join(segmentation_hbc %>% select(iso3,col)) 

f2.1.3a_data <-
  f2.1.3_data %>%
  filter(col=="a") %>%
  filter(year == 2020) %>%
  left_join(segmentation_hbc %>% select(iso3,col,pct_change=pct1920))  %>%
  arrange(pct_change) %>%
  mutate(country = factor(country))

f2.1.3b_data <-
  f2.1.3_data %>%
  filter(col=="b") %>%
  filter(year == 2020) %>%
  left_join(segmentation_hbc %>% select(iso3,col,pct_change=pct1920))  %>%
  arrange(pct_change) %>%
  mutate(country = factor(country))

f2.1.3e_data <-
  f2.1.3_data %>%
  filter(col=="e") %>%
  filter(year == 2020) %>%
  left_join(segmentation_hbc %>% select(iso3,col,pct_change=pct1920))  %>%
  arrange(pct_change) %>%
  mutate(country = factor(country))

f2.1.3a_txt <-
  f2.1.3_data %>%
  filter(col=="a", year==report_year-1) %>% nrow()


f2.1.3b_txt <-
  f2.1.3_data %>%
  filter(col=="b", year==report_year-1) %>% nrow()

  f2.1.3_data %>%
  filter(col=="b") 

f2.1.3c_txt <-
  f2.1.3_data %>%
  filter(col=="c", year==report_year-1) %>% nrow()


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.1.4 ----
# (Trends in case notifications of people newly diagnosed with TB, 30 high TB burden countries, 2016-2021)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
f2.1.4_data <-
  filter(segmentation, iso3 %in% list_hbcs_plus_wl$iso3)  %>%
  select(iso3,country,g_whoregion,pct1920,pct1921,pct1922) %>%
  pivot_longer(cols = 'pct1920':'pct1922', names_to = "shortfall", values_to = "value") %>%
  mutate(value=value+1)

f2.1.4_sel_order <-
  f2.1.4_data %>%
  filter(shortfall == "pct1920") %>%
  arrange(value) %>%
  mutate(country = factor(country))

f2.1.4_txt <-   f2.1.4_data %>%
  filter(shortfall == "pct1920" | shortfall == "pct1921" ) %>%
  arrange(value) %>%
  filter(value < 0.8, iso3!="RUS") %>%
  mutate(country = factor(country)) %>%
  select(country) %>%
  distinct(country)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.1.5 ----
# (Irwin's doughnuts -- % completion of UNHLM targets)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Summary dataset for simple quoting of numbers in the text of section 2.1
f2.1.5_txt <- filter(notification, year >= 2018) %>%
  summarise(across(c(c_newinc,
                     c_new_014,
                     conf_rrmdr_tx,
                     unconf_rrmdr_tx,
                     conf_rr_nfqr_tx,
                     unconf_rr_nfqr_tx,
                     conf_rr_fqr_tx,
                     rrmdr_014_tx,
                     rr_nfqr_014_tx), sum, na.rm=TRUE)) %>%

  # Derive total enrolled on MDR treatment
  rowwise() %>%
  mutate(rr_treated = sum(across(conf_rrmdr_tx:conf_rr_fqr_tx), na.rm = TRUE)) %>%
  mutate(rr_treated_014 = sum(across(rrmdr_014_tx:rr_nfqr_014_tx), na.rm = TRUE)) %>%
  ungroup() %>%
  select(-conf_rrmdr_tx,
         -unconf_rrmdr_tx,
         -conf_rr_nfqr_tx,
         -unconf_rr_nfqr_tx,
         -conf_rr_fqr_tx,
         -rrmdr_014_tx,
         -rr_nfqr_014_tx)

f2.1.5_data <- f2.1.5_txt %>%

  # Calculate percentage complete for each UNHLM 2018-2022 target
  mutate(c_newinc_pct = c_newinc * 100/ 40e6,  # target: 40 million notified
         c_new_014_pct = c_new_014 * 100 / 3.5e6, # target: 3.5 million children notified
         rr_treated_pct  = rr_treated * 100 / 1.5e6,  # target: 1.5 million treated for drug-resistant TB
         rrmdr_014_tx_pct = rr_treated_014 * 100 / 115e3 # target: 115 thousand children treated for drug-resistant TB
         ) %>%

  select(contains("_pct"))  %>%

  pivot_longer(cols = contains("_pct"),
               names_to = "target_completion")

# Supplementary data for quoting in the text
f2.1.5_kids_txt <- filter(notification, year >= 2019) %>%
  group_by(year) %>%
  summarise(sum_kids = sum(c_new_014, na.rm = TRUE)) %>%
  ungroup %>%
  pivot_wider(names_from = year,
              names_prefix = "kids_",
              values_from = sum_kids) %>%
  mutate(kids_change_pct = abs(kids_2020 - kids_2019) * 100 / kids_2019)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.1.6 ----
# (Bar chart showing numbers of adults and children notified with TB each year since 2015)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f2.1.6_data <- filter(notification, year >= 2010) %>%

  select(iso2, year, c_newinc, c_new_014) %>%

  group_by(year) %>%
  summarise(across(starts_with("c_new"), sum, na.rm = TRUE)) %>%

  # calculate the "adult" fraction
  mutate(c_new_15plus = c_newinc - c_new_014) %>%

  # switch to long format for plotting
  pivot_longer(cols = starts_with("c_new_"),
               names_to = "age_group",
               values_to = "how_many")

f2.1.6_txt <- filter(notification, year == report_year-1) %>%

  select(iso2, year, c_newinc, c_new_014, newrel_f15plus, newrel_m15plus) %>%

  group_by(year) %>%
  summarise(across(contains("new"), sum, na.rm = TRUE)) %>%

  # calculate the "adult" fraction
  mutate(c_new_15plus = c_newinc - c_new_014) %>%

  #calculate pct
  mutate(pct_m = newrel_m15plus/c_newinc * 100,
         pct_f = newrel_f15plus/c_newinc * 100,
         pct_c = c_new_014/c_newinc * 100)



# global notification by age and sex group
agesex_notification <- filter(notification, year >= 2013) %>%
  
  select(iso2, year, c_new_014, newrel_f15plus, newrel_m15plus) %>%
  
  group_by(year) %>%
  summarise(across(contains("new"), sum, na.rm = TRUE)) 

f2.1.6b_data <- agesex_notification %>%
  # switch to long format for plotting
  pivot_longer(cols = contains("new"),
               names_to = "age_sex",
               values_to = "how_many") %>%
  mutate(age_sex = factor(age_sex, levels = c("newrel_m15plus", "newrel_f15plus", "c_new_014" ),
                          labels = c("Men (aged \u226515 years)", "Women (aged \u226515 years)", "Children (aged 0\u201314 years)")))


f2.1.6b_txt <- agesex_notification %>%
  mutate(c_new_014_lag = lag(c_new_014),
         newrel_f15plus_lag = lag(newrel_f15plus),
         newrel_m15plus_lag = lag(newrel_m15plus)) %>%
  mutate(c_new_014_pct_dif = (c_new_014 - c_new_014_lag)*100/c_new_014_lag,
         newrel_f15plus_pct_dif = (newrel_f15plus - newrel_f15plus_lag)*100/newrel_f15plus_lag,
         newrel_m15plus_pct_dif = (newrel_m15plus - newrel_m15plus_lag)*100/newrel_m15plus_lag) %>%
  select(year, contains("pct")) %>%
  filter(year==2020) 

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.1.7 ----
# (Map showing percentage of new and relapse TB cases that were children)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

kids_data <- filter(notification, year>=report_year-2) %>%

  select(iso3,
         country,
         year,
         c_new_014,
         newrel_m15plus,
         newrel_mu,
         newrel_f15plus,
         newrel_sexunk15plus,
         newrel_fu) %>%

  # calculate % of children in the age/sex data
  rowwise() %>%
  mutate(agesex_tot = sum(c_across(c_new_014:newrel_fu), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(kids_pct = ifelse(agesex_tot > 0,
                           c_new_014 * 100 / agesex_tot,
                           NA)) %>%

  # Assign the categories for the map
  mutate(var = cut(kids_pct,
                   c(0, 5.0, 10.0, 15.0, Inf),
                   c('0-4.9', '5-9.9', '10-14.9', '\u226515'),
                   right=FALSE))

# Find the countries with empty data for latest year and see if there are data for the previous year
kids_prev_year_data <- kids_data %>%
  filter(year == report_year - 1 & is.na(kids_pct)) %>%
  select(iso3) %>%
  inner_join(filter(kids_data, year == report_year - 2), by = "iso3") %>%
  filter(!is.na(kids_pct))

# Now combine into one dataframe, with previous data used if latest year's data are not available
f2.1.7_data <- kids_data %>%
  filter(year == report_year - 1) %>%
  anti_join(kids_prev_year_data, by= "iso3") %>%
  rbind(kids_prev_year_data)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.1.8 ----
# (Map showing percentage of extrapulmonary cases among new and relapse TB cases)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

ep_data <- notification %>%
  filter(year  >= report_year - 2) %>%
  select(iso3,
         country,
         year,
         new_labconf, new_clindx, new_ep,
         ret_rel_labconf, ret_rel_clindx, ret_rel_ep) %>%

  # calculate % of extrapulmonary cases
  rowwise() %>%
  mutate(newrel_tot = sum(c_across(new_labconf:ret_rel_ep), na.rm = TRUE)) %>%
  mutate(ep_tot = sum(c_across(contains("_ep")), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(ep_pct = ifelse(newrel_tot > 0,
                         ep_tot * 100 / newrel_tot,
                         NA)) %>%

  # Assign the categories for the map
  mutate(var = cut(ep_pct,
                   c(0, 10, 20, 30, Inf),
                   c('0-9.9', '10-19', '20-29', '\u226530'),
                   right=FALSE))

# Find the countries with empty data for latest year and see if there are data for the previous year
ep_prev_year_data <- ep_data %>%
  filter(year == report_year - 1 & is.na(ep_pct)) %>%
  select(iso3) %>%
  inner_join(filter(ep_data, year == report_year - 2), by = "iso3") %>%
  filter(!is.na(ep_pct))

# Now combine into one dataframe, with previous data used if latest year's data are not available
f2.1.8_data <- ep_data %>%
  filter(year == report_year - 1) %>%
  anti_join(ep_prev_year_data, by= "iso3") %>%
  rbind(ep_prev_year_data)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.1.9 ----
# (Map showing % of foreign borne cases)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f2.1.9_data <- notification %>%
  select(iso3,country,year,c_notified,notif_foreign) %>%
  filter(year==report_year-1|year==report_year-2) %>%
  mutate(pct_foreign = notif_foreign/c_notified * 100) %>%
  pivot_wider(names_from = year, values_from = c_notified:pct_foreign) %>%
  mutate(pct_foreign = ifelse(is.na(pct_foreign_2022),pct_foreign_2021,pct_foreign_2022)) %>%
  # Assign the categories for the map
  mutate(var = cut(pct_foreign,
                   c(0, 5, 25, 50, 75, Inf),
                   c('0\u20134','5\u201324','25\u201349','50\u201374','\u226575'),
                   right=FALSE)) 

f2.1.9_data %>%
  filter(!is.na(pct_foreign)) %>% nrow()
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.1.10 ----
# (Panel plot of line charts showing percentage contribution of the private sector to TB notifications by year since 2010)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# for Monica: country selection
ppm_disp <- data_collection %>%
  filter(dc_ppm_display==1 & datcol_year==2023) %>%
  select(iso2)

# Get PPM data for 7 priority countries
f2.1.10_data <- 
  filter(strategy, year >= 2010 & iso2 %in% ppm_disp$iso2) %>%
  select(iso2, year, country, priv_new_dx,pub_new_dx) %>%
  
  # Merge with notifications
  inner_join(select(notification, iso2, year, c_notified), by = c("iso2", "year")) %>%
  
  # Calculate percent contributions
  mutate(private_pcnt = ifelse(c_notified > 0,
                               priv_new_dx * 100 / c_notified,
                               NA),
         public_pcnt = ifelse(c_notified > 0,
                              pub_new_dx * 100 / c_notified,
                              NA)) %>%
  select(iso2:country,private_pcnt:public_pcnt) %>%
  pivot_longer(names_to = "pp", cols = private_pcnt:public_pcnt, values_to = "pcnt") %>%
  mutate(pcnt = ifelse(pcnt==0,NA,pcnt)) 

writexl::write_xlsx(f2.1.10_data, here::here("./report/local/f2.1.10_data.xlsx"))

f2.1.10a_data <- 
  filter(f2.1.10_data, iso2 %in% c('BD', 'IN', 'ID', 'KE', 'NG', 'PK', 'PH')) %>%
  mutate(pcnt = ifelse(pcnt==0,NA,pcnt)) %>%
  filter(!is.na(pcnt))

# Get PPM data for 18 non-priority countries
f2.1.10b_data <- 
  filter(f2.1.10_data, iso2 %in% c('AF', 'KH', 'SZ', 'ET', 'GH','IR', 'IQ', 'JO','LB','MW', 'MA','MM', 'NP', 'LK','SO', 'TH', 'TZ', 'VN')) 

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.1.11 ----
# (Map showing percentage of management units with community contributions to case finding and/or treatment support)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f2.1.11_data <- strategy %>%
  filter(year==report_year - 1) %>%
  select(iso3,
         country,
         bmu,
         bmu_community_impl,
         community_data_available)%>%
  mutate(comm_pct = ifelse(bmu > 0,
                           bmu_community_impl * 100 / bmu,
                           NA))  %>%

  # Assign the categories for the map
  mutate(var = cut(comm_pct,
                   c(0, 25, 50, 75, Inf),
                   c('0-24', '25-49', '50-74', '\u226575'),
                   right=FALSE)) #%>%

  # Link to data request status (used in the footnote for the figure)
if(datacoll){

  # Get the variable to identify countries requested to report on community indicators
  comm_datarequest <- data_collection %>%
    filter(datcol_year==report_year) %>%
    select(iso3,
           dc_engage_community_display)
  
f2.1.11_data <- f2.1.11_data %>%
  inner_join(comm_datarequest, by = "iso3")

}




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 2.1.12 ----
# (Map showing which countries have case-based TB surveillance systems)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

if(datacoll){
  
cb_data <- strategy %>%
  filter(year >= report_year - 3) %>%
  select(iso3,
         country,
         year,
         caseb_err_nat)

# Make sure all ECDC countries are marked as having case-based surveillance for all TB cases
cb_ecdc <- data_collection %>%
  filter(datcol_year == report_year & dc_ecdc == 1) %>%
  select(iso3)

cb_data <- cb_data %>%
  mutate(caseb_err_nat = ifelse(iso3 %in% cb_ecdc$iso3, 42, caseb_err_nat)) %>%

  # UK hasn't responded, but we know it has a case-based system, so fudge it
  mutate(caseb_err_nat = ifelse(iso3=="GBR", 42, caseb_err_nat)) %>%

  # Assign the categories for the map
  mutate(var = factor(caseb_err_nat,
                      levels = c(0, 44, 43, 42),
                      labels = c("None", "Partially (in transition)", "MDR-TB patients only", "All TB patients")))

# Find the countries with empty data for latest year and see if there are data for the previous year
cb_prev_year_data <- cb_data %>%
  filter(year == report_year - 1 & is.na(caseb_err_nat)) %>%
  select(iso3) %>%
  inner_join(filter(cb_data, year >= report_year - 5), by = "iso3") %>%
  arrange(desc(year)) %>%
  filter(!is.na(caseb_err_nat)) %>% distinct(iso3, .keep_all = TRUE)

# Now combine into one dataframe, with previous data used if latest year's data are not available
f2.1.12_data <- cb_data %>%
  filter(year == report_year - 1) %>%
  anti_join(cb_prev_year_data, by= "iso3") %>%
  rbind(cb_prev_year_data)


# Simple summary for the section text
## the latest year
f2.1.12_txt <- filter(f2.1.12_data, caseb_err_nat == 42) %>%
  select(iso3) %>%
  inner_join(filter(notification, year==report_year-1), by = "iso3") %>%
  summarise(across(c_newinc, sum, na.rm=TRUE))

f2.1.12_txt <- filter(notification, year==report_year-1) %>%
  summarise(across(c_newinc, sum, na.rm=TRUE)) %>%
  select(c_newinc_glob = c_newinc) %>%
  cbind(f2.1.12_txt) %>%
  mutate(pct_notif_caseb = c_newinc*100/c_newinc_glob) %>%
  select(pct_notif_caseb)

f2.1.12_txt <- 
  filter(f2.1.12_data, caseb_err_nat == 42) %>% nrow() %>%
  cbind(f2.1.12_txt) %>%
  rename(cb_n = 1)
   
## the latest year - 1
cb_2021_data <- cb_data %>%
  filter(year == report_year - 2 & is.na(caseb_err_nat))  %>%
  select(iso3) %>%
  inner_join(filter(cb_data, year == report_year - 3), by = "iso3") %>%
  filter(!is.na(caseb_err_nat))

cb_2021_data <- cb_data %>%
  filter(year == report_year - 2) %>%
  anti_join(cb_2021_data, by= "iso3") %>%
  rbind(cb_2021_data)

cb_2021_data_txt <- filter(cb_2021_data, caseb_err_nat == 42) %>%
  select(iso3) %>%
  inner_join(filter(notification, year==report_year-2), by = "iso3") %>%
  summarise(across(c_newinc, sum, na.rm=TRUE))

cb_2021_data_txt <- filter(notification, year==report_year-2) %>%
  summarise(across(c_newinc, sum, na.rm=TRUE)) %>%
  select(c_newinc_glob = c_newinc) %>%
  cbind(cb_2021_data_txt) %>%
  mutate(pct_notif_caseb = c_newinc*100/c_newinc_glob) %>%
  select(pct_notif_caseb)

cb_2021_data_txt <- 
  filter(cb_2021_data, caseb_err_nat == 42) %>% nrow() %>%
  cbind(cb_2021_data_txt) %>%
  rename(cb_n = 1)

}

