library(ggplot2)
library(stringr)
library(dplyr)
library(tidyr)

# Establish the report year ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
report_year <- 2023
# And the latest year for which OECD data are being displayed in graphics. Always two years older than report year
latest_year <- 2021

# Load donor data from OECD - summary objects only ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Country info
base_folder <- dirname(here::here("FT_OECD.rmd")) 
data_folder_name = "finance/output" # 4 Sep 2023, moved from folder "local/mini" to current

country_info <- haven::read_dta(here::here(base_folder,data_folder_name,"country_info.dta"))

# Load data ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
most_recent_OECDworkbook = "OECD data ODA 2023_10_26_update.xlsx" # An update to OECD data has been made available in Sept 2023

# Bilateral and multilateral contributions reported to OECD
oecd <- readxl::read_excel(here::here("./finance/output",most_recent_OECDworkbook),sheet = "TB") %>% 
  rename(donor = 1, recipient = 2, year = 3, value = 4)

# US contribution reported in KFF
us_kff <- read.csv(here::here("./report/ft_oecd_data/data-y0Him.csv")) %>%
  rename(year = 1, value = 2)

# ODA bilateral contributions only
oda <- readxl::read_excel(here::here("./finance/output",most_recent_OECDworkbook),sheet = "Total PHP") %>% 
  rename(donor = 1, programme = 3, year = 2, value = 4)

# GF for year 2020-22
gf_20to22 <- readxl::read_excel(here::here("./finance/output",most_recent_OECDworkbook),sheet = "Perc-GF-contributions-2020-22") %>% 
  rename(donor = 1, pled = 2, cont = 3, pcnt = 4) 
  
gf_20to22 <- gf_20to22 %>%
  summarise(total_cont = sum(cont, na.rm = TRUE)) %>%
  cbind(gf_20to22) %>%
  select(donor, pled, cont, total_cont) %>%
  mutate(pcnt = cont/total_cont)
  
# GF for year 2017-19
gf_17to19 <- readxl::read_excel(here::here("./finance/output",most_recent_OECDworkbook),sheet = "Perc-GF-contributions-2017-19") %>% 
  rename(donor = 1, pled = 2, cont = 3, pcnt = 4)

gf_17to19 <- gf_17to19 %>%
  summarise(total_cont = sum(cont, na.rm = TRUE)) %>%
  cbind(gf_17to19) %>%
  select(donor, pled, cont, total_cont) %>%
  mutate(pcnt = cont/total_cont)

# GF for year 2014-16
gf_14to16 <- readxl::read_excel(here::here("./finance/output",most_recent_OECDworkbook),sheet = "Perc-GF-contributions-2014-16") %>% 
  rename(donor = 1, pled = 2, cont = 3, pcnt = 4) 

gf_14to16 <- gf_14to16 %>%
  summarise(total_cont = sum(cont, na.rm = TRUE)) %>%
  cbind(gf_14to16) %>%
  select(donor, pled, cont, total_cont) %>%
  mutate(pcnt = cont/total_cont)

# GF for year 2011-13
gf_11to13 <- readxl::read_excel(here::here("./finance/output",most_recent_OECDworkbook),sheet = "Perc-GF-contributions-2011-13") %>% 
  rename(donor = 1, pled = 2, cont = 3, pcnt = 4) 

gf_11to13 <- gf_11to13 %>%
  summarise(total_cont = sum(cont, na.rm = TRUE)) %>%
  cbind(gf_11to13) %>%
  select(donor, pled, cont, total_cont) %>%
  mutate(pcnt = cont/total_cont)

# merge GF data
gf <- rbind.data.frame(gf_20to22 %>% mutate(year = 2022), 
                       gf_20to22 %>% mutate(year = 2021),
                       gf_20to22 %>% mutate(year = 2020),
                       gf_17to19 %>% mutate(year = 2019),
                       gf_17to19 %>% mutate(year = 2018),
                       gf_17to19 %>% mutate(year = 2017),
                       gf_14to16 %>% mutate(year = 2016),
                       gf_14to16 %>% mutate(year = 2015),
                       gf_14to16 %>% mutate(year = 2014),
                       gf_11to13 %>% mutate(year = 2013))



#---------------------------
# fig 1
#---------------------------
Fig1 <- oecd %>%
  left_join(country_info %>% select(country,iso3), by = c("recipient" = "country"))

# Matching on country names is always messy. Here we list then label the countries mismatched
# Fig1 %>% filter(is.na(iso3) & year == 2021 & donor == "Official Donors, Total")
Fig1 <- Fig1 %>% 
  mutate(iso3 = ifelse(recipient=="Bolivia","BOL",iso3)) %>% 
  mutate(iso3 = ifelse(recipient=="China (People's Republic of)","CHN",iso3)) %>% 
  mutate(iso3 = ifelse(recipient=="Iran","IRN",iso3)) %>% 
  # mutate(iso3 = ifelse(recipient=="Kosovo","",iso3)) %>% 
  mutate(iso3 = ifelse(recipient=="Micronesia","FSM",iso3)) %>% 
  mutate(iso3 = ifelse(recipient=="Moldova","MDA",iso3)) %>% 
  mutate(iso3 = ifelse(recipient=="Tanzania","TZA",iso3)) %>% 
  mutate(iso3 = ifelse(recipient=="West Bank and Gaza Strip","PSE",iso3)) %>% 
  mutate(donor = ifelse(donor == "United Kingdom", "Other", donor)) %>%
  left_join(country_info %>% select(iso3,g_whoregion, g_income), by = "iso3")


all_donation <- Fig1 %>% 
  filter(year == latest_year & donor == "Official Donors, Total" & recipient == "Developing Countries, Total") %>% 
  summarise(sum(value)) %>% unlist() %>% round(0)

gf_donation <- Fig1 %>% 
  filter(year == latest_year & donor == "Global Fund" & recipient == "Developing Countries, Total") %>% 
  summarise(sum(value)) %>% unlist() %>% round(0)

Fig1 <- Fig1 %>%
  # filter(donor != "Official Donors, Total") %>%
  filter(recipient == "Developing Countries, Total" | !is.na(g_whoregion)) %>%
  filter(year >= 2013) %>% 
  mutate(donor = str_replace_all(donor,pattern = "Official Donors, Total",replacement = "Total")) %>% 
  # Prepare faceting variable, in factor form
  mutate(g_whoregion = ifelse(is.na(g_whoregion),"LMICs",g_whoregion)) %>% 
  mutate(g_whoregion = stringr::str_to_lower(g_whoregion)) %>% 
  mutate(g_whoregion = factor(g_whoregion,
                              levels = c(
                                "lmics", "afr","amr", "sea",
                                "eur","emr","wpr"),
                              labels = c( "Low- and middle-income\ncountriesᵃ",   #\u1d43 for superscript a
                                          "WHO African Region","WHO Region of\nthe Americas", 
                                          "WHO South-East Asia\nRegion","WHO European Region", 
                                          "WHO Eastern Mediterranean\nRegion","WHO Western Pacific\nRegion" ))) %>% 
  #Prepare series variable in factor format, ordered
  mutate(donor = factor(stringr::str_to_lower(donor),
                        levels = c("total","global fund","united states","other"),
                        labels = c("Total","Global Fund","United States of America","Other"))) %>% 
  group_by(year, donor, g_whoregion) %>% 
  summarise_at(.vars = "value", sum, na.rm = TRUE) %>% 
  ungroup() 


#---------------------------
# fig 2
#---------------------------
Fig2 <- us_kff # only to rename dataframe


#---------------------------
# fig 3
#---------------------------
# overall for 2013-21
## summarize contributions via GF
oecd_gf <- oecd %>%
  filter(year >= 2013 & year <= report_year-1 & donor == "Global Fund" & recipient == "Developing Countries, Total")


# reallocate Debt2 funding to country contributions
gf %>%
  filter(str_detect(donor, 'Debt2')) 

# gf <- gf %>%
#   mutate(donor= ifelse((str_detect(donor, 'Debt2') & str_detect(donor, 'Germany')), 'Germany', 
#                        ifelse((str_detect(donor, 'Debt2') & str_detect(donor, 'Spain')), 'Spain', 
#                               ifelse((str_detect(donor, 'Debt2') & str_detect(donor, 'Australia')), 'Australia', donor))))
  
oecd_gf <- gf %>%
  right_join(select(oecd_gf, year,value), by = c("year")) %>%
  mutate(gf_cont = value * pcnt)

Fig3_gf <- oecd_gf %>%
  select(donor, gf_cont) %>%
  group_by(donor) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  arrange(desc(gf_cont)) %>%
  ungroup()

## summarize ODA bilateral contributions
oda_tb <- oda %>%
  filter(year >= 2013 & year <= report_year-1 & programme == "Tuberculosis control" & donor != "Official Donors, Total")

oda_tb_trend <- oda %>%
  filter(year >= 2013 & year <= report_year-1 & programme == "Tuberculosis control" & donor == "Official Donors, Total") %>%
  select(year, tb_total = value)

oda_tb_total <- oda %>%
  filter(year >= 2013 & year <= report_year-1 & programme == "Tuberculosis control" & donor == "Official Donors, Total") %>%
  select(donor,value) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) 

Fig3_oda <- oda_tb %>%
  select(donor, oda_cont = value) %>%
  group_by(donor) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  arrange(desc(oda_cont)) %>%
  mutate(donor = ifelse(donor == "Korea", "Korea (Republic)", donor)) %>%
  ungroup()

## merge two datasets
Fig3_all <- Fig3_gf %>%
  full_join(Fig3_oda, by = c("donor"))

## calculate total contributions by entity
Fig3_all <- Fig3_all %>%
  rowwise() %>%
  mutate(entity_total = sum(gf_cont, oda_cont, na.rm = TRUE)) %>%
  arrange(desc(entity_total)) %>%
  ungroup()

entity_grand_total <- Fig3_all %>%
  summarise(entity_total = sum(entity_total, na.rm = TRUE)) 

other_total = oda_tb_total$value - entity_grand_total$entity_total

## calculate overall contributions from all entities
Fig3_all <- Fig3_all %>%
  mutate(grand_total = oda_tb_total$value) %>%
  mutate(pcnt = entity_total/grand_total * 100) 

Fig3_top10 <- Fig3_all %>%
  filter(!is.na(oda_cont)) %>%
  top_n(10)

Fig3_other <- Fig3_all %>%
  filter(!donor %in% Fig3_top10$donor) 

Fig3_other <- Fig3_other %>%
  add_row(donor = "Other reporting entities", entity_total = other_total, grand_total = oda_tb_total$value) %>%
  mutate(pcnt = entity_total/grand_total * 100) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE))

Fig3 <- plyr::rbind.fill(Fig3_top10,Fig3_other) %>%
  mutate(donor = ifelse(is.na(donor),"Other entities",donor),
         grand_total = ifelse(grand_total != oda_tb_total$value, oda_tb_total$value, grand_total),
         gf_cont = ifelse(donor == "Other entities", NA, gf_cont),
         oda_cont = ifelse(donor == "Other entities", NA, oda_cont)) 
  
writexl::write_xlsx(Fig3, here::here("./report/local/oecd_fig3_data.xlsx"))

Fig3 <- Fig3 %>%
  mutate(subgroup = ifelse(donor=="United States",2,
                           ifelse(donor=="Other entities",1,3)),
         donor = ifelse(donor == "United States", "United States of America", donor))

Fig3 <- Fig3 %>%
  mutate(donor = factor(donor, levels = c("United States of America","United Kingdom","France",
                                          "Germany","Japan","Canada",
                                          "Australia","Sweden","Norway",
                                          "Netherlands","Other entities")),
         text_col = "white",
         # text_col = ifelse(donor=="Other entities"|donor=="Norway"|donor=="Netherlands"|donor=="Sweden", "grey40", "white"),
         # fill_col = ifelse(donor=="United States of America","#004529","#4BB062" #"#80BC00"
         fill_col = ifelse(donor=="United States of America","#00205C", "#A6228C" 
                           ))

# US contribution trend for 2013-2021
## summarize contributions via GF
us_oda <- oecd %>%
  filter(year >= 2013 & year <= report_year-1 & donor == "United States" & recipient == "Developing Countries, Total") %>%
  select(year, oda_cont = value)

us_trend <- oecd_gf %>%
  filter(donor == "United States") %>%
  right_join(us_oda, by = c("year")) %>%
  select(donor, year, pled:total_cont, us_pcnt_gf = pcnt, gf_annual = value, us_gf_cont = gf_cont, us_oda_cont = oda_cont) %>%
  mutate(us_cont_total = us_gf_cont + us_oda_cont) %>%
  right_join(oda_tb_trend, by = c("year")) %>%
  mutate(us_pcnt = us_cont_total/tb_total*100) %>%
  arrange(year)

writexl::write_xlsx(us_trend, here::here("./report/local/oecd_us_trend_data.xlsx")) # share this with USAID as necessary.


#---------------------------
# fig 4
#---------------------------
Fig4 <- oda %>%
  filter(year >= 2013 & year <= report_year-1 & donor == "Official Donors, Total") %>%
  mutate(value = value/1e3L)

Fig4 <- Fig4 %>%
  group_by(year) %>% 
  mutate(programme = factor(programme,
                            labels = c("Malaria control", "STD control including HIV","Total health and population programmes","TB control"))) %>%
  mutate(programme = factor(programme,
                       levels = c("TB control","STD control including HIV","Malaria control", "Total health and population programmes"))) %>%
  ungroup()
