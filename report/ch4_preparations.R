# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data preparation script for ch4 and Featured Topic on donor financing 

# This script reads in some large datasets (.dta files) from the local folder,
# and produces smaller output files (.csv and .rds formats) into the data folder
# which is then referenced by the R Markdown script to render figures and dynamic
# texts. For best results, this script should be run only once on a local machine 
# (due to size of input files)
# 

# Created by Peter Nguhiu & Ines Garcia Baena in Jul - Sep 2021, updated
# version dated 13 Jun 2023 includes comments added to support portability.

# Updated current version dated 4 - 6 Sept 2023 thanks to Andrew Siroka and
# Hazim Timimi's work that led to incorporation of stata dataset and files into
# the github folder. One no longer needs to save RDS objects henceforth, since
# this preparations file will be parsed as an include for the ch4 markdown file.

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# First step. Confirm current project folder using the here package, by querying
# if this project file can be seen. 
#install.packages("here")
here::i_am("report/ch4_preparations.R")

base_folder <- dirname(here::here("ch4_preparations.R")) 

# Identify names of respective sub folders where data would be read from / written to
# To prevent clogging of GITHub, the respective folders with inputs and outputs are first set under local folder
data_folder_name = "finance/output" # 4 Sep 2023, moved from folder "local/mini" to current

outputs_folder_name = "report/ch4_data"
csv_folder_name = "report/local/figures/ch4" # Change this to your local folder as needed

# library(magrittr)
library(tidyverse)
# library(ggthemes)
# library(whomap)
# library(gridExtra)
# library("Cairo")
library("dplyr")
library(gtbreport)

# Using updated theme files from the GtbReport project (downloaded from Hazim's
# repository)

# source(paste0(base_folder,script_folder_name,"palette_gtb.R")) 
# PN Nov 2021: I wasn't able to use the pallete_gtb function to apply
# uniform colours, and therefore had to hard code the colours on each image. Mark
# for further improvement in coming years

wrapper <- function(x, ...) 
{
  # this function allows long titles and labels to be wrapped at a specified
  # distance x. From Stackoverflow.com
  paste(strwrap(x, ...), collapse = "\n")
}


####### Read required datasets #####

# Three datasets needed, a) Figuredata = imputed global finance data (balanced,
# const USD). In 2023 all LMICs were included in balanced dataset. Used 
# 2022 USD b) Tabledata = pre_imputation dataset with nominal currency (before
# conversion to constant USD) and all LMIC data. c) Global Plan 2018 - 2022
# (Jun 2020 update version) with values converted to 2022 dollars 

figuredata <- haven::read_dta(here::here(base_folder,data_folder_name,"finance_pred_cus_balanced.dta"))
# imputeddata <- haven::read_dta(here::here(base_folder,data_folder_name,"finance_pred_nus.dta"))
tabledata <- haven::read_dta(here::here(base_folder,data_folder_name,"finance_imputable_nomusd.dta"))

global_plan_18 <-  readxl::read_excel(here::here(base_folder,data_folder_name,"Ines_DataJuly2020_final.xls")) %>%
  select(iso3 = ISO3, g_inc_2018 = `Income Status`,year = Year, c("Program Support":"Total Need")) %>%
  select(-...18)
global_plan_23 <-  readxl::read_excel(here::here(base_folder,data_folder_name,"TBGP_2023_2030.xls")) %>%
  select(iso3 = ISO3, g_inc_2022 = `WB classification`,year, c("Drug_sensitive_TB":"Total")) %>%
  select(-starts_with("..."))
# define some constants
latest_year <- 2022 # For graph labels and extents. 
report_year <- 2023 # For text labels etc


# Sept 2023: Based on individual audit of country reported data, some small
# countries (identified via variable do_not_fill) don't have reliable rcvd_*
# data to inform the trends shown in the report

# figuredata %>%
#  filter(do_not_fill == 1) %>% select(iso3, year, rcvd_tot, rcvd_mdr,
#  rcvd_nmdr_dot, rcvd_nmdr_ndot_nhiv_oth, rcvd_int, rcvd_ext_gf) %>% View()

figuredata <- figuredata %>% 
  mutate(rcvd_tot = ifelse(do_not_fill==1,0,rcvd_tot),
         rcvd_mdr = ifelse(do_not_fill==1,0,rcvd_mdr),
         rcvd_nmdr = ifelse(do_not_fill==1,0,rcvd_nmdr),
         rcvd_mdr_sld = ifelse(do_not_fill==1,0,rcvd_mdr_sld),
         rcvd_nmdr_dot = ifelse(do_not_fill==1,0,rcvd_nmdr_dot),
         rcvd_nmdr_ndot_nhiv_oth = ifelse(do_not_fill==1,0,rcvd_nmdr_ndot_nhiv_oth),
         rcvd_nmdr_ndot_nhiv_noth = ifelse(do_not_fill==1,0,rcvd_nmdr_ndot_nhiv_noth),
         rcvd_int = ifelse(do_not_fill==1,0,rcvd_int),
         rcvd_ext_ngf = ifelse(do_not_fill==1,0,rcvd_ext_ngf),
         rcvd_ext_gf = ifelse(do_not_fill==1,0,rcvd_ext_gf))

# India 40 million in 2022 for staff
figuredata <- figuredata %>% 
  mutate(rcvd_staff = ifelse(iso3 == "IND" & year == 2022 & rcvd_staff == 15301073, rcvd_staff + 40e6, rcvd_staff ),
         rcvd_nmdr_dot = ifelse(iso3 == "IND" & year == 2022 & rcvd_nmdr_dot == 133118640, rcvd_nmdr_dot + 40e6, rcvd_nmdr_dot ),
         rcvd_int = ifelse(iso3 == "IND" & year == 2022 & rcvd_int == 211948067, rcvd_int + 40e6, rcvd_int ),
         rcvd_tot = ifelse(iso3 == "IND" & year == 2022 & rcvd_tot == 329668760, rcvd_tot + 40e6, rcvd_tot ))


## PN  Jun 2023
###### Resource needs estimated for LMICs, disaggregated and aggregated #####

# This section just confirms that the list of countries to be included in the analysis is complete
eligible_gf_23 <- readxl::read_xlsx(path = here::here(base_folder,data_folder_name,"GF eligible_21Jul21.xlsx"),
                                    sheet = "gf_elig_21",
                                    range = "B1:C139") %>%  #How many countries included? 137 LMICs
  select(iso3, GF_eligible_2021) %>%
  mutate(GF_eligible_2023 = GF_eligible_2021) %>% # plus a few changes outlined below}
  filter(!is.na(iso3))
# sum(eligible_gf_23$GF_eligible_2021) # As at Jan 2021, 109 countries were eligible
# length(eligible_gf_23$GF_eligible_2021) # This would be 137 from the 2021 file, even thoguh Romania is now HIC
# eligible_gf_23 <- eligible_gf_23 %>% filter(iso3 != "ROU")

# Updates from https://www.theglobalfund.org/media/11712/core_eligiblecountries2022_list_en.pdf and
# https://www.theglobalfund.org/media/12505/core_eligiblecountries2023_list_en.pdf
# PN 2023: Iran moved to LMC and is now eligible for TB funding.
eligible_gf_23$GF_eligible_2023[eligible_gf_23$iso3 == "IRN"] <- 1
# PN 2023: Jordan transitioned out in 2021, only eligible for transition funding in 2021. 
eligible_gf_23$GF_eligible_2023[eligible_gf_23$iso3 == "JOR"] <- 0
# Libya is eligible as from 2021.
eligible_gf_23$GF_eligible_2023[eligible_gf_23$iso3 == "LBY"] <- 1


###### Figure 4.1: Estimates of funding required for TB prevention, diagnosis ######
# and treatment in 128 low- and middle-income countries in
# the Global Plan to End TB

Fig4.1_disagg <-  eligible_gf_23 %>% 
  left_join(global_plan_18 , by = "iso3" )

# check how many come through? (129 in 2022, including Romania and Panama. Will filter off by current grouping of g_income later)
# Fig4.1 %>% filter(year == latest_year) %>% dim()

# Include a deflator plus most recent g_income variable from snapshot data
Fig4.1_disagg <- Fig4.1_disagg %>% left_join(tabledata %>% filter(g_income != "HIC" & year >= 2018 & year <= 2022) %>% 
                                 select(iso3, year, deflator_us, imf_deflator_us, g_income),
                               by = c("year","iso3"))
# Fig4.1 %>% filter(year == latest_year & g_income != "HIC") %>% dim()

# In 2023 report, the figures will be reported in constant 2018 US$ so no need to
# pick deflator value (to convert GP amounts from 2018 to current year const)
# deflator_us_2018 <- Fig4.1 %>%  
#   filter(year== 2018 & !is.na(deflator_us)) %>% 
#   select(deflator_us) %>%  
#   summarise_all(mean, na.rm=T) %>% unlist()


# All  LMICs in this year's lot (note that Romania  was transitioned back to HIC in July 2022)
Fig4.1_disagg <- Fig4.1_disagg %>% 
  mutate(GP_TBHIV = HIV,
         GP_TPT = `Preventative theraphy`,
         GP_DSTB = Fig4.1_disagg %>% dplyr::select(`Program Support`, FLD,`FL hospt and ambltry care`,`Enabling costs`) %>% rowSums(na.rm = T),
         GP_MDR = Fig4.1_disagg %>% dplyr::select(SLD, MDRMgmt,LAB, `SL hospt and ambltry care`) %>% rowSums(na.rm = T),
         GP_Total = `Total Need`) %>% 
  # If we only want the subset that is eligible for GF funding in 2023, then
  # filter(GF_eligible_2023==1) %>%
  #All LMICs that had a GP projection in 2018 - 128 since Romania was included in 2018 projection)
 filter(!is.na(GP_Total) & g_inc_2018 != "HI" & year >= 2018) %>% 
  # In 2023, it was agreed that the chart be reported in dollar values for the respective year of report
  # The code below would otherwise inflate GP to constant latest_year US$ values # 
  # mutate_at(vars(`Program Support`,FLD,SLD,MDRMgmt,LAB,HIV, `FL hospt and ambltry care`,
  #                `SL hospt and ambltry care`, `Preventative theraphy`,
  #                `Enabling costs`,`Total Need`,
  #                starts_with("GP_")),
  #           function(x) x/( deflator_us_2018) ) %>%
  arrange(iso3,year) %>% 
  select(iso3,year, g_income, starts_with("GP_")) 

Fig4.1 <- Fig4.1_disagg %>% 
  group_by(year) %>% 
  summarise(GP_TBHIV = sum(GP_TBHIV/1E09, na.rm = TRUE),
            GP_TPT = sum(GP_TPT/1E09, na.rm = TRUE),
            GP_DSTB = sum(GP_DSTB/1E09, na.rm = TRUE),
            GP_MDR = sum(GP_MDR/1E09, na.rm = TRUE),
            GP_Total = sum(GP_Total/1E09, na.rm = TRUE),
            n = n())

###### Figure 4.2: The 134 low- and middle-income countries ######
# included in analyses of TB financing, 2013–2022 

# a) List of countries, plus their inclusion status

Fig4.2 <- tabledata %>% 
  # filter(g_income != "HIC" | g_income == "RUS") %>% 
  filter(year == latest_year) %>% 
  mutate(var = ifelse(include == 0,"Not included","Included")) %>%  
  mutate(var = factor(var, levels = c("Included","Not included"),
                      labels = c("Included","Not included"))) %>% 
  select(iso3, var, g_income, country, c_notified) %>% 
  arrange(iso3) 


include_list <- Fig4.2  %>% filter(var == "Included") %>%  select(iso3) 
countries <- include_list %>% dim() #How many countries included?
countries <- countries[1]

# Associated proporion notified among countries included above
burden_tot <- Fig4.2  %>% group_by(var) %>%  
  select(c_notified) %>%  summarise_all(sum , na.rm = T)
# Percentage of all notified, that are accounted for in the current gtb report
burden_tot$prop <- NA
burden_tot$prop  <- round(100*prop.table(burden_tot[,2]), digits = 0) %>% unlist()
burden_inc <- burden_tot$prop[1]



# Next set of graphs rely on this file

Fig4.3_4.6 <- figuredata %>% 
  filter((iso3 %in% include_list$iso3)) %>% 
  filter(year >= 2013 & year <= report_year) 

# This block is retained for historical documentation purposes only, and ensures that
# rcvd_ variables have values in report_year. For the past when preliminary commitments 
# used to be reported as funding available in most recent year
Fig4.3_4.6 <- Fig4.3_4.6  %>% 
  mutate(rcvd_mdr  = ifelse(year == report_year, cf_mdr , rcvd_mdr ),
         rcvd_nmdr_dot = ifelse(year == report_year, cf_nmdr_dot, rcvd_nmdr_dot),
         rcvd_nmdr_ndot_hiv = ifelse(year == report_year, cf_nmdr_ndot_hiv, rcvd_nmdr_ndot_hiv),
         rcvd_nmdr_ndot_tpt = ifelse(year == report_year, cf_nmdr_ndot_tpt, rcvd_nmdr_ndot_tpt),
         rcvd_nmdr_ndot_nhiv = ifelse(year == report_year, cf_nmdr_ndot_nhiv, rcvd_nmdr_ndot_nhiv),
         rcvd_nmdr_ndot_nhiv_noth = ifelse(year == report_year, cf_nmdr_ndot_nhiv_noth, rcvd_nmdr_ndot_nhiv_noth),
         rcvd_nmdr_ndot_nhiv_oth = ifelse(year == report_year, cf_nmdr_ndot_nhiv_oth, rcvd_nmdr_ndot_nhiv_oth),
         rcvd_int = ifelse(year == report_year, cf_int, rcvd_int),
         rcvd_ext_gf = ifelse(year == report_year, cf_ext_gf, rcvd_ext_gf),
         rcvd_ext_ngf = ifelse(year == report_year, cf_ext_ngf, rcvd_ext_ngf),
         rcvd_tot = ifelse(year == report_year, cf_tot, rcvd_tot)
  ) 

# Generate BRICs code. This manual coding needs review in the case that BRIC
# countries increase to 11 in 2024
Fig4.3_4.6 <- Fig4.3_4.6  %>% 
  mutate(g_brics = ifelse(iso3 %in% c("CHN","BRA","IND","RUS","ZAF"),"bric",
                          ifelse(g_hb_tb == 1, "hbc","oth"))) 

# How many countries in each g_brics category? Obtain for labelling below
g_bric_count <- Fig4.3_4.6 %>% filter(year == latest_year) %>% group_by(g_brics) %>% summarise(count=n())
# label them
Fig4.3_4.6 <- Fig4.3_4.6 %>% 
  mutate(g_brics = factor(g_brics,
                          levels = c("bric","hbc","oth"),
                          labels = c(paste0("BRICS (n=",g_bric_count$count[g_bric_count$g_brics=="bric"],")"),
                                     paste0("High TB burden and global TB watchlist countries outside BRICS\u1d43 (n=",g_bric_count$count[g_bric_count$g_brics=="hbc"],")"),
                                     paste0("Rest of world (n=",g_bric_count$count[g_bric_count$g_brics=="oth"],")")
                          )))

##### Figure 4.3: Funding for TB prevention, diagnosis and treatment for XXX countries #####
# Compared with global target set at teh UN high level meeting on TB 

Fig4.3 <- Fig4.3_4.6 %>% 
  select(iso3, year)
Fig4.3$int <- Fig4.3_4.6 %>%  select(rcvd_int, c_ghs_nmdr, c_ghs_mdr) %>%   mutate_all(function(x) x/(1.0E09)) %>%  rowSums(na.rm = T)
Fig4.3$ext <- Fig4.3_4.6 %>%  select(rcvd_ext_gf, rcvd_ext_ngf) %>%   mutate_all(function(x) x/(1.0E09)) %>%  rowSums(na.rm = T)
Fig4.3$tot <- Fig4.3_4.6 %>%  select(rcvd_tot, c_ghs_nmdr, c_ghs_mdr) %>%   mutate_all(function(x) x/(1.0E09)) %>%  rowSums(na.rm = T)

Fig4.3 <- Fig4.3 %>%  
  filter(year >= 2018 & year <= latest_year)



# sapply(c("int","gf","ngf","oth"), palette_gtb)
# Stacked bar chart
Fig4.3 <- Fig4.3 %>% 
  select(-iso3, -tot) %>% 
  group_by(year) %>% 
  summarise_all(sum, na.rm = T) %>% 
  pivot_longer(cols = c("int","ext")) %>% 
  mutate(year = factor(year),
         name = factor(name, 
                       levels = c("int","ext"),
                       labels = c("Domestic funding","International donor funding"))) 



# Estimate how much domestic funding is captured here, and how much estimated GHS?
Fig4.3_domestic <- Fig4.3_4.6 %>% 
  select(year,rcvd_int, c_ghs_inpatient, c_ghs_outpatient) %>% 
  group_by(year) %>% 
  summarise_all(sum, na.rm = T) 

# The variables below are used for text portion of markdown file
ghs_2019 <- Fig4.3_domestic %>% filter(year==2019) %>% select(c_ghs_inpatient, c_ghs_outpatient) %>% sum() %>% {round(1E-9*.,1)}
ghs_2020 <- Fig4.3_domestic %>% filter(year==2020) %>% select(c_ghs_inpatient, c_ghs_outpatient) %>% sum() %>% {round(1E-9*.,1)}




###### Figure 4.4: Spending on for TB prevention, diagnosis and treatment in total #####
##and by category of expenditure, 2010–2021
# Create category variables (In billions)
Fig4.4 <- Fig4.3_4.6 %>% 
  select(iso3, g_brics, year) 
Fig4.4$DSTB <- Fig4.3_4.6 %>%  select(rcvd_nmdr_dot, rcvd_nmdr_ndot_nhiv, c_ghs_nmdr) %>%   mutate_all(function(x) x/(1.0E09)) %>%  rowSums(na.rm = T)  
Fig4.4$MDR <- Fig4.3_4.6 %>%  select(rcvd_mdr, c_ghs_mdr) %>% mutate_all(function(x) x/(1.0E09)) %>%  rowSums(na.rm = T)  
Fig4.4$TBHIV <- Fig4.3_4.6 %>% select(rcvd_nmdr_ndot_hiv) %>% mutate_all(function(x) x/(1.0E09)) %>% unlist() 
Fig4.4$TPT <- Fig4.3_4.6 %>%  select(rcvd_nmdr_ndot_tpt) %>% mutate_all(function(x) x/(1.0E09))  %>% unlist() 
# Fig4.4$Other <- Fig4.3_4.6 %>%  select(rcvd_nmdr_ndot_nhiv_oth) %>% mutate_all(function(x) x/(1.0E09)) %>% unlist() # commented out: TA and AS
Fig4.4$Total <- Fig4.3_4.6 %>%  select(rcvd_tot, c_ghs_mdr, c_ghs_nmdr) %>%   mutate_all(function(x) x/(1.0E09)) %>%  rowSums(na.rm = T)  

# save a CSV output of country disaggregated data in local folder
write_csv(Fig4.4 , file = here::here(base_folder,csv_folder_name,"Fig4_4_dis.csv"))

# Global summary by year
Fig4.4 <- Fig4.4 %>% 
  group_by(year) %>%
  summarise_at(vars(-iso3, -g_brics),sum, na.rm = T)
Fig4.4$TPT[Fig4.4$year < 2019] <- NA



##### Figure 4.5: Funding for drug-susceptible TB and MDR-TB, 2010–2021, by country group ######
# PN: Picking up from Fig 4.5 raw data, but keeping only DSTB and MDR costs keep only non 
# HIC + RUS using GF;s classification in the year of projection (which may be different from WB's current classification)

# Sum up individual groups
Fig4.5 <- Fig4.3_4.6 %>% 
  select(iso3, g_brics, year)
Fig4.5$DSTB <- Fig4.3_4.6 %>%  select(rcvd_nmdr_dot, rcvd_nmdr_ndot_nhiv_noth, c_ghs_nmdr) %>%   mutate_all(function(x) x/(1.0E06)) %>%  rowSums(na.rm = T)  
Fig4.5$MDR <- Fig4.3_4.6 %>%  select(rcvd_mdr, c_ghs_mdr) %>% mutate_all(function(x) x/(1.0E06)) %>%  rowSums(na.rm = T)  


# Aggregated by bric variable
Fig4.5 <- Fig4.5 %>%  
  group_by(year, g_brics) %>% 
  summarise_at(vars(DSTB,MDR),sum,na.rm = T ) 



##### Figure 4.6: Funding for TB prevention, diagnosis and treatment by funding #####
##### source, 2010–2021, 121 countries with 97% of reported TB cases

Fig4.6 <- Fig4.3_4.6 %>% 
  group_by(year) %>%
  select(iso3, g_brics, g_income, g_whoregion, g_hb_tb , rcvd_ext_gf, rcvd_ext_ngf) 

# Sum up individual groups
Fig4.6$int <- Fig4.3_4.6 %>%  select(rcvd_int, c_ghs_nmdr, c_ghs_mdr) %>%  mutate_all(function(x) x/(1.0E09)) %>%  rowSums(na.rm = T)
Fig4.6$ext <- Fig4.3_4.6 %>%  select(rcvd_ext_gf, rcvd_ext_ngf) %>%   mutate_all(function(x) x/(1.0E09)) %>%  rowSums(na.rm = T)
Fig4.6$tot <- Fig4.3_4.6 %>%  select(rcvd_tot, c_ghs_nmdr, c_ghs_mdr) %>%   mutate_all(function(x) x/(1.0E09)) %>%  rowSums(na.rm = T)

# Proportion domestically funded in latest year
perc_domestic <- Fig4.6 %>% 
  filter(year >= 2013) %>% 
  summarise_at(vars(int, ext, tot), function(x) sum(x, na.rm = T)) %>% 
  mutate(perc = round(100 * int / tot,1))

# Amounts domestically funded in latest year by g_bric
domestic_by_grp <- Fig4.6 %>% filter(year == latest_year) %>% 
  group_by(g_brics) %>% 
  summarise(int = sum(int))

# Percentage of NTP reported external funding that is from GF
perc_gf <- Fig4.6 %>% 
  group_by(year) %>% 
  select(rcvd_ext_gf, rcvd_ext_ngf) %>% 
  summarise_if(.predicate = is.numeric, .funs = sum, na.rm = T) %>% 
  mutate(perc = round(100 * rcvd_ext_gf / (rcvd_ext_gf + rcvd_ext_ngf),1))

# Percentage of BRICS, HBC, and LIC group spending that is domestic sourced
perc_group <- Fig4.6 %>% filter(g_brics == "BRICS (n=5)" & year == latest_year) %>% 
  select(int, tot) %>% summarise_all(.funs = sum) 
perc_group <- perc_group %>% 
  rbind(
    Fig4.6 %>% filter(g_income == "LIC" & year == latest_year) %>% 
      select(int, tot) %>% summarise_all(.funs = sum) 
  ) %>% 
  rbind(
    Fig4.6 %>% filter(g_hb_tb == 1 & g_brics != "BRICS (n=5)" & year == latest_year) %>% 
      select(int, tot) %>% summarise_all(.funs = sum) 
  )
perc_group <- perc_group %>% 
  mutate (group = c("brics","lic","hb"),
          perc = round(100 * int / tot,1))



#### Fig 4.7 (9  panel) ######
# Pick out from Fig 4.6 dataset (country level by source of available funding) and select specific groups
Fig4.7a <- Fig4.6 %>% 
  select(-iso3,-tot) %>% 
  group_by(year,g_brics) %>% 
  summarise_at(vars(int,ext), sum, na.rm = T) %>% 
  rename(grp = g_brics) %>% 
  # needs to revert back to simple labels temporarily
  mutate(grp = factor(grp, labels = c("brics","hbc","rest"))) 

Fig4.7b <- Fig4.6 %>% 
  select(-iso3,-tot) %>% 
  group_by(year,g_income) %>% 
  summarise_at(vars(int,ext), sum, na.rm = T) %>% 
  rename(grp = g_income)

Fig4.7c <- Fig4.6 %>% 
  select(-iso3,-tot) %>% 
  mutate(g_whoregion = ifelse(g_whoregion %in% c("AFR","SEA"),g_whoregion,
                              # Include WPR in SEA group, and the rest in OTH group
                              ifelse(g_whoregion == "WPR","SEA","OTH"))) %>% 
  group_by(year,g_whoregion) %>% 
  summarise_at(vars(int,ext), sum, na.rm = T)%>% 
  rename(grp = g_whoregion)


Fig4.7 <- rbind(Fig4.7a, Fig4.7b, Fig4.7c) %>%
  filter(year <= latest_year) %>% 
  mutate(grp = stringr::str_to_lower(grp)) %>% 
  # Order the grouping variable by factoring it
  mutate(grp = factor(grp,
                      levels = c("brics","hbc","rest",
                                 "lic","lmc","umc",
                                 "afr","sea","oth"),
                      labels = c(paste0("BRICS (n=",g_bric_count$count[1],")"),
                                 paste0("High TB burden and global TB watchlist countries outside BRICS\u1d43 (n=",g_bric_count$count[2],")"),
                                 paste0("Rest of world (n=",g_bric_count$count[3],")"),
                                 "Low-income countries","Lower-middle-income countries","Upper-middle-income countries",
                                 "Africa","Asia\u1d47","Other regions\u1d9c")))  %>% 
  group_by(year,grp) %>% 
  summarise_all(sum, na.rm = T) 

# PN: a similar graph is produced for the summary report, but combining the brics split (Fig4.7a) with the Fig 4.6
Fig4.6 %>% select(-iso3,-tot) %>% 
  group_by(year) %>% 
  summarise_at(vars(int,ext), sum, na.rm = T) %>% 
  mutate(grp = "all") %>% # To allow grouped data to be appended next
  rbind(Fig4.7a) %>% 
  filter(year <= latest_year) %>% 
  pivot_longer(cols = c("int","ext")) %>% 
  mutate(name = factor(name, 
                       levels = c("int","ext"),
                       labels = c("Domestic funding","International donor funding"))) %>% 
  mutate(grp = factor(grp,
                      levels = c("all","brics","hbc","rest"),
                      labels = c( paste0("All low and middle-income countries (n=",countries,")"), # making the count dynamic
                                  "BRICS (n=5)",
                                  "High TB burden and global TB watchlist countries outside BRICS\u1d43 (n=28)",
                                  paste0("Other low and middle-income countries (n=",countries - 33,")"))
  )) %>% 
  arrange(year, grp) %>% 
  # write_csv(paste0(base_folder,csv_folder_name,"fig32_data.csv")) %T>%
  ggplot(aes(x=year, y = value, col = name)) +
  geom_line(size=1.5, alpha=.85) +
  scale_y_continuous(name = paste0("Billions (constant ",report_year-1," US$)")) +
  scale_x_continuous("", breaks=seq(2012,latest_year,2),  expand=c(0, .1)) +
  scale_color_manual(values = c("#4ABAFC", "#E63E13" ))+
  facet_wrap(~grp, ncol = 2, scales = 'free_y', strip.position = "top" , labeller = label_wrap_gen(width = 25) ) +
  theme_gtb() -> Fig34_plt

ggsave(plot = Fig34_plt, file=here::here(base_folder,csv_folder_name,"Fig34.pdf"), width=8.3, height = 5.8)

Fig4.6 %>% select(-iso3,-tot) %>% 
  group_by(year) %>% 
  summarise_at(vars(int,ext), sum, na.rm = T) %>% 
  mutate(grp = "all") %>% # To allow grouped data to be appended next
  rbind(Fig4.7a) %>% 
  filter(year <= latest_year) %>% 
  pivot_longer(cols = c("int","ext")) %>% 
  mutate(name = factor(name, 
                       levels = c("int","ext"),
                       labels = c("Domestic funding","International donor funding"))) %>% 
  mutate(grp = factor(grp,
                      levels = c("all","brics","hbc","rest"),
                      labels = c( paste0("All low and middle-income countries (n=",countries,")"), # making the count dynamic
                                  "BRICS (n=5)",
                                  "High TB burden and global TB watchlist countries outside BRICS\u1d43 (n=28)",
                                  paste0("Other low and middle-income countries (n=",countries - 33,")"))
  )) %>% 
  rename(category = name) %>% 
  pivot_wider(id_cols = c(year,category), names_from = grp, values_from = value) %>% 
  arrange(category, year) %>%
  readr::write_csv(here::here(base_folder,csv_folder_name,"fig34_data.csv"))


#### Fig 4.8 country panel (here show only one of them, to save memory) ######
# PN 2023 Apr: Note that, these country graphs display only the country reported data (plus imputed values) but don't include
# the CHOICE estimated GHS costs. This is because the charts are designed to be consistent
# with the gap reported at country level (historically we used to show gap trend lines) 
Fig4.8 <- Fig4.3_4.6 %>% 
  filter(g_hb_tb == 1) %>% 
  select(country, year, rcvd_int, rcvd_ext, gap_tot) 

# If gap (or any value) is less tha zero, it is zeroed
Fig4.8 <- Fig4.8 %>% 
  ungroup() %>%
  # mutate(country = ifelse(country=="India","India\u1D47",country)) %>% 
  mutate_at(vars(rcvd_int, rcvd_ext, gap_tot), 
            function(x) ifelse(x < 0, 0 , x/1E06)) %>% 
  pivot_longer(cols = c("rcvd_int","rcvd_ext", "gap_tot")) 
# Note that, the variables are made into factors after filtering out the
# unwanted categories, when charting


Fig4.9 <- figuredata %>% 
  # filter((iso3 %in% include_list$iso3)) %>%
  filter(year >= 2013 & year <= report_year) %>% # Change in Sept 2023 to include provisionally reported budget and cf values
  group_by(g_income, g_whoregion, year, country) %>%
  mutate_at(vars(gap_tot), function(x) ifelse(x < 0, 0 , x)) %>% 
  # in Millions
  transmute_at(vars(gap_tot), function(x) x/1.0E06 ) %>% 
  ungroup()

Fig4.9_inc_grp_reportyear <- figuredata %>%
  filter(year == report_year) %>% 
  group_by(g_income) %>%
  summarise(gap_tot = sum(gap_tot/1e9, na.rm = TRUE)) %>%
  filter(g_income == "LIC")

Fig4.9_GP_inc_grp_reportyear <- global_plan_23 %>% 
  select(g_income = g_inc_2022, year, gp_tot = Total) %>%
  filter(year == report_year) %>% 
  group_by(g_income) %>% 
  summarise(gp_tot = sum(gp_tot/1e9, na.rm = TRUE)) %>%
  filter(g_income == "L") 

Fig4.9_inc_grp_reportyear_dif <- Fig4.9_inc_grp_reportyear$gap_tot / Fig4.9_GP_inc_grp_reportyear$gp_tot * 100


# Total gap in financing in the year of the report (based on countries that are included in the analysis) 
report_year_budgetgap_bn = Fig4.9 %>% ungroup() %>% 
  filter(gap_tot > 0 & !is.na(gap_tot)) %>% 
  filter(year == report_year & g_income != "HIC") %>% 
  summarise(sum(gap_tot)/1000) %>% 
  unlist() %>% 
  round(1)

# Total gap in financing in latest year referenced 
latest_year_budgetgap_bn = Fig4.9 %>% ungroup() %>% 
  filter(gap_tot > 0 & !is.na(gap_tot)) %>% 
  filter(year == latest_year & g_income != "HIC") %>% 
  summarise(sum(gap_tot)/1000) %>% 
  unlist() %>% 
  round(1)

# how many countries report having gaps/ shortfalls in financing?
# If gap (or any value) is less than zero, it is zeroed
countries_with_gaps <- Fig4.9 %>% ungroup() %>% 
  filter(gap_tot > 0 & !is.na(gap_tot)) %>% 
  filter(year == latest_year) %>% 
  summarise_at(vars(gap_tot), length) %>% 
  unlist()

# What are the top 5 country shortfalls in latest_year?
table_country_gaps <- Fig4.9 %>%
  filter(gap_tot > 0 & !is.na(gap_tot)) %>% 
  filter(year == latest_year ) %>% 
  group_by(year) %>% 
  arrange(desc(gap_tot)) %>% 
  select(country, gap_tot) %>% 
  slice(1:5) %>% 
  mutate(gap_tot  = round(gap_tot,0))

# Among LICs: How many countries? How much gap?
total_lic <- Fig4.2  %>% filter( g_income== "LIC") %>% dim() %>% '[['(1)
latest_year_budgetgap_lic <- Fig4.9 %>% ungroup() %>% 
  filter(gap_tot > 0 & !is.na(gap_tot)) %>% 
  filter(g_income == "LIC" & year == latest_year) %>% 
  summarise(sum(gap_tot)) %>% 
  unlist() %>% 
  round(0)

lics_with_gaps <- Fig4.9 %>% ungroup() %>% 
  filter(gap_tot > 0 & !is.na(gap_tot)) %>% 
  filter(g_income == "LIC" & year == latest_year) %>% 
  summarise_at(vars(gap_tot), length) %>% 
  unlist()

## PN 2022: Comparing latest year gaps between plan values versus gaps with reported budgets. By country groups
Fig4.9_inc_grp <- Fig4.1_disagg %>% 
  filter(!is.na(GP_Total) & year == latest_year) %>% #All that had a GP projection in 2018 - should be 127 (and not 128) in 2022 (since Romania is noe HIC)
  filter(g_income %in% c("LIC","LMC","UMC")) %>% #Roumaina was UMC in 2018 but was reclassified as HIC in 2022. Eliminate from estimate
  group_by(g_income) %>% 
  select(GP_Total) %>%
  # mutate_at(.vars = vars(GP_Total), .funs = function(x) x/deflator_us_2018) %>% 
  summarise(GP_Total = sum(GP_Total/1E09, na.rm = TRUE),
            GP_n = n())

figuredata %>% 
  filter((iso3 %in% include_list$iso3)) %>%
  filter(year == latest_year) %>%
  group_by(g_income) %>% 
  # in Billions
  summarise(cf_tot = sum(cf_tot, na.rm = T)/1.0E09, 
            gap_tot= sum(gap_tot, na.rm=T)/1.0E09, 
            n = n()) %>% 
  ungroup() %>% 
  right_join(Fig4.9_inc_grp, by="g_income") -> Fig4.9_inc_grp

# Compute proportion of reported gap (gap_tot) to GP gap (GP_Total - cf_tot) in each subgroup
Fig4.9_inc_grp <- Fig4.9_inc_grp %>% 
  add_row(Fig4.9_inc_grp %>% summarise_if(.predicate = is.numeric, .funs = sum)) %>% 
  mutate(g_income = ifelse(is.na(g_income),"All",g_income)) %>% 
  mutate(perc_diff = (100 * gap_tot / (GP_Total - cf_tot)) %>% round(1))




##### Fig 4.10 stacked bar chart sources with gap, 33 HB + outlook countries ##### 

# 
Fig4.10 <- figuredata %>% 
  filter(year == report_year & g_hb_tb == 1 & !is.na(cf_int)) %>%
  group_by(g_income, country) %>%
  mutate_at(.vars = vars(gap_tot), function(x) ifelse(x < 0, 0 , x)) %>% 
  # in Millions
  transmute_at(vars(cf_int, cf_ext_gf, cf_ext_ngf, gap_tot), function(x) x/1.0E06 ) %>% 
  ungroup()

Fig4.10 <- Fig4.10 %>% 
  # compute percentages of total committed funds + gaps 
  mutate(tot = Fig4.10 %>% dplyr::select(cf_int, cf_ext_gf, cf_ext_ngf, gap_tot) %>% rowSums(na.rm = T)) %>% 
  mutate(int_pct = round(100 * cf_int / tot, 1),
         gf_pct = round(100 * cf_ext_gf / tot, 1),
         oth_pct = round(100 * cf_ext_ngf / tot, 1),
         gap_pct = round(100 * gap_tot / tot, 1))

# Extend country names for DPRK and DRC
Fig4.10 <- Fig4.10 %>% 
  mutate(country = as.character(country)) %>% 
  mutate(country = ifelse(country == "DPR Korea","Democratic People's Republic of Korea",country)) %>% 
  mutate(country = ifelse(country == "DR Congo","Democratic Republic of the Congo",country)) %>% 
  mutate(country = ifelse(country == "UR Tanzania","United Republic of Tanzania",country))  


myvars <- c("country", "int_pct", "gf_pct", "oth_pct", "gap_pct")

# LIC plot
# Specify country names in order of increasing domestic funding
ctry <- Fig4.10 %>% filter(g_income == "LIC") %>% arrange(int_pct) %>% select(country) %>% unlist()
f4.10a<- Fig4.10  %>% 
  filter(g_income=="LIC")%>% 
  select(all_of(myvars)) %>% 
  reshape2::melt(id=c("country"))

f4.10a$variable<-factor(f4.10a$variable, levels=c("int_pct", "gf_pct", "oth_pct", "gap_pct"))

Fig4_10_1_plot <-ggplot(data=f4.10a, aes(country,  value, fill = variable)) +
  geom_col(width=.8, position = position_stack(reverse = TRUE)) +
  coord_flip() +
  theme_gtb() +
  scale_x_discrete(limits= ctry,
                   labels = function(x) str_wrap(x, width = 20))+
  xlab("")+ylab("")+
  theme_minimal()+
  # Prev hues were "#377eb8", "#4daf4a", "#984ea3", "#e41a1c"
  scale_fill_manual(values=c("#4ABAFC","#e41a1c", "#4daf4a", "#BBBBBB"), labels=c("Domestic funding", "Global Fund","Other", "Budget gap")) +
  theme(legend.position = "none")+
  expand_limits(x=c(0,9))+
  expand_limits(y=c(0,100))+
  theme(plot.margin=unit(c(0,0,0,0),units="npc"))+
  ggtitle("Low-income")

# Fig4_10_1  
# saveRDS(Fig4_10_1, here::here(base_folder,outputs_folder_name,"Fig4_10_1.rds"))

## LMC plot
# Specify country names in order of increasing domestic funding
ctry <- Fig4.10 %>% filter(g_income == "LMC") %>% arrange(int_pct) %>% select(country) %>% unlist()
f4.10a<- Fig4.10 %>% 
  filter(g_income=="LMC")%>%  
  select(all_of(myvars)) %>% 
  reshape2::melt(id=c("country"))

f4.10a$variable<-factor(f4.10a$variable, levels=c("int_pct", "gf_pct", "oth_pct", "gap_pct"))

Fig4_10_2_plot <-ggplot(data=f4.10a, aes(country,  value, fill = variable)) +
  geom_col(width=.8,position = position_stack(reverse = TRUE)) +
  coord_flip() +
  theme_gtb() +
  scale_x_discrete(limits=ctry,
                   labels = function(x) str_wrap(x, width = 20))+
  xlab("")+ylab("")+
  theme_minimal()+
  scale_fill_manual(values=c("#4ABAFC","#e41a1c", "#4daf4a", "#BBBBBB"), labels=c("Domestic funding", "Global Fund","Other", "Budget gap")) +
  theme(legend.position = "none")+
  expand_limits(x=c(0,10))+
  expand_limits(y=c(0,100))+
  ggtitle("Lower-middle-income")

# Fig4_10_2  
# saveRDS(Fig4_10_2, here::here(base_folder,outputs_folder_name,"Fig4_10_2.rds"))

# ## UMC plot
# Specify country names in order of increasing domestic funding
ctry <- Fig4.10 %>% filter(g_income == "UMC") %>% arrange(int_pct) %>% select(country) %>% unlist()
f4.10a<- Fig4.10 %>% 
  filter(g_income=="UMC") %>%  
  select(all_of(myvars)) %>% 
  reshape2::melt(id=c("country"))

f4.10a$variable<-factor(f4.10a$variable, levels=c("int_pct", "gf_pct", "oth_pct", "gap_pct"))

Fig4_10_3_plot <-ggplot(data=f4.10a, aes(country,  value, fill = variable)) +
  geom_col(width=.8,position = position_stack(reverse = TRUE)) +
  coord_flip() +
  theme_gtb() +
  scale_x_discrete(limits=ctry,
                   labels = function(x) str_wrap(x, width = 20))+
  xlab("")+ylab("Percentage")+
  theme_minimal()+
  theme(legend.position = "bottom")+
  scale_fill_manual(values=c("#4ABAFC","#e41a1c", "#4daf4a", "#BBBBBB"), labels=c("Domestic funding", "Global Fund","International donor funding (excluding Global Fund)", "Budget gap")) +
  theme(legend.title = element_blank())+
  expand_limits(x=c(0,6))+
  expand_limits(y=c(0,100))+
  ggtitle("Upper-middle-income")



Fig4.11 <- figuredata
Fig4.11$imf_gdp_pc_con_usd <- ifelse(Fig4.11$year==(latest_year - 1 ) & is.na(Fig4.11$imf_gdp_pc_con_usd),
                                     figuredata$imf_gdp_pc_con_usd[figuredata$year == (latest_year - 2 )],
                                     Fig4.11$imf_gdp_pc_con_usd)

Fig4.11 <- Fig4.11 %>% 
  # Received funds for DSTB and Other group (specifically the variables
  # rcvd_nmdr_dot, rcvd_nmdr_ndot_nhiv_noth, rcvd_nmdr_ndot_nhiv_oth and
  # c_ghs_nmdr) are divided by c_notif_less_mdr, and filtered to only countries with 
  # above 100 DSTB cases notified
  mutate(rcvd_nmdr_dot = ifelse(figuredata$rcvd_nmdr_dot == 0, figuredata$cf_nmdr_dot, figuredata$rcvd_nmdr_dot)) %>% 
  mutate(DSTB = figuredata %>% select(rcvd_nmdr_dot, rcvd_nmdr_ndot_nhiv_noth, rcvd_nmdr_ndot_nhiv_oth, c_ghs_nmdr) %>% rowSums(na.rm = T),
         c_pp_dots = DSTB / c_notif_less_mdr ,
         log10_c_pp_dots = ifelse(is.na(c_pp_dots),NA,log(c_pp_dots, base = 10)),
         log10_gdp_pc_con_usd = ifelse(is.na(imf_gdp_pc_con_usd), NA, log(imf_gdp_pc_con_usd, base = 10))) %>% 
  # Restrict to latest year, Exclude countries where c_pp_dots is empty and
  # where it cannot be computed accurately i.e where rcvd_nmdr_dot is empty, or c_ghs_nmdr is empty
  filter(year == latest_year & c_notif_less_mdr >= 100 & g_income != "HIC" & 
           !is.na(c_pp_dots) & !is.na(imf_gdp_pc_con_usd) &
           !is.na(rcvd_nmdr_dot) & !is.na(c_ghs_nmdr)) %>% 
  select(iso3, country, imf_gdp_pc_con_usd,log10_gdp_pc_con_usd, c_pp_dots,log10_c_pp_dots, 
         c_ghs_nmdr, DSTB, g_whoregion, g_income, g_hb_tb,
         c_notified=c_notif_less_mdr) %>% 
  # Sorted in descending order to let larger bubbles be plotted before smaller ones
  arrange(desc(c_notified)) %>% 
  # Label WHO regions 
  mutate(g_whoregion =  factor(g_whoregion,
                               levels = c("AFR","AMR","SEA","EUR", "EMR", "WPR"),
                               labels = c("African Region","Region of the Americas", "South-East Asia Region","European Region", "Eastern Mediterranean Region","Western Pacific Region" )))

  
# Compare with stata script's computed c_pp_dots..
# cbind(Fig4.11 %>% select(iso3, c_pp_dots) %>% arrange(iso3),
#       Fig4.11b %>% select(iso3, c_pp_dots) %>% arrange(iso3)) %>% 
#   View()

##CREATE SCATTERPLOT WHERE WHO REGIONS ARE DIFFERENT COLORS AND BUBBLE SIZE IS TB CASELOAD
z1 = ggplot(Fig4.11)+geom_point(aes(x=log10_gdp_pc_con_usd, y=log10_c_pp_dots, size=c_notified, 
                                    fill = factor(g_hb_tb,labels=c("Rest of world","30 High burden TB"))),
                                shape=21, color="white")
##SCALE SIZE CHANGES BUBBLE SIZES (relatively), guide ENLARGES FILL LEGEND BUBBLES and STAT SMOOTH ADDS CONFIDENCE BAND
z2=z1+ scale_size_continuous("", range = c(2, 30), breaks = c(50000,250000,1000000), guide = "none") +
  # scale_fill_manual(values = c("#FC1C1E","#91a93e","#00a76d","#FF8C1F","#218ed4","#b72270")) +
  scale_fill_manual("", values = c("#218ed4","#FC1C1E")) 
  # guides(#size = guide_legend(order = 1, title = "TB caseload (notified TB cases)", label.position= "top", override.aes = list(color="black")),
  #        fill = guide_legend(order = 2, title = "WHO Region", label.position= "right", override.aes = list(size=5), ncol = 2)) +
  # stat_smooth(aes(x=log10_gdp_pc_con_usd, y=log10_c_pp_dots), method="lm", level=0.99)

# Generate nudge distances for label texts for the 30 HBCs -MDR. This involves
# estimating position with respect to regression line, then assigning a suitable nudge.
Fig4.11$u <- lm(log10_gdp_pc_con_usd~log10_c_pp_dots, data = Fig4.11) %>% residuals()
nudge_pos <- Fig4.11 %>% filter(g_hb_tb==1) %>% select(log10_gdp_pc_con_usd, log10_c_pp_dots, u, country)
nudge_pos$posy <- ifelse(nudge_pos$u > 0, 0.4, -0.4)

##ADD TEXT TO POINTS, ensuring they don't overlap
z3=z2 + ggrepel::geom_text_repel(data=nudge_pos,size=3, max.overlaps = 20,
                                 force = 70, segment.color = "#bcbcbc", 
                                 nudge_y = nudge_pos$posy,
                                 aes(x=log10_gdp_pc_con_usd, y=log10_c_pp_dots, 
                                     label=str_wrap(country, width = 20,indent = 2)))   

##LABEL AXES
z4=z3+xlab(paste0("GDP per capita (",report_year-1," US$, log scale)")) +ylab(paste0("Cost per patient treated (",report_year-1," US$, log scale)"))
##FORMAT AXES
# check the range of gdp values and ensure they're included in overall chart axes
axis_x_range <- Fig4.11$log10_gdp_pc_con_usd %>%  range()
axis_x_range[1] <- min((axis_x_range[1]-0.005) %>% round(2), 2.50) # Either use 2.5 or lower 
axis_x_range[2] <- max((axis_x_range[2]+0.005) %>% round(2), 4.31) # Either use 4.31 or higher 
axis_y_range <- Fig4.11$log10_c_pp_dots %>%  range()
axis_y_range[1] <- min((axis_y_range[1]-0.005) %>% round(2), 1.0) # Either use 1.8 or lower 
axis_y_range[2] <- max((axis_y_range[2]+0.005) %>% round(2), 4.69) # Either use 4.31 or higher

z5=z4 +scale_x_continuous(limits=axis_x_range,breaks=c(2.69897, 3, 3.301029, 3.69897, 4, 4.301029 ), labels=c("500", "1 000","2 000" , "5 000", "10 000", "20 000"))
#  plot
Fig4.11_plot = z5+ scale_y_continuous(limits=axis_y_range,breaks=c(2,2.69897, 3, 3.69897, 4, 4.301029), labels=c("100","500", "1 000", "5 000", "10 000", "20 000"))+
  # ggtitle(paste0("Estimated cost per patient treated for drug-susceptible TB in ",dim(Fig4.11)[1]," countries, 2020")) +
  # theme_glb.rpt() 
  theme_gtb()

dstb_cpp_no <- Fig4.11_plot$data %>% dim()
dstb_cpp_no <- dstb_cpp_no[1]

# saveRDS(z6, here::here(base_folder,outputs_folder_name,"Fig4.11p.rds"))


# Median provider cost per case notified (DSTB)
c_pp_dots <- Fig4.11_plot$data %>% ungroup() %>% summarise(median(c_pp_dots)) %>% round(0) %>%  unlist()




###### Fig 4.12  TB 2nd LINE DRUG PATIENT COST ######
Fig4.12 <- figuredata
Fig4.12$imf_gdp_pc_con_usd <- ifelse(Fig4.12$year==(latest_year - 1) & is.na(Fig4.12$imf_gdp_pc_con_usd),
                                     figuredata$imf_gdp_pc_con_usd[figuredata$year == (latest_year - 2)],
                                     Fig4.12$imf_gdp_pc_con_usd)

Fig4.12 <- Fig4.12 %>% 
  # consistent with Fig 4.4 above, received funds for DSTB (including
  # c_ghs_nmdr) are divided by c_notified, only for countries with c_notified
  # above 100
  
  mutate(rcvd_mdr = ifelse(figuredata$rcvd_mdr == 0, figuredata$cf_mdr, figuredata$rcvd_mdr)) %>% 
  mutate(MDR = figuredata %>% select(rcvd_mdr, c_ghs_mdr) %>% rowSums(na.rm = T),
         c_pp_mdr = MDR / mdr_tx ,
         log10_c_pp_mdr = ifelse(is.na(c_pp_mdr), NA, log(c_pp_mdr, base = 10)),
         log10_gdp_pc_con_usd = ifelse(is.na(imf_gdp_pc_con_usd), NA, log(imf_gdp_pc_con_usd, base = 10))) %>% 

  # Restrict to latest year, Exclude countries where c_pp_mdr is empty and
  # where it cannot be computed accurately i.e where rcvd_mdr_sld is zero, 
  # or c_ghs_mdr is empty (but keep those with c_ghs_mdr == 0 eg CHN, KAZ, RUS etc)
  filter(year == latest_year & mdr_tx >= 20 & g_income != "HIC" & 
           !is.na(c_pp_mdr) & !is.na(imf_gdp_pc_con_usd) &
           !is.na(rcvd_mdr_sld) & rcvd_mdr_sld > 0) %>% 
  # In 2023 report, we drop these three outliers
  # filter(iso3 != "YEM" & iso3 != "LBR" & iso3 != "DJI") %>% 
  select(iso3, country, imf_gdp_pc_con_usd,log10_gdp_pc_con_usd, c_pp_mdr,log10_c_pp_mdr, 
         c_ghs_mdr, MDR,  g_whoregion, g_income, g_hb_mdr,mdr_tx) %>% 
  # Sorted in descending order to let larger bubbles be plotted before smaller ones
  arrange(desc(mdr_tx)) %>% 
  # Label WHO regions 
  mutate(g_whoregion =  factor(g_whoregion,
                               levels = c("AFR","AMR","SEA","EUR", "EMR", "WPR"),
                               labels = c("African Region","Region of the Americas", "South-East Asia Region","European Region", "Eastern Mediterranean Region","Western Pacific Region" )))


##CREATE SCATTERPLOT WHERE WHO REGIONS ARE DIFFERENT COLORS AND BUBBLE SIZE IS TB CASELOAD
# Note: aesthetics are set per each layer, to allow more flexible control eg over labeling
z1 = ggplot(Fig4.12)+geom_point(aes(x=log10_gdp_pc_con_usd, y=log10_c_pp_mdr, size=mdr_tx, 
                                    fill=factor(g_hb_mdr,labels = c("Rest of world","30 High burden MDR/RR-TB"))),
                                shape=21, color="white" )
##SCALE SIZE CHANGES BUBBLE SIZES (relatively), guide ENLARGES FILL LEGEND BUBBLES and STAT SMOOTH ADDS CONFIDENCE BAND
z2=z1+ scale_size_continuous(range = c(0.5, 30), guide = "none") +
  scale_fill_manual("", values = c("#218ed4","#FC1C1E")) 
  # scale_fill_manual(values = c("#FC1C1E","#91a93e","#00a76d","#FF8C1F","#218ed4","#b72270")) +
  # guides(#size = guide_legend(order = 1, title = "MDR-TB caseload (notified cases)", label.position= "top", override.aes = list(color="black")),
  #        fill = guide_legend(order = 2, title = "WHO Region", label.position= "right", override.aes = list(size=5), ncol = 2)) +
  # stat_smooth(aes(x=log10_gdp_pc_con_usd, y=log10_c_pp_mdr), method="lm", level=0.99)

# Generate nudge distances for label texts for the 30 HBCs -MDR. This involves
# estimating position with respect to regression line, then assigning a suitable nudge.
Fig4.12$u <- resid(lm(log10_gdp_pc_con_usd~log10_c_pp_mdr, data = Fig4.12))
nudge_pos <- Fig4.12 %>% filter(g_hb_mdr==1) %>% select(log10_gdp_pc_con_usd, log10_c_pp_mdr, u, country)
nudge_pos$posy <- ifelse(nudge_pos$u > 0, 0.4, -0.4)


##ADD TEXT TO POINTS
z3=z2 + ggrepel::geom_text_repel(data=nudge_pos,size=3,max.overlaps = 20,
                                 force = 70, segment.color = "#bcbcbc", 
                                 nudge_y = nudge_pos$posy, #nudge_x = nudge_pos$posx,
                                 aes(x=log10_gdp_pc_con_usd, y=log10_c_pp_mdr, 
                                     label=str_wrap(country, width = 25,indent = 2)))   
##LABEL AXES
z4=z3+xlab(paste0("GDP per capita (", report_year-1," US$, log scale)")) +ylab(paste0("Cost per patient treated (", report_year-1," US$, log scale)"))
##FORMAT AXES
# check the range of gdp values and ensure they're included in overall chart axes
axis_x_range <- Fig4.12$log10_gdp_pc_con_usd %>%  range()
axis_x_range[1] <- min((axis_x_range[1]-0.005) %>% round(2), 2.3) # Either use 2.5 or lower 
axis_x_range[2] <- max((axis_x_range[2]+0.005) %>% round(2), 4.31) # Either use 4.31 or higher 
axis_y_range <- Fig4.12$log10_c_pp_mdr %>%  range()
axis_y_range[1] <- min((axis_y_range[1]-0.005) %>% round(2), 2.7) # Either use 1.8 or lower 
axis_y_range[2] <- max((axis_y_range[2]+0.005) %>% round(2), 5.31) # Either use 4.31 or higher

z5=z4 +scale_x_continuous(limits=axis_x_range,breaks=c(2,2.301029, 2.69897, 3, 3.69897, 4, 4.301029, 4.477121254 ), labels=c("100","200","500", "1 000",  "5 000", "10 000", "20 000", "30 000"))

# Final figure
Fig4.12_plot = z5+ scale_y_continuous(limits=axis_y_range,breaks=c(2,2.69897, 3, 3.69897, 4, 4.301029, 4.69897), labels=c("100", "500", "1 000", "5 000", "10 000", "20 000", "50 000"))+
  theme_gtb()
mdr_cpp_no <- Fig4.12_plot$data %>% dim()
mdr_cpp_no <- mdr_cpp_no[1]

# saveRDS(Fig4.12_plot, here::here(base_folder,outputs_folder_name,"Fig4.12p.rds"))


# What's the Median provider cost per case notified (MDR)?
c_pp_mdr <- Fig4.12_plot$data %>% ungroup() %>% summarise(median(c_pp_mdr)) %>% round(0) %>% unlist()


##### Numbers for the methods box ######
# Summary of methods used to fill in missing rcvd_tot values
methods_box <- figuredata %>% 
  filter(year == latest_year & include == 1) %>% 
  select(iso3, rcvd_imputation, do_not_fill, rcvd_tot, c_notified)



###### Creating an extract of variables for the regional profiles website page #####
# Lines that produces data in csv format for theregional profiles. Adapted from
# Hazim's stata stript (Regional profiles.do) so as to work directly from github.

# Purpose is to produce a CSV output with the following variables in it:
# rcvd_tot, rcvd_int, c_ghs, rcvd_ext_gf, rcvd_ext_ngf, cf_int, cf_ext, gap_tot,
# g_whoregion, and year 

# The numbers should be the same that are used in the
# Global TB report, and should be disaggregated by WHO region and year

regional_profile <- figuredata %>% 
  group_by(year, g_whoregion) %>% 
  select(rcvd_tot, rcvd_int, c_ghs, rcvd_ext_gf, rcvd_ext_ngf, cf_int, cf_ext, gap_tot) %>% 
  summarise_all(.funs = sum, na.rm = T) %>% 
  group_by(year, g_whoregion) %>% 
  mutate_at(.vars = vars(-group_cols()), .funs = round, digits = 0)

# Clear c_ghs values for report_year since they are not used
regional_profile <- regional_profile %>% 
  mutate(c_ghs = ifelse(year == report_year,0,c_ghs))

regional_profile %>% 
  write_csv(here::here(base_folder,csv_folder_name,paste0("regionalglobalprofile_finance_v",Sys.Date(),".csv")))

figuredata %>% filter(year == 2022 & do_not_fill == 0) %>% select(iso3) %>% 
  write_csv(here::here(base_folder,csv_folder_name,paste0("countries_for_profile_finance_v",Sys.Date(),".csv")))

            
                       

