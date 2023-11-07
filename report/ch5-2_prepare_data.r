# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data preparation script for ch5-2_and_kendo.rmd
# Takuya Yamanaka, June 2023
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Load chapter 5 packages, settings and data
source(here::here('report/ch5_load_data.r'))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: fig 5.2.1 ----
# (National surveys of costs faced by TB patients and their households since 2015: progress and plans)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

palatte_fig5.2.1 = c("#8FC63F","#0491D1","#8FD8F8","blueviolet")

# f5.2.1_data <- read_excel(here::here('report/ch5_data/local/monthly_pcs_monitoring_2023.xlsx'),sheet="for map") %>%
#   filter(var!=0) %>%
#   mutate(var = factor(var, levels=c("Completed","Ongoing","Planned", "Repeat survey planned"))) %>%
#   select(iso3, var) 
# 
# save(f5.2.1_data, file = here::here('report/ch5_data/f5.2.1_data.Rdata'))

load(here::here('report/ch5_data/f5.2.1_data.Rdata'))
  
num <- table(f5.2.1_data$var)
labs <- c(
  paste0("Completed (n=",num[1],")"),
  paste0("Ongoing (n=",num[2],")"),
  paste0("Planned (n=",num[3],")"),
  paste0("Repeat survey planned (n=",num[4],")")
)

f5.2.2_txt <-  f5.2.1_data %>%
  filter(var == "Completed"|var == "Repeat survey planned") %>%
  nrow

f5.2.2_txt <- f5.2.1_data %>%
  filter(var == "Completed"|var == "Repeat survey planned") %>%
  filter(iso3 %in% iso3_hbc) %>%
  nrow %>% 
  cbind.data.frame(f5.2.2_txt) %>%
  rename(hbc30 = 1, all = 2)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: fig 5.2.3 ----
# (Selected baseline results from national surveys^a^ of costs faced by TB patients and their households, latest year   )
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# PCS book 20 countries
# f5.2.3_data <- read_excel(here::here('report/ch5_data/local/TBPCS_ALL_database_v23.xlsx')) %>% #from PCS book
#   subset((variable=="cc1"|variable=="cc2"|variable=="cc3")&
#            quintile=="all"&sex=="all") %>%
#   left_join(list_iso3_country) %>%
#   dplyr::select(iso3,country,year,dr,value,lci,uci) %>%
#   dplyr::rename(grp=dr,catast_pct=value,catast_pct_lo=lci,catast_pct_hi=uci) %>%
#   mutate_at(c("catast_pct","catast_pct_lo","catast_pct_hi"), ~.*100)
# 
# # additional 3 countries (COL, NER, ZAF) that have standard country profile used in PCS book
# f5.2.3_data_add <- read_excel(here::here('report/ch5_data/local/TBPCS_ALL_database_additional_v1.xlsx')) %>%
#   subset((variable=="cc1"|variable=="cc2"|variable=="cc3")&
#            quintile=="all"&sex=="all") %>%
#   left_join(list_iso3_country) %>%
#   dplyr::select(iso3,country,year,dr,value,lci,uci) %>%
#   dplyr::rename(grp=dr,catast_pct=value,catast_pct_lo=lci,catast_pct_hi=uci) %>%
#   mutate_at(c("catast_pct","catast_pct_lo","catast_pct_hi"), ~.*100)
# 
# f5.2.3_data <- rbind.data.frame(f5.2.3_data,f5.2.3_data_add)
# 
# # other countries that reported results of PCS to WHO
# f5.2.3b_data <- read_excel(here::here('report/ch5_data/local/catast indicator summary 2023_v1.xlsx')) %>%
#   select(-country) %>% left_join(list_iso3_country) %>%
#   select(iso3, country, year,grp,catast_pct,catast_pct_lo,catast_pct_hi) %>%
#   subset(iso3=="BEN"|iso3=="LSO"|iso3=="MDA"|iso3=="SLV"|iso3=="TLS"|iso3=="ZMB"|iso3=="NAM") %>%
#   mutate(grp=factor(grp,levels=c('all','ds','dr'))) %>%
#   mutate(grp=factor(grp,labels=c('overall','TB (first-line treatment)','Drug-resistant TB'))) %>%
#   filter(iso3!="MDA") %>%
#   arrange(country)
# 
# f5.2.3_data <- f5.2.3_data %>%
#   rbind.data.frame(.,f5.2.3b_data) %>%
#   # replace 95%CI for UGA and MLI with ones without adjustment for sampling effects: agreed with MB in Sep-Oct 2022.
#   mutate(catast_pct_lo = ifelse(iso3=="MLI"&grp=="Drug-resistant TB", 80.73638, catast_pct_lo),
#          catast_pct_lo = ifelse(iso3=="UGA"&grp=="Drug-resistant TB", 92.01535, catast_pct_lo),
#          catast_pct_hi = ifelse(iso3=="UGA"&grp=="Drug-resistant TB", 100.0, catast_pct_hi)) 
# 
# save(f5.2.3_data, file = here::here('report/ch5_data/f5.2.3_data.Rdata'))

load(here::here('report/ch5_data/f5.2.3_data.Rdata'))

# write csv for GTB Database (pass on to Hazim)
f5.2.3_data %>%
  mutate(across(5:7, round, 0)) %>%
  filter(#iso3 == "ZMB" | 
           iso3 == "NAM" ) %>% # filter countries with updates: 2023, Zambia and Namibia
  write_csv(paste0(here::here('report/local/'),"/tbpcs_catacost_update",report_year,".csv")) 

## Subset data for All TB to estimate pooled average
notification <- notification %>%
  mutate(c.notified=c_newinc) %>%
  mutate(c_ds=ifelse(year<2020,c.notified-conf_rrmdr,c.notified-(conf_rr_nfqr + conf_rr_fqr))) %>%
  mutate(conf_rrmdr=ifelse(year<2020,conf_rrmdr,conf_rr_nfqr + conf_rr_fqr))

f5.2.3a_data <- f5.2.3_data %>% 
  filter(grp=='overall') %>% 
  mutate(year=ifelse(year>report_year-1,report_year-1,year)) #%>%
  # mutate(country = ifelse(country=="Zambia","Zambia\u1D9C",country)) %>% 
  # mutate(country = ifelse(country=="Namibia","Namibia\u1D9C,\u1D48",country))

notification %>% select(iso3,year,c.notified) %>% right_join(f5.2.3a_data,by=c('iso3','year')) -> f5.2.3a_data

fit_all <-
  rma(
    yi = catast_pct,
    sei = (catast_pct_hi - catast_pct_lo)/3.92,
    data = f5.2.3a_data, 
    weights = c.notified
  )

f5.2.3a_data <- f5.2.3a_data %>% 
  add_row(iso3="AVE",country="Pooled average", 
          grp="ave",
          catast_pct    = as.numeric(fit_all$b),
          catast_pct_lo = fit_all$ci.lb,
          catast_pct_hi = fit_all$ci.ub) 

f5.2.3_sel_order <- 
  f5.2.3a_data %>% 
  arrange(catast_pct) %>% 
  arrange(grp) %>%
  mutate(country_a = ifelse(iso3=="ZMB","Zambia",
                            ifelse(iso3=="NAM","Namibia",country))) %>% 
  mutate(country = factor(country),
         country_a = factor(country_a)) 

## Subset data for DS-TB 
f5.2.3b_data <- f5.2.3_data %>% 
  filter(grp=='TB (first-line treatment)'|iso3=="TLS"|iso3=="SLV"|iso3=="FJI"|iso3=="SLB") %>% 
  mutate(year=ifelse(year>report_year-1,report_year-1,year)) #%>%  # replace with report_year-1 once 1st dataset of tb notification is ready
  # mutate(country = ifelse(country=="Zambia","Zambia\u1D9C",country)) %>% 
  # mutate(country = ifelse(country=="Namibia","Namibia\u1D9C,\u1D48",country))

notification %>% select(iso3,year,c_ds) %>% inner_join(f5.2.3b_data,by=c('iso3','year')) %>% distinct() -> f5.2.3b_data 

fit_all <-
  rma(
    yi = catast_pct,
    sei = (catast_pct_hi - catast_pct_lo)/3.92,
    data = f5.2.3b_data, 
    weights = c_ds
  )

f5.2.3b_data <- f5.2.3b_data %>% 
  add_row(iso3="AVE",country="Pooled average", 
          grp="ave",
          catast_pct    = as.numeric(fit_all$b),
          catast_pct_lo = fit_all$ci.lb,
          catast_pct_hi = fit_all$ci.ub) %>% 
  # mutate(country = ifelse(country=="Namibia","Namibia\u1D9C,\u1D48",country)) %>%
  mutate(country=factor(country,levels=rev(country))) #%>%
  # add_row(iso3="NAM",grp="TB (first-line treatment)",country="Namibia\u1D9C,\u1D48") 
  

f5.2.3b_data <- f5.2.3b_data %>% 
  mutate(grp=ifelse(grp=="overall","TB (first-line treatment)",grp))


## Subset data for DR-TB 
f5.2.3c_data <- f5.2.3_data %>% 
  filter(grp=='Drug-resistant TB') %>% 
  mutate(year=ifelse(year>report_year-1,report_year-1,year)) #%>%  # replace with report_year-1 once 1st dataset of tb notification is ready
  # mutate(country = ifelse(country=="Zambia","Zambia\u1D9C",country)) %>% 
  # mutate(country = ifelse(country=="Namibia","Namibia\u1D9C,\u1D48",country))

notification %>% select(iso3,year,conf_rrmdr) %>% right_join(f5.2.3c_data,by=c('iso3','year')) %>% distinct() -> f5.2.3c_data

fit_all <-
  rma(
    yi = catast_pct,
    sei = (catast_pct_hi - catast_pct_lo)/3.92,
    data = f5.2.3c_data, 
    weights = conf_rrmdr   
  )

f5.2.3c_data <- f5.2.3c_data %>% 
  add_row(iso3="AVE",country="Pooled average", 
          grp="ave",
          catast_pct    = as.numeric(fit_all$b),
          catast_pct_lo = fit_all$ci.lb,
          catast_pct_hi = fit_all$ci.ub) %>%
  # mutate(country = ifelse(country=="Namibia","Namibia\u1D9C,\u1D48",country)) %>% 
  mutate(country=factor(country,levels=rev(country))) %>%  # factorize in the order of rows 
  add_row(iso3="SLB",grp="Drug-resistant TB",country="Solomon Islands") %>%
  add_row(iso3="FJI",grp="Drug-resistant TB",country="Fiji") %>%
  add_row(iso3="TLS",grp="Drug-resistant TB",country="Timor-Leste") %>%
  add_row(iso3="SLV",grp="Drug-resistant TB",country="El Salvador") 
  # add_row(iso3="NAM",grp="Drug-resistant TB",country="Namibia\u1D9C,\u1D48") 


# extract pooled averages for texts
f5.2.3a_txt <- f5.2.3a_data %>%
  subset(iso3=="AVE") %>%
  mutate(grp="Overall\n(End TB Strategy indicator)") 

f5.2.3a_txt_lo <- f5.2.3a_data %>%
  arrange(catast_pct) %>%
  slice(1)

f5.2.3a_txt_hi <- f5.2.3a_data %>%
  arrange(desc(catast_pct)) %>%
  slice(1)

f5.2.3a_txt_num <- f5.2.3a_data %>%
  filter(iso3!="AVE")

f5.2.3c_txt <- f5.2.3c_data %>%
  subset(iso3=="AVE") %>%
  mutate(grp="Drug-resistant TB") 

f5.2.3c_txt_num <- f5.2.3c_data %>%
  filter(iso3!="AVE", !is.na(catast_pct))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: fig 5.2.4 ----
# (Distribution of costs faced by TB patients and their households in 25 national surveys completed since 2016)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
palatte_f5.2.4 = c("goldenrod2","dodgerblue1","darkblue")

# from PCS book
# f5.2.4_data <- read_excel(here::here('report/ch5_data/local/Fig3.3.1.cost drivers_v6_2022-06-14.xlsx')) %>%
#   rename(cat=3) %>%
#   mutate(value=value*100)
# 
# # for addtional 3 countries with standard country profile (COL, NER, ZAF)
# f5.2.4_data_add <- read_excel(here::here('report/ch5_data/local/TBPCS_ALL_database_additional_v1.xlsx')) %>% #from additional 3 countries for Lancet Global Health
#   subset((variable=="cat_med"|variable=="cat_nmed"|variable=="cat_indirect")&
#            dr=="overall"&quintile=="all"&sex=="all"&sum_fun=="mean")  %>%
#   dplyr::select(iso3,variable, value) %>%
#   left_join(list_iso3_country,by = "iso3") %>%
#   select(iso3,country,variable,value)
# 
# f5.2.4_data_add <- tidyr::spread(f5.2.4_data_add, key = variable, value = value)
# 
# f5.2.4_data_add <- f5.2.4_data_add %>%
#   mutate(tot=cat_med+cat_nmed+cat_indirect)%>%
#   mutate(p_med=cat_med/tot) %>%
#   mutate(p_nmed=cat_nmed/tot) %>%
#   mutate(p_indirect=cat_indirect/tot)
# 
# f5.2.4_data_add<- tidyr::gather(f5.2.4_data_add, key = variable, value = value, p_med,p_nmed,p_indirect) %>%
#   dplyr::select(iso3,country,cat=variable, value=value) %>%
#   mutate(cat=factor(cat,levels=c("p_indirect","p_nmed","p_med"))) %>%
#   arrange(iso3)%>%
#   mutate(value=value*100)
# 
# f5.2.4_data <- rbind.data.frame(f5.2.4_data,f5.2.4_data_add)
# 
# # from country reports
# f5.2.4_data <- read_excel(here::here('report/ch5_data/local/catast cost breakdown summary 2023_v1.xlsx'))  %>%
#   select(-country) %>% left_join(list_iso3_country) %>%
#   filter(grp=="all") %>% select(-source,-grp) %>%
#   arrange(pcost_med_pct) %>%
#   mutate(country=fct_reorder(country,pcost_med_pct)) %>%
#   select(iso3,country,pcost_med_pct,pcost_nmed_pct,pcost_indirect_pct) %>%
#   subset(iso3=="BEN"|iso3=="LSO"|iso3=="MDA"|iso3=="SLV"|iso3=="TLS"|iso3=="ZMB"|iso3=="NAM") %>%
#   rename(p_med=3,p_nmed=4,p_indirect=5) %>%
#   pivot_longer(p_med:p_indirect,names_to="cat") %>% rbind.data.frame(.,f5.2.4_data) %>%
#   arrange(iso3) 
# 
# save(f5.2.4_data, file = here::here('report/ch5_data/f5.2.4_data.Rdata'))

load(here::here('report/ch5_data/f5.2.4_data.Rdata'))

f5.2.4_sel_order <- 
  f5.2.4_data %>% 
  filter(cat == "p_med") %>% 
  arrange(value) %>% 
  mutate(country = factor(country))

f5.2.4_txt_num <- f5.2.4_data %>%
  filter(cat == "p_med")

f5.2.4_txt_med <- f5.2.4_data %>%
  filter(cat == "p_med", value > 20) %>%
  arrange(desc(value))

f5.2.4_txt_med_list <- f5.2.4_txt_med %>% nrow()

f5.2.4_txt_nmed <- f5.2.4_data %>%
  filter(cat == "p_nmed", value > 50) %>%
  arrange(as.character(country))

f5.2.4_txt_nmed_list <- f5.2.4_txt_nmed %>% nrow()

f5.2.4_txt_indirect <- f5.2.4_data %>%
  filter(cat == "p_indirect", value > 43.6) %>% # find cutoff indirect > nmed
  arrange(as.character(country))

f5.2.4_txt_indirect_list <- f5.2.4_txt_indirect %>% nrow()

# write csv for GTB Database (pass on to Hazim)
f5.2.4_data %>%
  mutate(value = ftb(value)) %>%
  filter(#iso3 == "ZMB" | 
           iso3 == "NAM" ) %>% # filter countries with updates: 2023, Zambia and Namibia
  write_csv(paste0(here::here('report/local/'),"/tbpcs_cost drivers_update",report_year,".csv")) 

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: fig 5.2.5 ----
# (Model-based estimates of cost faced by TB patients and their households in 135 low- and middle-income countries, WHO regions)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# f5.2.5_data <- read_excel(here::here('report/ch5_data/local/lancet_gh.xlsx')) %>%
#   left_join(who_region_shortnames, by = c("g_whoregion")) %>%
#   mutate(entity = ifelse(g_whoregion=="Global","All LMICs",as.character(entity))) %>%
#   mutate(entity = factor(entity, levels = c("All LMICs","Western Pacific Region","South-East Asia Region","European Region",
#                                             "Eastern Mediterranean Region","Region of the Americas","African Region")))
# 
# save(f5.2.5_data, file = here::here('report/ch5_data/f5.2.5_data.Rdata'))

load(here::here('report/ch5_data/f5.2.5_data.Rdata'))

f5.2.5_txt_global <- f5.2.5_data %>%
  filter(entity=="All LMICs")
  
f5.2.5_txt_afro <- f5.2.5_data %>%
  filter(entity=="African Region")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: tab 5.2.1 ----
# (Status of social protection for people with TB, 30 high TB burden countries)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

t5.2.1_data <- sp |>
  filter(year==report_year, iso3 %in% iso3_hbc) %>%
  mutate(other_social_protn = ifelse(iso3=="BRA","Individuals considered highly vulnerable",other_social_protn),
         other_social_protn = ifelse(iso3=="IDN","Cash transfers for people with drug-resistant TB",other_social_protn),
         other_social_protn = ifelse(iso3=="ZMB","Food and transportion supports for people with drug-resistant TB",other_social_protn)) %>%
  select (country,  social_protn, free_access_tbdx:enable_tx_adherence,cash_trans:food_security,other_social_protn) |>
  mutate(across(where(is.integer), as.factor))

t5.2.1_txt1 <- sp %>%
  select(country,year,iso3,social_protn) %>%
  filter(year>=report_year-1)  %>%
  group_by(year) %>%
  summarise(across(social_protn, sum, na.rm=TRUE)) %>%
  pivot_wider(names_from = year,
              values_from = social_protn) %>%
  rename(sp_2022=`2022`, sp_2023=`2023`)

t5.2.1_txt2 <- sp %>%
  select(country,year,iso3,free_access_tbtx) %>%
  filter(year==report_year)  %>%
  summarise(across(free_access_tbtx, sum, na.rm=TRUE)) 

t5.2.1_txt3 <- sp %>%
  select(country,year,iso3,enable_tx_adherence:food_security) %>%
  filter(year==report_year)  %>%
  mutate(sp_tot = rowSums(select(., enable_tx_adherence:food_security), na.rm = TRUE)) %>%
  filter(sp_tot>0) %>% nrow()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: tab 5.2.2 ----
# (Status of national laws and regulations against stigma, 30 high TB burden countries)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

t5.2.2_data <- sp |>
  filter(year==report_year, iso3 %in% iso3_hbc) %>%
  select (country,  protect_employment:protect_association) |>
  mutate(across(where(is.integer), as.factor))

t5.2.2_txt_list <- sp |>
  filter(year==report_year, iso3 %in% iso3_hbc) %>%
  select (country,  protect_employment:protect_association) %>%
  rowwise() %>%
  mutate(tot = sum(across(protect_employment:protect_association), na.rm = F)) %>%
  filter(tot == 0)

t5.2.2_txt <- t5.2.2_txt_list %>% nrow()

# testing text-based table for stigma and discrimination
# job protection
## yes
t5.2.2_data_1y <- t5.2.2_data %>%
  filter(protect_employment == 1) 

t5.2.2_1.1 <- paste0(t5.2.2_data_1y %>% nrow(), " countries\n(", gsub("(United)|(Democratic)|(Philip)|(Central)", "the \\1\\2\\3\\4", knitr::combine_words(t5.2.2_data_1y$country, oxford_comma=FALSE)), ")")

## no
t5.2.2_data_1n <- t5.2.2_data %>%
  filter(protect_employment == 0) 

t5.2.2_1.2 <- paste0(t5.2.2_data_1n %>% nrow(), " countries\n(", gsub("(United)|(Democratic)|(Philip)|(Central)", "the \\1\\2\\3\\4", knitr::combine_words(t5.2.2_data_1n$country, oxford_comma=FALSE)), ")")

## unknown
t5.2.2_data_1u <- t5.2.2_data %>%
  filter(protect_employment == 3) 

t5.2.2_1.3 <- paste0(t5.2.2_data_1u %>% nrow(), " countries\n(", gsub("(United)|(Democratic)|(Philip)|(Central)", "the \\1\\2\\3\\4", knitr::combine_words(t5.2.2_data_1u$country, oxford_comma=FALSE)), ")")

## merge
t5.2.2_1 <- data.frame(t5.2.2_1.1,t5.2.2_1.2,t5.2.2_1.3) %>%
  rename(Yes = 1, No = 2, Unknown = 3) %>% 
  mutate(Type = "Employment: protection from dismissal") %>%
  select(Type, Yes, No, Unknown)

# housing
## yes
t5.2.2_data_2y <- t5.2.2_data %>%
  filter(protect_housing == 1) 

t5.2.2_2.1 <- paste0(t5.2.2_data_2y %>% nrow(), " countries\n(", gsub("(United)|(Democratic)|(Philip)|(Central)", "the \\1\\2\\3\\4", knitr::combine_words(t5.2.2_data_2y$country, oxford_comma=FALSE)), ")")

## no
t5.2.2_data_2n <- t5.2.2_data %>%
  filter(protect_housing == 0) 

t5.2.2_2.2 <- paste0(t5.2.2_data_2n %>% nrow(), " countries\n(", gsub("(United)|(Democratic)|(Philip)|(Central)", "the \\1\\2\\3\\4", knitr::combine_words(t5.2.2_data_2n$country, oxford_comma=FALSE)), ")")

## unknown
t5.2.2_data_2u <- t5.2.2_data %>%
  filter(protect_housing == 3) 

t5.2.2_2.3 <- paste0(t5.2.2_data_2u %>% nrow(), " countries\n(", gsub("(United)|(Democratic)|(Philip)|(Central)", "the \\1\\2\\3\\4", knitr::combine_words(t5.2.2_data_2u$country, oxford_comma=FALSE)), ")")

## merge
t5.2.2_data2 <- data.frame(t5.2.2_2.1,t5.2.2_2.2,t5.2.2_2.3) %>%
  rename(Yes = 1, No = 2, Unknown = 3) %>% 
  mutate(Type = "Housing: protection from eviction") %>%
  select(Type, Yes, No, Unknown) %>%
  rbind(t5.2.2_1)

# parenting
## yes
t5.2.2_data_3y <- t5.2.2_data %>%
  filter(protect_parenting == 1) 

t5.2.2_3.1 <- paste0(t5.2.2_data_3y %>% nrow(), " countries", "(", gsub("(United)|(Democratic)|(Philip)|(Central)", "the \\1\\2\\3\\4", knitr::combine_words(t5.2.2_data_3y$country, oxford_comma=FALSE)), ")")

## no
t5.2.2_data_3n <- t5.2.2_data %>%
  filter(protect_parenting == 0) 

t5.2.2_3.2 <- paste0(t5.2.2_data_3n %>% nrow(), " countries\n(", gsub("(United)|(Democratic)|(Philip)|(Central)", "the \\1\\2\\3\\4", knitr::combine_words(t5.2.2_data_3n$country, oxford_comma=FALSE)), ")")

## unknown
t5.2.2_data_3u <- t5.2.2_data %>%
  filter(protect_parenting == 3) 

t5.2.2_3.3 <- paste0(t5.2.2_data_3u %>% nrow(), " countries\n(", gsub("(United)|(Democratic)|(Philip)|(Central)", "the \\1\\2\\3\\4", knitr::combine_words(t5.2.2_data_3u$country, oxford_comma=FALSE)), ")")

## merge
t5.2.2_data2 <- data.frame(t5.2.2_3.1,t5.2.2_3.2,t5.2.2_3.3) %>%
  rename(Yes = 1, No = 2, Unknown = 3) %>% 
  mutate(Type = "Parenting: parental rights protected") %>%
  select(Type, Yes, No, Unknown) %>%
  rbind(t5.2.2_data2)

# movement
## yes
t5.2.2_data_4y <- t5.2.2_data %>%
  filter(protect_movement == 1) 

t5.2.2_4.1 <- paste0(t5.2.2_data_4y %>% nrow(), " countries\n(", gsub("(United)|(Democratic)|(Philip)|(Central)", "the \\1\\2\\3\\4", knitr::combine_words(t5.2.2_data_4y$country, oxford_comma=FALSE)), ")")

## no
t5.2.2_data_4n <- t5.2.2_data %>%
  filter(protect_movement == 0) 

t5.2.2_4.2 <- paste0(t5.2.2_data_4n %>% nrow(), " countries\n(", gsub("(United)|(Democratic)|(Philip)|(Central)", "the \\1\\2\\3\\4", knitr::combine_words(t5.2.2_data_4n$country, oxford_comma=FALSE)), ")")

## unknown
t5.2.2_data_4u <- t5.2.2_data %>%
  filter(protect_movement == 3) 

t5.2.2_4.3 <- paste0(t5.2.2_data_4u %>% nrow(), " countries\n(", gsub("(United)|(Democratic)|(Philip)|(Central)", "the \\1\\2\\3\\4", knitr::combine_words(t5.2.2_data_4u$country, oxford_comma=FALSE)), ")")

## merge
t5.2.2_data2 <- data.frame(t5.2.2_4.1,t5.2.2_4.2,t5.2.2_4.3) %>%
  rename(Yes = 1, No = 2, Unknown = 3) %>% 
  mutate(Type = "Freedom of movement\u1D47: no restriction on access to congregate settings") %>%
  select(Type, Yes, No, Unknown) %>%
  rbind(t5.2.2_data2)

# association
## yes
t5.2.2_data_5y <- t5.2.2_data %>%
  filter(protect_movement == 1) 

t5.2.2_5.1 <- paste0(t5.2.2_data_5y %>% nrow(), " countries\n(", gsub("(United)|(Democratic)|(Philip)|(Central)", "the \\1\\2\\3\\4", knitr::combine_words(t5.2.2_data_5y$country, oxford_comma=FALSE)), ")")

## no
t5.2.2_data_5n <- t5.2.2_data %>%
  filter(protect_movement == 0) 

t5.2.2_5.2 <- paste0(t5.2.2_data_5n %>% nrow(), " countries\n(", gsub("(United)|(Democratic)|(Philip)|(Central)", "the \\1\\2\\3\\4", knitr::combine_words(t5.2.2_data_5n$country, oxford_comma=FALSE)), ")")

## unknown
t5.2.2_data_5u <- t5.2.2_data %>%
  filter(protect_movement == 3) 

t5.2.2_5.3 <- paste0(t5.2.2_data_5u %>% nrow(), " countries\n(", gsub("(United)|(Democratic)|(Philip)|(Central)", "the \\1\\2\\3\\4", knitr::combine_words(t5.2.2_data_5u$country, oxford_comma=FALSE)), ")")

## merge
t5.2.2_data2 <- data.frame(t5.2.2_5.1,t5.2.2_5.2,t5.2.2_5.3) %>%
  rename(Yes = 1, No = 2, Unknown = 3) %>% 
  mutate(Type = "Freedom of association: no compulsory isolation") %>%
  select(Type, Yes, No, Unknown) %>%
  rbind(t5.2.2_data2)

# arrange
t5.2.2_data2 <- t5.2.2_data2 %>%
  mutate(Type = factor(Type, levels = c("Employment: protection from dismissal",
                                        "Housing: protection from eviction",
                                        "Parenting: parental rights protected",
                                        "Freedom of movement\u1D47: no restriction on access to congregate settings",
                                        "Freedom of association: no compulsory isolation"))) %>%
  arrange(Type)

