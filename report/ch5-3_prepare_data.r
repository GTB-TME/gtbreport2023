# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data preparation script for ch5-3.rmd
# Takuya Yamanaka, June 2023
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Load chapter 6 packages, settings and data
source(here::here('report/ch5_load_data.r'))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: fig 5.3.1 ----
# (The relationship between GDP per capita and the prevalence of undernutrition, and TB incidence per 100 000 population)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

if(show_estimates){
f5.3.1a_data <-
  sdg %>% filter(year<=report_year) %>% arrange(-year) %>%
  group_by(iso3,indicator_id) %>% slice(1) %>%
  filter(indicator_id == 'NY.GDP.PCAP.PP.KD') %>%
  left_join(est %>% filter(year==report_year-1) %>% select(iso3,inc)) %>%
  mutate(gdp=value/1e3) %>%
  ungroup() %>%
  select(country,iso3,gdp,inc) 

f5.3.1b_data <- 
  sdg %>% filter(year<=report_year)  %>% arrange(-year) %>% 
  group_by(iso3,indicator_id) %>% slice(1) %>% 
  filter(indicator_id == 'SN.ITK.DEFC.ZS') %>%
  left_join(est %>% filter(year==report_year-1) %>% select(iso3,inc)) %>% 
  mutate(nut=value) %>% 
  ungroup() %>% 
  select(country,iso3,nut,inc) 

f5.3.1_data <- f5.3.1a_data %>%
  left_join(f5.3.1b_data, by = c("country","iso3","inc"))
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: fig 5.3.2 ----
# (Global estimates of the number of TB cases attributable to selected risk factors)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

if(show_estimates){
f5.3.2_data <- rf_global %>%
  filter(group_type=="global",sex=="a",risk_factor!="all") %>%
  mutate(risk_factor=factor(risk_factor, labels=c("HIV infection","Diabetes","Alcohol use disorders","Smoking","Undernourishment"))) %>%
  mutate(risk_factor=fct_rev(risk_factor)) 


f5.3.2_data$risk_factor <- forcats::fct_reorder(f5.3.2_data$risk_factor,f5.3.2_data$best,min) 
  
f5.3.2_txt <- f5.3.2_data %>%
  select(group_type, risk_factor, best) %>%
  pivot_wider(names_from = risk_factor, values_from = best) %>%
  rename(alcohol = `Alcohol use disorders`, hiv = `HIV infection`)
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: fig 5.3.3 ----
# (Estimated number of TB cases attributable to five risk factors per 100 000 population)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# merge pop to att
if(show_estimates){
att <- rf_country %>%
  subset((age=="15+"|age=="18+"|age=="a")&sex=="a")

db_estimates_country %>%
  filter((age_group=="15plus"|age_group=="a")&sex=="a") %>%
  select(iso3, age_group, best) %>%
  pivot_wider(names_from = age_group, values_from = best) %>%
  rename(adult=2,all=3) %>%
  right_join(att,by="iso3") -> att

att <- att %>%
  mutate(paf_pct=ifelse(age=="a",best/all*100,best/adult*100)) %>%
  right_join(list_iso3_country,by="iso3")

f5.3.3_data <- 
  att %>% 
  select(country,iso3,year,age,risk_factor,best,paf_pct) 

#### subset for malnutrition
palatte_fig5.3.3a = c(#"#FFEDA0",
  "#FED976","#FEB24C","#FD8D3C",#"#FC4E2A",
  "#E31A1C") 

f5.3.3a_data <- 
  f5.3.3_data %>% subset(risk_factor=="undernutrition") %>%
  mutate(var=cut(paf_pct,breaks=c(0,10,15,20,Inf),right = FALSE,labels=c("<10","10\u201314","15\u201319","\u226520"))
  )

#### subset for alcohol use
palatte_fig5.3.3b = c(#"#ECE7F2",
  "#D0D1E6","#A6BDDB","#74A9CF",#"#3690C0",
  "#0570B0") 

f5.3.3b_data <- 
  f5.3.3_data %>% subset(risk_factor=="alcohol") %>%
  mutate(var=cut(paf_pct,breaks=c(0,5,10,15,Inf),right = FALSE,labels=c("<5","5\u20139","10\u201314","\u226515"))
)

### subset for HIV
palatte_fig5.3.3c = c(#"#F7FCB9",
  "#D9F0A3","#ADDD8E","#78C679",#"#41AB5D",
  "#238443") 

f5.3.3c_data <- 
  f5.3.3_data %>% subset(risk_factor=="hiv") %>%
  mutate(var=cut(paf_pct,breaks=c(0,1,5,10,Inf),right = FALSE,labels=c("<1","1\u20134","5\u20139","\u226510"))
)

### subset for Diabetes
palatte_fig5.3.3d = c(#"#FDE0DD",
  "#FCC5C0","#FA9FB5","#F768A1",#"#DD3497",
  "#AE017E") 
f5.3.3d_data <- 
  f5.3.3_data %>% subset(risk_factor=="diabetes") %>%
  mutate(var=cut(paf_pct,breaks=c(0,3,4,5,Inf),right = FALSE,labels=c("<3","3\u20134","4\u20135","\u22655"))
  )

# subset for smoking
palatte_fig5.3.3e = c(#"#EFEDF5",
  "#DADAEB","#BCBDDC","#9E9AC8",#"#807DBA",
  "#6A51A3") 
f5.3.3e_data <- 
  f5.3.3_data %>% subset(risk_factor=="smoking") %>%
  mutate(var=cut(paf_pct,breaks=c(0,5,10,15,Inf),right = FALSE,labels=c("<5","5\u20139","10\u201314","\u226515"))
  )

}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: fig 5.3.4 ----
# (Estimated number of TB cases attributable to five risk factors, 30 high TB burden countries and three global TB watchlist countries)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

if(show_estimates){
palatte_f5.3.4 = c("#084EA2","#0491D1","#ED1D24","#B92270","#91A93E") 
# Blue (alcohol), Skyblue (diabetes), Red (HIV), purple (smoking), green (undernourishment)

#' # Risk Ratios (mean, sd)
#'
# rr.alc <-
#   c(3.33, (2.14 - 5.19) / 3.94) # alcohol, Eur Resp Dis 2017
# rr.dia <-
#   c(1.5, (1.76 - 1.28) / 3.92) # diabetes, Trop Med Int Health 2018
# rr.und <- c(3.2, 0.2 / 3.92) # under-nourishment, GTB 2018
# rr.smk <- c(1.57, (2.1 - 1.18) / 3.92) # smoking, GTB 2019
# #HIV globally: mean 19.2; SD 1.5
# rr.hiv <- c(19.2,1.5)

f5.3.4_data <- 
  rf_country %>% subset(sex=="a"&risk_factor!="all") %>% 
  right_join(list_iso3_country,by="iso3") %>%
  filter(iso3 %in% iso3_hbc, sex=='a') %>%
  select(country,iso3,year,age,risk_factor,best,lo,hi) %>%
  # mutate(across(6:8, ~./1000)) %>% 
  mutate(riskgrp=factor(risk_factor,levels=c('alcohol','diabetes','hiv','smoking','undernutrition'))) %>% 
  left_join(list_hbcs_plus_wl) %>% 
  mutate(risk_factor=fct_recode(risk_factor,'Alcohol use disorders'='alcohol',Smoking='smoking',Diabetes='diabetes',
                                HIV='hiv',Undernourishment='undernutrition')) %>%
  arrange(country,risk_factor)

}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: fig 5.3.5 ----
# (Status of selected SDG indicators beyond SDG 3 by country, latest available year)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Maps for SDG indicators 
sdgind_radar <- c("EG.CFT.ACCS.ZS","SN.ITK.DEFC.ZS","EN_LND_SLUM","per_allsp.cov_pop_tot",
                  "SI_POV_DAY1","SI.POV.GINI")

f5.3.5_data <- 
  sdg %>% 
  arrange(-year) %>% 
  group_by(iso3,indicator_id) %>% slice(1) %>% 
  filter(indicator_id %in% sdgind_radar) %>%
  pivot_wider(id_cols = c(iso3,country), names_from = indicator_id, values_from = value) %>% 
  mutate("Clean fuels"=EG.CFT.ACCS.ZS, "Undernourishment"=SN.ITK.DEFC.ZS, 
         "Living in slums"=EN_LND_SLUM, "Social protection"=per_allsp.cov_pop_tot, 
         "Living in poverty"=SI_POV_DAY1, "Income inequality"=SI.POV.GINI) %>% 
  select(iso3,country,'Clean fuels':'Income inequality') %>% 
  arrange(country)

f5.3.5_data <- f5.3.5_data %>% 
  mutate(country = ifelse(iso3=='PRK',"Democratic People's\nRepublic of Korea",country)) %>% 
  mutate(country = ifelse(iso3=='COD',"Democratic Republic\nof the Congo",country))

#### subset for clean fuels
f5.3.5a_data <- 
  f5.3.5_data %>% 
  mutate(var=cut(`Clean fuels`,breaks=c(0,30,60,80,Inf),right = FALSE,
                 labels=c("<30","30\u201359","60\u201379","\u226580")))  

#### subset for income equality
f5.3.5b_data <- 
  f5.3.5_data %>% 
  mutate(var=cut(`Income inequality`,breaks=c(0,32,35,42,Inf),right = FALSE,
                 labels=c("<32","32\u201334","35\u201341","\u226542")))  

### subset for living in poverty
f5.3.5c_data <- 
  f5.3.5_data %>% 
  mutate(var=cut(`Living in poverty`,breaks=c(0,1,2,20,Inf),right = FALSE,
                 labels=c("<1","1\u20131.9","2\u201319","\u226520"))) 

# subset for social protection
f5.3.5d_data <- 
  f5.3.5_data %>% 
  mutate(var=cut(`Social protection`,breaks=c(0,20,40,60,Inf),right = FALSE,
                 labels=c("<20","20\u201339","40\u201359","\u226560"))) 

# subset for living in slums
f5.3.5e_data <- 
  f5.3.5_data %>% 
  mutate(var=cut(`Living in slums`,breaks=c(0,15,30,50,Inf),right = FALSE,
                 labels=c("<15","15\u201329","30\u201349","\u226550")))  

# subset for malnutrition
f5.3.5f_data <- 
  f5.3.5_data %>% 
  mutate(var=cut(`Undernourishment`,breaks=c(0,3,5,15,Inf),right = FALSE,
                 labels=c("<3","3\u20134","5\u201314","\u226515")))  


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: fig 5.3.6 ----
# (Status of selected SDG indicators beyond SDG 3 in 30 high TB burden countries, latest available year)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f5.3.6_data <- 
  sdg %>% filter(iso3 %in% iso3_hbc) %>% 
  arrange(-year) %>%
  group_by(iso3,indicator_id) %>% slice(1) %>% 
  filter(indicator_id %in% sdgind_radar) %>%
  pivot_wider(id_cols = c(iso3,country), names_from = indicator_id, values_from = value) %>%
  mutate("Clean fuels"=EG.CFT.ACCS.ZS, "Nutrition"=100-SN.ITK.DEFC.ZS, 
         "Not in slums"=100-EN_LND_SLUM, "Social protection"=per_allsp.cov_pop_tot, 
         "Not in poverty"=100-SI_POV_DAY1, "Income equality"=100-SI.POV.GINI) %>% 
  select(iso3,country,'Clean fuels':'Income equality') %>% 
  arrange(country) %>%
  pivot_longer(cols = 'Clean fuels':'Income equality', names_to = "sdg", values_to = "value") 

f5.3.6_data <- f5.3.6_data %>% 
  mutate(country = ifelse(iso3=='PRK',"Democratic People's\nRepublic of Korea",country)) %>% 
  mutate(country = ifelse(iso3=='COD',"Democratic Republic\nof the Congo",country))

f5.3.6_data <- f5.3.6_data %>%
  mutate(value=as.integer(value))


f5.3.6b_data <- 
  sdg %>% filter(iso3 %in% iso3_hbc) %>% 
  group_by(iso3,indicator_id) %>% 
  filter(sex == "a", year<=report_year-1) %>%
  pivot_wider(id_cols = c(iso3,country,year), names_from = indicator_id, values_from = value) %>%
  mutate("Clean fuels"=EG.CFT.ACCS.ZS, "Nutrition"=100-SN.ITK.DEFC.ZS, 
         "Not in slums"=100-EN_LND_SLUM, "Social protection"=per_allsp.cov_pop_tot, 
         "Not in poverty"=100-SI_POV_DAY1, "Income equality"=100-SI.POV.GINI) %>% 
  select(iso3,country, year, 'Clean fuels':'Income equality') %>% 
  arrange(country) %>%
  pivot_longer(cols = 'Clean fuels':'Income equality', names_to = "sdg", values_to = "value") 

f5.3.6b_plot <- f5.3.6b_data %>% 
  filter(!is.na(value)) %>%
  ggplot(aes(x=year,y=value,col=sdg)) + 
  geom_line() + geom_point(size=2) +
  scale_colour_manual(values=c("#6363c0","#f26829","#bd53bd","#008dc9","#40bf73","#f4a81d")) +
  facet_wrap(~country, ncol = 3) +
  theme_gtb() +
  theme(panel.grid.major.y = element_blank(),
        axis.line.y.left = element_line(size = 0.5),
        axis.ticks.y.left = element_line(),
        axis.line.x = element_line(size = 0.5)) +
  ylim(0,100) + xlab("") +
  ylab("Index") 

ggsave(f5.3.6b_plot, file=here("./report/local/figures/ch5.3/f5.3.6b_plot.png"),width=10,height=20) 
