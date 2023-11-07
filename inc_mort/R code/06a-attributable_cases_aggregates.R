#' This scripts generates aggregated estimates of attributable incidence
#' for inclusion in a Global TB report table.
#' 
#' Deps: libraries data.table, here and propagate
#' 
#' 
#' Input: att.rda (see script ~/R/07-attr.R)
#'        global.rda (global estimates), est.rda (country estimates), rf.global.rda
#' 
#' 
#' 
#' Table 6.3.1 - Attributable cases
#'
#Run Functions script
source(here('inc_mort/R code/fun.R'))

suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(propagate))
suppressPackageStartupMessages(library(dplyr))

library(here)

# !!! add here the libraries needed to run lines 84-102 (coded added by TME)



load(here::here("inc_mort/analysis/atttributable_cases.rda"))
load(here::here("inc_mort/analysis/global.rda"))
load(here::here("inc_mort/analysis/est.rda"))
load(here::here("inc_mort/analysis/rf.global.rda"))

vglohi <- Vectorize(glohi, c("ev", "sd"))

att <- att %>% subset(sex=="a")

yr=2022


#' Global IRR
hiv <- est[year == yr, addXY(hiv, r.sd = hiv.sd, weights = pop)] # TY:changed from yr to year

DT <- cbind(
  inc.h = c(last(global$inc.h), last(global$inc.h.sd)),
  hiv = c(hiv$r, hiv$r.sd),
  inc.nh = c(last(global$inc.nh), last(global$inc.nh.sd))
)
EXPR <- expression((inc.h / hiv) / (inc.nh / (1 - hiv))) 
out <- propagate(EXPR, DT)

# # Risk Ratios (mean, 95%CIs)
rr.hiv <- with(out, c(prop[2], prop[5], prop[6]))  #generate "rr.hiv" object for table

rr.alc <- c(3.33, 2.14, 5.19)  # alcohol, Eur Resp Dis 2017
rr.dia <-  c(1.5, 1.28, 1.76) # diabetes, Trop Med Int Health 2018
rr.und <- c(3.2, 3.1, 3.3) # under-nourishment, GTB 2018
rr.smk <- c(1.57, 1.18, 2.1) # smoking, GTB 2019

# generate a base df for summarizing RRs
dta <- data.table(
  risk.factor = c("Alcohol", "Diabetes", "HIV", "Smoking", "Undernourishment"),
  rr = c(rr.alc[1], rr.dia[1], rr.hiv[1], rr.smk[1], rr.und[1]),
  rr.lo = c(rr.alc[2], rr.dia[2], rr.hiv[2], rr.smk[2], rr.und[2]),
  rr.hi = c(rr.alc[3], rr.dia[3], rr.hiv[3], rr.smk[3], rr.und[3])
)


# add size of exposed population into the base df 
h <- 1000
att[, exp.alc := adults * alcohol]
att[, exp.dia := adults * diabetes]
att[, exp.hiv := pop * hiv]
att[, exp.smk := adults * smoking]
att[, exp.und := pop * undernutrition]

exposed <- att %>% select(exp.alc,exp.dia,exp.hiv,exp.smk,exp.und) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) 

exposed <- as.data.frame(t(exposed)) %>%
  rename(exposed=1)

dta <- cbind.data.frame(dta,exposed)

# add PAF into the base df 
pop <- att %>% select(pop,adults) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) 

dta <- cbind.data.frame(dta,pop)

dta <- dta %>%
  mutate(PAF = paf(exposed/pop,rr))

# take attributable from rf.global.rda and add into the base df 
rf.global %>% filter(sex=="a") %>%
  arrange(as.character(risk.factor)) %>%
  select(best,lo,hi) %>% cbind.data.frame(dta,.) -> dta


# export a raw table
fwrite(dta, file = here::here("report/tab5.3.2_raw.csv"))

# formatting the table with GTB style
dta <- dta %>% 
  select(!pop&!adults) %>%
  mutate(exposed=exposed/1e6,
         PAF=PAF*100,
         best=best/1e6,
         lo=lo/1e6,
         hi=hi/1e6) 
  
dta <- dta %>% 
  mutate(across(where(is.numeric), ftb)) 

# export the formatted table for GTBR
fwrite(dta, file = here::here("report/tab5.3.2.csv"))


