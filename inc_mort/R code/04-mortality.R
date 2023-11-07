#' ---
#' title: Mortality
#' author: Mathieu Bastard, Philippe Glaziou
#' date: 2023-06-07
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

#' # Preamble
#' (Last updated: `r Sys.Date()`)
#'
#'
#' The mortality rate due to TB can be estimated using a variety of methods, including:
#' - Surveillance data: Health authorities can track the number of deaths caused by TB 
#'   that are reported through surveillance systems. This can help to estimate the 
#'   overall TB mortality rate in a population.
#' - Mathematical modeling: Researchers can use mathematical models to estimate TB 
#'   mortality based on data on factors such as population size, TB prevalence, 
#'   and the effectiveness of TB prevention and treatment programs.
#' - Epidemiological (cohort or retrospective cohort) studies: Researchers can 
#'   also conduct epidemiological studies to estimate TB mortality in a specific 
#'   population. These studies can involve collecting data on TB cases and deaths 
#'   and using statistical methods to estimate the overall TB mortality rate. This 
#'   appraoch is not currently used due to the lack of nationally representative 
#'   empirical data. Data from the cohort analysis of TB treatment outcomes suffer from
#'   the following limitations:
#'      - undocumented causes of deaths (all deaths occuring during TB treatment are counted,
#'      including deaths from causes other than TB)
#'      - incomplete reporting (deaths occuring among patients classified as unevaluated or 
#'      lost to follow-up (defaulters) are not accounted for)
#'      - TB deaths occuring among people not registered for TB treatment are not accounted 
#'      for. 
#'
#'
#' This script generates TB mortality estimates with HIV disaggregation using:
#' - vr.rda updated VR data stored in the WHO mortality database, with country-year
#'    data points selected based on data quality criteria as documented in the 
#'    technnical appendix and adjusted to account for incomplete coverage and garbage
#'    codes
#' - old.rda previously published mortality estimates, including those originating
#'    from IHME for the period 2000-2019 (not updated as of the summer of 2022)
#' - model.rda, extra.rda output from dynamic models over the period 2020-2021
#' - unaids.rda UNAIDS estimates of AIDS deaths (ensuring that HIV+ TB deaths do 
#'    not exceed AIDS deaths)
#' - R object est as obtained from the previous script in the sequence
#' 
#' Estimates over 2000-2019 are kept from the previously published series, except 
#' in countries with updated VR data in the WHO mortality database (many countries 
#' report with a delay of several years, particularly in E Europe). Estimates from IHME
#' were not updated.
#' 
#' For the period 2020-2021, the approach consists in using one of the following 3 sources:
#' - Nim's models output in countries with covid disruptions
#' - forward projections in high-income countries and countries with no significant 
#'    covid disruptions (as measured based on TB case notifications)
#' - updated VR data from the WHO database
#' - updated VR data directly obtained from NTP (RUS, CHN) with the usual statistical
#'    adjustments for incomplete coverage and garbage codes, with values hard-coded 
#'    in this script (given their small number and late availability during the summer of 2022, 
#'    but ideally, a separate input file should be used for this purpose and 
#'    merged with vr.rda)
#' 
#' Deps: 
#'  - libraries data.table and here, 
#'    imputeTS and propagate (as for the previous script in the sequence)
#'    
#'    
#' Input: 
#'  - output from the 4 previous scripts in the sequence
#'  - vr.rda (VR data in the WHO mortality database)
#'  - model.rda and extra.rda (Nim's models output)
#'  - unaids.rda (UNAIDS estimates of HIV burden)
#'  
#'  
#' Output:
#'  - est.rda and dated csv file 
#'  - numerous graphic checks in ~/output/checks
#' 
#' 
#'
# Mortality HIV-neg
#
# Load libraries and data
#
# Load libraries and data
#



suppressMessages(library(data.table))
suppressMessages(library(imputeTS))
suppressMessages(library(zoo))
suppressMessages(library(propagate))
suppressMessages(library(here))
suppressMessages(library(readxl))


rm(list=ls())

start_time <- Sys.time()


#Run Functions script
source(here('inc_mort/R code/fun.R'))

#Function to import db from GTB databases
source(here("import/load_gtb.R"))



tb <- load_gtb("tb",convert_dots = FALSE)
cty <- load_gtb("cty",convert_dots = FALSE)
pop <- load_gtb("pop",convert_dots = FALSE)
grpmbr <- load_gtb("grpmbr",convert_dots = FALSE)

#Nim results: update to the most recent one

#Country model
model=fread("dynamic/output/230803_Modelled_Countries.csv",
            col.names=c("iso3","scenario","hiv","measure","year","best","lo","hi"))
setkey(model, iso3)

#Extrapolated countries
extra=fread("dynamic/output/230802_Extrapolated_Countries.csv",
            col.names=c("iso3","scenario","hiv","measure","year","best","lo","hi"))
setkey(extra, iso3)

#load data generated by previous scripts
load(here('inc_mort/analysis/tbhiv.rda'))
load(here('inc_mort/estimates2022/old.rda'))
load(here("inc_mort/analysis/vr.rda"))

#load UNAIDS data
load(here('inc_mort/analysis/unaids.rda'))

#load India mortality estimate work based on SRS data
load(here('inc_mort/analysis/srsindia.rda'))

#India estimates received on July 2023
# Sent by Sandip Mandal

india=fread("inc_mort/input_inc/230802_India_outputs.csv")

# !!! update the following line as needed using the last incidence estimate dataset 

est <- fread('inc_mort/analysis/csv/est_inc_2023-08-03.csv')


setkey(est, iso3, year)


m <- 1e5
yr <- 2022



# vectorized lohi
#
vlohi <- Vectorize(lohi, c('ev', 'sd'))


# import vr
dim(est)
est <-
  merge(est, vr[, .(
    iso3,
    year,
    vr.keep = keep.vr,
    vr.garbage = garbage,
    vr.coverage,
    vr.quality = codqual,
    vr.env = env,
    # ghe.env, ghe.env.lo, ghe.env.hi,
    vr.mort.nh = tb.adj * m / pop,
    vr.raw = tb * m / pop,
    vr.mort.nh.sd = tb.adj.sd * m / pop
  )],
  by = c('iso3', 'year'), all.x = TRUE)
dim(est)


# incorporate old values of mortality (2000 - 2019)
est <-
  merge(est, old[, .(iso3,
                     year,
                     mort.nh,
                     mort.nh.sd,
                     mort.h,
                     mort.h.sd,
                     mort,
                     mort.sd,
                     old.source.mort = source.mort)], by = c('iso3', 'year'), all.x = TRUE)
dim(est)





# VR updates since the June 2021 snapshot of the mortality database
# RUS treated separatly

sel <- !is.na(est$vr.mort.nh) & est$vr.keep==TRUE & est$old.source.mort!='IHME' & est$iso3!="RUS"
table(sel)
est[sel, mort.nh := vr.mort.nh]
est[sel, mort.nh.sd := vr.mort.nh.sd]
est[sel, source.mort := 'VR']
est[is.na(source.mort), source.mort := old.source.mort]


### VR data from GTB database or VR DDI for 2020-2022
### Interpolate missing year and SD

vr.2020.2022.lst=c("AZE","BRA","CZE","EST","GEO","KAZ","RUS","UKR","ROU")
sel= est$iso3 %in% vr.2020.2022.lst  & est$year %in% 2020:2022 

est[sel, mort.nh := NA]
est[sel, mort.nh.sd := NA]

est[sel, mort.nh := vr.mort.nh]
est[sel, mort.nh.sd := vr.mort.nh.sd]
est[sel, source.mort := 'VR']
est[is.na(source.mort), source.mort := old.source.mort]

est[sel, mort.nh := na_interpolation(mort.nh)]
est[sel, mort.nh.sd := na_interpolation(mort.nh.sd)]






# check missing values: should only be for 2022
sum(is.na(est$mort.nh) &
      est$year < yr) == 0   # TRUE: only year==yr mort values are missing

# check VR updates where usable
est[year < yr &
      vr.keep == TRUE, sum(vr.mort.nh / mort.nh > 1.1, na.rm = T)]

est[year < yr &
      vr.keep == TRUE & vr.mort.nh / mort.nh > 1.1, .(iso3, year, mort.nh, vr.mort.nh)]
# all in ZAF, we ignore due to major miscoding issues btw HIV and TB causes


est[year < yr &
      vr.keep == TRUE, sum(vr.mort.nh / mort.nh < .9, na.rm = T)]





#  indirect estimates of mortality (using CFR*incidence), stored in e.mort.*

out1 <-
  est[, {
    tmp = inc2mort(inc, inc.sd, imp.newinc, tbhiv, tbhiv.sd, noHIV =
                     T)$prop

    list(mort.nh = tmp[2],
         mort.nh.sd = tmp[4])
  },
  by = .(iso3, year)]

out2 <-
  est[, {
    tmp = inc2mort(inc, inc.sd, imp.newinc, tbhiv, tbhiv.sd, noHIV =
                     F)$prop

    list(mort.h = tmp[2],
         mort.h.sd = tmp[4])
  },
  by = .(iso3, year)]

est[, e.mort.nh := out1$mort.nh]
est[, e.mort.nh.sd := out1$mort.nh.sd]
est[, e.mort.h := out2$mort.h]
est[, e.mort.h.sd := out2$mort.h.sd]
est[, e.mort := e.mort.h + e.mort.nh]
est[, e.mort.sd := sqrt(e.mort.h.sd ^ 2 + e.mort.nh.sd ^ 2)]



# complete countries with new VR data 2020-2022 (add HIV+ and totals)
sel= est$iso3 %in% vr.2020.2022.lst  & est$year %in% 2020:2022 
est[sel, mort.h := e.mort.h]
est[sel, mort.h.sd := e.mort.h.sd]
est[sel, mort := mort.nh + mort.h]
est[sel, mort.sd := sqrt(mort.h.sd ^ 2 + mort.nh.sd ^ 2)]
est[sel, source.mort := 'VR']



# incorporate "focus" countries (Nim)
#

# HIV+ and HIV-
B2 <- copy(est) # backup point

(md.lst <- unique(model$iso3))

# exclude some countries from Nim's models:
# - Georgia (comms 2022)
# - Ethiopia, DPRK, South Africa because shortfalls in line with pre-2020 general decline
#   not necessarily due to covid-related drops in detection & treatment
# - CHN and RUS (model will not used for incidence)
md.exclude <- c('GEO', 'ETH', 'PRK', 'ZAF', 'CHN', 'RUS',"SRB")
md.exclude  = c(md.exclude, vr.2020.2022.lst)


dim(est)
est <-
  merge(est, model[scenario == 'COVID' &
                     hiv == 'a' &
                     measure == 'mort' &
                     iso3 %ni% md.exclude &
                     year %in% 2020:yr, .(
                       iso3,
                       year,
                       mort.md = best,
                       mort.md.lo = lo,
                       mort.md.hi = hi
                     )],
        by = c('iso3', 'year'), all.x = T)
dim(est)


sel <- est$year %in% 2020:yr & est$iso3 %in% md.lst & est$iso3 %ni% md.exclude
table(sel)
est[sel, mort := mort.md]
est[sel, mort.sd := (mort.md.hi - mort.md.lo ) / 3.92]
est[sel, mort.lo := mort.md.lo]
est[sel, mort.hi := mort.md.hi]
est[sel, sum(is.na(mort))]
est[sel, sum(is.na(mort.sd))]

est[sel, source.mort := "Country model"]
est[year==yr, table(source.mort)]
est[year==yr-1, table(source.mort)]



# HIV+ only
(md.h.lst <- unique(model[hiv=='pos', iso3]))

dim(est)
est <-
  merge(est, model[scenario == 'COVID' &
                     hiv == 'pos' &
                     measure == 'mort' &
                     iso3 %ni% md.exclude &
                     year %in% 2020:yr, .(
                       iso3,
                       year,
                       mort.h.md = best,
                       mort.h.md.lo = lo,
                       mort.h.md.hi = hi
                     )],
        by = c('iso3', 'year'), all.x = T)
dim(est)

sel <- est$year %in% 2020:yr & est$iso3 %in% md.h.lst & est$iso3 %ni% md.exclude
table(sel)
est[sel, mort.h := mort.h.md]
est[sel, mort.h.sd := (mort.h.md.hi - mort.h.md.lo ) / 3.92]
est[sel, mort.h.lo := mort.h.md.lo]
est[sel, mort.h.hi := mort.h.md.hi]
est[sel, sum(is.na(mort.h))]
est[sel, sum(is.na(mort.h.sd))]

est[sel, mort.nh := mort - mort.h]
est[sel & mort.sd<mort.h.sd,.(iso3,year,mort,mort.h,mort.nh,mort.sd,mort.h.sd)]
est[sel & mort.sd>mort.h.sd, mort.nh.sd := sqrt(mort.sd^2 - mort.h.sd^2)]
est[sel & mort.sd<mort.h.sd, mort.nh.sd := mort.sd * mort.nh/mort]
est[sel & mort.sd<mort.h.sd, mort.h.sd := mort.sd * mort.h/mort]



# HIV+ missing
(mh.lst <- setdiff(md.lst, md.h.lst))
sel <- est$iso3 %in% mh.lst  & est$iso3 %ni% md.exclude
table(sel)

est[sel, h.ratio := mort.h / mort]
est[sel & is.na(h.ratio), table(year)]
est[sel, h.ratio := imputeTS::na_locf(h.ratio), by=iso3]
est[sel & is.na(mort.h), mort.h := mort * h.ratio]
est[sel & is.na(mort.h.sd), mort.h.sd := mort.sd * h.ratio]
est[sel & is.na(mort.nh), mort.nh := mort * (1 - h.ratio)]
est[sel & is.na(mort.nh.sd), mort.nh.sd := mort.sd * (1 - h.ratio)]
est['VNM',.(iso3,year,mort,mort.nh,mort.h,h.ratio)]
est[, h.ratio := NULL]






# incorporate "extrapolated" countries (Nim's regional models)
# exclude high-income and GEO
#
B3 <- copy(est)


(extra.lst <- unique(extra$iso3))

dim(est)
est <-
  merge(est, extra[scenario == 'COVID' &
                     hiv == 'a' &
                     measure == 'mort' &
                     iso3 %ni% md.exclude &
                     year %in% 2020:yr, .(
                       iso3,
                       year,
                       mort.rmd = best,
                       mort.rmd.lo = lo,
                       mort.rmd.hi = hi
                     )],
        by = c('iso3', 'year'), all.x = T)
dim(est)

(exclude <- c('GEO', unique(est[g.income=='HIC', iso3])))
sel <-
  est$year %in% 2020:yr &
  est$iso3 %in% extra.lst &
  est$iso3 %ni% unique(c(exclude,  md.exclude))
table(sel)

est[sel, unique(iso3)] # list of extrapolated countries


est[sel, mort := mort.rmd]
est[sel, mort.sd := (mort.rmd.hi - mort.rmd.lo ) / 3.92]
est[sel, mort.lo := mort.rmd.lo]
est[sel, mort.hi := mort.rmd.hi]
est[sel, source.mort := "Regional model"]

est[sel, sum(is.na(mort))]
est[sel, sum(is.na(mort.sd))]

est[year==yr, table(source.mort)]
est[year==yr-1, table(source.mort)]



(rmd.h.lst <- unique(extra[hiv=='pos', iso3]))

dim(est)
est <-
  merge(est, extra[scenario == 'COVID' &
                     hiv == 'pos' &
                     measure == 'mort' &
                     iso3 %ni% md.exclude &
                     year %in% 2020:yr, .(
                       iso3,
                       year,
                       mort.h.rmd = best,
                       mort.h.rmd.lo = lo,
                       mort.h.rmd.hi = hi
                     )],
        by = c('iso3', 'year'), all.x = T)
dim(est)

sel <- est$year %in% 2020:yr & est$iso3 %in% rmd.h.lst & est$iso3 %ni% unique(c(exclude, md.exclude))
table(sel)
est[sel, mort.h := mort.h.rmd]
est[sel, mort.h.sd := (mort.h.rmd.hi - mort.h.rmd.lo ) / 3.92]
est[sel, mort.h.lo := mort.h.rmd.lo]
est[sel, mort.h.hi := mort.h.rmd.hi]

est[sel, sum(is.na(mort.h))]
est[sel, sum(is.na(mort.h.sd))]

est[, sum(is.na(mort)), by=year]
est[, sum(is.na(mort.sd)), by=year]

est[sel, mort.nh := mort - mort.h]
est[sel & mort.sd<mort.h.sd,.(iso3,year,mort,mort.h,mort.nh,mort.sd,mort.h.sd)]
est[sel & mort.sd>mort.h.sd, mort.nh.sd := sqrt(mort.sd^2 - mort.h.sd^2)]
est[sel & mort.sd<mort.h.sd, mort.nh.sd := mort.sd * mort.nh/mort]

est[sel & mort.sd<mort.h.sd,.(iso3,year,mort,mort.h,mort.nh,mort.nh.sd,mort.sd,mort.h.sd)]
#est["SWZ",.(iso3,year,mort,mort.h,mort.nh,mort.nh.sd,mort.sd,mort.h.sd)]


# HIV+ missing
(rmh.lst <- setdiff(extra.lst, rmd.h.lst))
sel <- est$iso3 %in% rmh.lst  & est$iso3 %ni% unique(c(exclude, md.exclude))
table(sel)
(est[sel, unique(iso3)])

est[sel, h.ratio := mort.h / mort]
est[sel & is.na(h.ratio), table(year)]
est[sel, h.ratio := imputeTS::na_locf(h.ratio), by=iso3]
est[sel & is.na(mort.h), mort.h := mort * h.ratio]
est[sel & is.na(mort.h.sd), mort.h.sd := mort.sd * h.ratio]
est[sel & is.na(mort.nh), mort.nh := mort * (1 - h.ratio)]
est[sel & is.na(mort.nh.sd), mort.nh.sd := mort.sd * (1 - h.ratio)]
est[, h.ratio := NULL]




# VR/IHME countries not in previous lists:
# use LOCF
B3b <- copy(est)

(est['FRA',.(iso3,year,mort,mort.nh,e.mort.nh,mort.nh.hat,source.mort)])
(est['CHN',.(iso3,year,mort,mort.nh,e.mort.nh,mort.nh.hat,source.mort)])
(est['ZAF',.(iso3,year,mort,mort.nh,e.mort.nh,mort.nh.hat,source.mort)])
(est['RUS',.(iso3,year,mort,mort.nh,e.mort.nh,mort.nh.hat,source.mort)])
est[, sum(is.na(mort.nh)), by=year]

(nh.miss <- est[year==yr & is.na(mort.nh), iso3])
(vr.lst <- intersect(nh.miss, est[source.mort %in% c('VR','IHME'), unique(iso3)]))

#2023: Add FJI to LOCF list since incidence was also LOCF and previous mortality was LOCF
(vr.lst = c(vr.lst,"FJI"))


est[iso3 %in% setdiff(vr.lst, 'CHN') & year==2020, mort := NA]
est[iso3 %in% setdiff(vr.lst, 'CHN') & year==2020, mort.sd := NA]
est[iso3 %in% setdiff(vr.lst, 'CHN') & year==2020, mort.h := NA]
est[iso3 %in% setdiff(vr.lst, 'CHN') & year==2020, mort.h.sd := NA]
est[iso3 %in% setdiff(vr.lst, 'CHN') & year==2020, mort.nh := NA]
est[iso3 %in% setdiff(vr.lst, 'CHN') & year==2020, mort.nh.sd := NA]

est[iso3 %in% vr.lst, mort := imputeTS::na_locf(mort), by=iso3]
est[iso3 %in% vr.lst, mort.sd := imputeTS::na_locf(mort.sd), by=iso3]
est[iso3 %in% vr.lst, mort.h := imputeTS::na_locf(mort.h), by=iso3]
est[iso3 %in% vr.lst, mort.h.sd := imputeTS::na_locf(mort.h.sd), by=iso3]
est[iso3 %in% vr.lst, mort.nh := imputeTS::na_locf(mort.nh), by=iso3]
est[iso3 %in% vr.lst, mort.nh.sd := imputeTS::na_locf(mort.nh.sd), by=iso3]

est[iso3 %in% vr.lst & year == 2021, source.mort := "VR/IHME, extrapolated"]
est[iso3 %in% setdiff(vr.lst, 'CHN') & year==2020, source.mort := "VR/IHME, extrapolated"]

# TEMP: Bug fix for ZAF 2020 source because inherited "model" from last year so change
est[iso3 == 'ZAF' & year == 2020, source.mort := "VR/IHME, extrapolated", by=iso3]




# replace LOCF with mort.nh.hat (predicted using logistic regression) 
# in subset of countries with VR/IHME located in AFR & Latin AMR
# to capture declines
# (keep SDs from LOCF above)
# Add TURKEY, not modeled bow so go back to other method: declining trend

vr.sub <- intersect(vr.lst, est[g.whoregion %in% c('AFR','AMR'), unique(iso3)])
(vr.sub <- setdiff(vr.sub, c('USA','CAN')))
(vr.sub <- c(vr.sub,"TUR","SRB"))
sel <- est$iso3 %in% vr.sub & est$year > 2019
table(sel)

# check series before update
qplot(year, mort.nh, data=est[iso3 %in% vr.sub & year>2009], geom='line') +
  facet_wrap(~iso3, scales='free_y')


est[sel, mort.nh := mort.nh.hat]
est[sel, mort.h := mort.h.hat]
est[sel, mort := mort.h + mort.nh]
est[sel, mort.sd := sqrt(mort.nh.sd^2 + mort.h.sd^2)]
est[sel, source.mort := "VR/IHME, extrapolated"]


# check updated series
qplot(year, mort.nh, data=est[iso3 %in% vr.sub & year>2009], geom='line') +
  facet_wrap(~iso3, scales='free_y')
qplot(year, mort.h, data=est[iso3 %in% vr.sub & year>2009], geom='line') +
  facet_wrap(~iso3, scales='free_y')

est[, sum(is.na(mort.nh)), by=year]



# others = use indirect estimates
#
B4 <- copy(est)

(lst <- est[is.na(mort.nh), unique(iso3)])

sel <- est$iso3 %in% lst & est$year %in% 2020:yr
table(sel)
est[sel, table(year)]

est[sel, mort := e.mort]
est[sel, mort.nh := e.mort.nh]
est[sel, mort.h := e.mort.h]

est[sel, mort.sd := e.mort.sd]
est[sel, mort.nh.sd := e.mort.nh.sd]
est[sel, mort.h.sd := e.mort.h.sd]

est[sel, sum(is.na(mort))]
est[sel, sum(is.na(mort.nh))]
est[sel, sum(is.na(mort.h))]

est[sel, table(source.mort)]
est[sel, source.mort := 'Indirect']

est[, sum(is.na(mort)), by=year]
est[, sum(is.na(mort.h)), by=year]
est[, sum(is.na(mort.nh)), by=year]

est[, sum(is.na(mort.sd)), by=year]
est[, sum(is.na(mort.h.sd)), by=year]
est[, sum(is.na(mort.nh.sd)), by=year]

est[, sum(is.na(source.mort)), by=year]

est[year==2022, table(source.mort)]
est[year==2021, table(source.mort)]
est[year==2020, table(source.mort)]
est[year==2019, table(source.mort)]


# # GEO inelegant fix
# # HT: also did this for ETH, PRK and ZAF who were also removed from Nim's model
# (est['GEO',.(iso3,year,mort,mort.nh,e.mort.nh,mort.nh.hat,source.mort)])
# (est['ETH',.(iso3,year,mort,mort.nh,e.mort.nh,mort.nh.hat,source.mort)])
# (est['PRK',.(iso3,year,mort,mort.nh,e.mort.nh,mort.nh.hat,source.mort)])
# (est['ZAF',.(iso3,year,mort,mort.nh,e.mort.nh,mort.nh.hat,source.mort)])

# lst <- c('ETH','PRK','ZAF')
# est[iso3 %in% lst & year>2019, mort.nh := mort.nh.hat]
# est[iso3 %in% lst & year>2019, mort.h := mort.h.hat]
# est[iso3 %in% lst & year>2019, mort := mort.h + mort.nh]
# 
# # START OF HACK - - - - - - - - - - - - - - - - -
# # Use same mort.nh.sd/mort.nh ratio from 2019 for subsequent years for eacg country
# # Hazim's very crude night hack -- donethis way to avoid any inadvertant errors, but should
# # be done in a better way in future
# 
# # 0.05 GEO nh from    est[iso3=='GEO' & year == 2019, .(mort.nh.sd/mort.nh)]
# # 0.21 GEO h from     est[iso3=='GEO' & year == 2019, .(mort.h.sd/mort.h)]
# 
# est[iso3 == 'GEO' & year>2019, mort.nh.sd := mort.nh * 0.05]
# est[iso3 == 'GEO' & year>2019, mort.h.sd := mort.h * 0.21]
# 
# # 0.22 ETH nh from    est[iso3=='ETH' & year == 2019, .(mort.nh.sd/mort.nh)]
# # 0.18 ETH h  from    est[iso3=='ETH' & year == 2019, .(mort.h.sd/mort.h)]
# 
# est[iso3 == 'ETH' & year>2019, mort.nh.sd := mort.nh * 0.22]
# est[iso3 == 'ETH' & year>2019, mort.h.sd := mort.h * 0.18]
# 
# # 0.17 PRK nh from    est[iso3=='PRK' & year == 2019, .(mort.nh.sd/mort.nh)]
# # 0.25 PRK h  from    est[iso3=='PRK' & year == 2019, .(mort.h.sd/mort.h)]
# 
# est[iso3 == 'PRK' & year>2019, mort.nh.sd := mort.nh * 0.17]
# est[iso3 == 'PRK' & year>2019, mort.h.sd := mort.h * 0.25]
# 
# # 0.03 ZAF nh from    est[iso3=='ZAF' & year == 2019, .(mort.nh.sd/mort.nh)]
# # 0.38 ZAF h  from    est[iso3=='ZAF' & year == 2019, .(mort.h.sd/mort.h)]
# 
# est[iso3 == 'ZAF' & year>2019, mort.nh.sd := mort.nh * 0.03]
# est[iso3 == 'ZAF' & year>2019, mort.h.sd := mort.h * 0.38]
# 
# # END OF HACK - - - - - - - - - - - - - - - - -


# est[iso3 %in% md.exclude & year>2019, mort.sd := sqrt(mort.nh.sd^2 + mort.h.sd^2)]



###
### CHN estimates
###

# # replace CHN with VR when communicated
# # locf for 2022 in the meantime ;
# # reuse reported 2020 values
(est['CHN',.(iso3,year,mort,mort.nh,e.mort.nh,mort.nh.hat,source.mort)])

#2020
sel <- est$iso3 == 'CHN' & est$year == 2020
sel2 <- old$iso3 == 'CHN' & old$year == 2020
est$mort[sel] <- old$mort[sel2]
est$mort.sd[sel] <- old$mort.sd[sel2]
est$mort.h[sel] <- old$mort.h[sel2]
est$mort.h.sd[sel] <- old$mort.h.sd[sel2]
est$mort.nh[sel] <- old$mort.nh[sel2]
est$mort.nh.sd[sel] <- old$mort.nh.sd[sel2]


#2021
sel <- est$iso3 == 'CHN' & est$year == 2021
est$mort.nh[sel] <- 1.95
est$mort.nh.sd[sel] <- 0.053 * est$mort.nh[sel]
est$mort.h[sel] <- 0.071 * est$mort.nh[sel]
est$mort.h.sd[sel] <- 0.14 * est$mort.h[sel]
est[sel, mort := mort.nh + mort.h]
est[sel, mort.sd := sqrt(mort.nh.sd^2 + mort.h.sd^2)]


#2022 : LOCF 2021
sel <- est$iso3 == 'CHN' & est$year == 2022
sel2 <- est$iso3 == 'CHN' & est$year == 2021
est$mort.nh[sel] <- est$mort.nh[sel2]
est$mort.nh.sd[sel] <- est$mort.nh.sd[sel2]
est$mort.h[sel] <- est$mort.h[sel2]
est$mort.h.sd[sel] <- est$mort.h.sd[sel2]
est[sel, mort := mort.nh + mort.h]
est[sel, mort.sd := sqrt(mort.nh.sd^2 + mort.h.sd^2)]


est['CHN',.(iso3,year,mort.nh,mort.nh.sd,mort.h,mort.h.sd,mort,mort.sd)]


###
### PAK: mortality by the country model unreliable in 2022
### Decline in 2022 similar as before covid 2017-2019
###

est['PAK',.(iso3,year,mort.nh,mort.nh.sd,mort.h,mort.h.sd,mort,mort.sd)]
old['PAK',.(iso3,year,mort.nh,mort.nh.sd,mort.h,mort.h.sd,mort,mort.sd)]

qplot(year, mort, data=est[iso3=="PAK" & year>2010], geom='line')


pak.decline.mort=( (est$mort.nh[est$iso3=="PAK" & est$year==2019]/est$mort.nh[est$iso3=="PAK" & est$year==2017]) ^ (1/2))

sel <- est$iso3 == 'PAK' & est$year == 2022
sel2 <- est$iso3 == 'PAK' & est$year == 2021

est$mort.nh[sel] <- est$mort.nh[sel2]*pak.decline.mort
est$mort.nh.sd[sel] <- est$mort.nh.sd[sel2]*pak.decline.mort

est$mort[sel] <- est$mort.nh[sel]+est$mort.h[sel]
est$mort.sd[sel] <- sqrt(est$mort.nh.sd[sel]^2+est$mort.h.sd[sel]^2)

qplot(year, mort, data=est[iso3=="PAK" & year>2010], geom='line')

est['PAK',.(iso3,year,mort.nh,mort.nh.sd,mort.h,mort.h.sd,mort,mort.sd)]




###
### IND: mortality replace by previous estimates until including 2010
### For 2000-2019, use updated estimates using new SRS data and interpolation of missing years
###                and use VR method to derive mortality among HIV-neg
### 2020-2022: country model (Sandip Mandal)
###


est['IND',.(iso3,year,mort.nh,mort.nh.sd,mort.h,mort.h.sd,mort,mort.sd)]
srsindia['IND',.(iso3,year,mort.nh,mort.nh.sd,mort.nh.num)]
old['IND',.(iso3,year,mort.nh,mort.nh.sd,mort.h,mort.h.sd,mort,mort.sd)]

# Replace the whole series 2000-2019 with new estimates from SRS
# 
sel <- est$iso3 == 'IND' & est$year %in% 2000:2019
sel2 <- srsindia$iso3 == 'IND' & srsindia$year %in% 2000:2019

est$mort.nh[sel] <- srsindia$mort.nh[sel2]
est$mort.nh.sd[sel] <- srsindia$mort.nh.sd[sel2]
est$mort[sel] <- est$mort.nh[sel]+est$mort.h[sel]
est[sel, mort.sd := sqrt(mort.nh.sd^2 + mort.h.sd^2)]

india=subset(india,scenario=="COVID" & measure=="mort", select=c("iso3","year","best","lo","hi"))

#Use Sandip estimates for 2011:2022 

# No more need to rescale
#rescale <- est$mort.nh[est$iso3=="IND" & est$year==2019]/india$best[india$year==2019]

sel <- est$iso3=='IND' & est$year %in% 2020:2022
sel2 <- india$year %in% 2020:2022

est$mort.nh[sel] <- india$best[sel2]
est$mort.nh.sd[sel] <- ((india$hi[sel2]-india$lo[sel2])/3.92)

# sel <- est$iso3=='IND' & est$year %in% 2011:2022
# sel2 <- india$year %in% 2011:2022
# 
# est$mort.nh[sel] <- india$best[sel2]
# est$mort.nh.sd[sel] <- ((india$hi[sel2]-india$lo[sel2])/3.92)


# 2000:2010 rescaled using SRS

# rescale <- est$mort.nh[est$iso3=="IND" & est$year==2011]/srsindia$mort.nh[srsindia$iso3=="IND" & srsindia$year==2011]
# 
# sel <- est$iso3=='IND' & est$year %in% 2000:2010
# sel2 <- srsindia$year %in% 2000:2010
# 
# est$mort.nh[sel] <- srsindia$mort.nh[sel2]*rescale
# est$mort.nh.sd[sel] <- ((srsindia$mort.nh.hi[sel2]-srsindia$mort.nh.lo[sel2])/3.92) * rescale
# 
# est["IND",.(year,mort.nh,mort.nh.sd)]



# mort hiv+ India update

out2 <-
  est[, {
    tmp = inc2mort(inc, inc.sd, imp.newinc, tbhiv, tbhiv.sd, noHIV =
                     F)$prop
    
    list(mort.h = tmp[2],
         mort.h.sd = tmp[4])
  },
  by = .(iso3, year)]

sel <- est$iso3=='IND' & est$year %in% 2000:2022

est$mort.h[sel] <- out2$mort.h[sel]
est$mort.h.sd[sel] <- out2$mort.h.sd[sel]

est$mort[sel] <- est$mort.nh[sel]+est$mort.h[sel]
est[sel, mort.sd := sqrt(mort.nh.sd^2 + mort.h.sd^2)]


est['IND',.(iso3,year,mort.nh,mort.nh.sd,mort.h,mort.h.sd,mort,mort.sd)]

qplot(year, mort, data=est[iso3=="IND" & year>2010], geom='line')

# 
# 
# #2021 and 2020 by previous estimate
# sel <- est$iso3 == 'IND' & est$year %in% 2020:2021
# sel2 <- old$iso3 == 'IND' & old$year %in% 2020:2021
# 
# est$mort.nh[sel] <- old$mort.nh[sel2]
# est$mort.nh.sd[sel] <- old$mort.nh.sd[sel2]
# est$mort.h[sel] <- old$mort.h[sel2]
# est$mort.h.sd[sel] <- old$mort.h.sd[sel2]
# est$mort[sel] <- old$mort[sel2]
# est$mort.sd[sel] <- old$mort.sd[sel2]
# 
# #LOCF 2022
# sel <- est$iso3 == 'IND' & est$year %in% 2022
# sel2 <- est$iso3 == 'IND' & est$year %in% 2021
#   
# est$mort.nh[sel] <- est$mort.nh[sel2]
# est$mort.nh.sd[sel] <- est$mort.nh.sd[sel2]
# est$mort.h[sel] <- est$mort.h[sel2]
# est$mort.h.sd[sel] <- est$mort.h.sd[sel2]
# est$mort[sel] <- est$mort[sel2]
# est$mort.sd[sel] <- est$mort.sd[sel2]
# 
# est['IND',.(iso3,year,mort.nh,mort.nh.sd,mort.h,mort.h.sd,mort,mort.sd)]
# 



# mort.h greater than mort.hiv?
sel <- est$mort.h >= est$mort.hiv & est$mort.h>0 & est$mort.hiv > 0
table(sel)
est[sel, unique(iso3)]
est[sel, table(year)]
est[sel, summary(mort.h/mort.hiv)]
est[!sel & mort.hiv>0, summary(mort.h/mort.hiv)]
# set cap at 60%
est[sel, mort.h := mort.hiv * 0.6]
est[sel, mort.h.sd := mort.h * 0.25]
est[sel, mort := mort.nh + mort.h]
est[sel, mort.sd := sqrt(mort.nh.sd^2 + mort.h.sd^2)]

est[is.na(mort.h.sd), ]
est[is.na(mort.h.sd), mort.h.sd := 0]


# mort.nh greater than mort?
est[mort.nh>mort, .(iso3,year,mort,mort.nh,mort.h)]
est[mort.nh>mort, mort.nh := mort - mort.h]

# checks
(sum(is.na(est$mort)) == 0)
(sum(is.na(est$mort.sd)) == 0)
(sum(is.na(est$mort.nh)) == 0)
(sum(is.na(est$mort.nh.sd)) == 0)
(sum(is.na(est$mort.h)) == 0)
(sum(is.na(est$mort.h.sd)) == 0)

est[, test.AgeB(mort, mort.nh)]
est[mort.hiv>0 & mort.h>0, test.AgeB(mort.hiv, mort.h)]




### Fix bug for countries where mort of last year != new series

tofix.lst=c("CUW","URY","SLV","MDV","HKG","ARE","BHR","SYR","EGY","BIH")

sel= est$iso3 %in% tofix.lst & est$year %in% 2019:2022
est[sel,.(iso3, year,mort,mort.nh,mort.h,vr.mort.nh,e.mort.nh,mort.nh.hat,source.mort)]

# Countries for which there was VR data in 2019 or before: use  use trend mort.hat
# For indirect, keep indirect estimates
# in 2023, all listed except CUW  and BHR should be changed from indirect to precovid trend

tofix.lst=c("URY","SLV","MDV","HKG","ARE","SYR","EGY","BIH")

sel= est$iso3 %in% tofix.lst & est$year %in% 2020:2022
sel2= est$iso3 %in% tofix.lst & est$year ==2019

sel20= est$iso3 %in% tofix.lst & est$year %in% 2020
sel21= est$iso3 %in% tofix.lst & est$year %in% 2021
sel22= est$iso3 %in% tofix.lst & est$year %in% 2022

est[sel, mort.nh := mort.nh.hat]
est[sel, mort.h := mort.h.hat]
est[sel, mort := mort.h + mort.nh]

est$mort.nh.sd[sel20]= est$mort.nh.sd[sel21]=est$mort.nh.sd[sel22] <- est$mort.nh.sd[sel2]
est$mort.h.sd[sel20]= est$mort.h.sd[sel21]=est$mort.h.sd[sel22] <- est$mort.h.sd[sel2]

est[sel, mort.sd := sqrt(mort.nh.sd^2 + mort.h.sd^2)]
est[sel, source.mort := "VR/IHME, extrapolated"]

est[sel,.(iso3, year,mort,mort.sd,mort.nh,mort.nh.sd,mort.h,vr.mort.nh,e.mort.nh,mort.nh.hat,source.mort)]




# Fix LCA to 1 deaths in 2022
sel= est$iso3=="LCA" & est$year==2022
est[sel, mort := 1/pop*m]
est[sel, mort.nh := mort-mort.h]



# add bounds and counts
#
B5 <- copy(est)

sel <- est$mort > 0 & est$mort.sd > 0
table(sel)

est[sel & (mort.sd/1e5)^2 > mort/1e5 * (1-mort/1e5), mort.sd := mort * .2]

out <- vlohi(est$mort[sel] / m, est$mort.sd[sel] / m)

est$mort.lo[sel] <- out[1, ] * m
est$mort.hi[sel] <- out[2, ] * m

sel <- est$mort.sd == 0 & !is.na(est$mort.sd)
table(sel)
est$mort.lo[sel] <- est$mort[sel]
est$mort.hi[sel] <- est$mort[sel]

sel <-
  (est$mort.lo > est$mort) |
  (est$mort.hi < est$mort) &
  (!is.na(est$mort) &
     !is.na(est$mort.lo) & !is.na(est$mort.hi))
table(sel)
# est[sel, .(iso3, year, inc, mort, mort.sd, mort.lo, mort.hi)]
# est[sel, mort.hi := mort + 1.96 * mort.sd]
est[mort.lo == 0, .(iso3, year, mort, mort.sd, mort.lo, mort.hi)]
est[mort > 0, test.bounds(mort, mort.lo, mort.hi)]

est[is.na(mort.lo), mort.lo := 0]
est[is.na(mort.hi), mort.hi := mort + 1.96 * mort.sd]



# HIV-neg

sel <- est$mort.nh > 0 & est$mort.nh.sd > 0 
table(sel)

selcheck <- est$year %in% 2020:yr  & est$iso3 %ni% unique(c(exclude, md.exclude))
est[selcheck & sel, .(iso3,year,mort.nh)]

est[sel & (mort.nh.sd/1e5)^2 > mort.nh/1e5 * (1-mort.nh/1e5)]
est[sel & (mort.nh.sd/1e5)^2 > mort.nh/1e5 * (1-mort.nh/1e5), mort.nh.sd := mort.nh * .2]

out <- vlohi(est$mort.nh[sel] / m, est$mort.nh.sd[sel] / m)

est$mort.nh.lo[sel] <- out[1, ] * m
est$mort.nh.hi[sel] <- out[2, ] * m

sel <- est$mort.nh.sd == 0 & !is.na(est$mort.nh.sd)
table(sel)
est$mort.nh.lo[sel] <- est$mort.nh[sel]
est$mort.nh.hi[sel] <- est$mort.nh[sel]

sel <-
  (est$mort.nh.lo > est$mort.nh) |
  (est$mort.nh.hi < est$mort.nh) &
  (!is.na(est$mort.nh) &
     !is.na(est$mort.nh.lo) & !is.na(est$mort.nh.hi))
table(sel)
# est[sel, .(iso3, year, inc, mort.nh, mort.nh.sd, mort.nh.lo, mort.nh.hi)]
# est[sel, mort.nh.hi := mort.nh + 1.96 * mort.nh.sd]
est[mort.nh.lo == 0, .(iso3, year, mort.nh, mort.nh.sd, mort.nh.lo, mort.nh.hi)]
est[mort.nh > 0, test.bounds(mort.nh, mort.nh.lo, mort.nh.hi)]

est[is.na(mort.nh.lo), mort.nh.lo := 0]
est[is.na(mort.nh.hi), mort.nh.hi := mort.nh + 1.96 * mort.nh.sd]



# HIV-pos
sel <- est$mort.h > 0 & est$mort.h.sd > 0 & (est$mort.h.sd/m)^2 >= est$mort.h/m * (1 - est$mort.h/m)
table(sel)
est[sel, .(iso3,mort.h, mort.h.sd)]
est[sel, mort.h.sd := mort.h * .2]

sel <- est$mort.h > 0 & est$mort.h.sd > 0
table(sel)


out <- vlohi(est$mort.h[sel] / m, est$mort.h.sd[sel] / m)

est$mort.h.lo[sel] <- out[1, ] * m
est$mort.h.hi[sel] <- out[2, ] * m

sel <- est$mort.h.sd == 0 & !is.na(est$mort.h.sd)
table(sel)
est$mort.h.lo[sel] <- est$mort.h[sel]
est$mort.h.hi[sel] <- est$mort.h[sel]

sel <-
  (est$mort.h.lo > est$mort.h) |
  (est$mort.h.hi < est$mort.h) &
  (!is.na(est$mort.h) &
     !is.na(est$mort.h.lo) & !is.na(est$mort.h.hi))
table(sel)
# est[sel, .(iso3, year, inc, mort.h, mort.h.sd, mort.h.lo, mort.h.hi)]
# est[sel, mort.h.hi := mort.h + 1.96 * mort.h.sd]
est[mort.h.lo == 0, .(iso3, year, mort.h, mort.h.sd, mort.h.lo, mort.h.hi)]
est[mort.h > 0, test.bounds(mort.h, mort.h.lo, mort.h.hi)]

est[is.na(mort.h.lo), mort.h.lo := 0]
est[is.na(mort.h.hi), mort.h.hi := mort.h + 1.96 * mort.h.sd]

sel <- est$iso3=='MSR' & est$year==yr
est[sel, .(iso3,year,mort,mort.sd,mort.nh,mort.nh.sd,mort.h,mort.h.sd,mort.h.lo,mort.h.hi)]
est[sel, mort.sd := 0]
est[sel, mort.hi := 0]
est[sel, mort.nh.sd := 0]
est[sel, mort.nh.hi := 0]
est[sel, mort.h.sd := 0]
est[sel, mort.h.hi := 0]












#
# Add absolut number for mortality and incidence
#


est <- within(est, {
  mort.num <- mort * pop / m
  mort.lo.num <- mort.lo * pop / m
  mort.hi.num <- mort.hi * pop / m

  mort.nh.num <- mort.nh * pop / m
  mort.nh.lo.num <- mort.nh.lo * pop / m
  mort.nh.hi.num <- mort.nh.hi * pop / m

  mort.h.num <- mort.h * pop / m
  mort.h.lo.num <- mort.h.lo * pop / m
  mort.h.hi.num <- mort.h.hi * pop / m

  inc.num <- inc * pop / m
  inc.lo.num <- inc.lo * pop / m
  inc.hi.num <- inc.hi * pop / m
  inc.nh.num <- inc.nh * pop / m
  inc.nh.lo.num <- inc.nh.lo * pop / m
  inc.nh.hi.num <- inc.nh.hi * pop / m
  inc.h.num <- inc.h * pop / m
  inc.h.lo.num <- inc.h.lo * pop / m
  inc.h.hi.num <- inc.h.hi * pop / m

})


# checks
#
est[, test.bounds(mort, mort.lo, mort.hi)]
est[, test.bounds(mort.nh, mort.nh.lo, mort.nh.hi)]
est[, test.bounds(mort.h, mort.h.lo, mort.h.hi)]

est[, .(
  sums(mort.nh.num),
  sums(mort.h.num),
  sums(mort.num),
  sums(inc.num),
  sums(inc.nh.num),
  sums(inc.h.num),
  sums(c.newinc),
  sums(mort.nh.num) / sums(inc.nh.num)
), by = year]
old[, .(
  sums(mort.nh.num),
  sums(mort.h.num),
  sums(mort.num),
  sums(inc.num),
  sums(inc.nh.num),
  sums(inc.h.num),
  sums(c.newinc),
  sums(mort.nh.num) / sums(inc.nh.num)
), by = year]



###
### CFR calculation
###


cfr <- est[,
           {
             tmp = divXY(mort, inc, mort.sd, inc.sd)
             
             list(cfr = tmp[[1]],
                  cfr.sd = tmp[[2]])
           }, by = c("iso3", "year")]

# Set CFR to a maximum of 1
cfr$cfr[cfr$cfr > 1 & !is.na(cfr$cfr)] <- 1

# Calculate lower and upper bounds using the beta distribution
# The final condition (cfr$cfr.sd^2 < (cfr$cfr*(1-cfr$cfr))) suggested
# by Mathieu to exclude countries where the beta distribution conditions don;t apply
# and therefore we approximate the bounds using the normal distribution
sel_beta <- cfr$cfr > 0 & cfr$cfr < 1 & !is.na(cfr$cfr) & (cfr$cfr.sd^2 < (cfr$cfr*(1-cfr$cfr)))
out <- with(cfr[sel_beta], vlohi(cfr, cfr.sd))
cfr$cfr.lo[sel_beta] <- out[1,]
cfr$cfr.hi[sel_beta] <- out[2,]

# Apply the normal distribution to countries where the beta distribution was not applicable
sel_norm <- !sel_beta & !is.na(cfr$cfr.sd)
cfr$cfr.lo[sel_norm] <- cfr$cfr[sel_norm]-1.96*cfr$cfr.sd[sel_norm]
cfr$cfr.hi[sel_norm] <- cfr$cfr[sel_norm]+1.96*cfr$cfr.sd[sel_norm]

# Set lower bound of CFR to a minimum of 0
cfr$cfr.lo[sel_norm & cfr$cfr.lo < 0 ] <- 0

# Set upper bound of CFR to a maximum of 1
cfr$cfr.hi[sel_norm & cfr$cfr.hi > 1 ] <- 1

# Set all remaining values to NA
cfr$cfr[!sel_beta & !sel_norm] <- NA
cfr$cfr.sd[!sel_beta & !sel_norm] <- NA
cfr$cfr.lo[!sel_beta & !sel_norm] <- cfr$cfr.hi[!sel_beta & !sel_norm] <- NA

dim(est)
est=merge(est,cfr,by=c("iso3","year"),all.x = T)
dim(est)



#######################
# PLOTS by WHO region #
#######################

wr <- unique(as.character(est$g.whoregion))

#Mortality total

for (i in wr) {
  p <-
    qplot(
      year,
      mort,
      data = subset(est, g.whoregion == i & year>=2010),
      geom = 'line',
      colour = I('blue')
    ) +
    geom_ribbon(
      aes(year, ymin = mort.lo, ymax = mort.hi),
      fill = I('blue'),
      alpha = I(0.4)
    ) +
    geom_line(
      aes(year, mort),
      data = subset(old, g.whoregion == i & year>=2010),
      colour = I('red'),
      linetype = I(2)
    ) +
    scale_x_continuous(breaks=c(2010,2015,2020)) +
    
    facet_wrap(~ iso3, scales = 'free_y') + xlab('') + ylab('Rate per 100,000/year')
  suppressWarnings(print(p))

  suppressWarnings(ggsave(here(
    paste('inc_mort/output/checks/mort_', i, '_compare.pdf', sep = '')
  ),
  width = 14,
  height = 8))
}


#Mortality HBCs
hbc.lst <- est[g.hbc == T, unique(iso3)]

p <- qplot(year,
           mort,
           data = subset(est, iso3 %in% hbc.lst  & year >= 2010),
           geom = 'line',
           colour = I('blue')) +
  geom_ribbon(
    aes(
      year,
      ymin = mort.lo,
      ymax = mort.hi
    ),
    fill = I('blue'),
    alpha = I(.4)
  ) +
  geom_line(
    aes(year, mort),
    data = subset(old, iso3 %in% hbc.lst & year >= 2010),
    colour = I('red'),
    linetype = I(2))+
  scale_x_continuous(breaks=c(2010,2015,2020)) +
  facet_wrap(~ iso3, scales = 'free_y') + xlab('') + ylab('Moratality rate per 100k/yr')

suppressWarnings(ggsave(
  here(paste('inc_mort/output/checks/mort_hbc.pdf', sep = '')),
  plot = p,
  width = 14,
  height = 8
))


#Mortality HIV positive


for (i in wr) {
  p <-
    qplot(
      year,
      mort.h,
      data = subset(est, g.whoregion == i & year>=2010),
      geom = 'line',
      colour = I('blue')
    ) +
    geom_ribbon(
      aes(year, ymin = mort.h.lo, ymax = mort.h.hi),
      fill = I('blue'),
      alpha = I(0.4)
    ) +
    geom_line(
      aes(year, mort.h),
      data = subset(old, g.whoregion == i & year>=2010),
      colour = I('red'),
      linetype = I(2)
    ) +
    scale_x_continuous(breaks=c(2010,2015,2020)) +
    
    facet_wrap(~ iso3, scales = 'free_y') + xlab('') + ylab('Rate per 100,000/year')
  suppressWarnings(print(p))

  suppressWarnings(ggsave(here(
    paste('inc_mort/output/checks/mort.h_', i, '_compare.pdf', sep = '')
  ),
  width = 14,
  height = 8))
}

#Mortality HIV negative


for (i in wr) {
  p <-
    qplot(
      year,
      mort.nh,
      data = subset(est, g.whoregion == i & year>=2010),
      geom = 'line',
      colour = I('blue')
    ) +
    geom_ribbon(
      aes(year, ymin = mort.nh.lo, ymax = mort.nh.hi),
      fill = I('blue'),
      alpha = I(0.4)
    ) +
    geom_line(
      aes(year, mort.nh),
      data = subset(old, g.whoregion == i & year>=2010),
      colour = I('red'),
      linetype = I(2)
    ) +
    scale_x_continuous(breaks=c(2010,2015,2020)) +
    
    facet_wrap(~ iso3, scales = 'free_y') + xlab('') + ylab('Rate per 100,000/year')
  suppressWarnings(print(p))

  suppressWarnings(ggsave(here(
    paste('inc_mort/output/checks/mort.nh_', i, '_compare.pdf', sep = '')
  ),
  width = 14,
  height = 8))
}


# for (i in wr) {
#   p <-
#     qplot(
#       year,
#       mort.nh,
#       data = subset(est, g.whoregion == i & year>=yr-5),
#       geom = 'line',
#       colour = I('blue')
#     ) +
#     geom_ribbon(
#       aes(year, ymin = mort.nh.lo, ymax = mort.nh.hi),
#       fill = I('blue'),
#       alpha = I(0.4)
#     ) +
#     geom_line(
#       aes(year, mort.nh),
#       data = subset(old, g.whoregion == i & year>=yr-5),
#       colour = I('red'),
#       linetype = I(2)
#     ) +
#     facet_wrap(~ iso3, scales = 'free_y') + xlab('') + ylab('Rate per 100,000/year')
#   suppressWarnings(print(p))
# 
#   suppressWarnings(ggsave(here(
#     paste('inc_mort/output/checks/mort.nh_', i, '_5y.pdf', sep = '')
#   ),
#   width = 14,
#   height = 8))
# }




est[, table(source.mort)]
est[, sum(is.na(source.mort)), by=year]
est[is.na(source.mort) & year==2022, source.mort := "Current trends"]

# Bug fix suggested by Philippe
# (when mort.nh is the same as mort.nh.hat then the method is indirect using CFR, notification and incidence)
est[mort.nh == e.mort.nh & source.mort != "Indirect", source.mort := "Indirect"]

est[, sum(is.na(source.mort))==0]
est[, sum(is.na(source.inc))==0]






# check global aggregates
#
(est[, sum(mort.num), by=year])
(est[iso3 %in% md.lst & iso3 %ni% md.exclude, sum(mort.num), by=year])
(est[, sum(mort.h.num), by=year])
(est[, sum(mort.nh.num), by=year])
(est[, sum(mort.num), by=year])


# late update from CHN and RUS
# est[iso3=='CHN' & year==yr, source.mort := 'VR']
# est[iso3=='RUS' & year==yr, source.mort := 'VR']


# update source.mort (Feb 2023)
est[source.mort=="Country model", source.mort := "Country-specific dynamic model"]
est[source.mort=="Regional model", source.mort := "Region-specific dynamic model"]
est[(iso3 %in% c('CHN','RUS') | (iso3 %in% vr.2020.2022.lst)) & year>2019, source.mort := "VR data for 2020-2022"]
est[source.mort=="VR", source.mort := "VR data and pre-2020 trend"]
est[source.mort=="VR/IHME, extrapolated", source.mort := "VR data and pre-2020 trend"]

#india
est[iso3=="IND" & year %in% 2010:2019, source.mort := "VR data"]
est[iso3=="IND" & year %in% 2020:2022, source.mort := "Country-specific dynamic model"]



# remove carriage returns in source.inc
est[, source.inc := gsub("\nS"," s",source.inc)]
est[, source.inc := gsub("\nE"," e",source.inc)]

# save
#

# est_temporary=est
# save(est_temporary, file = here('inc_mort/analysis/est_temporary.rda'))


attr(est, "timestamp") <- Sys.Date() #set date
save(est, file = here('inc_mort/analysis/est.rda'))
fwrite(est, file = here(paste0('inc_mort/analysis/csv/est_', Sys.Date(), '.csv')))



end_time <- Sys.time()

end_time - start_time