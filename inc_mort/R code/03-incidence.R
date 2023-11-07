#' ---
#' title: Estimate TB incidence
#' author: Mathieu Bastard, Philippe Glaziou
#' date: 2022-06-06
#' output:
#'    html_document:
#'      mode: selfcontained
#'      toc: true
#'      toc_depth: 3
#'      toc_float: true
#'      theme: flatly
#'      highlight: zenburn
#'      df_print: paged
#'      code_folding: hide
#' ---

#' (Last updated: `r Sys.Date()`)
#'
#' Estimate TB incidence (inc), disaggregate it by HIV (inc.h and inc.nh for HIV-pos and HIV-neg,
#' respectively).
#'
#' The incidence of TB can be estimated using a variety of methods, including:
#' - Surveillance data: Health authorities can track the number of new TB cases 
#'   that are reported through surveillance systems. This can help to estimate 
#'   the overall incidence of TB in a population.
#' - Mathematical modeling: Researchers can use mathematical models to estimate 
#'   the incidence of TB based on data on factors such as population size, 
#'   TB prevalence, and the effectiveness of TB prevention and treatment programs.
#' - Epidemiological (cohort) studies: Researchers can also conduct epidemiological 
#'   studies to estimate the incidence of TB in a specific population. These studies 
#'   can involve collecting data on TB cases and using statistical methods to estimate 
#'   the overall incidence of TB. No national representative cohort study of TB incidence
#'   as ever been conducted, all past studies were either geographically limited (e.g. selected 
#'   districts in India) or population-limited (e.g. civil servants in Rep of Korea).
#'   
#' This script generates TB and TB/HIV incidence estimates based on the above 2 approaches, 
#' specifically using:
#'
#' - the pre-covid 2000-2019 estimates taken from previous report, stored in 
#'    ~/data/old.rda (created by 00-init.R)
#' - UNAIDS HIV estimates, stored in ~/data/unaids (created by unaids.R)
#' - notifications, stored in ~/data/tb (created by 01-odbc.R)
#' - HIV prevalence in TB, stored in ~/data/tbhiv (created by 02-tbhiv.R)
#' - Nim's model outputs for 2020 onwards, stored in ~/data/model and ~/data/model2 
#'    (created by 03-model.R)
#' - Nim's extra model for India 2015-2021, stored in ~/Nim/IND_20220906.csv 
#'    based on extensive discussions with India's TB programme
#' - Summary of rules to use for each country, many based on expert opinion 
#'    from regional workshops of 2009-2011, stored in ~/input/inc/rules_inc.csv
#'    This file was created years ago as a repository of estimated low and high
#'    uncertainty bounds for ratios of incidence/notifications based on expert 
#'    opinion or other as documented very briefly in the comments variable. 
#'    Unfortunately, no systematic documentation of the rationale for the low and
#'    high uncertainty bounds have been compiled. Most originate from formal discussions
#'    in regional or country meetings, others from email or telephone communications 
#'    with NTP or partners, which were not compiled in a systematic manner. From 2000 onwards,
#'    the rules_inc.csv file is superseded by the output from Nim's models or by results of 
#'    forward projections of the 2000-2019 series where dynamic model outputs were judged 
#'    suboptimal.
#'    
#'  In the future, a decision should be carefully weighted about the continued reliance 
#'  of dynamic model results based on the previously published 2000-2019 series. A fundamental
#'  difficulty is that incidence is seldom approached though directly measured empirical data
#'  except in high-income countries with state-of-the-art TB surveillance. Even there, 
#'  weaknesses in case definitions for TB remain,, that is, a variable proportion of 
#'  reported cases lack any form of bacteriological confirmation and there is no 
#'  documentation in the global TB database (though annual national reporting) of 
#'  the evidence that supported the diagnostic of TB. Ideally, external clinical audits 
#'  of medical files made on a nationally representative sample of reported cases would 
#'  provide information on:
#'    - % of cases adequately investigated (CXR, repeat testing, testing for differential diagnoses, etc)
#'    - % of cases confirmed by auditing
#'    - % of cases considered doubtful by auditing
#'    - % of cases considered misclassified by auditing
#'  
#' key ancillary variables used to generate inc, inc.h, inc.nh:
#'
#' - *.hat variables: projections of 2017-2019 trends into 2020 onwards using logistic regression
#' - imp.newinc: imputed notifications when reported numbers are missing
#' - inc.md.*: incidence from Nim's country dynamic model
#' - inc.rmd.*: incidence from Nim's regional model
#'
#'
#'
#' # Deps:
#' - output from all previous scripts in the sequence, including 02-tbhiv.R and 03-model.R
#' - helper functions in fun.R
#' - libraries data.table", imputeTS (imputation of time series data), propagate (propagation of 
#'     errors through calculus using the delta-method or based on a second order expansion. 
#'     The propagate library can be a little difficult to install on MacOS due to dependences 
#'     on specific FORTRAN libraries (an install using brew may help). 
#'     Installation of propagate on windows OS is straightforward. 
#'
#'
#' Input: 
#'  - output from the 3 previous scripts in the sequence
#'  - model.rda and extra.rda (Nim's models output)
#'  - ~/input/inc/rules_inc.csv set of low/high uncertainty bounds (with specified time period and
#'      briefly documented source -- see comment above)
#'  
#'
#' # Output:
#' - est:  dated csv file (est_04inc_TIMESTAMP.csv) and rda file
#' - numerous graphic checks in ~/output/checks
#'
#'



rm(list=ls())

# Load libraries and data
#
suppressMessages(library(data.table))
suppressMessages(library(imputeTS))
suppressMessages(library(zoo))
suppressMessages(library(propagate))
suppressMessages(library(here))
suppressMessages(library(readxl))

#Run Functions script
source(here('inc_mort/R code/fun.R'))

#Function to import db from GTB databases
source(here("import/load_gtb.R"))

#load data from gtb

tb <- load_gtb("tb",convert_dots = FALSE)
cty <- load_gtb("cty",convert_dots = FALSE)
pop <- load_gtb("pop",convert_dots = FALSE)
grpmbr <- load_gtb("grpmbr",convert_dots = FALSE)

#Nim results: Update to the most recent one 

model=fread("dynamic/output/230803_Modelled_Countries.csv",
            col.names=c("iso3","scenario","hiv","measure","year","best","lo","hi"))
setkey(model, iso3)

extra=fread("dynamic/output/230802_Extrapolated_Countries.csv",
            col.names=c("iso3","scenario","hiv","measure","year","best","lo","hi"))
setkey(extra, iso3)




#India estimates received on 22 June 2023
# Sent by Sandip Mandal

#india=read_excel("inc_mort/input_inc/India_Incidence 2011_2022.xlsx")
india=fread("inc_mort/input_inc/230802_India_outputs.csv")


#load data generated by previous scripts
load(here('inc_mort/analysis/tbhiv.rda'))
load(here('inc_mort/estimates2022/old.rda'))

#load UNDAIS data
load(here('inc_mort/analysis/unaids.rda'))


vlohi <- Vectorize(lohi, c('ev', 'sd'))

#Year of estimate
yr <- 2022

#Unit of rate (/100000pop)
m <- 1e5


# incorporate inc rules
rules <- fread('inc_mort/input_inc/rules_inc.csv')
setkey(rules, iso3)

# incorporate IND update 2015-
#ind <- fread('Nim/IND_20220906.csv')
#setkey(ind, iso3, year)


#' # Current trends
#'
#' Create projections of 2017-2019 estimates to subsequent years using logistic regression
#' and store in est$*.hat variables
#'


# Trends in inc and newinc before 2020
# as published in Global TB Report 2020,
# based on 2017-2019 series (excluding high income)
#
project <- function(y,
                    time = 2017:2019,
                    target = yr) {
  stopifnot(sum(!is.na(y)) >= 2)
  stopifnot(y[!is.na(y)] >= 0 & y[!is.na(y)] <= 1)

  period <- time[1]:target
  yhat <-
    predict(
      glm(y ~ time, family = quasibinomial),
      newdata = data.frame(time =
                             period),
      type = "response"
    )

  if (any(y==0 & !is.na(y))) yhat <- mean(y, na.rm=TRUE)

  return(data.frame(time = period,
                    y.hat = yhat))
}

trends <-
  old[year %in% 2017:2019, .(iso3, year, inc, newinc, mort, mort.nh, mort.h)]

trends <-
  rbind(trends, trends[year == 2019, .(
    iso3,
    year = 2020,
    inc = NA,
    newinc = NA,
    mort = NA,
    mort.nh = NA,
    mort.h = NA
  )], trends[year == 2019, .(
    iso3,
    year = 2021,
    inc = NA,
    newinc = NA,
    mort = NA,
    mort.nh = NA,
    mort.h = NA
  )], trends[year == 2019, .(
    iso3,
    year = 2022,
    inc = NA,
    newinc = NA,
    mort = NA,
    mort.nh = NA,
    mort.h = NA
  )]
  )

setkey(trends, iso3, year)

trends[, inc.hat := project(inc[1:3] / 1e5)$y.hat * 1e5, by = iso3]
trends[, mort.hat := project(mort[1:3] / 1e5)$y.hat * 1e5, by = iso3]
trends[, mort.nh.hat := project(mort.nh[1:3] / 1e5)$y.hat * 1e5, by = iso3]
trends[, mort.h.hat := project(mort.h[1:3] / 1e5)$y.hat * 1e5, by = iso3]

trends[, nnewinc := sum(!is.na(newinc)), by = iso3]
trends[nnewinc >= 2, newinc.hat := project(newinc[1:3] / 1e5)$y.hat * 1e5, by = iso3]
trends[nnewinc < 2, newinc.hat := mean(newinc, na.rm = TRUE, by = iso3)]





#' # Create the new incidence estimates dataset
#'
#' Create the 'est' dataset to store this year's estimates, using last year's ones in 'old'
#' as the starting point
#'


(dim(tbhiv))
(dim(old))
est <-
  merge(
    tbhiv,
    old[, list(iso3, year, inc, inc.sd, source.inc)],
    by = c('iso3', 'year'),
    all.x = TRUE,
    all.y = FALSE
  )
(dim(est))
est <-
  merge(
    est,
    pop[, .(iso3, year, pop = e.pop.num)],
    by = c('iso3', 'year'),
    all.x = TRUE,
    all.y = FALSE
  )
(dim(est))

# check missing values
sum(is.na(est$inc) &
      est$year < yr) == 0   # TRUE: only year==yr inc values are missing

(sum(is.na(est$newinc)))
(sum(is.na(est$newinc) & est$year == yr))
(sum(is.na(est$c.newinc) & est$year == yr))
est[!is.na(c.newinc) & year==yr, newinc := c.newinc * 1e5 / pop]
(sum(is.na(est$newinc) & est$year == yr))




#'
#' Add the 'trends' data calculated earlier to the 'est' dataset
#'

est <-
  merge(est, trends[year >= 2020, .(iso3,
                                    year,
                                    inc.hat,
                                    newinc.hat,
                                    mort.hat,
                                    mort.nh.hat,
                                    mort.h.hat)], by = c('iso3', 'year'), all.x = TRUE)
(dim(est))



# add country groupings
#
income <- grpmbr[group.type == 'g_income']
setnames(income, 'group.name', 'g.income')
est <-
  merge(est, income[, .(iso3, g.income)], by = 'iso3', all.x = TRUE)
est$g.income <- as.character(est$g.income)

gbd <- grpmbr[group.type == 'g_gbd']
setnames(gbd, 'group.name', 'g.gbd')
est <- merge(est, gbd[, .(iso3, g.gbd)], by = 'iso3', all.x = TRUE)

gest <- grpmbr[group.type == 'g_est']
eeur <- gest[group.name == 'EEUR', iso3]
est[, g.mdr := g.whoregion]
est[iso3 %in% eeur, g.mdr := 'EEU']

hbc <- as.character(grpmbr[group.type == 'g_hb_tb']$iso3)
est$g.hbc <- est$iso3 %in% hbc
hbmdr <- as.character(grpmbr[group.type == 'g_hb_mdr']$iso3)
est$g.hbmdr <- est$iso3 %in% hbmdr
hbtbhiv <- as.character(grpmbr[group.type == 'g_hb_tbhiv']$iso3)
est$g.hbtbhiv <- est$iso3 %in% hbtbhiv




#' # Tidy up notifications
#'
#' Remove outliers and impute missing values
#'



# check for outliers in newinc
est[, imp.newinc := newinc] # will hold imputed values

# check for outliers
#
(est[pop > 1e5, .(outlier = sum(imp.newinc > 3 * mean(imp.newinc, na.rm =
                                                        T)) > 0), by = iso3][outlier == T])
(est['STP', .(iso3, year, imp.newinc)])
sel <- est$iso3 == "STP" & est$year == 2003
est$imp.newinc[sel] <- NA # reset outlier to missing
(est['MDA', .(iso3, year, imp.newinc)])
sel <- est$iso3 == "MDA" & est$year == 2003
est$imp.newinc[sel] <- NA # reset outlier to missing
# sel <- est$iso3 == "KGZ" & est$year == 2003
# est$imp.newinc[sel] <- NA # reset outlier to missing



# list outliers in the notification series in countries with pop>1e5,
#
(est[pop > 1e5, .(outlier = sum(imp.newinc == 0)), by = iso3][outlier ==T])

# KHM (Aug - NTP mentioned that 2014 peak due to 5000 false pos in children)
#
(est['KHM', .(iso3, year, imp.newinc)])
sel <- est$iso3 == 'KHM' & est$year == 2014
est$imp.newinc[sel] <- NA # reset outlier to missing


# Interpolation of missing notifications
# using Kalman smoothing on structural TS, where possible
#
B1 <- copy(est)  # backup point

interp <- c('SMR', 'MSR', 'VGB')

est[iso3 %in% interp, imp.newinc := na_interpolation(imp.newinc), by = iso3]
est[iso3 %ni% interp, imp.newinc := na_kalman(imp.newinc, type = 'trend'), by = iso3]

est[, test.ispos(imp.newinc)]



# check imputations
#
wr <- c('AMR', 'AFR', 'EMR', 'EUR', 'SEA', 'WPR')

for (i in wr) {
  p <-
    qplot(year, newinc, data = est[g.whoregion == i], geom = 'point') +
    geom_line(aes(year, imp.newinc), colour = I('red')) +
    facet_wrap( ~ iso3, scales = 'free_y')
  suppressWarnings(print(p))
  suppressWarnings(ggsave(here(
    paste('inc_mort/output/checks/imputations', i, '_newinc.pdf', sep = '')
  ),
  width = 14,
  height = 8))
}



#'
#' # Incorporate Nim's country-level dynamic model for incidence
#'

# exclude some countries from Nim's models:
# - Georgia (comms 2022)
# - Ethiopia, DPRK, South Africa because shortfalls in line with pre-2020 general decline
#   not necessarily due to covid-related drops in detection & treatment

# 2023: exclude CHN

md.exclude <- c('GEO', 'ETH', 'PRK', 'ZAF','CHN','RUS')

B2 <- copy(est) # another backup point

md.lst <- unique(model$iso3)

dim(est)
est <-
  merge(est, model[scenario == 'COVID' &
                     hiv == 'a' &
                     measure == 'inc' &
                     iso3 %ni% md.exclude &
                     year %in% 2020:yr, .(
                       iso3,
                       year,
                       inc.md = best,
                       inc.md.lo = lo,
                       inc.md.hi = hi
                     )],
        by = c('iso3', 'year'), all.x = T)
dim(est)



sel <- est$year %in% 2020:2022 & est$iso3 %in% md.lst & est$iso3 %ni% md.exclude
table(sel)
est[sel, inc := inc.md]
est[sel, inc.sd := (inc.md.hi - inc.md.lo ) / 3.92]
est[sel, inc.lo := inc.md.lo]
est[sel, inc.hi := inc.md.hi]

est[sel, source.inc := "Country model"]
est[year==yr, table(source.inc)]
est[year==yr-1, table(source.inc)]
est[year==yr-1 & iso3 %in% c('UGA','UKR'), source.inc := NA]
est[year==yr-1, table(source.inc)]



#'
#' # Incorporate Nim's regional-level extrapolated model for incidence
#'


B3 <- copy(est)

extra.lst <- unique(extra$iso3)

dim(est)
est <-
  merge(est, extra[scenario == 'COVID' &
                     hiv == 'a' &
                     measure == 'inc' &
                     iso3 %ni% md.exclude &
                     year %in% 2020:yr, .(
                       iso3,
                       year,
                       inc.rmd = best,
                       inc.rmd.lo = lo,
                       inc.rmd.hi = hi
                     )],
        by = c('iso3', 'year'), all.x = T)
dim(est)


sel <- est$year %in% 2020:2022 & est$iso3 %in% extra.lst & est$iso3 %ni% md.exclude
table(sel)

est[sel, inc := inc.rmd]
est[sel, inc.sd := (inc.rmd.hi - inc.rmd.lo ) / 3.92]
est[sel, inc.lo := inc.rmd.lo]
est[sel, inc.hi := inc.rmd.hi]
est[sel, source.inc := "Regional model"]

est[year==yr, table(source.inc)]
est[year==yr-1, table(source.inc)]

est[, sum(is.na(inc)), by=year]





#'
#' # Derive Incidence from notifications, standard adjustment
#'
#'


std <-
  function(x,
           ISO3,
           f.lo = 1,
           f.hi,
           ystart = yr,
           h = 1,
           source = 'Standard adjustment',
           smooth = FALSE) {
    #' $I = f N$
    #'
    #' @param x datatable
    #' @param country iso3
    #' @param f low bound
    #' @param f.hi high bound
    #' @param h 1 - over diagnosis
    #' @param smooth MA with exponential weighting
    #' @export
    f <- mean(c(f.hi, f.lo))
    sel <- x$iso3 == ISO3 & x$year >= ystart

    if (smooth == FALSE) {
      x[sel, inc := imp.newinc * f * h]
    } else {
      x[sel, inc := na_ma(imp.newinc, k = 4, weighting = 'exponential') * f * h]
    }
    x[sel, inc.sd := inc * (f.hi - 1) / 3.92]
    x[sel, source.inc := source]
  }

hic <- est[g.income == 'HIC', unique(iso3)]

# high-income countries, PICs, misc
# 2023: LCA added, as per method before COVID
# 2023: DJI added, as per method before COVID
# 2023: SLV added, as per method before COVID

hic <- c(hic, 'ANT', 'WLF', 'WSM','FSM','PLW','SLB','TON','BTN','PSE','LBN',"LCA","DJI","SLV")
nhic <- setdiff(est[, unique(iso3)], hic)   # not high-income


for (i in hic) {
  # inefficient, but runs fast enough
  est <-
    std(
      est,
      ISO3 = i,
      f.lo = rules[i, lo],
      f.hi = rules[i, hi],
      ystart = rules[i, ystart],
      h = 1,
      source = 'Case notifications, Standard adjustment',
      smooth = ifelse(rules[i, hi] >= 1.5 &
                        !is.na(rules[i, hi]), TRUE, FALSE)
    )
}

sel <- est$iso3 %in% c('DEU','GBR','NLD')
est[sel, source.inc := 'Inventory study']

est[!is.na(inc), test.ispos(inc)]
est[is.na(inc) & year == yr, .(iso3, year, inc, newinc, inc.hat)]
est[year<yr, table(source.inc)]
est[year==yr, table(source.inc)]






#'
#' # Use projected 2017-2019 trends (inc.hat) for the remaining countries
#'
#'


B4 <- copy(est)

sel <- is.na(est$inc) & !is.na(est$inc.hat)
table(sel)
hat.lst <- unique(est[sel, iso3])



#Exlcude India from this list to keep 2020-2021 old estimates
hat.lst=hat.lst[hat.lst!="IND"]
hat.lst=hat.lst[hat.lst!="IND"]
hat.lst=hat.lst[hat.lst!="IND"]


est[iso3 %in% hat.lst & year %in% 2020:yr, inc := inc.hat]
est[iso3 %in% hat.lst, inc.sd := imputeTS::na_locf(inc.sd), by = iso3]
est[iso3 %in% hat.lst & year %in% 2020:yr, source.inc := "Current trends"]

est[year==yr, table(source.inc)]
est[, sum(is.na(inc)), by=year]
est[, sum(is.na(inc.sd)), by=year]
est[, sum(is.na(source.inc)), by=year]





#'
#' # Carry over 2019 values where an increase looks doubtful (use locf)
#'

#2023: add TLS

B5 <- copy(est)

lst <-
  c('FJI', 'MHL', 'TUV', 'BGD', 'BTN', 'LBY', 'SUR', 'COK', 'DMA','TLS')
(est['FJI', .(iso3, year, inc, inc.sd, source.inc)])
est[iso3 %in% lst & year %in% 2020:yr, inc := NA]
est[iso3 %in% lst & year %in% 2020:yr, inc.sd := NA]
est[iso3 %in% lst & year %in% 2020:yr, source.inc := NA]
est[iso3 %in% lst, inc := imputeTS::na_locf(inc), by = iso3]
est[iso3 %in% lst, inc.sd := imputeTS::na_locf(inc.sd), by = iso3]
est[iso3 %in% lst, source.inc := zoo::na.locf(source.inc, type='locf'), by = iso3]

(est['FJI', .(iso3, year, inc, inc.sd)])




#'
#' # Carry over 2020 case detection rate for CHN  (hard-coded)
#'
#'
#'In 2022, an inventory survey about the report quality of TB was implemented, 
#'including 142 hospitals., 
#'the rate of over diagnosis was 4.3%, 
#'the rate of underreporting was 29.3%, 
#'and the rate of under diagnosis was 9.3%.

sel <- est$iso3=='CHN' & est$year==2020
est$inc[sel] <- est$newinc[sel]/0.75
est$inc.sd[sel] <- est$inc.sd[est$iso3=="CHN"& est$year==2019]
est$inc.lo[sel] <- est$inc.hi[sel] <- NA

sel <- est$iso3=='CHN' & est$year==2021
est$inc[sel] <- est$newinc[sel]/0.75
est$inc.sd[sel] <- est$inc.sd[est$iso3=="CHN"& est$year==2019]
est$inc.lo[sel] <- est$inc.hi[sel] <- NA

#Change in 2020 in Std adjustment

sel <- est$iso3=='CHN' & est$year==2022
est$inc[sel] <- est$newinc[sel]*(1-0.043)/((1-0.293)*(1-0.093))
est$inc.sd[sel] <- est$inc.sd[est$iso3=="CHN"& est$year==2019]
est$inc.lo[sel] <- est$inc.hi[sel] <- NA



#RUSSIA
#case detection rate 17% in 2020 and 2021, but back to pre-pandemic in 2022
#Fix SD for incidence, should not be 4.3 for RUS, use LOCF for now but 9.95 = SD of 2019

sel <- est$iso3=='RUS' & est$year==2020
est$inc[sel] <- est$newinc[sel]/0.83
est$inc.sd[sel] <- est$inc.sd[est$iso3=="RUS"& est$year==2019]
est$inc.lo[sel] <- est$inc.hi[sel] <- NA

sel <- est$iso3=='RUS' & est$year==2021
est$inc[sel] <- est$newinc[sel]/0.83
est$inc.sd[sel] <- est$inc.sd[est$iso3=="RUS"& est$year==2019]
est$inc.lo[sel] <- est$inc.hi[sel] <- NA


selus=est$iso3=='RUS'& est$year %in% 2010:2019
precov.cdr.RUS=mean(est$newinc[selus]/est$inc[selus])

sel <- est$iso3=='RUS' & est$year==2022
est$inc[sel] <- est$newinc[sel]/precov.cdr.RUS
est$inc.sd[sel] <- est$inc.sd[est$iso3=="RUS"& est$year==2019]
est$inc.lo[sel] <- est$inc.hi[sel] <- NA

est[year>2019 & iso3 %in% c('CHN','RUS'), source.inc := NA]
est[iso3 %in% c('CHN','RUS'), source.inc := zoo::na.locf(source.inc, type='locf'), by = iso3]






#
# Ethiopia : incidence increase same as notification increase 2021-2022
#

est["ETH", .(iso3, year, inc, inc.sd, c.newinc, newinc,imp.newinc)]

#rate of increase in 2022/2021
sel22=est$iso3=='ETH' & est$year==2022
sel21=est$iso3=='ETH' & est$year==2021

rate.eth=est$newinc[sel22]/est$newinc[sel21]
est$inc[sel22]=est$inc[sel21]*rate.eth
est$inc.sd[sel22]=est$inc.sd[sel21]*rate.eth






#'
#' # Incorporate the new estimates for India 2015-2019 developed by Nim with India TB programme
#'
#' (Also re-scale the previously-published 2000-2014 estimates to fit the new 2015-onwards estimates)


# #For now use incidence in India LOCf of 2021 for 2022
# 
# est$inc[est$iso3=="IND" & est$year==2022]= est$inc[est$iso3=="IND" & est$year==2021]
# est$inc.sd[est$iso3=="IND" & est$year==2022]= est$inc.sd[est$iso3=="IND" & est$year==2021]


B6=copy(est)

# colnames(india)[1]="inc.india"
# india=melt(setDT(india), id.vars = c("inc.india"), variable.name = "year")
# 
# 
# for (i in 2011:yr) {
#   est[iso3=="IND" & year==i, inc:=india$value[india$inc.india=="mid" & india$year==i]]
#   est[iso3=="IND" & year==i, inc.lo:=india$value[india$inc.india=="lo" & india$year==i]]
#   est[iso3=="IND" & year==i, inc.hi:=india$value[india$inc.india=="hi" & india$year==i]]
#   est[iso3=="IND" & year==i, inc.sd:=(inc.hi-inc.lo)/3.92]
# }
india.old=subset(old, iso3=="IND", select=c("iso3","year","inc","inc.sd","inc.lo","inc.hi"))


india=subset(india,scenario=="COVID" & measure=="inc", select=c("iso3","year","best","lo","hi"))

for (i in 2011:yr) {
  est[iso3=="IND" & year==i, inc:=india$best[india$year==i]]
  est[iso3=="IND" & year==i, inc.lo:=india$lo[india$year==i]]
  est[iso3=="IND" & year==i, inc.hi:=india$hi[india$year==i]]
  est[iso3=="IND" & year==i, inc.sd:=(inc.hi-inc.lo)/3.92]
}

sel <- est$year %in% 2011:2022 & est$iso3 =="IND"
est[sel, source.inc := "Country model"]


#rescale old values 2000:2010
rescale <- est$inc[est$iso3=="IND" & est$year==2011]/india.old$inc[india.old$iso3=="IND" & india.old$year==2011]

sel <- est$iso3=='IND' & est$year %in% 2000:2010
est[sel, inc := inc * rescale]
est[sel, inc.sd := inc.sd * rescale]

est["IND",.(year,inc,inc.lo,inc.hi,inc.sd)]

#update md.lst : include IND
md.lst=c(md.lst,"IND")

#plot india
p=qplot(year,
           inc,
           data = subset(est, iso3=="IND"  & year >= 2010),
           geom = 'line',
           colour = I('blue')) +
  geom_ribbon(
    aes(
      year,
      ymin = inc - 1.96 * inc.sd,
      ymax = inc + 1.96 * inc.sd
    ),
    fill = I('blue'),
    alpha = I(.4)
  ) +
  scale_x_continuous(breaks=c(2010,2015,2020)) +
  geom_line(aes(year, newinc)) +
  facet_wrap(~ iso3, scales = 'free_y')+  xlab('') + ylab('Incidence rate per 100k/yr')
suppressWarnings(ggsave(
  here(paste('inc_mort/output/checks/inc_india.pdf', sep = '')),
  plot = p,
  width = 14,
  height = 8
))


# 
# ind[, inc.sd := (inc.hi-inc.lo)/3.92]
# 
# sel <- est$iso3=='IND' & est$year %in% 2015:2019
# est[sel, inc := ind[year %in% 2015:2019, inc]]
# (est[sel, inc])
# rescale <- ind[year %in% 2015:2019, inc] / old['IND'][year %in% 2015:2019, inc]
# (est[sel, inc.sd])
# est[sel, inc.sd := inc.sd * rescale]
# 
# sel <- est$iso3=='IND' & est$year %in% 2000:2014
# est[sel, inc := inc * rescale[1]]
# est[sel, inc.sd := inc.sd * rescale[1]]



# plot incidence from modeled coutries (country specific model)

#update md.lst : exclude BGD and TLS and RUS
md.lst=md.lst=md.lst[md.lst %in% c("BGD", "TLS","RUS") == FALSE] 
md.lst.check=unique(est$iso3[est$source.inc=="Country model"  & est$year==2022])
setdiff(md.lst,md.lst.check)

p <- qplot(year,
           inc,
           data = subset(est, iso3 %in% md.lst  & year >= 2010),
           geom = 'line',
           colour = I('blue')) +
  geom_ribbon(
    aes(
      year,
      ymin = inc - 1.96 * inc.sd,
      ymax = inc + 1.96 * inc.sd
    ),
    fill = I('blue'),
    alpha = I(.4)
  ) +
  scale_x_continuous(breaks=c(2010,2015,2020)) +
  geom_line(aes(year, newinc)) +
  facet_wrap(~ iso3, scales = 'free_y') + xlab('') + ylab('Incidence rate per 100k/yr')

suppressWarnings(ggsave(
  here(paste('inc_mort/output/checks/inc_model.pdf', sep = '')),
  plot = p,
  width = 14,
  height = 8
))


#update extra.lst
extra.lst=unique(est$iso3[est$source.inc=="Regional model"  & est$year==2022]) 
extra.lst=extra.lst[!is.na(extra.lst)]

p <- qplot(year,
           inc,
           data = subset(est, iso3 %in% extra.lst  & year >= 2010),
           geom = 'line',
           colour = I('blue')) +
  geom_ribbon(
    aes(
      year,
      ymin = inc - 1.96 * inc.sd,
      ymax = inc + 1.96 * inc.sd
    ),
    fill = I('blue'),
    alpha = I(.4)
  ) +
  geom_line(
    aes(year, inc),
    data = subset(old, iso3 %in% extra.lst & year >= 2010),
    colour = I('red'),
    linetype = I(2)
  ) +
  scale_x_continuous(breaks=c(2010,2015,2020)) +
  geom_line(aes(year, newinc)) +
  facet_wrap(~ iso3, scales = 'free_y') + xlab('') + ylab('Incidence rate per 100k/yr')

suppressWarnings(ggsave(
  here(paste('inc_mort/output/checks/inc_regional_model_compare.pdf', sep = '')),
  plot = p,
  width = 14,
  height = 8
))




# # Comparison plots with last year's report, focus on recent trends
# #
# for (i in wr) {
#   p <- qplot(
#     year,
#     inc,
#     data = subset(est, g.whoregion == i & year >= 2010),
#     geom = 'line',
#     colour = I('grey90')
#   ) +
#     geom_ribbon(
#       aes(
#         year,
#         ymin = inc - 1.96 * inc.sd,
#         ymax = inc + 1.96 * inc.sd
#       ),
#       fill = I('blue'),
#       alpha = I(.4)
#     ) +
#     geom_line(
#       aes(year, inc),
#       data = subset(old, g.whoregion == i & year >= 2010),
#       colour = I('red'),
#       linetype = I(2)
#     ) +
#     scale_x_continuous(breaks=c(2010,2015,2020)) +
#     geom_line(aes(year, newinc)) +
#     facet_wrap(~ iso3, scales = 'free_y') + xlab('') + ylab('Incidence rate per 100k/yr')
# 
#   suppressWarnings(ggsave(
#     here(paste(
#       'inc_mort/output/checks/incidence_', i, '_compare.pdf', sep = ''
#     )),
#     plot = p,
#     width = 14,
#     height = 8
#   ))
# }





#'
#' # Derive Incidence by HIV
#'


# Incorporate UNAIDS estimates
B6 <- copy(est)

(dim(est))
est <-
  merge(
    est,
    unaids[, .(
      iso3,
      year,
      hiv.num,
      hiv.lo.num,
      hiv.hi.num,
      mort.hiv.num,
      mort.hiv.lo.num,
      mort.hiv.hi.num
    )],
    by = c('iso3', 'year'),
    all.x = T,
    all.y = F
  )
(dim(est))

est[, hiv := hiv.num / pop]
est[, hiv.lo := hiv.lo.num / pop]
est[, hiv.hi := hiv.hi.num / pop]
est[, hiv.sd := (hiv.hi - hiv.lo) / 3.92]

est[, mort.hiv := mort.hiv.num / pop * m]
est[, mort.hiv.lo := mort.hiv.lo.num / pop * m]
est[, mort.hiv.hi := mort.hiv.hi.num / pop * m]
est[, mort.hiv.sd := (mort.hiv.hi - mort.hiv.lo) / 3.92]


### Impute UNAIDS estimate for 2022 since not yet available for 1st round
# 
# sum(is.na(est$hiv[est$year==2022]))
# 
# 
# View(est[,c("iso3","year","hiv")])
# 
# 
# est[,hiv:=nafill(hiv,type="locf"), by=iso3]
# est[,hiv.lo:=nafill(hiv.lo,type="locf"), by=iso3]
# est[,hiv.hi:=nafill(hiv.hi,type="locf"), by=iso3]
# est[,hiv.sd:=nafill(hiv.sd,type="locf"), by=iso3]
# 
# est[,mort.hiv:=nafill(mort.hiv,type="locf"), by=iso3]
# est[,mort.hiv.lo:=nafill(mort.hiv.lo,type="locf"), by=iso3]
# est[,mort.hiv.hi:=nafill(mort.hiv.hi,type="locf"), by=iso3]
# est[,mort.hiv.sd:=nafill(mort.hiv.sd,type="locf"), by=iso3]
# 
# View(est[,c("iso3","year","hiv","hiv.lo","hiv.hi","mort.hiv")])




#'
#' # Reuse old tbhiv and inc.h estimates up to 2019
#'
#' (Stored as otbhiv.* )
#'



est <- merge(est,
             old[,.(iso3,year,inc.h,inc.h.sd,inc.nh, inc.nh.sd, otbhiv=tbhiv,otbhiv.sd=tbhiv.sd)],
             by=c('iso3','year'),all.x=T)
dim(est)

est[year<=2019, tbhiv := otbhiv]
est[year<=2019, tbhiv.sd := otbhiv.sd]

est[, sum(is.na(inc.h)), by=year]
est[, sum(is.na(tbhiv)), by=year]


# PNG 2008
#
sel <- est$iso3 == 'PNG' & est$year == 2008
tbhiv.png <- c(mean(c(0.11529, 0.11989)), 0.047)
out <- lohi(tbhiv.png[1], tbhiv.png[2])
est$tbhiv[sel] <- tbhiv.png[1]
est$tbhiv.sd[sel] <- tbhiv.png[2]
est$tbhiv.lo[sel] <- out[1]
est$tbhiv.lo[sel] <- out[2]


# GABON 2012 and 2013 for tbihv and inc.h
# tbihv outlier, set to NA and then impute it
est[iso3=="GAB" & year %in% 2012:2013, tbhiv := NA]
est[iso3=="GAB", tbhiv := zoo::na.approx(tbhiv, na.rm = FALSE)]
est[iso3=="GAB" & year %in% 2012:2013, tbhiv.sd := NA]
est[iso3=="GAB", tbhiv.sd:= zoo::na.locf(tbhiv.sd, na.rm = FALSE)]


# ANGOLA SD in 2011, prob an error, looks too narrow
# set to 2010 value
est[iso3=="AGO" & year %in% 2011, tbhiv.sd := NA]
est[iso3=="AGO", tbhiv.sd:= zoo::na.locf(tbhiv.sd, na.rm = FALSE)]


#'
#' # Incorporate Nim's country-level dynamic model for inc.h then derive tbhiv (2020 onwards)
#'


dim(est)
est <-
  merge(est, model[scenario == 'COVID' &
                     hiv == 'pos' &
                     measure == 'inc' &
                     iso3 %ni% md.exclude &
                     year %in% 2020:yr, .(
                       iso3,
                       year,
                       inc.h.md = best,
                       inc.h.md.lo = lo,
                       inc.h.md.hi = hi
                     )],
        by = c('iso3', 'year'), all.x = T)
dim(est)

mdh.lst <- unique(model[hiv=='pos', iso3])

sel <- est$year %in% 2020:2022 & est$iso3 %in% mdh.lst & est$iso3 %ni% md.exclude
table(sel)
est[sel, inc.h := inc.h.md]
est[sel, inc.h.sd := (inc.h.md.hi - inc.h.md.lo ) / 3.92]
est[sel, inc.h.lo := inc.h.md.lo]
est[sel, inc.h.hi := inc.h.md.hi]

out <- with(est[sel], divXY(inc.h, inc, inc.h.sd, inc.sd))

est[sel, tbhiv := out[[1]]]
est[sel, tbhiv.sd := out[[2]]]







#'
#' # Incorporate Nim's regional-level extrapolated model for inc.h then derive tbhiv (2020 onwards)
#'


dim(est)
est <-
  merge(est, extra[scenario == 'COVID' &
                     hiv == 'pos' &
                     measure == 'inc' &
                     iso3 %ni% md.exclude &
                     year %in% 2020:yr, .(
                       iso3,
                       year,
                       inc.h.rmd = best,
                       inc.h.rmd.lo = lo,
                       inc.h.rmd.hi = hi
                     )],
        by = c('iso3', 'year'), all.x = T)
dim(est)

rmdh.lst <- unique(extra[hiv=='pos', iso3])

sel <- est$year %in% 2020:2022 & est$iso3 %in% rmdh.lst & est$iso3 %ni% md.exclude
table(sel)
est[sel, inc.h := inc.h.rmd]
est[sel, inc.h.sd := (inc.h.rmd.hi - inc.h.rmd.lo ) / 3.92]
est[sel, inc.h.lo := inc.h.rmd.lo]
est[sel, inc.h.hi := inc.h.rmd.hi]

out <- with(est[sel], divXY(inc.h, inc, inc.h.sd, inc.sd))

est[sel, tbhiv := out[[1]]]
est[sel, tbhiv.sd := out[[2]]]






#'
#' # Use locf to derive missing tbhiv in remaining countries and then derive inc.h
#'


B7 <- est

# Handle exclusion list
model_list <- c(mdh.lst,rmdh.lst)
model_list <- model_list[model_list %ni% md.exclude]


est[year>=2019 & iso3 %ni% model_list, sum(is.na(tbhiv)), by=year]
lst <- est[year == 2019 & iso3 %ni% model_list & !is.na(tbhiv), iso3]
length(lst)

est[year==2020 & iso3=='ZAF', tbhiv := NA]
est[year==2020 & iso3=='ZAF', tbhiv.sd := NA]
est[year>=2019 & iso3=='ZAF', tbhiv := imputeTS::na_interpolation(tbhiv)]
est[year>=2019 & iso3=='ZAF', tbhiv.sd := imputeTS::na_interpolation(tbhiv.sd)]

est[year>=2019 & iso3 %in% lst, tbhiv := imputeTS::na_locf(tbhiv), by=iso3]

est[year>=2019 & iso3 %in% lst, tbhiv.sd := imputeTS::na_locf(tbhiv.sd), by=iso3]
est[year>=2019 & iso3 %ni% c(mdh.lst,rmdh.lst), sum(is.na(tbhiv)), by=iso3]

est[year>=2019, sum(is.na(tbhiv)), by=year]
est[year>=2019, sum(is.na(tbhiv.sd)), by=year]
est[year>=2019, sum(is.na(inc.h)), by=year]
est[year>=2019, sum(is.na(inc.h.sd)), by=year]


#reset PNG inc.h for 2020 2021 and 2022 because PNG is now excluded from model
est$inc.h[est$iso3=="PNG" & (est$year==2020 | est$year==2021)]=NA
est$inc.h.sd[est$iso3=="PNG" & (est$year==2020 | est$year==2021)]=NA



# inc.h
sel <- (is.na(est$inc.h) & !is.na(est$tbhiv) & est$year>=2020) | (est$iso3=='PNG' & est$year==2008)
table(sel)
inch <- with(est[sel], prodXY(inc, tbhiv, inc.sd, tbhiv.sd))
est[sel, inc.h := inch[[1]]]
est[sel, inc.h.sd := inch[[2]]]
est[year>=2019, sum(is.na(inc.h)), by=year]
est[year>=2019, sum(is.na(inc.h.sd)), by=year]



# inc.nh
sel <- est$year>=2020 & !is.na(est$tbhiv) | (est$iso3=='PNG' & est$year==2008)
table(sel)
incnh <-
  with(est[sel], prodXY(inc, (1 - tbhiv), inc.sd, tbhiv.sd))
est[sel, inc.nh := incnh[[1]]]
est[sel, inc.nh.sd := incnh[[2]]]
est[year>=2019, sum(is.na(inc.nh)), by=year]
est[year>=2019, sum(is.na(inc.nh.sd)), by=year]


#Fix GABON 2012-2013
sel <- (est$iso3=='GAB' & est$year %in% 2012:2013)
inch <- with(est[sel], prodXY(inc, tbhiv, inc.sd, tbhiv.sd))
est[sel, inc.h := inch[[1]]]
est[sel, inc.h.sd := inch[[2]]]
est["GAB",.(year,inc,inc.h,inc.lo,inc.hi,inc.sd,tbhiv)]

incnh <- with(est[sel], prodXY(inc, (1 - tbhiv), inc.sd, tbhiv.sd))
est[sel, inc.nh := incnh[[1]]]
est[sel, inc.nh.sd := incnh[[2]]]



# force of infection in HIV+
fi.h <-
  with(est, divXY(inc.h / m, hiv, (inc.h.sd / m), hiv.sd))
est$fi.h <- fi.h[[1]]
est$fi.h.sd <- fi.h[[2]]


# force of infection in HIV-
fi.nh <-
  with(est, divXY(inc.nh / m, (1 - hiv), (inc.nh.sd / m), hiv.sd))
est$fi.nh <- fi.nh[[1]]
est$fi.nh.sd <- fi.nh[[2]]


# incidence rate ratio, ignoring covariance
irr <- with(est, divXY(fi.h, fi.nh, fi.h.sd, fi.nh.sd))
est$irr <- irr[[1]]
est$irr.sd <- irr[[2]]
sel <- !is.na(est$irr) & est$irr > 1e3
table(sel)
est[sel, .(iso3, year, inc, tbhiv, inc.nh, inc.h, hiv, fi.h, fi.nh, irr)]
est$irr[sel] <- est$irr.sd[sel] <- NA

sel <-
  est$irr.sd == 0 & est$hiv > 0 &
  !is.na(est$irr) & !is.na(est$irr.sd)
table(sel)
est$irr[sel] <- NA
est$irr.sd[sel] <- NA
lst <- unique(as.character(est$iso3[sel]))

est[iso3 %in% lst, irr := na_interpolation(irr), by = iso3]
est[iso3 %in% lst, irr.sd := na_interpolation(irr.sd), by = iso3]


lcty <- unique(as.character(est$iso3))

est[!is.na(tbhiv), test.isbinom(tbhiv)]
est[!is.na(irr), test.ispos(irr)]



# no irr, impute it
#
est[, ghiv := hiv > 0.1]
est[is.na(ghiv), ghiv := F]
est[, hincome := g.income == 'HIC']
est[is.na(hincome), hincome := T]

out <-
  est[, .(n = .N, irr = weighted.mean(irr, w = pop, na.rm = TRUE)),
      by = list(year, ghiv, hincome)]

# out <-
#   est[, .(n = .N, irr = weighted.mean((inc * tbhiv / hiv) / (inc * (1 - tbhiv) /
#                                                                (1 - hiv)), w = pop, na.rm = TRUE),
#           irr.sd = sd(irr)),
#       by = list(year, ghiv, hincome)]
(out)

out[is.infinite(irr), irr := NA]
out[is.nan(irr), irr := NA]
# out[24, irr := NA]
out[, irr := na_interpolation(irr), by = list(ghiv, hincome)]

# mirr <- out[ghiv == F & hincome == T, mean(irr, na.rm = T)]
# out[is.na(irr), irr := mirr]
(out)

# use g.income and generalized HIV as predictors of IRR
#
(dim(est))
est <- merge(est,
             out[, .(year, ghiv, hincome, e.irr = irr)],
             by = c('year', 'ghiv', 'hincome'),
             all.x = T)
(dim(est))
setkey(est, iso3, year)


# one IRR value available
#
est[, n.irr := sum(!is.na(irr) & irr > 0), by = iso3]
est[n.irr == 1, f.irr := irr / e.irr]
est[n.irr == 1, f.irr.sd := irr.sd / irr]
est[n.irr == 1, f.irr := max(f.irr, na.rm = TRUE), by = iso3]
est[n.irr == 1, f.irr.sd := max(f.irr.sd, na.rm = TRUE), by = iso3]
est[n.irr == 1, irr := e.irr * f.irr]
est[n.irr == 1, irr.sd := irr * f.irr.sd]


# multiple IRR available (todo: fix this hack)
#
est[n.irr > 1, f.irr := mean(irr, na.rm = T) / mean(e.irr, na.rm = T)]
est[n.irr > 1, f.irr.sd := mean(irr.sd, na.rm = T) / mean(irr, na.rm = T)]
est[n.irr > 1, f.irr := max(f.irr, na.rm = TRUE), by = iso3]
est[n.irr > 1, f.irr.sd := max(f.irr.sd, na.rm = TRUE), by = iso3]
est[n.irr > 1, irr := e.irr * f.irr]
est[n.irr > 1, irr.sd := irr * f.irr.sd]

# no IRR available
#
est[n.irr == 0, irr := e.irr]
est[n.irr == 0, irr.sd := e.irr * 0.25]


est[!is.na(irr), test.ispos(irr)]


est[, n.irr := NULL]
est[, f.irr := NULL]
est[, f.irr.sd := NULL]
est[is.infinite(irr.sd), irr.sd := irr * .25]
est[is.na(hiv.sd) & !is.na(hiv), hiv.sd := hiv * .25]

# e.tbhiv using imputed IRR
#
sel <- !is.na(est$irr) & is.na(est$irr.sd)
table(sel)
# est$irr.sd[sel] <- .25 * est$irr[sel] # arbitrary
# est[irr.sd > irr * .25, irr.sd := irr * .25]   # as well

excl <- c('BLZ')
sel <-
  !is.na(est$irr) & !is.na(est$hiv) & est$hiv > 0 &
  est$iso3 %ni% excl
table(sel)

out <- est[sel, {
  tmp = h2t(hiv, hiv.sd, irr, irr.sd)$prop

  list(e.tbhiv = tmp[2],
       e.tbhiv.sd = tmp[4])
},
by = .(iso3, year)]


# out[is.infinite(e.tbhiv), e.tbhiv := NA]
# out[is.infinite(e.tbhiv.sd), e.tbhiv.sd := NA]

# smooth e.tbhiv series
out[, e.tbhiv := predict(loess(e.tbhiv ~ year, span = 0.6)), by = iso3]
out[, e.tbhiv.sd := predict(loess(e.tbhiv.sd ~ year, span = 0.6)), by =
      iso3]
out[e.tbhiv < 0, e.tbhiv := 0]
out[e.tbhiv.sd < 0, e.tbhiv.sd := 0]

out[!is.na(e.tbhiv), test.isbinom(e.tbhiv)]
out[!is.na(e.tbhiv), test.isbinom(e.tbhiv.sd)]

est$e.tbhiv[sel] <- out$e.tbhiv
est$e.tbhiv.sd[sel] <- out$e.tbhiv.sd





# impute missing tbhiv with e.tbhiv
#
sel <- is.na(est$tbhiv) & !is.na(est$e.tbhiv)
table(sel)
# est$tbhiv[sel] <- est$e.tbhiv[sel]
# est$tbhiv.sd[sel] <- est$e.tbhiv.sd[sel]

est[!is.na(tbhiv), test.isbinom(tbhiv)]
est[!is.na(tbhiv.sd), test.isbinom(tbhiv.sd)]

sum(is.na(est$tbhiv))
sum(is.na(est$tbhiv.sd))




# fixes
#
# sel <- est$iso3 == 'KHM' & est$year %in% 2000:2009
# sel2 <- old$iso3 == 'KHM' & old$year %in% 2000:2009
# est$tbhiv[sel] <- old$tbhiv[sel2]
# est$tbhiv.sd[sel] <- old$tbhiv.sd[sel2]

# incl <- c('ZAF', 'ZMB', 'ZWE')
# sel <-
#   est$year < 2010 & is.na(est$tbhiv.routine.ok) & est$iso3 %in% incl
# table(sel)
# est$tbhiv[sel] <- est$e.tbhiv[sel]
sel <- est$iso3=='CHN' & est$year>=2018
est[sel, tbhiv.sd := tbhiv[1]]

sel <- abs(est$inc - est$inc.nh - est$inc.h) > 1
table(sel)
est[sel, inc.nh := inc - inc.h]



# Fix Serbia 2020/2021/2022
# Serbia not anymore regional model
# For 2022: use notification and previous CDR
# For 2020/2021: interpolate incidence using <2020 and 2022 value

sel = est$iso3=="SRB" & est$year %in% 2020:2022
est[sel, inc:=NA]
est[sel, inc.sd:=NA]
est[sel, inc.nh:=NA]
est[sel, inc.nh.sd:=NA]

#Fix 2022
sel = est$iso3=="SRB" & est$year==2019
srbcdr2019=est$inc[sel]/est$newinc[sel]

sel = est$iso3=="SRB" & est$year==2022
est[sel,inc:=newinc*srbcdr2019]

#interpolate 2020 and 2021 and LOCF SD for 2020-2022
est[iso3=="SRB", inc := na_interpolation(inc)]
est[iso3=="SRB", inc.sd := na_locf(inc.sd)]

#inc.nh
sel = est$iso3=="SRB" & est$year %in% 2020:2022
incnh <- with(est[sel], prodXY(inc, (1 - tbhiv), inc.sd, tbhiv.sd))
est[sel, inc.nh := incnh[[1]]]
est[sel, inc.nh.sd := incnh[[2]]]

est[sel, source.inc := "Case notifications, standard adjustment"]
est["SRB",.(year,inc,inc.sd,inc.h,inc.h.sd,inc.nh,inc.nh.sd)]



# check imputed series

# Uncomment when redoing a new snapshot to check again the new imputed series

# for (i in wr) {
#   p <-
#     qplot(
#       year,
#       0,
#       data = subset(est, g.whoregion == i & year>=2010),
#       geom = 'line',
#       colour = I('grey90')
#     ) +
#     geom_line(aes(year, tbhiv)) +
#     geom_ribbon(
#       aes(
#         year,
#         ymin = tbhiv - 1.96 * tbhiv.sd,
#         ymax = tbhiv + 1.96 * tbhiv.sd
#       ),
#       fill = I('blue'),
#       alpha = I(.4)
#     ) +
#     geom_point(aes(year, tbhiv.routine),
#                colour = I('black'),
#                shape = I(4)) +
#     geom_point(aes(year, tbhiv.routine.ok),
#                colour = I('blue'),
#                shape = I(4)) +
#     geom_point(aes(year, tbhiv.surv),
#                colour = I('green'),
#                shape = I(2)) +
#     geom_point(aes(year, tbhiv.sentin),
#                colour = I('red'),
#                shape = I(3)) +
#     scale_x_continuous(breaks=c(2010,2015,2020)) +
#     facet_wrap(~ iso3, scales = 'free_y') + xlab('') + ylab('HIV prevalence in TB')
#   suppressWarnings(print(p))
#   suppressWarnings(ggsave(here(
#     paste('inc_mort/output/checks/imputed_tbhiv_', i, '.pdf', sep = '')
#   ),
#   width = 14,
#   height = 8))
# }


# 
# # compare with last year
# #
# for (i in wr) {
#   p <-
#     qplot(
#       year,
#       0,
#       data = subset(est, g.whoregion == i),
#       geom = 'line',
#       colour = I('grey90')
#     ) +
#     geom_ribbon(
#       aes(
#         year,
#         ymin = tbhiv - 1.96 * tbhiv.sd,
#         ymax = tbhiv + 1.96 * tbhiv.sd
#       ),
#       fill = I('blue'),
#       alpha = I(.4)
#     ) +
#     geom_line(
#       aes(year, tbhiv),
#       data = subset(old, g.whoregion == i),
#       colour = I('red'),
#       linetype = I(2)
#     ) +
#     facet_wrap(~ iso3, scales = 'free_y') + xlab('') + ylab('HIV prevalence in TB')
#   suppressWarnings(print(p))
#   suppressWarnings(ggsave(here(
#     paste('inc_mort/output/checks/tbhiv', i, '_compare.pdf', sep = '')
#   ),
#   width = 14,
#   height = 8))
# }



# global TBHIV
#
(est[, weighted.mean(tbhiv, w = inc * pop / 1e5, na.rm = T), by = year])

# # inc.h
# inc.h <- with(est, prodXY(inc, tbhiv, inc.sd, tbhiv.sd))
# est$inc.h <- inc.h[[1]]
# est$inc.h.sd <- inc.h[[2]]
#
# # inc.nh
# inc.nh <-
#   with(est, prodXY(inc, (1 - tbhiv), inc.sd, tbhiv.sd))
# est$inc.nh <- inc.nh[[1]]
# est$inc.nh.sd <- inc.nh[[2]]
#
# # force of infection in HIV+
# fi.h <-
#   with(est, divXY(inc.h / m, hiv, (inc.h.sd / m), hiv.sd))
# est$fi.h <- fi.h[[1]]
# est$fi.h.sd <- fi.h[[2]]
#
# # force of infection in HIV-
# fi.nh <-
#   with(est, divXY(inc.nh / m, (1 - hiv), (inc.nh.sd / m), hiv.sd))
# est$fi.nh <- fi.nh[[1]]
# est$fi.nh.sd <- fi.nh[[2]]
#
# # incidence rate ratio, ignoring covariance
# irr <- with(est, divXY(fi.h, fi.nh, fi.h.sd, fi.nh.sd))
# est$irr <- irr[[1]]
# est$irr.sd <- irr[[2]]
# sel <- !is.na(est$irr) & est$irr > 1e3
# table(sel)
#
# (est[sel, .(iso3, year, inc, tbhiv, inc.nh, inc.h, hiv, fi.h, fi.nh, irr)])
# est$irr[sel] <- est$irr.sd[sel] <- NA
#




### Countries where estimated incidence < notified TB case rate
(inc.low.lst=unique(est[year==2022 & inc<=newinc,iso3]))
est[iso3 %in% inc.low.lst & year>2019, .(iso3,year,inc,newinc,c.newinc)]


### Countries not HIC where estimated incidence < 1.15 * notified TB case rate (15% as applied for HIC)

(inc.low.lst.15=unique(est[iso3 %ni% hic & iso3 %ni% extra.lst & year==2022 & inc<=1.15*newinc,iso3]))
tocheck.country=data.table(est[iso3 %in% inc.low.lst.15 & iso3 %ni% hic & year>2019, .(iso3,year,inc,newinc,c.newinc)])


# Plot for these countries

p=qplot(year,
           inc,
           data = subset(est, iso3 %in% inc.low.lst.15  & year >= 2010),
           geom = 'line',
           colour = I('blue')) +
  geom_ribbon(
    aes(
      year,
      ymin = inc - 1.96 * inc.sd,
      ymax = inc + 1.96 * inc.sd
    ),
    fill = I('blue'),
    alpha = I(.4)
  ) +
  geom_line(
    aes(year, inc),
    data = subset(old, iso3 %in% inc.low.lst.15 & year >= 2010),
    colour = I('red'),
    linetype = I(2))+
  scale_x_continuous(breaks=c(2010,2015,2020)) +
  geom_line(aes(year, newinc)) +
  facet_wrap(~ iso3, scales = 'free_y') + xlab('') + ylab('Incidence rate per 100k/yr')

suppressWarnings(ggsave(
  here(paste('inc_mort/output/checks/inc_tocheck_countries.pdf', sep = '')),
  plot = p,
  width = 14,
  height = 8
))






# bounds
#
B8 <- copy(est)


sel1 <- est$inc > 0
table(sel1)
table(sel1 &
        (est$inc.sd / 1e5) ^ 2 >= est$inc / 1e5 * (1 - est$inc / 1e5))
est[sel1 &
      (est$inc.sd / 1e5) ^ 2 >= est$inc / 1e5 * (1 - est$inc / 1e5), .(iso3, year, inc, inc.sd)]
est[sel1 &
      (est$inc.sd / 1e5) ^ 2 >= est$inc / 1e5 * (1 - est$inc / 1e5), inc.sd := inc * 0.2]

out1 <- vlohi(est$inc[sel1] / m, est$inc.sd[sel1] / m)

sel2 <- est$inc.nh > 0 & !is.na(est$inc.nh)
table(sel2)
table(sel2 &
        (est$inc.nh.sd / 1e5) ^ 2 >= est$inc.nh / 1e5 * (1 - est$inc.nh / 1e5))
est[sel2 &
      (est$inc.nh.sd / 1e5) ^ 2 >= est$inc.nh / 1e5 * (1 - est$inc.nh / 1e5), .(iso3, year, inc.nh, inc.nh.sd)]
est[sel2 &
      (est$inc.nh.sd / 1e5) ^ 2 >= est$inc.nh / 1e5 * (1 - est$inc.nh / 1e5), inc.nh.sd := inc.nh * 0.2]
out2 <- vlohi(est$inc.nh[sel2] / m, est$inc.nh.sd[sel2] / m)

sel3 <-
  est$inc.h > 0 &
  !is.na(est$inc.h) & est$inc.h.sd > 0 & !is.na(est$inc.h.sd)
table(sel3)
table(sel3 &
        (est$inc.h.sd / 1e5) ^ 2 >= est$inc.h / 1e5 * (1 - est$inc.h /
                                                         1e5))
est[sel3 &
      (est$inc.h.sd / 1e5) ^ 2 >= est$inc.h / 1e5 * (1 - est$inc.h / 1e5), .(iso3, year, inc.h, inc.h.sd)]
est[sel3 &
      (est$inc.h.sd / 1e5) ^ 2 >= est$inc.h / 1e5 * (1 - est$inc.h / 1e5), inc.h.sd := inc.h * 0.2]
out3 <- vlohi(est$inc.h[sel3] / m, est$inc.h.sd[sel3] / m)

est$inc.lo[sel1] <- out1[1,] * m
est$inc.hi[sel1] <- out1[2,] * m
est$inc.lo[!sel1] <- est$inc.hi[!sel1] <- est$inc[!sel1]


# bounds of country and regional model back
fixbound.md= (est$iso3 %in% md.lst & est$iso3 != "IND") & est$year %in% 2020:2022
est$inc.lo[fixbound.md] <- est$inc.md.lo[fixbound.md]
est$inc.hi[fixbound.md] <- est$inc.md.hi[fixbound.md]

fixbound.rmd= (est$iso3 %in% extra.lst) & est$year %in% 2020:2022
est$inc.lo[fixbound.rmd] <- est$inc.rmd.lo[fixbound.rmd]
est$inc.hi[fixbound.rmd] <- est$inc.rmd.hi[fixbound.rmd]



est$inc.nh.lo[sel2] <- out2[1,] * m
est$inc.nh.hi[sel2] <- out2[2,] * m
est$inc.nh.lo[!sel2 & est$inc.nh == 0 & est$inc.nh.sd == 0] <- 0
est$inc.nh.hi[!sel2 & est$inc.nh == 0 & est$inc.nh.sd == 0] <- 0

est$inc.h.lo[sel3] <- out3[1,] * m
est$inc.h.hi[sel3] <- out3[2,] * m
est$inc.h.lo[!sel3 & est$inc.h == 0 & est$inc.h.sd == 0] <- 0
est$inc.h.hi[!sel3 & est$inc.h == 0 & est$inc.h.sd == 0] <- 0

sel4 <-
  (est$inc.h.lo > est$inc.h) |
  (est$inc.h.hi < est$inc.h) &
  (!is.na(est$inc.h) & !is.na(est$inc.h.lo) & !is.na(est$inc.h.hi))
table(sel4)
est[sel4, .(iso3, year, inc, inc.h, inc.h.sd, inc.h.lo, inc.h.hi)]
est[sel4, inc.h.lo := 0]
est[sel4, inc.h.hi := inc.h + 1.96 * inc.h.sd]

sel <-
  (est$inc.nh.lo > est$inc.nh) |
  (est$inc.nh.hi < est$inc.nh) &
  (!is.na(est$inc.nh) &
     !is.na(est$inc.nh.lo) & !is.na(est$inc.nh.hi))
table(sel)
est[sel, .(iso3, year, inc, inc.nh, inc.nh.sd, inc.nh.lo, inc.nh.hi)]
# est[sel, inc.nh.hi := inc.nh + 1.96 * inc.nh.sd]

est[is.na(inc.nh.lo), inc.nh.lo := 0]
est[is.na(inc.nh.hi), inc.nh.hi := inc.nh + 1.96 * inc.nh.sd]

sel <- !is.na(est$inc.h)
est[sel & is.na(inc.h.lo), inc.h.lo := 0]
est[sel & is.na(inc.h.hi), inc.h.hi := inc.h + 1.96 * inc.h.sd]


sel <- est$tbhiv > 0 & est$tbhiv < 1 & !is.na(est$tbhiv)
table(sel)
table(sel & (est$tbhiv.sd) ^ 2 >= est$tbhiv * (1 - est$tbhiv))
est[sel &
      (est$tbhiv.sd) ^ 2 >= est$tbhiv * (1 - est$tbhiv), .(iso3, year, tbhiv, tbhiv.sd)]
est[sel &
      (est$tbhiv.sd) ^ 2 >= est$tbhiv * (1 - est$tbhiv), tbhiv.sd := tbhiv * 0.2]

out <- vlohi(est$tbhiv[sel], est$tbhiv.sd[sel])
est$tbhiv.lo[sel] <- out[1,]
est$tbhiv.hi[sel] <- out[2,]


sel <- est$tbhiv == 0 & !is.na(est$tbhiv)
table(sel)
est$tbhiv.lo[sel] <- 0
est$tbhiv.hi[sel] <- est$tbhiv.sd[sel] * 1.96

sel <- est$tbhiv == 1 & !is.na(est$tbhiv)
table(sel)
est$tbhiv.hi[sel] <- 1
est$tbhiv.lo[sel] <- pmax(1 - est$tbhiv.sd[sel] * 1.96, 0)

est[tbhiv.hi < tbhiv, .(iso3, tbhiv, tbhiv.sd, tbhiv.lo, tbhiv.hi)]
est[tbhiv.hi < tbhiv, tbhiv.hi := tbhiv + 1.96 * tbhiv.sd]





# checks
#
est[, test.isbinom(inc / m)]
est[!is.na(inc.nh), test.isbinom(inc.nh / m)]
est[!is.na(tbhiv), test.isbinom(tbhiv)]
est[!is.na(inc.h), test.isbinom(inc.h / m)]

est[, test.bounds(inc, inc.lo, inc.hi)]
est[!is.na(inc.nh), test.bounds(inc.nh, inc.nh.lo, inc.nh.hi)]
est[!is.na(inc.h), test.bounds(inc.h, inc.h.lo, inc.h.hi)]
est[!is.na(tbhiv), test.bounds(tbhiv, tbhiv.lo, tbhiv.hi)]

est[!is.na(inc.h), sum(abs(inc.h + inc.nh - inc) > 1) == 0]
est[!is.na(inc.h), test.ispos(inc.h)]
est[!is.na(inc.h), test.ispos(inc.h.sd)]
est[!is.na(inc.h), test.ispos(inc.nh)]
est[!is.na(inc.h), test.ispos(inc.nh.sd)]
est[!is.na(irr), test.ispos(irr)]





# 
# ### PLOTS 
# 
# # Comparison plots with last year's report, focus on recent trends
# #
for (i in wr) {
  p <- qplot(
    year,
    inc,
    data = subset(est, g.whoregion == i & year >= 2010),
    geom = 'line',
    colour = I('grey90')
  ) +
    geom_ribbon(
      aes(
        year,
        ymin = inc.lo,
        ymax = inc.hi
      ),
      fill = I('blue'),
      alpha = I(.4)
    ) +
    geom_line(
      aes(year, inc),
      data = subset(old, g.whoregion == i & year >= 2010),
      colour = I('red'),
      linetype = I(2)
    ) +
    scale_x_continuous(breaks=c(2010,2015,2020)) +
    geom_line(aes(year, newinc)) +
    facet_wrap(~ iso3, scales = 'free_y') + xlab('') + ylab('Incidence rate per 100k/yr')

  suppressWarnings(ggsave(
    here(paste(
      'inc_mort/output/checks/incidence_', i, '_compare.pdf', sep = ''
    )),
    plot = p,
    width = 14,
    height = 8
  ))
}

# 
# # Incidence of HBC
# hbc.lst <- est[g.hbc == T, unique(iso3)]
# 
# 
# p <- qplot(year,
#            inc,
#            data = subset(est, iso3 %in% hbc.lst  & year >= 2010),
#            geom = 'line',
#            colour = I('blue')) +
#   geom_ribbon(
#     aes(
#       year,
#       ymin = inc.lo,
#       ymax = inc.hi
#     ),
#     fill = I('blue'),
#     alpha = I(.4)
#   ) +
#   geom_line(
#     aes(year, inc),
#     data = subset(old, iso3 %in% hbc.lst & year >= 2010),
#     colour = I('red'),
#     linetype = I(2))+
#   scale_x_continuous(breaks=c(2010,2015,2020)) +
#   geom_line(aes(year, newinc)) +
#   facet_wrap(~ iso3, scales = 'free_y') + xlab('') + ylab('Incidence rate per 100k/yr')
# 
# suppressWarnings(ggsave(
#   here(paste('inc_mort/output/checks/inc_hbc.pdf', sep = '')),
#   plot = p,
#   width = 14,
#   height = 8
# ))
# 
# 
# 
# 
# 
# 
# 
# # Comparison plots with focus on recent trends, HIV- incidence
# for (i in wr) {
#   p <- qplot(
#     year,
#     inc.nh,
#     data = subset(est, g.whoregion == i & year >= 2010),
#     geom = 'line',
#     colour = I('grey90')
#   ) +
#     geom_ribbon(
#       aes(
#         year,
#         ymin = inc.nh.lo,
#         ymax = inc.nh.hi
#       ),
#       fill = I('blue'),
#       alpha = I(.4)
#     ) +
#     geom_line(
#       aes(year, inc.nh),
#       data = subset(old, g.whoregion == i & year >= 2010),
#       colour = I('red'),
#       linetype = I(2)
#     ) +
#     scale_x_continuous(breaks=c(2010,2015,2020)) +
#     facet_wrap( ~ iso3, scales = 'free_y') + xlab('') + ylab('Incidence rate per 100k/yr')
#   suppressWarnings(print(p))
#   suppressWarnings(ggsave(here(
#     paste('inc_mort/output/checks/inc.hivneg_', i, '_compare.pdf', sep = '')
#   ),
#   width = 14,
#   height = 8))
# }
# 
# 
# 
# 
# # Comparison plots with focus on recent trends, HIV+ incidence
# for (i in wr) {
#   p <- qplot(
#     year,
#     inc.h,
#     data = subset(est, g.whoregion == i & year >= 2010),
#     geom = 'line',
#     colour = I('grey90')
#   ) +
#     geom_ribbon(
#       aes(
#         year,
#         ymin = inc.h.lo,
#         ymax = inc.h.hi
#       ),
#       fill = I('blue'),
#       alpha = I(.4)
#     ) +
#     geom_line(
#       aes(year, inc.h),
#       data = subset(old, g.whoregion == i & year >= 2010),
#       colour = I('red'),
#       linetype = I(2)
#     ) +
#     scale_x_continuous(breaks=c(2010,2015,2020)) +
#     facet_wrap( ~ iso3, scales = 'free_y') + xlab('') + ylab('Incidence rate per 100k/yr')
#   suppressWarnings(print(p))
#   suppressWarnings(ggsave(here(
#     paste('inc_mort/output/checks/inc.hivpos_', i, '_compare.pdf', sep = '')
#   ),
#   width = 14,
#   height = 8))
# }
# 
# 
# # Comparison plots with last year's global TB report
# for (i in wr) {
#   p <- qplot(
#     year,
#     hiv,
#     data = subset(est, g.whoregion == i & year>=2010),
#     geom = 'line',
#     colour = I('grey90')
#   ) +
#     geom_ribbon(
#       aes(
#         year,
#         ymin = hiv.lo,
#         ymax = hiv.hi
#       ),
#       fill = I('blue'),
#       alpha = I(.4)
#     ) +
#     geom_line(
#       aes(year, hiv),
#       data = subset(old, g.whoregion == i & year>=2010),
#       colour = I('red'),
#       linetype = I(2)
#     ) +
#     scale_x_continuous(breaks=c(2010,2015,2020)) +
#     facet_wrap(~ iso3, scales = 'free_y') + xlab('') + ylab('HIV prevalence')
#   suppressWarnings(print(p))
#   suppressWarnings(ggsave(here(
#     paste('inc_mort/output/checks/hiv_prev_', i, '_compare.pdf', sep = '')
#   ),
#   width = 14,
#   height = 8))
# }
# 
# 
# 
# # Comparison plots with last year's global TB report
# for (i in wr) {
#   p <- qplot(
#     year,
#     tbhiv,
#     data = subset(est, g.whoregion == i & year>=2010),
#     geom = 'line',
#     colour = I('grey90')
#   ) +
#     geom_ribbon(
#       aes(
#         year,
#         ymin = tbhiv.lo ,
#         ymax = tbhiv.hi
#       ),
#       fill = I('blue'),
#       alpha = I(.4)
#     ) +
#     geom_line(
#       aes(year, tbhiv),
#       data = subset(old, g.whoregion == i & year>=2010),
#       colour = I('red'),
#       linetype = I(2)
#     ) +
#     scale_x_continuous(breaks=c(2010,2015,2020)) +
#     facet_wrap(~ iso3, scales = 'free_y') + xlab('') + ylab('HIV prevalence in TB')
#   suppressWarnings(print(p))
#   suppressWarnings(ggsave(here(
#     paste('inc_mort/output/checks/tbhiv_', i, '_compare.pdf', sep = '')
#   ),
#   width = 14,
#   height = 8))
# }
# 


# global aggregates
#
(est[, .(inc.num = as.integer(sum(inc * pop / 1e5))), by = year])
(est[, .(inc.h.num = as.integer(sums(inc.h * pop / 1e5))), by =
       year])





B10=copy(est)



# save
#



attr(est, "timestamp") <- Sys.Date() #set date
save(est, file = here('inc_mort/analysis/est.rda'))
fwrite(est, file = here(paste0('inc_mort/analysis/csv/est_inc_', Sys.Date(), '.csv')))

#to check country list
fwrite(tocheck.country, file = here(paste0('inc_mort/analysis/csv/tocheck_country_', Sys.Date(), '.csv')))
