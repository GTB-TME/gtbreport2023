#' ---
#' title: Data preparation, Global TB Report 2023
#' author: Mathieu Bastard
#' date: 30/06/2023
#' output:
#'    html_document:
#'      toc: true
#'      highlight: zenburn
#' ---

#' # Preamble
#' (Last updated: `r Sys.Date()`)
#'
#' Export files to GTB database
#'
#'

library(data.table)
library(here)
library(gtbreport)
library(plotly)
library(dplyr)
library(tidyverse)

#Run Functions script
source(here('inc_mort/R code/fun.R'))

#Function to import db from GTB databases
source(here("import/load_gtb.R"))


load(here('inc_mort/analysis/est.rda'))
load(here('inc_mort/analysis/regional.rda'))
load(here('inc_mort/analysis/hbc.rda'))
load(here('inc_mort/analysis/unaids.rda'))
load(here('inc_mort/analysis/model.rda'))
global22=load(here('inc_mort/analysis/global22.rda'))
globalold=global
load(here('inc_mort/analysis/global.rda'))

load(here('inc_mort/analysis/vr.rda'))

tb=load_gtb("tb",convert_dots = FALSE)
cty=load_gtb("cty",convert_dots = FALSE)
pop=load_gtb("pop",convert_dots = FALSE)
grpmbr=load_gtb("grpmbr",convert_dots = FALSE)
monthly=load_gtb("monthly",convert_dots = FALSE)

load(here('inc_mort/estimates2022/old.rda'))
#
load(here('disaggregation/reportoutput/h30splt.rda'))
load(here('disaggregation/reportoutput/globsplt.rda'))
load(here('disaggregation/reportoutput/regsplt.rda'))
#
load(here('disaggregation/reportoutput/Mglobsplt.rda'))
load(here('disaggregation/reportoutput/MPglobsplit.rda'))
load(here('disaggregation/reportoutput/Mregsplt.rda'))

load(here("inc_mort/analysis/excessmort.rda"))

load(here('drtb/dboutput/db_dr_group.rda'))


top10 <- fread(here('data/mortality/top10.csv'))


yr <- 2022
vlohi <- Vectorize(lohi, c('ev', 'sd'))

snapshot_date <- latest_snapshot_date()
est_date <- attr(est, which = "timestamp")

# Function for writing thousands

thous=function(x){
  return(paste0(x," ","000"))
}




# region names
regional$region <- factor(regional$g.whoregion, levels=c('AFR','AMR','SEA','EUR','EMR','WPR'))
levels(regional$region) <-
  c(
    'African Region',
    'Region of the Americas',
    'South-East Asia Region',
    'European Region',
    'Eastern Mediterranean Region',
    'Western Pacific Region'
  )
regional$region <- ordered(regional$region, )

# regional inc and mort milestones
regional[, mort.milestone := mort.num[year==2015] * 0.25, by = g.whoregion]
regional[, inc.milestone := inc[year==2015] * 0.5, by = g.whoregion]


# EURO notifications uses imputed values if missing in 2022
# c.newinc and newinc recalculated
# Update global too for 2022

# sel=regional$g.whoregion=="EUR" & regional$year==2022
# sel2=est$g.whoregion=="EUR" & est$year==2022
#
# observed.euro.notif22=regional$c.newinc[sel]
# imputed.euro.notif22=sum(est$imp.newinc[sel2]*est$pop[sel2]/1e5)
# diff.observed.imputed.euro.notif22=round(imputed.euro.notif22-observed.euro.notif22,0)
#
# regional$c.newinc[sel]=sum(est$imp.newinc[sel2]*est$pop[sel2]/1e5)
#
# regional$newinc[sel]=regional$c.newinc[sel]/regional$pop[sel]*1e5
#
# sel=global$year==2022
# global$c.newinc[sel]=global$c.newinc[sel]+diff.observed.imputed.euro.notif22
# global$newinc[sel]=global$c.newinc[sel]/global$pop[sel]*1e5
#


### Uncomment when spit data available


regsplt$name <- factor(regsplt$name,
                       levels=c('Africa','The Americas','South-East Asia','Europe',
                                'Eastern Mediterranean','Western Pacific'))
levels(regsplt$name) <-
  c(
    'African Region',
    'Region of the Americas',
    'South-East Asia Region',
    'European Region',
    'Eastern Mediterranean Region',
    'Western Pacific Region'
  )
regsplt$name <- ordered(regsplt$name)

Mregsplt$name <- factor(Mregsplt$g.whoregion, levels=c('AFR','AMR','SEA','EUR','EMR','WPR'))
levels(Mregsplt$name) <-
  c(
    'African Region',
    'Region of the Americas',
    'South-East Asia Region',
    'European Region',
    'Eastern Mediterranean Region',
    'Western Pacific Region'
  )
Mregsplt$name <- ordered(Mregsplt$name)



# mn codes
# report_frequency:  70 = monthly, 71 = quarterly.
# report_coverage: 34 =   includes reports from all units, 35 = report is only from some units
mn <- monthly

dim(mn)
mn2 <- merge(mn, est[year==yr, .(iso3, g.hbc)], by='iso3', all.x=T)
mn2[, remarks := NULL]
mn2[, country := factor(country)]
levels(mn2$country)[match('Democratic Republic of the Congo', levels(mn2$country))] <-
  'Democratic Republic\nof the Congo'
levels(mn2$country)[match('United Republic of Tanzania', levels(mn2$country))] <-
  'United Republic\nof Tanzania'
levels(mn2$country)[match("Democratic People's Republic of Korea", levels(mn2$country))] <-
  "Democratic People's\nRepublic of Korea"
levels(mn2$country)[match("Central African Republic", levels(mn2$country))] <-
  'Central African\nRepublic'




# monthly
monthly <- melt(mn2[report.frequency==70 & g.hbc==TRUE, .(iso3,country,g.hbc,year,m.01,m.02,m.03,m.04,m.05,m.06,
                                               m.07,m.08,m.09,m.10,m.11,m.12)],
                measure.vars = c('m.01','m.02','m.03','m.04','m.05','m.06',
                                 'm.07','m.08','m.09','m.10','m.11','m.12'))
monthly[, variable := as.integer(gsub('^m.','', variable))]
dim(monthly)
monthly <- merge(monthly, tb[year==2019,.(iso3, n2019=c.newinc)], by=c('iso3'), all.x=TRUE)
dim(monthly)

monthly[, time := as.Date(paste0('01','-',variable,'-',year), format = '%d-%m-%Y')]



# quarterly
qty <- melt(mn2[report.frequency==71 & g.hbc==TRUE, .(iso3,country,g.hbc,year,q.1,q.2,q.3,q.4)],
            measure.vars = c('q.1','q.2','q.3','q.4'))
qty[, variable := as.integer(gsub('^q.','', variable))]
dim(qty)
qty <- merge(qty, tb[year==2019,.(iso3, n2019=c.newinc)], by=c('iso3'), all.x=TRUE)
dim(qty)

qty[, time := as.Date(paste0('01','-', variable * 3 - 2, '-', year), format = '%d-%m-%Y')]



# India monthly
# ind[, time := paste0('01-', time)]
# ind[, month := as.Date(time, format=c("%d-%b-%y"))]
# ind[, n2019 := tb[year==2019 & iso3=='IND', as.numeric(c.newinc)]]
# ind[, n2019 := n2019/12]




# model
model <- merge(model, old[year == 2019, .(iso3, country)], by = 'iso3')
model[ , rebest := best / best[1], by=.(iso3, scenario, hiv, measure)]

# model2 <- merge(model2, old[year == 2019, .(iso3, country)], by = 'iso3')
# model2[ , rebest := best / best[1], by=.(iso3, scenario, hiv, measure)]




# output for manual manipulation to incorporate into other's files
#
# Table EA 5.1 - Sources of data (Marie)
#
tab <-
  as.data.frame(est[year == yr][g.hbc==TRUE, list(iso3, source.mort)])
(tab)
vr <- merge(vr, est[year==yr, .(iso3, g.hbc)], by='iso3')
(vr[g.hbc==T & !is.na(tb),.(min.year = min(year), max.year = max(year)), by='iso3'])

(est[g.hbc==T & !is.na(vr.raw),.(min.year = min(year), max.year = max(year)), by='iso3'])
# (est[g.hbc==T & year==yr & old.source.mort %in% c('VR','IHME'), .(iso3, source.mort)])


# Country name to est dataset
est=merge(est,subset(cty,select=c("iso3","country")),by="iso3",all.x=T)



# chapter 2 tables
#
#' vectorized lohi
#'
vlohi <- Vectorize(lohi, c("ev", "sd"))

# monthly
#
monthly[, nm := sum(!is.na(value)), by=.(iso3, year)]





# Estimated epidemiological burden of TB in 2022 for 30 high TB burden countries,
# WHO regions and globally.
tab1.1 <- subset(
  est,
  g.hbc == TRUE & year == yr,
  select = c(
    "country",
    "pop",
    "inc.num",
    "inc.lo.num",
    "inc.hi.num",
    "inc.h.num",
    "inc.h.lo.num",
    "inc.h.hi.num",
    "mort.nh.num",
    "mort.nh.lo.num",
    "mort.nh.hi.num",
    "mort.h.num",
    "mort.h.lo.num",
    "mort.h.hi.num"
  )
)

tab1.2 <- subset(
  hbc,
  year == yr,
  select = c(
    "pop",
    "inc.num",
    "inc.lo.num",
    "inc.hi.num",
    "inc.h.num",
    "inc.h.lo.num",
    "inc.h.hi.num",
    "mort.nh.num",
    "mort.nh.lo.num",
    "mort.nh.hi.num",
    "mort.h.num",
    "mort.h.lo.num",
    "mort.h.hi.num"
  )
)

tab1.3 <- subset(
  regional,
  year == yr,
  select = c(
    "g.whoregion",
    "pop",
    "inc.num",
    "inc.lo.num",
    "inc.hi.num",
    "inc.h.num",
    "inc.h.lo.num",
    "inc.h.hi.num",
    "mort.nh.num",
    "mort.nh.lo.num",
    "mort.nh.hi.num",
    "mort.h.num",
    "mort.h.lo.num",
    "mort.h.hi.num"
  )
)

tab1.4 <- subset(
  global,
  year == yr,
  select = c(
    "pop",
    "inc.num",
    "inc.lo.num",
    "inc.hi.num",
    "inc.h.num",
    "inc.h.lo.num",
    "inc.h.hi.num",
    "mort.nh.num",
    "mort.nh.lo.num",
    "mort.nh.hi.num",
    "mort.h.num",
    "mort.h.lo.num",
    "mort.h.hi.num"
  )
)


setnames(tab1.1, "country", "rowname")
setnames(tab1.3, "g.whoregion", "rowname")
tab1.1 <- tab1.1[order(rowname)]
tab1.3 <- tab1.3[order(rowname)]
tab1.2 <- cbind(rowname = "High TB burden countries", tab1.2)
tab1.4 <- cbind(rowname = "Global", tab1.4)

tab1 <- rbind(tab1.1, tab1.2, tab1.3, tab1.4)
tab1[, pop := round(pop)]
tab1[, 2:14 := lapply(.SD, function(x)
  x / 1000), .SDcols = 2:14]
tab1[, 2:14 := lapply(.SD, ftb), .SDcols = 2:14]

names(tab1) <- c(
  " ",
  "Population",
  "Total TB Incidence",
  " ",
  " ",
  "HIV-positive TB incidence",
  " ",
  " ",
  "HIV-negative TB mortality",
  " ",
  " ",
  "HIV-positive TB mortality",
  " ",
  " "
)

tab1[32:37, 1] <- c(
  'African Region',
  'Region of the Americas',
  'Eastern Mediterranean Region',
  'European Region',
  'South-East Asia Region',
  'Western Pacific Region'
)

(tabinc <- copy(tab1[c(1:33,36,35,34,37,38), ]))




# table of DR proportions
#
# ghr2 <- copy(ghr)
# ghr2[, 2:10 := lapply(.SD, function(x)pmin(100, x * 100/last(global$inc.num))), .SDcols = 2:10]
# ghr2[, 2:10 := lapply(.SD, ftb), .SDcols = 2:10]
#
# ghr2[3, INH := 'Total']
# setnames(ghr2, c(' ', 'RR', ' ', ' ', 'RS', ' ', ' ', 'Total', ' ', ' '))
#
# ghr2[3,9] <- '-'
# ghr2[3,10] <- '-'




# MDR mortality
#
#' M2m <- function(M,
#'                 M.sd,
#'                 p,
#'                 p.sd,
#'                 r = 2.26,
#'                 r.sd = 0.45) {
#'   #' m = Mpr
#'   #' @param m = MDR-TB mortality, the unknown factor
#'   #' @param M = total (HIV- and HIV+) TB mortality
#'   #' @param p = overall proportion RR among prevalent TB, combined (among new and retreated) estimate
#'   #' @param r = RR or risk of dying from MDR compared to non-MDR TB (proxy for RR deaths),
#'   #'     approximated with the OR from meta-analysis 2.26 (1.54-3.33), SE=0.45
#'   #' @export
#'   #'
#'   require(propagate)
#'
#'   DT <- cbind(M = c(M, M.sd),
#'               p = c(p, p.sd),
#'               r = c(r, r.sd))
#'   EXPR <- expression(M * p * r / (1 - p + p * r))
#'   out <-
#'     propagate(
#'       EXPR,
#'       data = DT,
#'       type = "stat",
#'       do.sim = F,
#'       second.order = T
#'     )$prop[c(2, 4:6)]
#'   names(out) <- c("m", "m.sd", "m.lo", "m.hi")
#'
#'   return(out)
#' }
#'
#' global.mort.rr <- M2m(last(global$mort),
#'                       last(global$mort.sd),
#'                       p = 0.05804381,
#'                       p.sd = 0.004730819) * last(global$pop) / 1e5
#'
#' reg.mort.rr <- regional[year == yr][, {
#'   tmp <-
#'     M2m(mort, mort.sd, p = 0.05804381, p.sd = 0.004730819) * pop / 1e5
#'
#'   list(m = tmp[1],
#'        m.lo = tmp[3],
#'        m.hi = tmp[4])
#' }, by = .(g.whoregion)]




# Epi burden (rates)
#
tab1b.1 <- subset(
  est,
  g.hbc == TRUE & year == yr,
  select = c(
    "country",
    "pop",
    "inc",
    "inc.lo",
    "inc.hi",
    "inc.h",
    "inc.h.lo",
    "inc.h.hi",
    "mort.nh",
    "mort.nh.lo",
    "mort.nh.hi",
    "mort.h",
    "mort.h.lo",
    "mort.h.hi"
  )
)

tab1b.2 <- subset(
  hbc,
  year == yr,
  select = c(
    "pop",
    "inc",
    "inc.lo",
    "inc.hi",
    "inc.h",
    "inc.h.lo",
    "inc.h.hi",
    "mort.nh",
    "mort.nh.lo",
    "mort.nh.hi",
    "mort.h",
    "mort.h.lo",
    "mort.h.hi"
  )
)

tab1b.3 <- subset(
  regional,
  year == yr,
  select = c(
    "g.whoregion",
    "pop",
    "inc",
    "inc.lo",
    "inc.hi",
    "inc.h",
    "inc.h.lo",
    "inc.h.hi",
    "mort.nh",
    "mort.nh.lo",
    "mort.nh.hi",
    "mort.h",
    "mort.h.lo",
    "mort.h.hi"
  )
)


tab1b.4 <- subset(
  global,
  year == yr,
  select = c(
    "pop",
    "inc",
    "inc.lo",
    "inc.hi",
    "inc.h",
    "inc.h.lo",
    "inc.h.hi",
    "mort.nh",
    "mort.nh.lo",
    "mort.nh.hi",
    "mort.h",
    "mort.h.lo",
    "mort.h.hi"
  )
)


setnames(tab1b.1, "country", "rowname")
setnames(tab1b.3, "g.whoregion", "rowname")
tab1b.1 <- tab1b.1[order(rowname)]
tab1b.3 <- tab1b.3[order(rowname)]
tab1b.2 <- cbind(rowname = "High TB burden countries", tab1b.2)
tab1b.4 <- cbind(rowname = "Global", tab1b.4)

tab1b <- rbind(tab1b.1, tab1b.2, tab1b.3, tab1b.4)

tab1b[, 6:8 := lapply(.SD, function(x)
  x), .SDcols = 6:8]
tab1b[, 3:14 := lapply(.SD, ftb), .SDcols = 3:14]
tab1b[, pop := NULL]
names(tab1b) <- c(
  " ",
  "Incidence",
  " ",
  " ",
  "HIV Incidence",
  " ",
  " ",
  "HIV-negative TB mortality",
  " ",
  " ",
  "HIV-positive TB mortality",
  " ",
  " "
)

tab1b[32:37, 1] <- c(
  'African Region',
  'Region of the Americas',
  'Eastern Mediterranean Region',
  'European Region',
  'South-East Asia Region',
  'Western Pacific Region'
)

(tabmort <- copy(tab1b[c(1:33,36,35,34,37,38), ]))



# HBCs
#
hest <- subset(est, g.hbc == TRUE)

levels(hest$country)[match('Democratic Republic of the Congo', levels(hest$country))] <-
  'Democratic Republic\nof the Congo'
levels(hest$country)[match('United Republic of Tanzania', levels(hest$country))] <-
  'United Republic\nof Tanzania'
levels(hest$country)[match("Democratic People's Republic of Korea", levels(hest$country))] <-
  "Democratic People's\nRepublic of Korea"

hest[, inc.milestone := inc[year==2015] * 0.5, by = iso3]
hest[, mort.milestone := mort.num[year==2015] * 0.25, by = iso3]

est[, inc.milestone := inc[year==2015] * 0.5, by = iso3]
est[, mort.milestone := mort.num[year==2015] * 0.25, by = iso3]

hest[, country2 := country]
hest[grep('Republic', country2), country2 := paste('the',country2)]


# DR
#
# ghr[, 2:10 := lapply(.SD, function(x) ftb(x / 1000)), .SDcols = 2:10]
#
# rrinc[32:37, 1] <- c(
#   'African Region',
#   'Region of the Americas',
#   'Eastern Mediterranean Region',
#   'European Region',
#   'South-East Asia Region',
#   'Western Pacific Region'
# )
# rrinc <- rrinc[c(32,33,36,35,34,37,38), ]
# rrinc[, 2:14 := lapply(.SD, function(x) ftb(x)), .SDcols = 2:14]
#
# Data_Figs_5_6_7_d <- data.table(Data_Figs_5_6_7_d)
# rn9 <- data.table(rn9)
# rp9 <- data.table(rp9)
# dataFig237 <- data.table(dataFig237)
#
# ghr$INH <- c('Isoniazid-resistant','Isoniazid-susceptible','Global')
#
#
# hrinc <- copy(hrg)
# hrinc[, 2:7 := lapply(.SD, function(x) x * 100), .SDcols = 2:7]
# hrinc[, 8:10 := lapply(.SD, function(x) x / 1e3), .SDcols = 8:10]
# hrinc[, 2:13 := lapply(.SD, function(x) ftb(x)), .SDcols = 2:13]
# hrinc[, 1] <- rrinc[, 1]
# hrinc <- hrinc[,.(region, prop.hr.new, prop.hr.new.lo, prop.hr.new.hi,
#                   prop.hr.ret, prop.hr.ret.lo, prop.hr.ret.hi,
#                   inc.hr.num, inc.hr.num.lo, inc.hr.num.hi,
#                   inc.hr,inc.hr.lo,inc.hr.hi)]
#
#
#
# # Trends in DR
# #
# ystart <- 2010
#
# dr <-
#   merge(drnew[all.areas.covered.new == 1],
#         drs[, .(iso3, year, pulm.labconf.new, pulm.labconf.unk)],
#         by.x = c('iso3', 'year.new'),
#         by.y = c('iso3', 'year'))
#
# dr[, dst.cov.new := dst.rlt.new / pulm.labconf.new]
# dta <- est[,.(iso3, year, g.whoregion, pop, not=imp.newinc)]
# dta <-
#   merge(dta, dr[year.new >= 1999, .(iso3,
#                                     year = year.new,
#                                     rr.new.pcnt,
#                                     mdr.new.pcnt,
#                                     rr.new,
#                                     mdr.new,
#                                     dst.rlt.new,
#                                     dst.cov.new,
#                                     pulm.labconf.new,
#                                     pulm.labconf.unk)],
#         by = c('iso3', 'year'), all.x = T)
#
# dta <- unique(dta, by=c('iso3','year'))
#
# dta[, mdr := mdr.new / dst.rlt.new]
# dta[, rr := rr.new / dst.rlt.new]
# dta[!is.na(rr.new.pcnt), rr := rr.new.pcnt/100]
# dta[!is.na(mdr.new.pcnt), mdr := mdr.new.pcnt/100]
# dta[!is.na(mdr), test.isbinom(mdr)]
# dta[rr>1, rr := NA]
# dta[!is.na(rr), test.isbinom(rr)]
#
# dta[mdr>rr, mdr := NA]
#
# dta[year>=ystart, nrr := sum(!is.na(rr) & rr > 0), by=iso3]
# dta[year>=ystart, nmdr := sum(!is.na(mdr) & mdr > 0), by=iso3]
#
# dta[mdr > 0 & year >= ystart, c.mdr := coef(lm(log(mdr) ~ year))[2], by=iso3]
# dta[year >= ystart, c.not := coef(lm(log(not+.5) ~ year))[2], by=iso3]
#
# dta[mdr > 0 & year >= ystart & !is.na(c.mdr), c.mdr.sd := summary(lm(log(mdr) ~ year))$coefficients[2,2], by=iso3]
# dta[year >= ystart & !is.na(c.not), c.not.sd := summary(lm(log(not + .5) ~ year))$coefficients[2,2], by=iso3]
#
# dta[, nnot := not * pop / 1e5]
#
# dta2 <- merge(dta[nmdr>2], est[,.(iso3,country,year,inc)],by=c('iso3','year'), all.x=T, all.y=F)
#
# dta2[, pops := last(pop), by=iso3]
# dta2[, cases := last(not) * pops /1e5, by=iso3]
#
# sel <- dta2$cases>1000 & dta2$iso3 %ni% c('ETH','UGA', 'GHA', 'NAM', 'ZMB', 'ZWE')
# dta2[sel, slope.mdr := coef(lm(log(mdr + 0.01)~year))[2] * 100, by=iso3]
# dta2[iso3=='GBR', country := 'United Kingdom']
#
# trenddr <- copy(dta2[sel])
# rm(dta2, dta)




# Confirmed
#
tb[, wconf := rowSums(cbind(new.labconf, ret.rel.labconf, new.clindx, ret.rel.clindx),
                      na.rm = TRUE)]
dta <-
  merge(tb[year > 1999, .(iso3, g.whoregion, year, newinc, conf, wconf, ep, sex.ratio)],
        est[, .(iso3, g.hbc, g.income, year, inc, tbhiv)], by = c('iso3', 'year'))

sel <- dta$wconf >= 100 & !is.na(dta$wconf) & dta$year > 2012

(dta[sel, weighted.mean(conf,
                        w = wconf,
                        na.rm = TRUE)
     , by = .(year)][order(year)])

(dta[sel, {
  tmp = weighted.quantile(
    conf,
    probs = c(0, 0.25, 0.5, 0.75, 1),
    w = wconf,
    na.rm = TRUE
  )

  list(
    conf.med = tmp[3],
    conf.min = tmp[1],
    conf.q1 = tmp[2],
    conf.q3 = tmp[4],
    conf.max = tmp[5]
  )

}, by = .(g.income == 'HIC', year)][order(year)])

(conf <- dta[sel, {
  tmp = weighted.quantile(
    conf,
    probs = c(0, 0.25, 0.5, 0.75, 1),
    w = wconf,
    na.rm = TRUE
  )

  list(
    conf.med = tmp[3],
    conf.min = tmp[1],
    conf.q1 = tmp[2],
    conf.q3 = tmp[4],
    conf.max = tmp[5]
  )

}, by = .(g.income, year)][order(year)])

conf[, income := factor(
  g.income,
  levels = c('LIC', 'LMC', 'UMC', 'HIC'),
  labels = c(
    'low-income',
    'lower-middle-income',
    'upper-middle-income',
    'high-income'
  )
)]


# ### Uncomment when spit available


globsplt[, child := age %in% c('0-4','5-14')]
Mglobsplt[, child := age %in% c('0-4','5-14')]
Mglobsplt[, age := gsub(' 65','65', age)]
Mregsplt[, age := gsub(' 65','65', age)]


est[, source.inc := gsub('Current','Pre-2020',source.inc)]
est[, source.mort := gsub('Current','Pre-2020',source.mort)]




# Split Incidence

inctot=global$inc.num[global$year==2022]
inctot.lo=global$inc.lo.num[global$year==2022]
inctot.hi=global$inc.hi.num[global$year==2022]

inc.men=sum(globsplt$inc[globsplt$sex=="M"&globsplt$child==F])/global$inc.num[global$year==2022]*inctot
inc.men.lo=sum(globsplt$inc[globsplt$sex=="M"&globsplt$child==F])/global$inc.num[global$year==2022]*inctot.lo
inc.men.hi=sum(globsplt$inc[globsplt$sex=="M"&globsplt$child==F])/global$inc.num[global$year==2022]*inctot.hi

inc.women=sum(globsplt$inc[globsplt$sex=="F"&globsplt$child==F])/global$inc.num[global$year==2022]*inctot
inc.women.lo=sum(globsplt$inc[globsplt$sex=="F"&globsplt$child==F])/global$inc.num[global$year==2022]*inctot.lo
inc.women.hi=sum(globsplt$inc[globsplt$sex=="F"&globsplt$child==F])/global$inc.num[global$year==2022]*inctot.hi

inc.child=sum(globsplt$inc[globsplt$child==T])/global$inc.num[global$year==2022]*inctot
inc.child.lo=sum(globsplt$inc[globsplt$child==T])/global$inc.num[global$year==2022]*inctot.lo
inc.child.hi=sum(globsplt$inc[globsplt$child==T])/global$inc.num[global$year==2022]*inctot.hi

# Split mortality

#Global

death22=global$mort.nh.num[global$year==2022]

mort.split=Mglobsplt %>% pivot_wider(names_from = sex, values_from = mort)
mort.split$M=round(mort.split$M*death22,1)
mort.split$F=-round(mort.split$F*death22,1)
mort.split$i=seq(1,dim(mort.split)[1],1)
mort.split=mort.split %>% mutate(age=ifelse(age=="65plus","\u226565",as.character(age)))
mort.split=mort.split %>% arrange(rev(i))


# Global HIV pos
death22.h=global$mort.h.num[global$year==2022]

mort.split.h=MPglobsplit %>% pivot_wider(names_from = sex, values_from = deaths.pc)
mort.split.h$M=round(mort.split.h$M/100*death22.h,1)
mort.split.h$F=-round(mort.split.h$F/100*death22.h,1)
mort.split.h$i=seq(1,dim(mort.split.h)[1],1)
mort.split.h=mort.split.h %>% mutate(acat=ifelse(acat=="0-14","<15","\u226515"))
mort.split.h=mort.split.h %>% arrange(rev(i))


# By region

deaths22_afr=regional$mort.nh.num[regional$year==2022 & regional$g.whoregion=="AFR"]
deaths22_amr=regional$mort.nh.num[regional$year==2022 & regional$g.whoregion=="AMR"]
deaths22_emr=regional$mort.nh.num[regional$year==2022 & regional$g.whoregion=="EMR"]
deaths22_eur=regional$mort.nh.num[regional$year==2022 & regional$g.whoregion=="EUR"]
deaths22_sea=regional$mort.nh.num[regional$year==2022 & regional$g.whoregion=="SEA"]
deaths22_wpr=regional$mort.nh.num[regional$year==2022 & regional$g.whoregion=="WPR"]


mort.split.reg=Mregsplt %>% pivot_wider(names_from = sex, values_from = mort)

mort.split.reg$M[mort.split.reg$g.whoregion=="AFR"]=round(mort.split.reg$M[mort.split.reg$g.whoregion=="AFR"]*deaths22_afr,1)
mort.split.reg$F[mort.split.reg$g.whoregion=="AFR"]=-round(mort.split.reg$F[mort.split.reg$g.whoregion=="AFR"]*deaths22_afr,1)

mort.split.reg$M[mort.split.reg$g.whoregion=="AMR"]=round(mort.split.reg$M[mort.split.reg$g.whoregion=="AMR"]*deaths22_amr,1)
mort.split.reg$F[mort.split.reg$g.whoregion=="AMR"]=-round(mort.split.reg$F[mort.split.reg$g.whoregion=="AMR"]*deaths22_amr,1)

mort.split.reg$M[mort.split.reg$g.whoregion=="EMR"]=round(mort.split.reg$M[mort.split.reg$g.whoregion=="EMR"]*deaths22_emr,1)
mort.split.reg$F[mort.split.reg$g.whoregion=="EMR"]=-round(mort.split.reg$F[mort.split.reg$g.whoregion=="EMR"]*deaths22_emr,1)

mort.split.reg$M[mort.split.reg$g.whoregion=="EUR"]=round(mort.split.reg$M[mort.split.reg$g.whoregion=="EUR"]*deaths22_eur,1)
mort.split.reg$F[mort.split.reg$g.whoregion=="EUR"]=-round(mort.split.reg$F[mort.split.reg$g.whoregion=="EUR"]*deaths22_eur,1)

mort.split.reg$M[mort.split.reg$g.whoregion=="SEA"]=round(mort.split.reg$M[mort.split.reg$g.whoregion=="SEA"]*deaths22_sea,1)
mort.split.reg$F[mort.split.reg$g.whoregion=="SEA"]=-round(mort.split.reg$F[mort.split.reg$g.whoregion=="SEA"]*deaths22_sea,1)

mort.split.reg$M[mort.split.reg$g.whoregion=="WPR"]=round(mort.split.reg$M[mort.split.reg$g.whoregion=="WPR"]*deaths22_wpr,1)
mort.split.reg$F[mort.split.reg$g.whoregion=="WPR"]=-round(mort.split.reg$F[mort.split.reg$g.whoregion=="WPR"]*deaths22_wpr,1)


mort.split.reg.afr=subset(mort.split.reg,g.whoregion=="AFR")
mort.split.reg.afr$i=seq(1,dim(mort.split.reg.afr)[1],1)
mort.split.reg.afr=mort.split.reg.afr %>% mutate(age=ifelse(age=="65plus","\u226565",as.character(age)))
mort.split.reg.afr=mort.split.reg.afr %>% arrange(rev(i))

mort.split.reg.amr=subset(mort.split.reg,g.whoregion=="AMR")
mort.split.reg.amr$i=seq(1,dim(mort.split.reg.amr)[1],1)
mort.split.reg.amr=mort.split.reg.amr %>% mutate(age=ifelse(age=="65plus","\u226565",as.character(age)))
mort.split.reg.amr=mort.split.reg.amr %>% arrange(rev(i))

mort.split.reg.emr=subset(mort.split.reg,g.whoregion=="EMR")
mort.split.reg.emr$i=seq(1,dim(mort.split.reg.emr)[1],1)
mort.split.reg.emr=mort.split.reg.emr %>% mutate(age=ifelse(age=="65plus","\u226565",as.character(age)))
mort.split.reg.emr=mort.split.reg.emr %>% arrange(rev(i))

mort.split.reg.eur=subset(mort.split.reg,g.whoregion=="EUR")
mort.split.reg.eur$i=seq(1,dim(mort.split.reg.eur)[1],1)
mort.split.reg.eur=mort.split.reg.eur %>% mutate(age=ifelse(age=="65plus","\u226565",as.character(age)))
mort.split.reg.eur=mort.split.reg.eur %>% arrange(rev(i))

mort.split.reg.sea=subset(mort.split.reg,g.whoregion=="SEA")
mort.split.reg.sea$i=seq(1,dim(mort.split.reg.sea)[1],1)
mort.split.reg.sea=mort.split.reg.sea %>% mutate(age=ifelse(age=="65plus","\u226565",as.character(age)))
mort.split.reg.sea=mort.split.reg.sea %>% arrange(rev(i))

mort.split.reg.wpr=subset(mort.split.reg,g.whoregion=="WPR")
mort.split.reg.wpr$i=seq(1,dim(mort.split.reg.wpr)[1],1)
mort.split.reg.wpr=mort.split.reg.wpr %>% mutate(age=ifelse(age=="65plus","\u226565",as.character(age)))
mort.split.reg.wpr=mort.split.reg.wpr %>% arrange(rev(i))




## Excess

# Excess

dta=excessmort

dta$mort.diff.num=dta$mort.num-dta$mort.cf.num
dta$mort.diff.lo.num=dta$mort.lo.num-dta$mort.cf.num
dta$mort.diff.hi.num=dta$mort.hi.num-dta$mort.cf.num

excess=sum(dta$mort.diff.num[dta$year==2020],dta$mort.diff.num[dta$year==2021],dta$mort.diff.num[dta$year==2022])
excess.lo=sum(dta$mort.diff.lo.num[dta$year==2020],dta$mort.diff.lo.num[dta$year==2021],dta$mort.diff.lo.num[dta$year==2022])
excess.hi=sum(dta$mort.diff.hi.num[dta$year==2020],dta$mort.diff.hi.num[dta$year==2021],dta$mort.diff.hi.num[dta$year==2022])




### TB deaths in children



death22=global$mort.nh.num[global$year==2022]
death22.lo=global$mort.nh.lo.num[global$year==2022]
death22.hi=global$mort.nh.hi.num[global$year==2022]

m.nh.l5=sum(Mglobsplt$mort[Mglobsplt$age=="0-4"])*death22
m.nh.l5.lo=sum(Mglobsplt$mort[Mglobsplt$age=="0-4"])*death22.lo
m.nh.l5.hi=sum(Mglobsplt$mort[Mglobsplt$age=="0-4"])*death22.hi

m.nh.5.14=sum(Mglobsplt$mort[Mglobsplt$age=="5-14"])*death22
m.nh.5.14.lo=sum(Mglobsplt$mort[Mglobsplt$age=="5-14"])*death22.lo
m.nh.5.14.hi=sum(Mglobsplt$mort[Mglobsplt$age=="5-14"])*death22.hi

m.nh.tot=m.nh.l5+m.nh.5.14
m.nh.tot.lo=m.nh.l5.lo+m.nh.5.14.lo
m.nh.tot.hi=m.nh.l5.hi+m.nh.5.14.hi


death22.h=global$mort.h.num[global$year==2022]
death22.h.lo=global$mort.h.lo.num[global$year==2022]
death22.h.hi=global$mort.h.hi.num[global$year==2022]

m.h.tot=sum(MPglobsplit$deaths.pc[MPglobsplit$acat=="0-14"])/100*death22.h
m.h.tot.lo=sum(MPglobsplit$deaths.pc[MPglobsplit$acat=="0-14"])/100*death22.h.lo
m.h.tot.hi=sum(MPglobsplit$deaths.pc[MPglobsplit$acat=="0-14"])/100*death22.h.hi


m.tot.children=m.nh.tot+m.h.tot
m.tot.children.lo=m.nh.tot.lo+m.h.tot.lo
m.tot.children.hi=m.nh.tot.hi+m.h.tot.hi



### TB deaths in adults



death22=global$mort.nh.num[global$year==2022]
death22.lo=global$mort.nh.lo.num[global$year==2022]
death22.hi=global$mort.nh.hi.num[global$year==2022]

m.nh.male=sum(Mglobsplt$mort[Mglobsplt$child==F & Mglobsplt$sex=="M"])*death22
m.nh.male.lo=sum(Mglobsplt$mort[Mglobsplt$child==F & Mglobsplt$sex=="M"])*death22.lo
m.nh.male.hi=sum(Mglobsplt$mort[Mglobsplt$child==F & Mglobsplt$sex=="M"])*death22.hi

m.nh.fem=sum(Mglobsplt$mort[Mglobsplt$child==F & Mglobsplt$sex=="F"])*death22
m.nh.fem.lo=sum(Mglobsplt$mort[Mglobsplt$child==F & Mglobsplt$sex=="F"])*death22.lo
m.nh.fem.hi=sum(Mglobsplt$mort[Mglobsplt$child==F & Mglobsplt$sex=="F"])*death22.hi

death22.h=global$mort.h.num[global$year==2022]
death22.h.lo=global$mort.h.lo.num[global$year==2022]
death22.h.hi=global$mort.h.hi.num[global$year==2022]

m.h.male=sum(MPglobsplit$deaths.pc[MPglobsplit$acat=="15+" & MPglobsplit$sex=="M"])/100*death22.h
m.h.male.lo=sum(MPglobsplit$deaths.pc[MPglobsplit$acat=="15+" & MPglobsplit$sex=="M"])/100*death22.h.lo
m.h.male.hi=sum(MPglobsplit$deaths.pc[MPglobsplit$acat=="15+" & MPglobsplit$sex=="M"])/100*death22.h.hi

m.h.fem=sum(MPglobsplit$deaths.pc[MPglobsplit$acat=="15+" & MPglobsplit$sex=="F"])/100*death22.h
m.h.fem.lo=sum(MPglobsplit$deaths.pc[MPglobsplit$acat=="15+" & MPglobsplit$sex=="F"])/100*death22.h.lo
m.h.fem.hi=sum(MPglobsplit$deaths.pc[MPglobsplit$acat=="15+" & MPglobsplit$sex=="F"])/100*death22.h.hi


m.tot.male=m.nh.male+m.h.male
m.tot.male.lo=m.nh.male.lo+m.h.male.lo
m.tot.male.hi=m.nh.male.hi+m.h.male.hi

m.tot.fem=m.nh.fem+m.h.fem
m.tot.fem.lo=m.nh.fem.lo+m.h.fem.lo
m.tot.fem.hi=m.nh.fem.hi+m.h.fem.hi


