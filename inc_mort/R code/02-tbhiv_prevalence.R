#' ---
#' title: HIV among TB
#' author: Mathieu Bastard, Philippe Glaziou
#' date: 2023-06-06
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

#' (Last updated: `r Sys.Date()`)
#'
#' # Prevalence of HIV among TB cases
#'
#' This script estimates the prevalence of HIV among TB cases using the notifications data
#' and non-routine surveillance data (HIV surveys among TB and HIV sentinel surveillance with TB
#' defining a sentinel group) reported by countries. 
#' 
#' The historical data on TBHIV suffers from severe quality issues (implausible values, missing data), 
#' it should be consolidated in a manner similar to the DR data, through a process of documenting 
#' and rejecting all implausible and outlier values (e.g. ZAF 2020 prev of HIV in routinely tested 
#' suddenly increased from less than 60% to over 70% while testing coverage dropped from over 
#' 80% to around 60% due to covid disruptions -- that prev value should be rejected (see around 
#' line 433) and an estimate imputed through interpolation). Many data entry errors are c
#' orrected/rejecte-d in this script, an error-prone and unsatisfactory approach to 
#' consolidating the data. Most outliers and implausible values are spotted using graphic 
#' checks of the empirical data rather than though a systematic statistical approach 
#' applied to time-series.
#' 
#' Priority is given to the survey data (though very few country-year 
#' datapoints are reported overall) particularly where testing coverage is low (e.g. early KHM reports), 
#' but there are a number of routine testing results suspected to be reported as 
#' survey results possibly due to misunderstandings of relevant questions in the data collection
#' form. A threshold of 60% routine testing coverage is used (see around line 119). 
#' When both routine (coverage > threshold) and sentinel data are available, country-year points 
#' are combined in a statistical ensemble. Missing values surrounded (in country time-series) with 
#' valid values are interpolated. In previous years, missing tbhiv were indirectly imputed using
#' a predictive model. This year, the output from previous years is reused.
#' 
#' *In future exercises, this script should be reworked from scratch instead of reused/adapted, 
#' following a careful and documented consolidation of the empirical data in a process similar to the 
#' one used on the DR data.*
#'
#'
#' Dependences: 
#'  ~/data: tb.rda (notifications), sty.rda (non routine TBHIV data), 
#'  old.rda (reuse of previous estimates accounting for "bad" data that have not been 
#'  subsequently corrected)
#'  
#'  ~/R/fun.R utility functions
#'  
#'  library zoo for a selection of its time-series functions
#'  library data.table and here as in all other R scripts in this folder
#'
#'
#' Output:
#' - tbhiv  (both a dated csv file and an rda file that may rewrite a previous version)
#' - sets of graphical checks in ~/output/checks
#' 


rm(list=ls())


# Load libraries and data
#

suppressMessages(library(data.table))
suppressMessages(library(here))
suppressMessages(library(zoo))

#Run Functions script
source(here('inc_mort/R code/fun.R'))

#Function to import db from GTB databases
source(here("import/load_gtb.R"))


#Load notification and non-routine TB/HIV data
tb <- load_gtb("tb",convert_dots = FALSE)
sty <- load_gtb("sty",convert_dots = FALSE)


#Load previous year estimate data
#Seems not used here

load(here('inc_mort/estimates2022/old.rda'))


tbhiv <-
  merge(tb[year > 1999, .(
    iso3,
    g.whoregion,
    year,
    newinc,
    c.newinc,
    newrel.hivpos,
    newrel.hivtest,
    newrel.art,
    hivtest.f,
    hivtest.p,
    hivtest.pos.f,
    hivtest.pos.p,
    hiv.art.f,
    hiv.art.p,
    newrel.tbhiv.flg,
    c.notified
  )],
  sty[year > 1999, .(
    iso3,
    year,
    tbhiv.surv.prev = tbhiv.surv.prev / 100,
    tbhiv.surv.yr,
    tbhiv.surv.cil = tbhiv.surv.cil / 100,
    tbhiv.surv.ciu = tbhiv.surv.ciu / 100,
    tbhiv.sentin.prev = tbhiv.sentin.prev / 100,
    tbhiv.sentin.yr,
    tbhiv.sentin.cil = tbhiv.sentin.cil / 100,
    tbhiv.sentin.ciu = tbhiv.sentin.cil / 100
  )],
  by = c('iso3', 'year'))

(dim(tbhiv))


#' # Routine HIV testing coverage
#'
#' **testing coverage threshold**
#'
threshold <- 0.6

tbhiv[, hivtest.coverage := hivtest.f / c.notified]
tbhiv[year == 2014, hivtest.coverage := hivtest.p / c.notified]
tbhiv[year > 2014, hivtest.coverage := newrel.hivtest / c.notified]
(summary(tbhiv$hivtest.coverage))
(tbhiv[!is.na(hivtest.coverage), sum(hivtest.coverage > threshold)])


# # Surveys
#
# check reported survey years
#
(tbhiv[!is.na(tbhiv.surv.prev) &
         tbhiv.surv.yr != year, list(iso3,
                                     year,
                                     tbhiv.surv.yr,
                                     tbhiv.surv.prev,
                                     tbhiv.surv.cil,
                                     tbhiv.surv.ciu)])


# move survey results to their correct row wherever years are inconsistent
sel <-
  !is.na(tbhiv$tbhiv.surv.prev) &
  !is.na(tbhiv$tbhiv.surv.yr) & tbhiv$tbhiv.surv.yr != tbhiv$year
(table(sel))

(tbhiv[sel, .(iso3,
              year,
              tbhiv.surv.yr,
              tbhiv.surv.prev,
              tbhiv.surv.cil,
              tbhiv.surv.ciu)])

tmp <-
  tbhiv[sel, list(
    iso3,
    year = tbhiv.surv.yr,
    tbhiv.surv = tbhiv.surv.prev,
    tbhiv.surv.lo = tbhiv.surv.cil,
    tbhiv.surv.hi = tbhiv.surv.ciu
  )]

#Remove duplicates
tmp <- tmp[!duplicated(iso3, tbhiv.surv)]

#Remove error or when estimate < or =0
(tmp <- tmp[tbhiv.surv > 0])



tbhiv2 <- merge(
  tbhiv,
  tmp[iso3 != 'CUW'],
  by = c('iso3', 'year'),
  all.x = T,
  all.y = T
)
dim(tbhiv2)
dim(tbhiv)

tbhiv2[is.na(tbhiv.surv), tbhiv.surv := tbhiv.surv.prev]
tbhiv2[is.na(tbhiv.surv.lo), tbhiv.surv.lo := tbhiv.surv.cil]
tbhiv2[is.na(tbhiv.surv.hi), tbhiv.surv.hi := tbhiv.surv.ciu]

(tbhiv2[sel, list(iso3,
                  year,
                  tbhiv.surv,
                  tbhiv.surv.lo,
                  tbhiv.surv.hi,
                  tbhiv.surv.prev)])

#Check estimate = 0
sel <- tbhiv2$tbhiv.surv == 0
(tbhiv2[sel, .(iso3,
               year,
               tbhiv.surv,
               tbhiv.surv.lo,
               tbhiv.surv.hi,
               tbhiv.surv.prev)])

#Set estimate and CI to NA
tbhiv2[sel, tbhiv.surv := NA]
tbhiv2[sel, tbhiv.surv.lo := NA]
tbhiv2[sel, tbhiv.surv.hi := NA]

#Idem if =1, set to NA
sel <- tbhiv2$tbhiv.surv == 1
(tbhiv2[sel, .(iso3,
               year,
               tbhiv.surv,
               tbhiv.surv.lo,
               tbhiv.surv.hi,
               tbhiv.surv.prev)])
tbhiv2[sel, tbhiv.surv := NA]
tbhiv2[sel, tbhiv.surv.lo := NA]
tbhiv2[sel, tbhiv.surv.hi := NA]

#Check if >1
sel <- tbhiv2$tbhiv.surv > 1
(tbhiv2[sel, .(iso3,
               year,
               tbhiv.surv,
               tbhiv.surv.lo,
               tbhiv.surv.hi,
               tbhiv.surv.prev)])


# fix data entry errors
#
rsurv <- function(iso, yrs) {
  sel <- tbhiv2$iso3 == iso & tbhiv2$year %in% yrs
  tbhiv2[sel, tbhiv.surv := NA]
  tbhiv2[sel, tbhiv.surv.lo := NA]
  tbhiv2[sel, tbhiv.surv.hi := NA]
}
rsurv('EGY', 2007)
rsurv('BFA', 2002)
rsurv('CAN', 2007:2008)
rsurv('CMR', 2008:2009)
rsurv('CUB', 2009)
rsurv('DOM', 2007:2011)
rsurv('DZA', 2007:2011)
rsurv('GHA', 2007:2008)
rsurv('GIN', 2009)
rsurv('KAZ', 2008)
rsurv('LKA', 2011)
rsurv('MAR', 2008:2009)
rsurv('MLI', 2008)
rsurv('MRT', c(2003, 2007))
rsurv('NER', 2011)
rsurv('TJK', 2008)
rsurv('KHM', 2007)
rsurv('KIR', 2009)



# fix inconsistent bounds (arbitrary +/- 20% of best estimate)
#
#
tbhiv2[!is.na(tbhiv.surv), test.isbinom(tbhiv.surv)]

#If one of the CI bound is NA, fix it to +/-20% of best estimate
sel <-
  !is.na(tbhiv2$tbhiv.surv) &
  (is.na(tbhiv2$tbhiv.surv.lo) | is.na(tbhiv2$tbhiv.surv.hi))
(tbhiv2[sel, list(iso3,
                  year,
                  tbhiv.surv,
                  tbhiv.surv.lo,
                  tbhiv.surv.hi)])
tbhiv2[sel, tbhiv.surv.lo := tbhiv.surv * 0.8]
tbhiv2[sel, tbhiv.surv.hi := pmin(tbhiv.surv * 1.2, 1)]

#If lower bound of CT > estiamte or higher bound < estimate, then replace by +/-20% of estimate
sel <-
  (tbhiv2$tbhiv.surv.lo > tbhiv2$tbhiv.surv) |
  (tbhiv2$tbhiv.surv.hi < tbhiv2$tbhiv.surv)
(tbhiv2[sel, list(iso3,
                  year,
                  tbhiv.surv,
                  tbhiv.surv.lo,
                  tbhiv.surv.hi)])
tbhiv2[sel, tbhiv.surv.lo := tbhiv.surv * 0.8]
tbhiv2[sel, tbhiv.surv.hi := pmin(tbhiv.surv * 1.2, 1)]

tbhiv2[!is.na(tbhiv.surv), test.bounds(tbhiv.surv, tbhiv.surv.lo, tbhiv.surv.hi)]






# # Sentinel
#
# move sentinel results to the correct row if reported years are inconsistent
sel <-
  !is.na(tbhiv2$tbhiv.sentin.prev) &
  tbhiv2$tbhiv.sentin.yr != tbhiv2$year
(tbhiv2[sel, .(iso3,
               year,
               tbhiv.sentin.yr,
               tbhiv.sentin.prev,
               tbhiv.sentin.cil,
               tbhiv.surv.ciu)])

tmp <-
  tbhiv2[sel, list(
    iso3,
    year = tbhiv.sentin.yr,
    tbhiv.sentin = tbhiv.sentin.prev,
    tbhiv.sentin.lo = tbhiv.sentin.cil,
    tbhiv.sentin.hi = tbhiv.sentin.ciu
  )]
(dim(tmp))
tmp <- tmp[!duplicated(tmp$iso3, tmp$tbhiv.sdntin)] # remove dups
(dim(tmp))
tmp <- tmp[year != 0]
(dim(tmp))
(tmp)


tbhiv3 <- merge(
  tbhiv2,
  tmp,
  by = c('iso3', 'year'),
  all.x = T,
  all.y = T
)
dim(tbhiv2)
dim(tbhiv3)

tbhiv3[is.na(tbhiv.sentin), tbhiv.sentin := tbhiv.sentin.prev]
tbhiv3[is.na(tbhiv.sentin.lo), tbhiv.sentin.lo := tbhiv.sentin.cil]
tbhiv3[is.na(tbhiv.sentin.hi), tbhiv.sentin.hi := tbhiv.sentin.ciu]

(tbhiv3[sel, list(iso3,
                  year,
                  tbhiv.sentin,
                  tbhiv.sentin.lo,
                  tbhiv.sentin.hi,
                  tbhiv.sentin.prev)])
sel <- tbhiv3$tbhiv.sentin == 0
tbhiv3[sel, tbhiv.sentin := NA]
tbhiv3[sel, tbhiv.sentin.lo := NA]
tbhiv3[sel, tbhiv.sentin.hi := NA]


# fix a selection of data entry errors
#
sel <- tbhiv3$iso3 == 'LBN' & tbhiv$year == 2011
tbhiv3[sel, tbhiv.sentin := NA]
tbhiv3[sel, tbhiv.sentin.lo := NA]
tbhiv3[sel, tbhiv.sentin.hi := NA]

sel <- tbhiv3$iso3 == 'ERI' & tbhiv$year == 2007
tbhiv3[sel, tbhiv.sentin := NA]
tbhiv3[sel, tbhiv.sentin.lo := NA]
tbhiv3[sel, tbhiv.sentin.hi := NA]

sel <- tbhiv3$iso3 == 'CHN' & tbhiv$year == 2012
tbhiv3[sel, tbhiv.sentin := NA]
tbhiv3[sel, tbhiv.sentin.lo := NA]
tbhiv3[sel, tbhiv.sentin.hi := NA]



# fix inconsistent bounds (arbitrary +/- 20% of best estimate)
#
#
tbhiv3[!is.na(tbhiv.sentin), test.isbinom(tbhiv.sentin)]

sel <-
  !is.na(tbhiv2$tbhiv.sentin) &
  (is.na(tbhiv2$tbhiv.sentin.lo) | is.na(tbhiv2$tbhiv.sentin.hi))
(table(sel))
sel <-
  (tbhiv3$tbhiv.sentin.lo > tbhiv2$tbhiv.sentin) |
  (tbhiv2$tbhiv.sentin.hi < tbhiv2$tbhiv.sentin)
table(sel)
sel <-
  !is.na(tbhiv3$tbhiv.sentin &
           (
             is.na(tbhiv3$tbhiv.sentin.lo) | is.na(tbhiv3$tbhiv.sentin.hi)
           ))
(table(sel))

tbhiv3[sel, tbhiv.sentin.lo := tbhiv.sentin * 0.8]
tbhiv3[sel, tbhiv.sentin.hi := pmin(tbhiv.sentin * 1.2, 1)]

tbhiv3[!is.na(tbhiv.sentin), test.bounds(tbhiv.sentin, tbhiv.sentin.lo, tbhiv.sentin.hi)]





# # routine testing
#
tbhiv3[, tbhiv.routine := hivtest.pos.f / hivtest.f]
tbhiv3[year == 2014, tbhiv.routine := hivtest.pos.p / hivtest.p]
tbhiv3[year > 2014, tbhiv.routine := newrel.hivpos / newrel.hivtest]




# # remove other outliers
#
# MMR
#
sel <- tbhiv3$iso3 == 'MMR' & tbhiv3$year == 2012
tbhiv3$tbhiv.surv[sel] <- NA
tbhiv3$tbhiv.surv.lo[sel] <- NA
tbhiv3$tbhiv.surv.hi[sel] <- NA
sel <- tbhiv3$iso3 == 'MMR' & tbhiv3$year == 2008
tbhiv3$tbhiv.routine[sel] <- NA

# AGO
#
sel <- tbhiv3$iso3 == 'AGO' & tbhiv3$year == 2010
tbhiv3$tbhiv.routine[sel] <- NA

# BFA
#
sel <- tbhiv3$iso3 == 'BFA' & tbhiv3$year == 2004
tbhiv3$tbhiv.routine[sel] <- NA

# PER
#
sel <- tbhiv3$iso3 == 'PER' & tbhiv3$year == 2005
tbhiv3$tbhiv.routine[sel] <- NA

# ARG
#
sel <- tbhiv3$iso3 == 'ARG' & tbhiv3$year == 2006
tbhiv3$tbhiv.routine[sel] <- NA

# LBR
#
sel <- tbhiv3$iso3 == 'LBR' & tbhiv3$year == 2008
tbhiv3$tbhiv.routine[sel] <- NA

# BTN
#
sel <- tbhiv3$iso3 == 'BTN' & tbhiv3$year == 2012
tbhiv3$tbhiv.routine[sel] <- NA

# GNQ
#
sel <- tbhiv3$iso3 == 'GNQ' & tbhiv3$year == 2008
tbhiv3$tbhiv.routine[sel] <- NA


# ZAF 2020
#
sel <- tbhiv3$iso3 == 'ZAF' & tbhiv3$year == 2020
tbhiv3$tbhiv.routine[sel] <- NA



# wrong entries on surveys
#
sel <- tbhiv3$iso3 %in% c('LTU', 'LBN', 'ASM', 'ZMB', 'ZWE')
tbhiv3$tbhiv.surv[sel] <-
  tbhiv3$tbhiv.surv.lo[sel] <- tbhiv3$tbhiv.surv.hi[sel] <- NA
tbhiv3[is.infinite(tbhiv.routine), tbhiv.routine := NA]
tbhiv3[is.infinite(hivtest.coverage), hivtest.coverage := NA]
tbhiv3[hivtest.coverage > 1, hivtest.coverage := 1]



# check
#
tbhiv3[!is.na(tbhiv.routine), test.ispos(tbhiv.routine)]
tbhiv3[!is.na(tbhiv.routine), test.AgeB(1, tbhiv.routine)]
tbhiv3[!is.na(hivtest.coverage), test.ispos(hivtest.coverage)]
tbhiv3[!is.na(hivtest.coverage), test.AgeB(1, hivtest.coverage)]



# do not use routine if survey results
#
tbhiv3[, tbhiv.routine.ok := tbhiv.routine]
tbhiv3[hivtest.coverage < threshold &
         !is.na(hivtest.coverage), tbhiv.routine.ok := NA]
tbhiv3[is.na(hivtest.coverage) |
         !is.na(tbhiv.surv), tbhiv.routine.ok := NA]


# add last data point in PNG (52% cov)
#
# sel <- tbhiv3$iso3 == 'PNG' & tbhiv3$year == 2018
# tbhiv3[sel, tbhiv.routine.ok := tbhiv.routine]



# data entry errors
#
sel <- tbhiv3$iso3 == 'RUS' & tbhiv3$year == 2007
tbhiv3$tbhiv.routine.ok[sel] <- NA



# clean-up
#
est <- copy(tbhiv3)
est[, tbhiv.surv.cil := NULL]
est[, tbhiv.surv.ciu := NULL]
est[, tbhiv.sentin.cil := NULL]
est[, tbhiv.sentin.ciu := NULL]




# binomial errors
#
sel <- !is.na(est$tbhiv.routine.ok)
table(sel)

out <-
  est[sel, {
    tmp = binom.test(round(tbhiv.routine.ok * c.newinc), c.newinc)$conf.int

    list(tbhiv.routine.sd = (tmp[2] - tmp[1]) / 3.92)
  },
  by = .(iso3, year)]
est2 <- merge(est, out, by = c('iso3', 'year'), all.x = TRUE)
(dim(est))
(dim(est2))

est2[!is.na(tbhiv.surv), tbhiv.surv.sd := (tbhiv.surv.hi - tbhiv.surv.lo) /
       3.92]
est2[!is.na(tbhiv.sentin), tbhiv.sentin.sd := (tbhiv.sentin.hi - tbhiv.sentin.lo) /
       3.92]

sel <- est2$tbhiv.surv.sd == 0
table(sel)

est2[sel, tbhiv.surv.sd := NA]
est2[sel, tbhiv.surv := NA]
est2[sel, tbhiv.surv.lo := NA]
est2[sel, tbhiv.surv.hi := NA]

sel <- est2$tbhiv.sentin.sd == 0
table(sel)



# check SDs are positive
#
est2[!is.na(tbhiv.routine.sd), test.ispos(tbhiv.routine.sd)]
est2[!is.na(tbhiv.surv.sd), test.ispos(tbhiv.surv.sd)]
est2[!is.na(tbhiv.sentin.sd), test.ispos(tbhiv.sentin.sd)]



# check all tbhiv are in [0, 1]
#
est2[!is.na(tbhiv.routine.ok), test.isbinom(tbhiv.routine.ok)]
est2[!is.na(tbhiv.surv), test.isbinom(tbhiv.surv)]
est2[!is.na(tbhiv.sentin), test.isbinom(tbhiv.sentin)]




# # survey data takes precedence as a source on tbhiv
#
sel <- !is.na(est2$tbhiv.surv) & est2$tbhiv.surv > 0
(table(sel))
(table(sel & !is.na(est2$tbhiv.routine.ok)))

est2[sel, tbhiv := tbhiv.surv]
est2[sel, tbhiv.sd := tbhiv.surv.sd]
(est2[tbhiv.surv == 0, .N])


# # combine data-sources on HIV prevalence in TB
#
sel <-
  is.na(est2$tbhiv) &
  (!is.na(est2$tbhiv.routine.ok) |
     !is.na(est2$tbhiv.sentin))
(table(sel))

sel2 <-
  (is.na(est2$tbhiv) &
     est2$tbhiv.routine.ok > 0 & est2$tbhiv.routine.ok < 1) &
  (is.na(est2$tbhiv.surv) |
     (est2$tbhiv.sentin > 0 | is.na(est2$tbhiv.sentin)))
(table(sel2))


# use an ensemble of beta distributions if possible
#
out <-
  est2[sel2, {
    tmp = ensbeta(na.omit(c(tbhiv.routine.ok, tbhiv.sentin)),
                  na.omit(c(tbhiv.routine.sd, tbhiv.sentin.sd)))

    list(tbhiv = tmp$post.ev,
         tbhiv.sd = tmp$post.sd)
  },
  by = .(iso3, year)]

est2[!is.na(tbhiv), .N]
est2[sel2, tbhiv := out$tbhiv]
est2[sel2, tbhiv.sd := out$tbhiv.sd]
est2[!is.na(tbhiv), .N]
est2[!is.na(tbhiv.sd), .N]

est3 <- copy(est2)


# ensemble not possible
#
sel3 <-
  is.na(est3$tbhiv) &
  ((est3$tbhiv.routine.ok == 0 |
      est3$tbhiv.routine.ok == 1) &
     !is.na(est3$tbhiv.sentin))
(table(sel3))

(est3[sel3, .(
  iso3,
  year,
  tbhiv.routine.ok,
  tbhiv.routine.sd,
  tbhiv.sentin,
  tbhiv.sentin.sd,
  tbhiv.surv,
  tbhiv.surv.sd,
  tbhiv,
  tbhiv.sd
)])
est3[sel3, tbhiv := tbhiv.routine.ok]
est3[sel3, tbhiv.sd := tbhiv.routine.sd]


sel4 <-
  is.na(est3$tbhiv) &
  is.na(est3$tbhiv.routine.ok) &
  !is.na(est3$tbhiv.sentin) &
  est3$tbhiv.sentin > 0
(table(sel4))
(est3[sel4, .(
  iso3,
  year,
  tbhiv.routine.ok,
  tbhiv.routine.sd,
  tbhiv.sentin,
  tbhiv.sentin.sd,
  tbhiv.surv,
  tbhiv.surv.sd,
  tbhiv,
  tbhiv.sd
)])
est3[sel4, tbhiv := tbhiv.sentin]
est3[sel4, tbhiv.sd := tbhiv.sentin.sd]

sel5 <- is.na(est3$tbhiv) & !is.na(est3$tbhiv.routine.ok)
(table(sel5))
est3[sel5, tbhiv := tbhiv.routine.ok]
est3[sel5, tbhiv.sd := tbhiv.routine.sd]

est3[!is.na(tbhiv), .N]
est3[!is.na(tbhiv.sd), .N]


# check
#
est3[!is.na(tbhiv), test.isbinom(tbhiv)]
est3[!is.na(tbhiv), test.isbinom(tbhiv.sd)]

tbhiv <- copy(est3)
tbhiv <- setkey(tbhiv, iso3, year)



# aggregates
#
# sum of notifications where there is a valid TBHIV measure
#
(tbhiv[!is.na(tbhiv), .(c.newinc = sums(c.newinc)), by = year][order(year)])

# global prev of HIV among TB
#
(tbhiv[, .(tbhiv = weighted.mean(tbhiv,
                                 w = c.newinc,
                                 na.rm = TRUE)), by =
         year])



# Fix CZE as per their feedback, not use TBHIV routine 
sel=tbhiv$iso3=="CZE" & tbhiv$year==2022
tbhiv[sel, tbhiv:=NA]
tbhiv[sel, tbhiv.sd:=NA]

# # Interpolation of missing values
#
# no interpolation before first value or after last value
#
tbhiv[, ntbhiv := sum(!is.na(tbhiv)), by = iso3]
tbhiv[ntbhiv > 2, itbhiv := zoo::na.approx(tbhiv, na.rm = FALSE), by = 'iso3']
tbhiv[, ntbhiv := NULL]





# plot
#
wr <- unique(as.character(tbhiv$g.whoregion))

#+ fig.width=16, fig.height=12
for (i in wr) {
  p <-
    qplot(
      year,
      0,
      data = subset(tbhiv, g.whoregion == i),
      geom = 'line',
      colour = I('grey90')
    ) +
    geom_point(aes(year, tbhiv.routine),
               colour = I('black'),
               shape = I(4)) +
    geom_point(aes(year, tbhiv.routine.ok),
               colour = I('blue'),
               shape = I(4)) +
    geom_point(aes(year, tbhiv.surv),
               colour = I('green'),
               shape = I(2)) +
    geom_point(aes(year, tbhiv.sentin),
               colour = I('red'),
               shape = I(3)) +
    geom_line(aes(year, tbhiv), colour = I('blue')) +
    geom_line(aes(year, itbhiv), colour = I('red')) +
    geom_point(aes(year, tbhiv),
               colour = I('blue'),
               shape = I(1)) +
    facet_wrap( ~ iso3, scales = 'free_y') + xlab('') +
    ylab('HIV prevalence in TB') +
    theme_bw(base_size = 14)
  suppressWarnings(print(p))
  suppressWarnings(ggsave(here(
    paste('inc_mort/output/checks/hiv_', i, '_surv.pdf', sep = '')
  ),
  width = 14,
  height = 8))
}




# save
#
attr(tbhiv, "timestamp") <- Sys.Date() #set date
save(tbhiv, file = here('inc_mort/analysis/tbhiv.rda'))
fwrite(tbhiv, file = here(paste0('inc_mort/analysis/csv/tbhiv_', Sys.Date(), '.csv')))
detach(package:zoo)



