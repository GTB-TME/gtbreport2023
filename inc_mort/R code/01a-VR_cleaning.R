#' ---
#' title: VR data
#' author: Mathieu Bastard, Philippe Glaziou, Anna Dean
#' date: 2023/06/06
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

#' This scripts pre-processes VR data from the global WHO mortality database, 
#' stored in an excel file.
#' 
#' 
#' Deps: libraries data.table, imputeTS, haven, readxl, stringr, here
#' 
#' 
#' Input: excel file in ~/input/VR (VR data)
#'        VR quality codes published by WHO in WHS: ~/input/VR/vrqual.csv
#'        (four quality codes prepared by the data division and published in WHS: 
#'        high, medium, low, very low (or no VR) - only data with the top
#'        two quality codes are retained (flag: keep.vr==TRUE), 
#'        the other variables in the vrqual 
#'        dataset are not used in these scripts)
#'        IHME estimates (published pre-covid, 2020 GBD) used in selected countries: 
#'           ihmetb.rda (TB estimates), 
#'           ihmeall.rda (total deaths)
#'        GHE envelope estimates: input/GHE/dths_total.csv, obtained from 
#'        the data division (Cao Bochen or Doris Mafat)
#' 
#' Output: vr.rda and related timestamped csv file
#' 
#' 
#' 
#' Last updated: `r Sys.Date()`
#'


rm(list=ls())

# Load libraries and data
library(data.table)
library(imputeTS)
library(haven) # read_dta
library(readxl)
library(stringr)
library(here)

#Function to import db from GTB databases
source(here("import/load_gtb.R"))


# load data
load(here('inc_mort/estimates2022/old.rda'))   
tb <- load_gtb("tb",convert_dots = FALSE)
cty <- load_gtb("cty",convert_dots = FALSE)
pop <- load_gtb("pop",convert_dots = FALSE)

#VR from GTB database
vrgtb <- load_gtb("vrgtb",convert_dots = FALSE)


#load UNDAIS data
load(here('inc_mort/analysis/unaids.rda'))
load(here('inc_mort/estimates2022/ovr.rda'))  # vr from last year (this year, data from 2000-2004 are missing from the excel file)

source(here('inc_mort/R code/fun.R'))

#Import new dataset 2023

xlfn <- 'data/mortality/info_TB_may2023.xls'


m <- 1e5
yr <- 2022

# convert VR data in excel
# adapting code from Pete (2019)
#
# function for reformatting
refrm <- function(indat) {
  # rename & aggregate
  indat <- indat[, .(
    Country,
    name,
    Year,
    icd,
    Cause,
    cause1,
    Sex,
    `0-4` = rowSums(cbind(
      Deaths2, Deaths3, Deaths4, Deaths5, Deaths6
    ), na.rm = T),
    `5-14` = rowSums(cbind(Deaths7, Deaths8), na.rm = T),
    `15-24` = rowSums(cbind(Deaths9, Deaths10), na.rm = T),
    `25-34` = rowSums(cbind(Deaths11, Deaths12), na.rm = T),
    `35-44` = rowSums(cbind(Deaths13, Deaths14), na.rm = T),
    `45-54` = rowSums(cbind(Deaths15, Deaths16), na.rm = T),
    `55-64` = rowSums(cbind(Deaths17, Deaths18), na.rm = T),
    `65plus` = rowSums(
      cbind(
        Deaths19,
        Deaths20,
        Deaths21,
        Deaths22,
        Deaths23,
        Deaths24,
        Deaths25
      ),na.rm = T))
    ]

  # Sequelae
  seq <- c("B90", "B900", "B901", "B902", "B908", "B909", "B077")
  indat[Cause %in% seq, cause1 := "tbseq"]
  indat[, Cause := NULL]

  ## reshape
  MM <-
    melt(indat, id = c("Country", "name", "Year", "icd", "cause1", "Sex"))
  MM$sex <- c('M', 'F')[as.numeric(MM$Sex)]
  MM[is.na(sex), sex := 'U']
  MM$sex <- factor(MM$sex)
  MM$age <- factor(MM$variable, levels = agz3, ordered = TRUE)
  MM[, age_group := gsub('-', '_', age)]
  MM[, age := NULL]
  MM
}

# Some useful age range vectors:
agz <-
  c('04', '514', '1524', '2534', '3544', '4554', '5564', '65') #for extract
agz2 <-
  c('0_4',
    '5_14',
    '15_24',
    '25_34',
    '35_44',
    '45_54',
    '55_64',
    '65plus') #for labels
agz3 <- gsub('_', '-', agz2)
agz4 <- c(rev(rev(agz3)[-1]), "\u2265 65")
kds <- c('0_4', '5_14')
kdz <- c('04', '514')
AA <-
  data.table(
    a1 = agz,
    age_group = agz2,
    age = agz3,
    agegt = agz4
  ) #for conversion


typz <- c('text',
          'text',
          'text',
          'text',
          rep('text', 5),
          rep('numeric', 26)) #specify column types to avoid warning





## -------- ICD>10.1
M1 <-
  as.data.table(read_excel(
    xlfn,
    sheet = 3,
    #skip = 1,
    col_names = TRUE,
    col_types = typz
  ))
M1 <- refrm(M1)


## -------- ICD 10.1
M2 <-
  as.data.table(read_excel(
    xlfn,
    sheet = 4,
    #skip = 1,
    col_names = TRUE,
    col_types = typz
  ))
M2 <- refrm(M2)


## -------- ICD 9
M3 <-
  as.data.table(read_excel(
    xlfn,
    sheet = 5,
    #skip = 1,
    col_types = typz
  ))

# remove double counted B02
M3[, dble := sums(str_count(Cause, 'B02[0-9]')), by = .(name, Year, Sex)]
M3[Cause == 'B02' & dble > 0, drop := TRUE]
(M3[, table(drop)])
M3 <- M3[is.na(drop)]
M3[, drop := NULL]
M3[, dble := NULL]
M3 <- refrm(M3)


## -------- ICD 8
M4 <-
  as.data.table(read_excel(
    xlfn,
    sheet = 6,
    #skip = 1,
    col_types = typz
  ))
M4 <- refrm(M4)


## --- join ---
VR <- rbind(M1, M2, M3, M4)


## Differences in names:
(vrbad <- setdiff(VR[, unique(name)],
                  pop[, unique(country)]))

## direct renamings:
# done <- c()
# 
# # for (i in seq_along(vrbad)) {
# #   newnm <- grep(vrbad[i], pop[, unique(country)], value = TRUE)
# #   if (length(newnm) > 0) {
# #     print(newnm)
# #     VR[name == vrbad[i], name := newnm]
# #     done <- c(done, i)
# #   }
# # }
# vrbad <- vrbad[-done]

## renaming
vrbad
(newnm <- grep('Czech', pop[, unique(country)], value = TRUE))
VR[name == grep('Czech', vrbad, value = TRUE), name := newnm]
(newnm <- grep('Serbia', pop[, unique(country)], value = TRUE)[1])
#VR[name == grep('Serb', vrbad, value = TRUE), name := newnm]
(newnm <-
    grep('Macedonia', pop[, unique(country)], value = TRUE)[1])
VR[name == grep('Mace', vrbad, value = TRUE), name := newnm]
(newnm <- grep('Vincent', pop[, unique(country)], value = TRUE)[1])
VR[name == grep('Vincent', vrbad, value = TRUE), name := newnm]
(newnm <- grep('Libya', pop[, unique(country)], value = TRUE))
VR[name == grep('Libya', vrbad, value = TRUE), name := newnm]
(newnm <- grep('Palestinian', pop[, unique(country)], value = TRUE))
VR[name == grep('Palestinian', vrbad, value = TRUE), name := newnm]

## sub-countries
VR[name %in% c(
  "French Guiana",
  "Martinique",
  "Reunion",
  "Mayotte",
  "Guadeloupe",
  "Saint Pierre and Miquelon"
), name := "France"]
VR[name %in% c("Rodrigues"), name := "Mauritius"]
VR[name %in% c("Virgin Islands (USA)"), name := "United States of America"]

## check
(dropname <- setdiff(VR[, unique(name)], cty[, unique(country)]))

(newnm <- grep('Hong', pop[, unique(country)], value = TRUE))
VR[name == grep('Hong', vrbad, value = TRUE), name := newnm]

VR[name=='Netherlands', name:='Netherlands (Kingdom of the)']
VR[name=='Turkey', name:='Türkiye']
VR[name=='United Kingdom', name:='United Kingdom of Great Britain and Northern Ireland']
VR[name=='Venezuela', name:='Venezuela (Bolivarian Republic of)']

#Check
(dropname <- setdiff(VR[, unique(name)], cty[, unique(country)]))


VR <- VR[name %ni% dropname]

VR[, year := as.integer(Year)]
VR[, Year := NULL]



## Add iso3, and tidy up
VR <- merge(VR,
            cty[, .(iso3, country)],
            by.x = 'name',
            by.y = "country",
            all.x = TRUE)
(VR[is.na(iso3), unique(name)])

VR[, Sex := NULL]
VR[, age_group := variable]

VR$age_group <-
  factor(gsub("-", "_", VR$age_group),
         levels = agz2,
         ordered = TRUE)
VR[, variable := NULL]

setkey(VR, iso3)
rm(M1, M2, M3, M4)

# save(VR, file = 'data/VR.rda')



# aggregate and reshape long to wide
#
vr <- VR[, .(value = sums(value)),
         by = .(iso3, year, cause1)]

vr <- dcast(vr, ... ~ cause1)
setkey(vr, iso3)
setnames(vr, c('iso3','year','hiv','tb','ill_def','tb_seq','total'))





# # Country data additions to WHO database
#

# * Azerbaijan additions
#
# source: epi review, the Hague 2017
#

vr['AZE']
NewYEARS <- 2010:2015
addAZE <- as.data.frame(matrix(nrow=length(NewYEARS),ncol=ncol(vr)))
names(addAZE) <- names(vr)
addAZE$iso3 <- "AZE"
addAZE$year[1:nrow(addAZE)] <- NewYEARS

dim(vr)
vr2 <- rbind(vr, addAZE, use.names = TRUE)
setkey(vr2, iso3)
sel <- vr2$iso3 == 'AZE' & vr2$year %in% 2010:2015

# vr2$vr.coverage[sel] <- rep(1, 6)
vr2$total[sel] <- c(53580, 53726, 55017, 54383, 55648, 54697)
vr2$tb[sel] <- c(709, 577, 373, 378, 372, 485)
vr2$ill_def[sel] <- c(1343, 1771, 1836, 1892, 2440, 1864)
vr2['AZE']

# checking that each combination iso3-year appears only once in the database
tCheck <- as.data.frame(table(paste(vr2$iso3,vr2$year)))
nrow(tCheck[ tCheck[,2]>1 , ])==0

# #!!!
# # check whether the following 2 blocks do not overwrite newly reported VR data
# #!!!
# 
# # RUS 2020
# #
# # Total deaths = 2138586
# # Ill-defined = 142370
# # TB deaths = 6841
# addRUS <- vr2['RUS'][year==2019]
# addRUS$year <- 2020
# addRUS$total <- 2138586
# addRUS$ill_def <- 142370
# addRUS$tb <- 6841
# addRUS$hiv <- unaids['RUS'][year==2020, mort.hiv.num]
# vr3 <- rbind(vr2, addRUS, use.names = TRUE)
# setkey(vr3, iso3)
# 
# vr <- copy(vr3)
# (dim(vr))
# # 
# # 
# # 
# # RUS 2021
# #
# # Total deaths =
# # Ill-defined =
# # TB deaths =
# addRUS <- vr['RUS'][year==2020]
# addRUS$year <- 2021
# addRUS$total <- 2441594
# addRUS$ill_def <- 137231
# addRUS$tb <- 6313
# addRUS$hiv <- unaids['RUS'][year==2021, mort.hiv.num]
# vr4 <- rbind(vr, addRUS, use.names = TRUE)
# setkey(vr4, iso3)
# 
# vr <- copy(vr4)
# (dim(vr))
# 



# check that the additions do not end-up duplicating country-year entries
#
sum(duplicated(vr[,.(iso3,year)]))==0



##
## VR from GTB database
##

#At least Total death and TB deaths available

vrgtb=subset(vrgtb, !is.na(total.deaths.vr)  & !is.na(tbdeaths.vr))


#Cleaning according to remarks (changing year of VR data)
vrgtb[iso3=='BEL' & year==2019, year:=2017]
vrgtb[iso3=='BEL' & year==2020, year:=2018]
vrgtb[iso3=='BEL' & year==2021, year:=2019]

vrgtb[iso3=='POL' & year==2019, year:=2018]


#Remove line with total death==0 (unreliable)
vrgtb=subset(vrgtb,total.deaths.vr>0)


#Renaming to match VR dataset
vrgtb$total=vrgtb$total.deaths.vr
vrgtb$ill_def=vrgtb$r00.r99.deaths.vr
vrgtb$tb=vrgtb$tbdeaths.vr


vrgtb=subset(vrgtb, select=c("iso3","year","total","ill_def","tb"))
vrgtb$tb_seq=NA
vrgtb$hiv=NA

vr5=subset(vr2,select=c("iso3","year"))
vr5$alreadyinvr=1

vrgtb=merge(vrgtb,vr5,by=c("iso3","year"),all.x = TRUE)
vrgtb[is.na(alreadyinvr),alreadyinvr:=0]


#Observations to keep to import to main VR data
# If not already in VR
# Brazil 2020 (already in VR but most recent data)
# GEO data
# KAZ
# ROU from 2020
# RUS from 2020

vrlst=c("GEO","KAZ","ROU","RUS")

vrgtb$tokeep.vrgtb=ifelse(vrgtb$alreadyinvr==1,0,1)
vrgtb[iso3 %in% vrlst, tokeep.vrgtb:=1]
vrgtb[iso3=="BRA" & year==2020, tokeep.vrgtb:=1]

vrgtb=subset(vrgtb, tokeep.vrgtb==1)
vrgtb$alreadyinvr=NULL
vrgtb$tokeep.vrgtb=NULL

#Drop or set to NA unreliable data or inconsistent

#unreliable
sel=vrgtb$iso3=="TKM" | vrgtb$iso3=="SVK" | vrgtb$iso3=="FRA" | vrgtb$iso3=="ISR"
vrgtb=vrgtb[!sel,]

#MDA ill defined = total deat, set to NA
sel=(vrgtb$iso3=="MDA"& vrgtb$year %in% 2020:2022)
vrgtb[sel, ill_def:=NA]



vr5 <- rbind(vr2, vrgtb, use.names = TRUE)

setkey(vr5, iso3)

# Duplicates in iso/years
sel=duplicated(vr5[,c("iso3","year")])
vr5[sel==TRUE]

# Drop them from VR DDI
sel=(vr2$iso3=="BRA" & vr2$year==2020) |
    (vr2$iso3=="GEO" & (vr2$year==2020 | vr2$year==2021)) |
    (vr2$iso3=="KAZ" & (vr2$year==2019 | vr2$year==2020 | vr2$year==2021)) |
    (vr2$iso3=="ROU" & vr2$year==2019)  |
    (vr2$iso3=="RUS" & vr2$year==2019)
dim(vr2)  
vr2=vr2[!sel,]
dim(vr2)  


# Some ill_defined death = total death: MDA, TKM

# Merge VR DDI updated and VRGTB
vr5 <- rbind(vr2, vrgtb, use.names = TRUE)
# Duplicates in iso/years
sel=duplicated(vr5[,c("iso3","year")])
vr5[sel==TRUE]


vr <- copy(vr5)
(dim(vr))
setkey(vr, iso3)

# # process raw VR data
#
(dim(vr))

# missing tb deaths
miss.tb <- unique(vr$iso3[is.na(vr$tb)])

vr[iso3 %in% miss.tb, tb.nm := sum(!is.na(tb)), by = iso3]
vr[, table(tb.nm)]
vr[tb.nm == 0]

(dim(vr))
vr <- vr[tb.nm > 0 | iso3 %ni% miss.tb]
(dim(vr))

vr[tb.nm == 1] # not yet imputable


vr[, prop.tb := tb / total]

#Impute TB deaths if >= 2 data points
vr[tb.nm >= 2, tb.imp := na_interpolation(round(prop.tb * total)), by =
     iso3]
(summary(vr$tb.imp))

#Replace TB deaths in the new imputed var tb.imp by the observed value in tb
vr[!is.na(tb) & is.na(tb.imp), tb.imp := tb]
(vr['ZAF'])
(vr['RUS'])
(vr['KAZ'])

# clean up
#
vr[, tb.nm := NULL]
rm(miss.tb)



# missing ill-defined
#Same process, impute if >=2 data points

miss.ill <- unique(vr$iso3[is.na(vr$ill_def)])

vr[iso3 %in% miss.ill, ill.nm := sum(!is.na(ill_def)), by = iso3]
(vr[, table(ill.nm)])

vr[, garbage := ill_def / total]

vr[ill.nm >= 2, garbage.imp := na_interpolation(garbage), by = iso3]
(summary(vr$garbage.imp))
vr[!is.na(garbage) & is.na(garbage.imp), garbage.imp := garbage]
(vr['ZAF'])
(vr['RUS'])
(vr['KAZ'])
(vr['MDA'])
(vr['BRA'])



# clean up
#
vr[, ill.nm := NULL]
rm(miss.ill)


# missing tb_seq
#
vr[, sum(is.na(tb_seq))]


# total TB deaths
#
vr[, totaltb := rowSums(cbind(vr$tb.imp, vr$tb_seq), na.rm = TRUE)]

# proportion sequelae
#
vr[, seq.prop := tb_seq / totaltb]


setkey(vr, iso3, year)



# proportion of TB deaths out of well documented deaths
#
vr[, tb.prop := totaltb / (total - ill_def)]

#If missing ill-defined, use imputed ill-defined proportion
vr[is.na(tb.prop), tb.prop := totaltb / (total - (garbage.imp*total))]



# check
#
vr[!is.na(tb.prop), test.isbinom(tb.prop)]

vr[tb.prop>1]
(vr['RUS'])
(vr['KAZ'])
(vr['MDA'])
(vr['BRA'])


# # VR quality
vrqual <- fread('data/mortality/vrqual.csv')
vrqual <- vrqual[country != ""]
(vrqual)


## Differences in country names:
(vrqbad <- setdiff(vrqual[, unique(country)],
                  cty[, country]))

#Replace country name in vrqual from the corresponding correct name in cty

(newnm <- grep('Macedonia', cty[, country], value = TRUE))
vrqual[country == grep('Macedonia', vrqbad, value = TRUE), country := newnm]
(newnm <- grep('Britain', cty[, country], value = TRUE))
vrqual[country == grep('United Kingdom', vrqbad, value = TRUE), country := newnm]
(newnm <- grep('Türkiye', cty[, country], value = TRUE))
vrqual[country == grep('Turkey', vrqbad, value = TRUE), country := newnm]

vrqual[country == "Netherlands", country := "Netherlands (Kingdom of the)"]


vrqual <- merge(vrqual, cty[,.(country, iso3)], by='country', all.x =TRUE)
sum(is.na(vrqual$iso3))==0 # check

#Merge VR and VR qual dataset
vr <-
  merge(vr, vrqual[, .(iso3,
                       codqual = quality,
                       min.coverage,
                       max.coverage,
                       min.usability,
                       max.usability,
                       use.ghe)], by = 'iso3')

#VR to keep if high or medium quality: generate a variable keep.vr
vr[codqual %in% c("high", "medium"), keep.vr := T]
vr[codqual %ni% c("high", "medium"), keep.vr := F]
vr[, summary(keep.vr)]

#Remove the char % from the coverage
remove.pc <- function(x) as.numeric(gsub("%", "", x))

vr[, min.coverage := remove.pc(min.coverage)]
vr[, max.coverage := remove.pc(max.coverage)]
vr[, min.usability := remove.pc(min.usability)]
vr[, max.usability := remove.pc(max.usability)]

# vr[, vr.coverage := min.coverage]



### Check VR imported from GTB

(vrgtb.lst <- unique(vrgtb$iso3))
vr[iso3=="BRA",.(year,tb,ill_def,total,prop.tb,tb.prop)]
vr[iso3=="GEO",.(year,tb,ill_def,total,prop.tb,tb.prop)]
vr[iso3=="KAZ",.(year,tb,ill_def,total,prop.tb,tb.prop)]
vr[iso3=="MDA",.(year,tb,ill_def,total,prop.tb,tb.prop)]
vr[iso3=="RUS",.(year,tb,ill_def,total,prop.tb,tb.prop)]
vr[iso3=="SRB",.(year,tb,ill_def,total,prop.tb,tb.prop)]


# GHE
# dths1 = males
# dths2 = females
#
ghe <- fread('data/mortality/dths_total.csv')

ghe[causename=='All Causes', ghe.env := rowSums(cbind(dths1, dths2), na.rm=TRUE)]

vr2 <- merge(vr, ghe[!is.na(ghe.env),.(iso3, year, ghe.env)], by=c('iso3','year'), all.x=TRUE, all.y=FALSE)
(dim(vr))
(dim(vr2))

# default envelope
vr2[, env := ghe.env]  

#Ask PG why for these country we use total death as envelope
vr2[iso3 %in% c('DMA','PRI','SMR','KNA'), env := total]

vr2[, env.nm := sum(!is.na(env)), by=iso3]
vr2[, table(env.nm)]

#Impute missing envelope if more than 2 data points
vr2[env.nm >=2, env := na_interpolation(env), by=iso3]
vr2[, sum(is.na(env))]


vr2[, vr.coverage := pmin(1, total / env)] # VR coverage
vr2[is.na(vr.coverage), vr.coverage := total] # non-GHE estimate where missing GHE


# update RUS
vr2[iso3=='RUS' & year==yr-2, ghe.env := total]
vr2[iso3=='RUS' & year==yr-1, ghe.env := total]
vr2[iso3=='RUS' & year==yr-1, env := total]






# proportion of TB deaths out of well documented deaths
#
vr2[, tb.prop := totaltb / (total - ill_def)]

#If missing ill-defined, use imputed ill-defined proportion
vr2[is.na(tb.prop), tb.prop := totaltb / (total - (garbage.imp*total))]



#
# adjusted TB deaths
#
vr2[, tb.adj := env * tb.prop]

vr <- copy(vr2)

# check
#
vr[!is.na(tb.prop), test.isbinom(tb.prop)]




# # SDs
#
# assume TB deaths between 0.5 and 1.5 times observed $t$ rate among
# garbage $g$ and non covered $c$
#
# $t_{adj} = \frac{t}{c(1-g)}$
#
# $\text{SD}(t_{adj}) = \frac{t}{4} \left(\frac{1}{c(1-g)} - 1\right)$
#
vr[, tb.adj.sd := (tb.adj / 4) * (1 / (vr.coverage * (1 - garbage)) - 1)]

vr[!is.na(tb.adj.sd), test.ispos(tb.adj.sd)]
vr[keep.vr == T & is.na(tb.adj.sd), tb.adj.sd := tb.adj * .2]
vr[keep.vr == T, summary(tb.adj.sd / tb.adj)]


vr2  <-
  merge(vr[iso3 %ni% 'VIR'], pop[, .(iso3, year, pop = e.pop.num)], by =
          c('iso3', 'year'), all.x = TRUE)
dim(vr[iso3 %ni% 'VIR'])
dim(vr2)
vr2[is.na(pop)]

# exclude MNE and SRB 2000:2004
vr <- copy(vr2[!is.na(pop)])







# Codecorrect from IHME (GBD 2020, accessed June 2021)
#
load('data/ihme/ihmetb.rda')   # see ihme.R
load('data/ihme/ihmeall.rda')


# envelope ratios GHO/IHME
#
ihme.all <-
  ihmeall[year >= 2000 &
         cause == 'All causes' & measure == 'Deaths']
ihme.all <- ihme.all[!is.na(iso3)]
gbd <-
  merge(ihme.all[, .(iso3,
                     year,
                     ihme = val,
                     ihme.lo = lower,
                     ihme.hi = upper)],
        ghe,
        by = c('iso3', 'year'),
        all.y = T)


# missing IHME envelopes?
#
gbd[, sapply(.SD, function(x)
  sum(is.na(x)))]


# WHO/IHME env ratio
#
gbd[, env.ratio := ghe.env / ihme]

vr2 <- merge(vr, gbd[!is.na(env.ratio),.(iso3,year,ihme.env=ihme,env.ratio)], by=c('iso3','year'), all.x=TRUE)
(dim(vr))
(dim(vr2))

vr <- copy(vr2)

vr[, prop.tb := NULL]





#!!! 
# add missing 2000-2004 data 
# (next year, check for completeness of excel file)
dim(vr); dim(ovr[year<2005])
vr2 <- rbind(ovr[year<2005], vr)
dim(vr2)

vr <- copy(vr2)

sum(duplicated(vr[,.(iso3,year)]))==0 
setkey(vr, iso3, year)
#
#!!!



# simple checks
ovr[, table(year)]
vr[, table(year)]

ovr[, sum(tb, na.rm=TRUE), by=year]
vr[, sum(tb, na.rm=TRUE), by=year]




#  save vr dataset
#

attr(vr, "timestamp") <- Sys.Date() #set date
save(vr, file = 'inc_mort/analysis/vr.rda')
fwrite(vr, file = paste0('inc_mort/analysis/csv/vr_', Sys.Date(), '.csv'))





