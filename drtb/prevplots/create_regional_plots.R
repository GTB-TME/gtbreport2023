## making proportion plots (usable by others)

## libraries
library(here)
library(glue)
library(data.table)
library(ggplot2)

## utilities
gh <- function(x) glue(here(x))
rot45 <- theme(axis.text.x = element_text(angle = 45, hjust = 1))
source(gh('drtb/R/utils/plotsetc.R'))

## data
mdl <- scan(here('drtb/R/utils/modelchoice.txt'),what='char') #NOTE depends choice
cat('Using model: ',mdl,'\n')
load(file=gh('drtb/prevplots/KO.{mdl}.Rdata'))
load(file=gh('drtb/prevplots/RPD.Rdata'))
RPD <- merge(RPD,unique(KO[,.(iso3,g_whoregion)]),by='iso3',all.x=TRUE)
RPD <- RPD[!is.na(g_whoregion)]

## make the plots
fn <- gh('drtb/prevplots/plots/RR_props_')
makeregionplots(KO,fn)

