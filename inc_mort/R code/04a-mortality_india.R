### India 2023

rm(list=ls())

library(data.table)
library(imputeTS)
library(haven)
library(readxl)
library(stringr)
library(here)
library(ggplot2)
library(gtbreport)
library(scales)
suppressMessages(library(imputeTS))


# load data from previous estimates 2022 report
load(here('inc_mort/estimates2022/old.rda'))  

#Function to import db from GTB databases
source(here("import/load_gtb.R"))

#load data from gtb

tb <- load_gtb("tb",convert_dots = FALSE)
cty <- load_gtb("cty",convert_dots = FALSE)
pop <- load_gtb("pop",convert_dots = FALSE)

#Load IHME data
load(here('data/ihme/ihmeall.rda'))

# Functions
source(here('inc_mort/R code/fun.R'))


#Input source data
year=2000:2019
srsindia=data.table(year)


srsindia$srs_raw[srsindia$year==2005]=5.2
srsindia$srs_raw[srsindia$year==2008]=4.4
srsindia$srs_raw[srsindia$year==2012]=3.7
srsindia$srs_raw[srsindia$year==2015]=3.2
srsindia$srs_raw[srsindia$year==2016]=3
srsindia$srs_raw[srsindia$year==2017]=3
srsindia$srs_raw[srsindia$year==2018]=2.9


srsindia$illdef[srsindia$year==2005]=7.6
srsindia$illdef[srsindia$year==2008]=8.4
srsindia$illdef[srsindia$year==2012]=12.4
srsindia$illdef[srsindia$year==2015]=14.4
srsindia$illdef[srsindia$year==2016]=14.4
srsindia$illdef[srsindia$year==2017]=13
srsindia$illdef[srsindia$year==2018]=12.2

srsindia$ihme.propdeath=c(NA,NA,NA,NA,6.1,5.9,5.7,5.6,5.6,5.5,5.3,5.2,5.2,5.2,5.2,5,4.9,4.9,4.7,4.5)
srsindia$mccd.propdeath=c(NA,NA,NA,NA,NA,NA,NA,NA,4.8,4.4,4.1,4.1,4.3,3.9,3.6,3.2,2.9,2.9,2.7,2.5)



srsindia$srs.cfr.overall=c(8.5,8.4,8.2,8,7.5,7.6,7.5,7.4,7.4,7.3,7.2,7.1,7.0,7.0,6.7,6.5,6.4,6.3,6.2,6)

srsindia$srs_raw=srsindia$srs_raw/100
srsindia$illdef=srsindia$illdef/100

newdata=data.table(year)
mymodel=glm(srsindia$srs_raw~srsindia$year, family=quasibinomial)
mymodel2=glm(srsindia$illdef~srsindia$year, family=quasibinomial)

srsindia$srs_hat_linear=predict(mymodel, type="response",newdata=newdata)
srsindia$illdef_hat_linear=predict(mymodel2, type="response",newdata=newdata)

preds=predict(mymodel, interval="confidence",newdata=newdata,type = "link", se.fit = TRUE)
srsindia$fit.lo=preds$fit - (1.96 * preds$se.fit)
srsindia$fit.hi=preds$fit + (1.96 * preds$se.fit)
srsindia$fit=preds$fit

srsindia$fit.lo=invlogit(srsindia$fit.lo)
  
srsindia$fit.hi=invlogit(srsindia$fit.hi)
srsindia$fit=invlogit(srsindia$fit)


#Interpolation for ill defined: Kalman + LOCF
srsindia$imp.illdef= na_interpolation(srsindia$illdef)
srsindia$imp.illdef= na_kalman(srsindia$illdef,type="trend")
srsindia$imp.illdef[srsindia$year==2019]= NA
srsindia$imp.illdef= imputeTS::na_locf(srsindia$imp.illdef)




ghe <- fread('data/mortality/dths_total.csv')
ghe[causename=='All Causes', ghe.env := rowSums(cbind(dths1, dths2), na.rm=TRUE)]
ghe=ghe[causename=='All Causes',]
ghe=ghe[iso3=="IND",]

ihmemort=subset(ihmeall, iso3=="IND", select=c("year","val","upper","lower"))

srsindia=merge(srsindia,ghe,by="year")
srsindia=merge(srsindia,ihmemort,by="year")

names(srsindia)[names(srsindia) == "val"] <- "deathihme"

#View(srsindia[,c("year","srs_raw","ghe.env")])

oldind=subset(old, iso3=="IND" & year<2020)
srsindia$old.mort.nh.num=oldind$mort.nh.num
srsindia$diff.mort.num=srsindia$old.mort.nh.num-srsindia$mort.nh.num





#Coverage
#v=1

#proportion of ill-defined causes
#g=srsindia$imp.illdef

# Range of upward adjustment = SRS/IHME in 2008-2013
# From 10 to 30% upward adjustment

# Min adjustment : theta = 0.9
theta=0.85

#srsindia[,tb.adj.min:=srs_hat_linear*ghe.env/(v*(1-g))]
srsindia[,tb.adj.min:=srs_hat_linear*ghe.env/theta]

#SD
srsindia[,tb.adj.min.sd:=(tb.adj.min / 4) * (1 / ((theta)) - 1)]

#Merge population estimate
srsindia=merge(srsindia,pop, by=c("iso3","year"))

#Moratality rates and bounds
m=1e5
srsindia$mort.nh.min=srsindia$tb.adj.min*m/srsindia$pop
srsindia$mort.nh.min.sd=srsindia$tb.adj.min.sd*m/srsindia$pop

vlohi <- Vectorize(lohi, c('ev', 'sd'))

out=vlohi(srsindia$mort.nh.min/m,srsindia$mort.nh.min.sd/m)
srsindia$mort.nh.min.lo=out[1,]*m
srsindia$mort.nh.min.hi=out[2,]*m

srsindia[,c("year","mort.nh.min","mort.nh.min.sd","mort.nh.min.lo","mort.nh.min.hi")]

#mortality in number and bound

srsindia[,mort.nh.min.num:=mort.nh.min * pop / m]

srsindia[,mort.nh.min.lo.num:=mort.nh.min.lo * pop / m]
srsindia[,mort.nh.min.hi.num:=mort.nh.min.hi * pop / m]


# Min adjustment : theta = 0.7
theta=0.7

#srsindia[,tb.adj.min:=srs_hat_linear*ghe.env/(v*(1-g))]
srsindia[,tb.adj.max:=srs_hat_linear*ghe.env/theta]

#SD
srsindia[,tb.adj.max.sd:=(tb.adj.max / 4) * (1 / ((theta)) - 1)]


#Moratality rates and bounds
m=1e5
srsindia$mort.nh.max=srsindia$tb.adj.max*m/srsindia$pop
srsindia$mort.nh.max.sd=srsindia$tb.adj.max.sd*m/srsindia$pop


out=vlohi(srsindia$mort.nh.max/m,srsindia$mort.nh.max.sd/m)
srsindia$mort.nh.max.lo=out[1,]*m
srsindia$mort.nh.max.hi=out[2,]*m

srsindia[,c("year","mort.nh.max","mort.nh.max.sd","mort.nh.max.lo","mort.nh.max.hi")]

#mortality in number and bound

srsindia[,mort.nh.max.num:=mort.nh.max * pop / m]

srsindia[,mort.nh.max.lo.num:=mort.nh.max.lo * pop / m]
srsindia[,mort.nh.max.hi.num:=mort.nh.max.hi * pop / m]


# Building final series
# central value = mean (mort.nh.min, mort.nh.max)

srsindia$mort.nh=(srsindia$mort.nh.min+srsindia$mort.nh.max)/2
srsindia$mort.nh.sd=sqrt((srsindia$mort.nh.min-srsindia$mort.nh)^2+
                         (srsindia$mort.nh.max-srsindia$mort.nh)^2)
  
out=vlohi(srsindia$mort.nh/m,srsindia$mort.nh.sd/m)

srsindia$mort.nh.lo=out[1,]*m
srsindia$mort.nh.hi=out[2,]*m

srsindia[,mort.nh.num:=mort.nh * pop / m]
srsindia[,mort.nh.lo.num:=mort.nh.lo * pop / m]
srsindia[,mort.nh.hi.num:=mort.nh.hi * pop / m]


srsindia[,.(year,mort.nh.num,mort.nh.lo.num,mort.nh.hi.num)]


save(srsindia, file = 'inc_mort/analysis/srsindia.rda')

fwrite(srsindia, file = paste0('inc_mort/analysis/csv/mort_india_', Sys.Date(), '.csv'))


# Plot deaths total IHME and GHE and RGI


srsindia$rgi.death=srsindia$srs.cfr.overall*srsindia$pop /1000

plot.india.death=ggplot(
  mapping = aes(year,ghe.env),
  data = subset(srsindia,year<2020),
  colour = I('red'),
) +
  geom_line(
    colour = I('red')
  ) +
  # geom_ribbon(
  #   aes(year, ymin = dths_low1+dths_low2, ymax = dths_up1+dths_up2),
  #   fill = I('red'),
  #   alpha = I(0.4)
  # ) +
  geom_line(
    aes(year,deathihme),
    colour = I('darkgreen')
  ) +
  geom_line(
    aes(year,rgi.death)
  ) +
  
  # geom_ribbon(
  #   aes(year, ymin = lower, ymax = upper),
  #   fill = I('darkgreen'),
  #   alpha = I(0.4)
  # ) +
  # annotate(
  #   'text',
  #   x = 2015,
  #   y = 8500000,
  #   label = 'Estimated WHO total deaths',
  #   hjust = 0,
  #   size = I(3),
  #   colour="red"
  # )+ 
  # annotate(
  #   'text',
  #   x = 2015,
  #   y = 9500000,
  #   label = 'Estimated IHME total deaths',
  #   hjust = 0,
  #   size = I(3),
  #   colour="darkgreen"
  # )+ 
  
  scale_x_continuous(name="Year", breaks=seq(2000,2019,1))+
  scale_y_continuous(name="Total deaths",breaks=seq(8e6,10e6,5e5))+

  theme_gtb()


plot.india.death
ggsave(here('inc_mort/output/plot.india.death_260723.png'), plot=plot.india.death, width=9, height=6)





# Plot SRS and IHME

plot.india.propdeath=ggplot(
  mapping = aes(year, 100*srs_raw ),
  data = subset(srsindia,year<2020),
  colour = I('blue'),
) +
  geom_point(
    colour = I('blue')
  ) +
  geom_line(
    colour = I('blue'),
    linetype = I(2),
    data=subset(srsindia,!is.na(srs_raw) & year<2020)
  ) +
  
  geom_point(
    aes(year,ihme.propdeath),
    colour = I('darkgreen')
  ) +
  geom_line(
    aes(year,ihme.propdeath),
    colour = I('darkgreen'),
    linetype = I(2)
  ) +
  geom_point(
    aes(year,mccd.propdeath),
  ) +
  geom_line(
    aes(year,mccd.propdeath),
    linetype = I(2)
  ) +
  annotate(
    'text',
    x = 2005,
    y = 4.1,
    label = 'SRS raw data',
    hjust = 0,
    size = I(3),
    colour="blue"
  )+
  annotate(
    'text',
    x = 2011,
    y = 6.1,
    label = 'IHME data',
    hjust = 0,
    size = I(3),
    colour="darkgreen"
  )+ 
  
  scale_x_continuous(name="Year", breaks=seq(2000,2019,1))+
  scale_y_continuous(name="% of TB deaths", breaks=seq(0,8,1))+
  expand_limits(y = 0) +
  
  theme_gtb()


plot.india.propdeath
ggsave(here('inc_mort/output/plot.india.propdeath_260723.png'), plot=plot.india.propdeath, width=9, height=6)



# Plot of observed data

plot.india.obs=ggplot(
  mapping = aes(year, 100*srs_raw ),
  data = subset(srsindia,year<2020),
  colour = I('blue'),
) +
  geom_point(colour = I('blue'))+
  geom_line(
    aes(year,100*fit),
    colour = I('red'),
  ) +
  geom_ribbon(
    aes(year, ymin = 100*fit.lo, ymax = 100*fit.hi),
    fill = I('red'),
    alpha = I(0.4)
  ) +
  geom_point(
    aes(year,ihme.propdeath),
    colour = I('darkgreen')
  ) +
  geom_line(
    aes(year,ihme.propdeath),
    colour = I('darkgreen'),
    linetype = I(2)
  ) +
  annotate(
    'text',
    x = 2005,
    y = 4.1,
    label = 'SRS raw data',
    hjust = 0,
    size = I(3),
    colour="blue"
  )+
  annotate(
    'text',
    x = 2005,
    y = 3.9,
    label = 'Predicted SRS data',
    hjust = 0,
    size = I(3),
    colour="red"
  )+ 
  annotate(
    'text',
    x = 2011,
    y = 6.1,
    label = 'IHME data',
    hjust = 0,
    size = I(3),
    colour="darkgreen"
  )+ 
  
  scale_x_continuous(name="Year", breaks=seq(2000,2019,1))+
  scale_y_continuous(name="% of TB deaths", breaks=seq(0,8,1))+
  expand_limits(y = 0) +

  theme_gtb()


plot.india.obs
ggsave(here('inc_mort/output/india_mort_obs_060723.png'), plot=plot.india.obs, width=9, height=6)





#Plot updated and previous mortality

plot.india=ggplot(
  mapping = aes(year, mort.nh.num ),
  data = subset(srsindia,year<2020)
) +
  geom_line(size=1, colour = I('blue')) +
  geom_ribbon(
    aes(year, ymin = mort.nh.lo.num, ymax = mort.nh.hi.num),
    fill = I('blue'),
    alpha = I(0.4)
  ) +
  
  geom_line(
    aes(year, mort.nh.num),
    data = subset(old, iso3=="IND" & year<2020),
    colour = I('red'),
    linetype = I(2)
  ) +
  
  geom_ribbon(
    aes(year, ymin = mort.nh.lo.num, ymax = mort.nh.hi.num),
    data = subset(old, iso3=="IND" & year<2020),
    fill = I('red'),
    alpha = I(0.4)
  ) +
  geom_line(
    aes(year, diff.mort.num),
    data = srsindia,
    colour = I('black'),
    linetype = I(2)
  )+
  annotate(
    'text',
    x = 2014,
    y = 580000,
    label = 'Mortality published in 2022 report',
    hjust = 0,
    size = I(3),
    colour="red"
  )+
  
  annotate(
    'text',
    x = 2014,
    y = 180000,
    label = 'Updated estimates of mortality',
    hjust = 0,
    size = I(3),
    colour="blue"
  )+

  scale_x_continuous(name="Year", breaks=seq(2000,2019,1))+
  scale_y_continuous(name="Number of deaths", breaks=seq(0,1e6,100000),labels = comma)+
  
  expand_limits(y = 0) +
  theme_gtb()


plot.india  

plotly::ggplotly(plot.india )

ggsave(here('inc_mort/output/india_mort_210723.png'), plot=plot.india, width=9, height=6)













