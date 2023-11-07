#' title: Estimate excess of TB deaths during COVID disruptions
#' author: Mathieu Bastard
#' date: 27/07/2023
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
vlohi <- Vectorize(lohi, c('ev', 'sd'))

#Function to import db from GTB databases
source(here("import/load_gtb.R"))

#load data from gtb

tb <- load_gtb("tb",convert_dots = FALSE)
cty <- load_gtb("cty",convert_dots = FALSE)
pop <- load_gtb("pop",convert_dots = FALSE)
grpmbr <- load_gtb("grpmbr",convert_dots = FALSE)

load(here('inc_mort/analysis/est.rda'))
load(here('inc_mort/analysis/global.rda'))

#Year of estimate
yr <- 2022

#Unit of rate (/100000pop)
m <- 1e5


# Counter factual

dta=subset(global,year>=2016, select=c("year","c.newinc","mort","mort.sd","mort.lo","mort.hi","pop"))
dta$mort.cf=dta$mort
dta$mort.cf.sd=dta$mort.sd

dta$mort.cf[dta$year %in% 2020:2022] = NA
dta$mort.cf.sd[dta$year %in% 2020:2022] = NA

mymodel=glm(dta$mort.cf/m~dta$year, family=quasibinomial)
dta$mort.hat=predict(mymodel, type="response",newdata=dta)*m

sel = dta$year %in% 2020:2022
dta[sel, mort.cf := mort.hat]
dta[, mort.cf.sd := imputeTS:: na_locf(mort.cf.sd)]


# bounds of cf
out <- vlohi(dta$mort.cf / m, dta$mort.cf.sd / m)

dta$mort.cf.lo <- out[1, ] * m
dta$mort.cf.hi <- out[2, ] * m


#Add 2010-2015

dta2=subset(global,year<2016, select=c("year","mort","mort.sd","mort.lo","mort.hi","pop"))
dta=rbind(dta2,dta, fill=T)

sel = dta$year %in% 2000:2015
dta[sel, mort.cf := mort]
dta[sel, mort.cf.sd := mort.sd]
dta[sel, mort.cf.lo := mort.lo]
dta[sel, mort.cf.hi := mort.hi]


# Num

dta <- within(dta, {
  mort.num <- mort * pop / m
  mort.lo.num <- mort.lo * pop / m
  mort.hi.num <- mort.hi * pop / m
  
  mort.cf.num <- mort.cf * pop / m
  mort.cf.lo.num <- mort.cf.lo * pop / m
  mort.cf.hi.num <- mort.cf.hi * pop / m
  
})



# Excess

dta$mort.diff.num=dta$mort.num-dta$mort.cf.num
dta$mort.diff.lo.num=dta$mort.lo.num-dta$mort.cf.num
dta$mort.diff.hi.num=dta$mort.hi.num-dta$mort.cf.num

excess=sum(dta$mort.diff.num[dta$year==2020],dta$mort.diff.num[dta$year==2021],dta$mort.diff.num[dta$year==2022])
excess.lo=sum(dta$mort.diff.lo.num[dta$year==2020],dta$mort.diff.lo.num[dta$year==2021],dta$mort.diff.lo.num[dta$year==2022])
excess.hi=sum(dta$mort.diff.hi.num[dta$year==2020],dta$mort.diff.hi.num[dta$year==2021],dta$mort.diff.hi.num[dta$year==2022])

dta[,.(year,mort.num,mort.lo.num,mort.hi.num,mort.cf.num,mort.cf.lo.num,mort.cf.hi.num)]




# Plot


#Plot updated and previous mortality

plot.excess.mort=ggplot(
  mapping = aes(year, mort.num ),
  data = subset(dta,year>=2010)
) +
  geom_line(size=1, colour = I('blue')) +
  geom_ribbon(
    aes(year, ymin = mort.lo.num, ymax = mort.hi.num),
    fill = I('blue'),
    alpha = I(0.4)
  ) +
  
  geom_line(
    aes(year, mort.cf.num),
    data = subset(dta,year>=2019),
    colour = I('red'),
    linetype = I(2)
  ) +
  geom_ribbon(
    aes(year, ymin = mort.cf.num, ymax = mort.num),
    fill = I('red'),
    data = subset(dta,year>=2019),
    alpha = I(0.4)
  ) +
  # geom_ribbon(
  #   aes(year, ymin = mort.cf.lo.num, ymax = mort.cf.hi.num),
  #   fill = I('red'),
  #   data = subset(dta,year>=2019),
  #   alpha = I(0.4)
  # ) +
  annotate(
    'text',
    x = 2018,
    y = 900000,
    label = paste('Excess TB deaths: +',round(excess,0),
                  "( UI ",round(excess.lo,0),"-",round(excess.hi,0),")") ,
    hjust = 0,
    size = I(3),
    colour="red"
  )+
  # 
  # annotate(
  #   'text',
  #   x = 2014,
  #   y = 180000,
  #   label = 'Updated estimates of mortality',
  #   hjust = 0,
  #   size = I(3),
  #   colour="blue"
  # )+
  
  scale_x_continuous(name="Year", breaks=seq(2010,2022,2))+
  scale_y_continuous(name="Number of deaths")+
  
  expand_limits(y = 0) +
  theme_gtb()


plot.excess.mort  


ggsave(here('inc_mort/output/excess_mort.png'), plot=plot.excess.mort, width=9, height=6)




# What if no recovery in notification
# Increase in mortality similar to covid period


dta$mort.no.recov22=NA
covid.rate.mort=dta$mort[dta$year==2021]/dta$mort[dta$year==2020]
dta$mort.no.recov22[dta$year==2022]=covid.rate.mort*dta$mort[dta$year==2021]

dta$mort.no.recov22.num=dta$mort.no.recov22 * dta$pop/m

dta[,.(year,mort,mort.cf,mort.no.recov22,mort.num,mort.cf.num,mort.no.recov22.num)]

dta$mort.diff.no.recov22.num=dta$mort.no.recov22.num-dta$mort.num
dta$mort.diff.no.recov22.num.lo=ifelse(dta$mort.no.recov22.num-dta$mort.hi.num>0,dta$mort.no.recov22.num-dta$mort.hi.num,0)
dta$mort.diff.no.recov22.num.hi=dta$mort.no.recov22.num-dta$mort.lo.num

dta[,.(year,mort.num,mort.no.recov22.num,mort.diff.no.recov22.num,mort.diff.no.recov22.num.lo,mort.diff.no.recov22.num.hi)]

dta$mort.no.recov22.num[dta$year==2021]=dta$mort.num[dta$year==2021]


plot.excess.avert.mort=ggplot(
  mapping = aes(year, mort.num ),
  data = subset(dta,year>=2010)
) +
  geom_line(size=1, colour = I('blue')) +
  geom_ribbon(
    aes(year, ymin = mort.lo.num, ymax = mort.hi.num),
    fill = I('blue'),
    alpha = I(0.4)
  ) +
  
  geom_line(
    aes(year, mort.cf.num),
    data = subset(dta,year>=2019),
    colour = I('red'),
    linetype = I(2)
  ) +
  geom_ribbon(
    aes(year, ymin = mort.cf.num, ymax = mort.num),
    fill = I('red'),
    data = subset(dta,year>=2019),
    alpha = I(0.4)
  ) +
  geom_line(
    aes(year, mort.no.recov22.num),
    data = subset(dta,year>=2021),
    colour = I('darkgreen'),
    linetype = I(2)
  ) +
  geom_ribbon(
    aes(year, ymin = mort.num, ymax = mort.no.recov22.num),
    fill = I('darkgreen'),
    data = subset(dta,year>=2021),
    alpha = I(0.4)
  ) +
  # geom_ribbon(
  #   aes(year, ymin = mort.cf.lo.num, ymax = mort.cf.hi.num),
  #   fill = I('red'),
  #   data = subset(dta,year>=2019),
  #   alpha = I(0.4)
  # ) +
  annotate(
    'text',
    x = 2018,
    y = 900000,
    label = paste('Excess TB deaths: +',round(excess,0),
                  "( UI ",round(excess.lo,0),"-",round(excess.hi,0),")") ,
    hjust = 0,
    size = I(3),
    colour="red"
  )+
  annotate(
    'text',
    x = 2018,
    y = 1800000,
    label = paste('Averted TB deaths: -',round(dta$mort.diff.no.recov22.num[dta$year==2022],0),
                  "( UI ",-round(dta$mort.diff.no.recov22.num.hi[dta$year==2022],0),"-",round(dta$mort.diff.no.recov22.num.lo[dta$year==2022],0),")") ,
    hjust = 0,
    size = I(3),
    colour="darkgreen"
  )+

  
scale_x_continuous(name="Year", breaks=seq(2010,2022,2))+
  scale_y_continuous(name="Number of deaths")+
  
  expand_limits(y = 0) +
  theme_gtb()


plot.excess.avert.mort  


ggsave(here('inc_mort/output/excess_avert_mort.png'), plot=plot.excess.avert.mort, width=9, height=6)


#Save

excessmort=dta

attr(excessmort, "timestamp") <- Sys.Date() #set date
save(excessmort, file = here('inc_mort/analysis/excessmort.rda'))
fwrite(excessmort, file = here(paste0('inc_mort/analysis/csv/excessmort_', Sys.Date(), '.csv')))



