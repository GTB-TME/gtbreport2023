## 9 Oct 2023 - using updated survey results for KHM

## libraries
library(here)
library(glue)
library(data.table)
library(ggplot2)
library(MASS)
library(patchwork)
gh <- function(x) glue(here(x))
rot45 <- theme(axis.text.x = element_text(angle = 45, hjust = 1))


## ============= DATA ===========================

load(here('drtb/data/RPD.Rdata'))

KHMD <- RPD[grepl('KHM',id)]

## NOTE updating the 2018 data:
## New: 1.2% (0.6-2.6%)
## Previously treated: 13.2% (7.2% - 23.0%)
## adjust
KHMD[patients=='new' & year==2018,
     c('RR.mid','RR.lo','RR.hi'):=.(1.2, 0.6, 2.6)]
KHMD[patients=='ret' & year==2018,
     c('RR.mid','RR.lo','RR.hi'):=.(13.2, 7.2, 23.0)]

## graph
ggplot(KHMD,aes(x=year,y=RR.mid,ymin=RR.lo,ymax=RR.hi,shape=patients,col=patients))+
  geom_pointrange()+scale_y_sqrt()+facet_wrap(~patients)


## ============= PREVALENCE ===========================


## use Bayesian LR in log space with known errors
## y = x^Tb+e
## var(e_i) = s_i^2
## S = diag(s_i^2)
## posterior (vague conjugate prior) b ~ MVN(a,V)
## V = (X^TS^{-1}X)^{-1}
## a = V X^{T}S^{-1}y


getLNmu <- function(m,s){log(m^2/sqrt(m^2+s^2))}
getLNsig <- function(m,s){sqrt(log(1+s^2/m^2))}

KHMD[,Sreal:=(RR.hi-RR.lo)/396]
KHMD[,mu:=getLNmu(RR.mid/1e2,Sreal)]
KHMD[,S:=getLNsig(RR.mid/1e2,Sreal)]
KHMD[,DT:=year-2000]

## === new
NY <- 23 #number of years

## fit NOTE excluding old survey
newd <- KHMD[year>2001 & patients=='new',.(DT,mu,S)]
X <- cbind(c(1,1),newd$DT)
iSIG <- diag(1/newd$S^2)
invV <- t(X) %*% iSIG %*% X
V <- solve(invV)
a <- V %*% t(X) %*% iSIG %*% newd$mu

## predictions
B <- mvrnorm(1e4,a,V)
A <- list()
for(i in 1:NY) A[[i]] <- data.table(DT=rep(i-1,nrow(B)),iter=1:nrow(B),
                                    RR = 1e2*exp(B[,1] + B[,2]*(i-1)) )
A <- rbindlist(A)
A[,year:=2000 + DT]
Anew <- A <- A[,.(RR.mid=mean(RR),RR.lo=quantile(RR,0.025),RR.hi=quantile(RR,0.975)),by=year]

## tmp <- KHMD[patients=='new',.(year,RR.mid=mu,RR.lo=mu-1.96*S,RR.hi=mu+1.96*S)]
tmp <- KHMD[patients=='new',.(year,RR.mid,RR.lo,RR.hi)]

gpn <- ggplot(A,aes(year,RR.mid,ymin=RR.lo,ymax=RR.hi))+
  geom_ribbon(fill='grey',col=NA,alpha=0.5) +
  geom_line()+
  geom_pointrange(data=tmp)+
  ylab('RR prevalence (%)')+ggtitle('New patients')


## ret NOTE excluding old survey
retd <- KHMD[year>2001 & patients=='ret',.(DT,mu,S)]

X <- cbind(c(1,1),retd$DT)
iSIG <- diag(1/retd$S^2)
invV <- t(X) %*% iSIG %*% X
V <- solve(invV)
a <- V %*% t(X) %*% iSIG %*% retd$mu

## predictions
B <- mvrnorm(1e4,a,V)
A <- list()
for(i in 1:NY) A[[i]] <- data.table(DT=rep(i-1,nrow(B)),iter=1:nrow(B),
                                    RR = 1e2*exp(B[,1] + B[,2]*(i-1)) )
A <- rbindlist(A)
A[,year:=2000 + DT]
Aret <- A <- A[,.(RR.mid=mean(RR),RR.lo=quantile(RR,0.025),RR.hi=quantile(RR,0.975)),by=year]

## tmp <- KHMD[patients=='ret',.(year,RR.mid=mu,RR.lo=mu-1.96*S,RR.hi=mu+1.96*S)]
tmp <- KHMD[patients=='ret',.(year,RR.mid,RR.lo,RR.hi)]

gpr <- ggplot(A,aes(year,RR.mid,ymin=RR.lo,ymax=RR.hi))+
  geom_ribbon(fill='grey',col=NA,alpha=0.5) +
  geom_line()+
  geom_pointrange(data=tmp)+
  ylab('RR prevalence (%)')+ggtitle('Ret patients')

## joint fit plot

GP <- gpn + gpr
GP

ggsave(GP,file=gh('drtb/prevplots/plots/KHMrefit.png'),w=7,h=5)


## joint data
Anew[,c('iso3','patients','type','g_whoregion'):=.('KHM','new','RR','SEA')]
Aret[,c('iso3','patients','type','g_whoregion'):=.('KHM','ret','RR','SEA')]
KO <- rbind(Anew,Aret)


## ================= INCIDENCE ============

## utilities
source(here('import/load_gtb.R'))
source(here('drtb/R/utils/incdr.R'))
source(here('drtb/R/utils/ftb.R'))
gh <- function(x) glue(here(x))
ssum <- function(x,...) sqrt(sum(x^2,...)) #sqrt sum sqrs
brkt <- function(x,y) paste0(ftb(x),' (',
                             ftb(pmax(0,x-1.96*y)),' - ',
                             ftb(x+1.96*y),')')
getdate <- function()glue(gsub('-','-',Sys.Date()))

## NOTE
estyr <- 2022

## main data
load(file=here('drtb/data/rhofnr.Rdata'))
est <- load_gtb('est',convert_dashes=TRUE) #NOTE not there yet

## results from this year on
cutyr <- 2015

## create incidence
tout <- addIncidence(KO[year>1999],secondorder = FALSE)


## --- create source string ---
load(gh('drtb/data/LB.Rdata'))
load(gh('drtb/data/YB.Rdata'))
svy.new <- YB[year>=cutyr & patients=='new',unique(iso3)]
svy.ret <- YB[year>=cutyr & patients=='ret',unique(iso3)]
svl.new <- LB[year>=cutyr & patients=='new',unique(iso3)]
svl.ret <- LB[year>=cutyr & patients=='ret',unique(iso3)]
src.new <- data.table(iso3=est[,unique(iso3)])
src.ret <- data.table(iso3=est[,unique(iso3)])
## new
src.new[iso3 %in% svy.new,source_new:='Survey']
src.new[iso3 %in% svl.new,source_new:='Surveillance']
src.new[iso3 %in% intersect(svy.new,svl.new),
        source_new:='Survey & Surveillance']
src.new[is.na(source_new),source_new:='Model']
src.new[,table(source_new)]
## ret
src.ret[iso3 %in% svy.ret,source_ret:='Survey']
src.ret[iso3 %in% svl.ret,source_ret:='Surveillance']
src.ret[iso3 %in% intersect(svy.ret,svl.ret),
        source_ret:='Survey & Surveillance']
src.ret[is.na(source_ret),source_ret:='Model']
src.ret[,table(source_ret)]
## join
src.both <- merge(src.new,src.ret,by='iso3')

## --------- country-level db output

## Filename
## db_dr_country_yyyy-mm-dd.csv
## Structure
## iso3,
## year,
## source_new,                           (“Survey” or “Surveillance” or “Model”)
## source_drs_year_new,
## source_drs_all_areas_new,       (1 for national, 0 for sub-national data source)
## e_rr_prop_new,
## e_rr_prop_new_lo,
## e_rr_prop_new_hi,
## e_rr_prop_new_se,
## e_mdr_prop_rr_new,
## source_ret,                        (“Survey” or “Surveillance” or “Model”)
## source_drs_year_ret,
## source_drs_all_areas_ret,       (1 for national, 0 for sub-national data source)
## e_rr_prop_ret,
## e_rr_prop_ret_lo,
## e_rr_prop_ret_hi,
## e_rr_prop_ret_se,
## e_mdr_prop_rr_ret,
## e_inc_rr_num,
## e_inc_rr_num_lo,
## e_inc_rr_num_hi,


## join proportion and incidence data
KOD <- dcast(KO,iso3 + year + g_whoregion ~ patients,
             value.var = c('RR.mid','RR.lo','RR.hi'))
KOD <- KOD[,.(iso3,year,
              e_rr_prop_new=RR.mid_new/1e2,
              e_rr_prop_new_lo=RR.lo_new/1e2,
              e_rr_prop_new_hi=RR.hi_new/1e2,
              e_rr_prop_new_se=(RR.hi_new-RR.lo_new)/392,
              e_rr_prop_ret=RR.mid_ret/1e2,
              e_rr_prop_ret_lo=RR.lo_ret/1e2,
              e_rr_prop_ret_hi=RR.hi_ret/1e2,
              e_rr_prop_ret_se=(RR.hi_ret-RR.lo_ret)/392
              )]
KOE <- tout[,.(iso3,year,
               e_inc_rr_num=inc.rr,
               e_inc_rr_num_lo=pmax(0,inc.rr-1.96*inc.rr.sd),
               e_inc_rr_num_hi=inc.rr+1.96*inc.rr.sd
               )]
KOE <- merge(KOD,KOE,by=c('iso3','year'))
attr(KOE, "timestamp") <- Sys.Date() #set date

## save all time version
fn <- gh('drtb/outdata/KOE.KHMfix.rda')
save(KOE,file=fn)


## restrict to post 2015
db_dr_country <- KOE[year>=cutyr]
db_dr_country <- merge(src.both,db_dr_country,by='iso3',all.y=TRUE) #merge in src
attr(db_dr_country, "timestamp") <- Sys.Date() #set date

fn <- gh('drtb/outdata/db_dr_country.KHMfix.rda')
save(db_dr_country,file=fn)


## read in db version and correct
fn <- gh('drtb/outdata/db_dr_country.KHMfix.rda')
load(file=fn)

fn <- gh('drtb/dboutput/db_dr_country.KHMfix.KHMonly.rda')
save(db_dr_country,file=fn)

fix <- copy(db_dr_country)

fn <- gh('drtb/dboutput/db_dr_country.rda')
load(file=fn)
db_dr_country[iso3=='KHM']

db_dr_country <- db_dr_country[iso3!='KHM']
db_dr_country <- rbind(db_dr_country,fix)
db_dr_country <- db_dr_country[order(iso3,year)]
attr(db_dr_country, "timestamp") <- Sys.Date() #set date

fn <- gh('drtb/dboutput/db_dr_country.KHMfix.Allcountries.rda')
save(db_dr_country,file=fn)
