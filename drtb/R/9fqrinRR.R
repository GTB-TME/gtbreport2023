## simple version of FQR in RR
rm(list = ls())

## libraries
library(here)
library(glue)
library(data.table)
library(ggplot2)
library(scales)
library(metafor)
gh <- function(x)glue(here(x))
ssum <- function(x,...) sqrt(sum(x^2,...)) #sqrt sum sqrs
rot45 <- theme(axis.text.x = element_text(angle = 45, hjust = 1))

## utilities &  helper functions for binomial CIs, incidence calculator
source(gh('drtb/R/utils/fun.R'))
source(gh('drtb/R/utils/CIhelpers.R'))
## source(here('drtb/R/utils/incdr.R'))
source(here('import/load_gtb.R'))

## globals
yr <- 2022

## double check
dic <- load_gtb('dic')
setkey(dic,variable_name)
names(dic)

dic['rr_dst_rlt',definition] #N
dic['rr_dr_fq',definition] #k

dic['rr_dst_rlt_fq',definition] #N
dic['rr_fqr',definition]        #k


## data
drfq <- load_gtb('drfq')

noests.yet <- FALSE
if(noests.yet){
  load(here('drtb/tmpdata/est.rda'))
} else {
  est <- load_gtb('est',convert_dashes=TRUE)
}
## 30 hi mdr in different form
## tmp <- unique(grpmbr[,.(iso3,g.mdr=FALSE)])
## tmp[iso3 %in% grpmbr[group_type=='g_hb_mdr',iso3],g.mdr:=TRUE]
key <- unique(est[,.(iso3,g.mdr)])
key[,unique(g.mdr)]


## NOTE example bad
drfq[iso3=='ZWE',.(iso3,g_whoregion,year,mdr_dr_fq,mdr_dst_rlt)]

## surveillance
FDl <- drfq[,.(iso3,g_whoregion,year,
             rr.k=rr_dr_fq,
             rr.N=rr_dst_rlt,
             `rr+.k`=rr_fqr,
             `rr+.N`=rr_dst_rlt_fq,
             mdr.k=mdr_dr_fq,mdr.N=mdr_dst_rlt
             )]
## surveys
FDy <- drfq[!is.na(rr_fqr_pcnt),
            .(iso3,g_whoregion,year,type='rr',N=NA_integer_,k=NA_integer_,
              FQR.mid=rr_fqr_pcnt,FQR.lo=rr_fqr_pcnt_lo,FQR.hi=rr_fqr_pcnt_hi,
              source='survey')]
## removal flags
FDl[,droprr:=ifelse(is.na(rr.k+rr.N)| rr.N==0,TRUE,FALSE)]
FDl[,droprrp:=ifelse(is.na(`rr+.k`+`rr+.N`)| `rr+.N`==0,TRUE,FALSE)]
FDl[,dropmdr:=ifelse(is.na(mdr.k+mdr.N)| mdr.N==0,TRUE,FALSE)]

## reshape
FM <- melt(FDl,id=c('iso3','g_whoregion','year','droprr','droprrp','dropmdr'))
FM[,c('type','frac'):=tstrsplit(variable,split='\\.')]


## do removals
FM <- FM[!(droprr==TRUE & type=='rr')]
FM <- FM[!(droprrp==TRUE & type=='rr+')]
FM <- FM[!(dropmdr==TRUE & type=='mdr')]

FM <- FM[,.(value=sum(value)),by=.(iso3,g_whoregion,year,type,frac)] #NOTE a few repeats

## right format
FM <- dcast(FM[,.(iso3,g_whoregion,year,type,frac,value)],
            iso3 + g_whoregion + year + type ~ frac,
            value.var = 'value')

## add CIs
FM[,c('FQR.mid','FQR.lo','FQR.hi'):=MLH(k,N)]
FM[,source:='surveillance']

## both types
FB <- rbind(FM,FDy)
save(FB,file=gh('drtb/data/FB.Rdata'))

## plot
for(reg in unique(FB[,g_whoregion])){
  print(reg)
  GP <- ggplot(FB[g_whoregion==reg],aes(year,FQR.mid,
                                         ymin=FQR.lo,ymax=FQR.hi,
                                         shape=type,col=source))+
    geom_line()+geom_point()+geom_pointrange()+
    scale_y_sqrt()+
    facet_wrap(~iso3,scales='free')+
    rot45
  ggsave(GP,file=gh('drtb/plots/f_{reg}.pdf'),w=12,h=15)
}


## carry on
load(file=gh('drtb/data/FB.Rdata'))


## simple meta-analyses
FB[,range(year)]
FB[,table(iso3)]
FB[,FQR.sd:=(FQR.hi-FQR.lo)/3.92]
cnt <- FB[,.(n=.N),by=iso3] #data count
FB <- merge(FB,cnt,by='iso3',all.x = TRUE) #merge in

## step 1 MA for countries with >1 point
FBS <- FB[n>1]
FBR <- list()
for(iso in FBS[,unique(iso3)]){
  print(iso)
  tmp <- FBS[iso3==iso]
  fit <- rma(yi = FQR.mid,sei = FQR.sd,data = tmp)
  imp <- fit$b[, 1]
  imp.sd <- fit$se
  if(abs(imp)>1e-10){       #>0
    if(abs(imp-100)<1e-10){#all(mid==100) use halway from 100 to mean of lo
      imp.ui <- vlohi(tmp[,50+0.5*mean(FQR.lo)]/1e2, imp.sd/1e2)*1e2
    } else { #normal case 0<mid<100
      imp.ui <- vlohi(unlist(imp)/1e2, imp.sd/1e2)*1e2
    }
  } else { #all(mid==0) use half mean of hi 
    imp.ui <- vlohi(tmp[,0.5*mean(FQR.hi)]/1e2, imp.sd/1e2)*1e2
  }
  tmp <- tmp[1]
  tmp[,c('year','N','k'):=NA_integer_]
  tmp[,c('FQR.mid','FQR.lo','FQR.hi','FQR.sd'):=.(imp,imp.ui[1],imp.ui[2],imp.sd)]
  FBR[[iso]] <- tmp
}
FBR <- rbindlist(FBR)

## step 2 MA
FBR <- rbind(FBR,FB[n==1]) #add in the single year ones to make full data set for MA
FBR <- merge(FBR,key,by='iso3',all.x=TRUE)
fit <- rma(yi = FQR.mid,sei = FQR.sd,data = FBR,mods = ~ g.mdr-1)
reg <- names(table(FBR$g.mdr))


## extract
imp <- fit$b[, 1]
imp.sd <- fit$se
imp.ui <- vlohi(unlist(imp)/1e2, imp.sd/1e2)*1e2

xtra <- key[!iso3 %in% FBR$iso3]
xtra[,c('FQR.mid','FQR.lo','FQR.hi','FQR.sd'):=NA_real_]

## check the order of regions in reg is the same as in imp
all.equal(reg, gsub('g.mdr', '', dimnames(fit$beta)[[1]]))

## loop over regions
for (i in 1:length(reg)){
  print(reg[i])
  xtra[g.mdr==reg[i],
       c('FQR.mid','FQR.lo','FQR.hi','FQR.sd'):=.(
         imp[i],imp.ui[1,i],imp.ui[2,i],imp.sd[i]
       )]
}

## join to non-imputed
FQR <- rbind(FBR[,.(iso3,FQR.mid,FQR.lo,FQR.hi,FQR.sd)],
             xtra[,.(iso3,FQR.mid,FQR.lo,FQR.hi,FQR.sd)])

save(FQR,file=gh('drtb/data/FQR.Rdata'))


## aggregate reformat, out
FQR_in_RR_country <- FQR
attr(FQR_in_RR_country, "timestamp") <- Sys.Date() #set date
save(FQR_in_RR_country,file=gh('drtb/dboutput/FQR_in_RR_country.rda'))

load(here('drtb/dboutput/db_dr_country.rda'))


WHOkey <- unique(est[,.(iso3,g.whoregion)])
FQR <- merge(FQR,db_dr_country[year==yr,.(iso3,e_inc_rr_num)],by='iso3')
FQR <- merge(FQR,WHOkey,by='iso3')

## regional
FQR_in_RR_region <- FQR[,.(FQR.mid=weighted.mean(FQR.mid,w=e_inc_rr_num),
                           FQR.sd=sqrt(sum(FQR.sd^2*e_inc_rr_num^2)/sum(e_inc_rr_num)^2)),
                        by=g.whoregion]
FQR_in_RR_region[,c('FQR.lo','FQR.hi'):=.(pmax(0,FQR.mid-1.96*FQR.sd),
                                          pmin(100,FQR.mid+1.96*FQR.sd))]
attr(FQR_in_RR_region, "timestamp") <- Sys.Date() #set date
save(FQR_in_RR_region,file=gh('drtb/dboutput/FQR_in_RR_region.rda'))


## global
FQR_in_RR_global <- FQR[,.(FQR.mid=weighted.mean(FQR.mid,w=e_inc_rr_num),
                           FQR.sd=sqrt(sum(FQR.sd^2*e_inc_rr_num^2)/sum(e_inc_rr_num)^2))]
FQR_in_RR_global[,c('FQR.lo','FQR.hi'):=.(pmax(0,FQR.mid-1.96*FQR.sd),
                                          pmin(100,FQR.mid+1.96*FQR.sd))]
attr(FQR_in_RR_global, "timestamp") <- Sys.Date() #set date
save(FQR_in_RR_global,file=gh('drtb/dboutput/FQR_in_RR_global.rda'))
