library(HEdtree)
library(discly)

logit <- function(x) log(odds(x))
ilogit <- function(x) iodds(exp(x))
odds <- function(x) x/(1-x)
iodds <- function(x) x/(1+x)


## making life years
GetLifeYears <- function(isolist,discount.rate,yearfrom){
    ## template:
    LYT <- data.table(age=0:4,
                      age_group=rep('0-4',5),
                      LYS=0.0)
    ## make country/age key
    LYK <- list()
    for(iso in isolist){
        ## iso <- cn
        tmp <- copy(LYT)
        tmp[,iso3:=iso]
        for(ag in tmp$age)
            tmp[age==ag,LYS:=discly::discly(iso3=iso,
                                            age=ag,
                                            yearnow=yearfrom,
                                            sex='Total',
                                            endyear = 2098,
                                            HR=1,
                                            dr=discount.rate,
                                            hiv='both'
                                            )]
        LYK[[iso]] <- tmp
    }
    LYK <- rbindlist(LYK)
    ## assume unweighted & collapse
    LYK <- LYK[,.(LYS=mean(LYS)),by=.(iso3,age=age_group)]
    setkey(LYK,age)
    LYK
}


## actually do work if needed
isoz <- c('KHM','CMR','CIV','MOZ','SLE','UGA','ZMB')
key <- data.table(iso3=isoz,country=cnz)
fn <- here('data/LYK.Rdata')
if(!file.exists(fn)){
  ## make discounted life-years if they haven't been done
  LYKc <- GetLifeYears(isolist=isoz,discount.rate=0.03,yearfrom=2022)
  LYK <- GetLifeYears(isolist=isoz,discount.rate=0.00,yearfrom=2022)
  LYK <- merge(LYK,key,by='iso3')
  LYKc <- merge(LYKc,key,by='iso3')
  save(LYKc,file=here('data/LYKc.Rdata'))
  save(LYK,file=fn)
} else {
  load(fn)
  load(file=here('data/LYKc.Rdata'))
}

## CFRs

## prior parameters
PD0 <- read.csv(here('data/SAMparameters.csv')) #read in

## combine different parameter types
P <- parse.parmtable(PD0)             #convert into parameter object

## for now neglect HIV
AddCFRs <- function(D){
  ## background SAM mortality
  D[,SAMmort:= P$s.SAMmort$r(nrow(D))]
  ## WHO version
  D[TB=='not TB' & who.ATT==0,who.cfr:=0+SAMmort]
  D[TB=='not TB' & who.ATT==1,who.cfr:=0+SAMmort]
  D[TB=='TB' & who.ATT==0,who.cfr:=cfr.noatt+SAMmort]
  D[TB=='TB' & who.ATT==1,who.cfr:=cfr.att+SAMmort]
  ## SOC
  D[TB=='not TB' & soc.ATT==0,soc.cfr:=0+SAMmort]
  D[TB=='not TB' & soc.ATT==1,soc.cfr:=0+SAMmort]
  D[TB=='TB' & soc.ATT==0,soc.cfr:=cfr.noatt+SAMmort]
  D[TB=='TB' & soc.ATT==1,soc.cfr:=cfr.att+SAMmort]
  ## 1 step
  D[TB=='not TB' & tbs1.ATT==0,tbs1.cfr:=0+SAMmort]
  D[TB=='not TB' & tbs1.ATT==1,tbs1.cfr:=0+SAMmort]
  D[TB=='TB' & tbs1.ATT==0,tbs1.cfr:=cfr.noatt+SAMmort]
  D[TB=='TB' & tbs1.ATT==1,tbs1.cfr:=cfr.att+SAMmort]
  ## 2 step
  D[TB=='not TB' & tbs2.ATT==0,tbs2.cfr:=0+SAMmort]
  D[TB=='not TB' & tbs2.ATT==1,tbs2.cfr:=0+SAMmort]
  D[TB=='TB' & tbs2.ATT==0,tbs2.cfr:=cfr.noatt+SAMmort]
  D[TB=='TB' & tbs2.ATT==1,tbs2.cfr:=cfr.att+SAMmort]
  ## cap
  cap <- c('who.cfr','soc.cfr','tbs1.cfr','tbs2.cfr')
  D[,(cap):=lapply(.SD,function(x)pmin(1,x)),.SDcols=cap]
}


## ## NOTE try to gather all stochastic things into there
## note we have a stochastic model
getAlgoParms <- function(N,hiv=NULL){
  D <- data.table(id=1:N)
  ## coverage of elements
  D[,ptb:=ifelse(P$s.soc.ptbcov$r(nrow(D))>runif(nrow(D)),1,0)]
  D[,testing.done:=ifelse(P$s.soc.testingcov$r(nrow(D))>runif(nrow(D)),1,0)]
  D[,xray.only:=ifelse(P$s.soc.CXRonly$r(nrow(D))>runif(nrow(D)),1,0)]
  ## accuracy
  D[,clin.sense:=ifelse(P$sens.clin$r(nrow(D))>runif(nrow(D)),1,0)]
  D[,clin.spec:=ifelse(P$spec.clin$r(nrow(D))>runif(nrow(D)),1,0)]
  D[,clin.senseX:=ifelse(P$sens.clinCXR.soc$r(nrow(D))>runif(nrow(D)),1,0)]
  D[,clin.specX:=ifelse(P$spec.clinCXR.soc$r(nrow(D))>runif(nrow(D)),1,0)]
  ## combining clinical and GA
  tmp.sens <- 1 - (1-P$sens.clin$r(nrow(D))) * (1-P$sens.xga$r(nrow(D)))
  tmp.spec <- 1 - (P$spec.clin$r(nrow(D))) * (1-P$spec.xga$r(nrow(D)))
  D[,clin.senseU:=ifelse(tmp.sens>runif(nrow(D)),1,0)]
  D[,clin.specU:=ifelse(tmp.spec>runif(nrow(D)),1,0)]
  ## reassessment etc
  D[,reassess:=ifelse(P$s.reassess$r(nrow(D))>runif(nrow(D)),1,0)]
  ## CFRs for assigment
  D[,cfr.noatt:=P$notx.u5$r(nrow(D))]
  D[,cfr.att:=P$ontx.u5$r(nrow(D))]
  if(!is.null(hiv)){
    ## -- hivartOR on tx
    Z <- P$hivartOR$r(nrow(D))
    Z <- rowSums(Z) #HIV+/ART+
    D[,cfr.att:=ilogit(
         logit(cfr.att) + Z * hiv
       )]
    ## -- HIV/ART off tx
    D[hiv==1,cfr.noatt:=P$notxHA.u5$r(sum(hiv==1))]
  }
  return(D)
}


## ## parameter notes
## ## s.soc.ptbcov - based on expert opinion @ 80%, take +/-10
## (tpz <- getAB(0.8,(10/196)^2))
## curve(dbeta(x,tpz$a,tpz$b),n=500)
## ## s.soc.testingcov
## (tpz <- getAB((0.8+0.95)/2,(0.95-0.8)^2))
## curve(dbeta(x,tpz$a,tpz$b),n=500) #NOTE mode @ 1 but concords with description from experts
## ## s.soc.CXRonly
## (tpz <- getAB(0.75,(50/392)^2))
## curve(dbeta(x,tpz$a,tpz$b),n=500) #NOTE 1-frac with GA done as well
## s.reassess - 10,70,50 
## (tpz <- getAB(0.5,((70-10)/392)^2))
## curve(dbeta(x,tpz$a,tpz$b),n=500)



RRD <- fread(here('data/MDR_RR_TB_burden_estimates_2020-10-15.csv'))
RRD[,c('rr.mid','rr.sd'):=.(e_rr_pct_new/100,(e_rr_pct_new_hi-e_rr_pct_new_lo)/392)]
## model as gamma parameters
RRD[,theta:=rr.sd^2/rr.mid]
RRD[,k:=rr.mid/theta]
## check
## ckk <- 5
## curve(dgamma(x,shape=RRD$k[ckk],scale=RRD$theta[ckk]),from = 0,to=0.1,n=500)

makeRRdata <- function(N){
  RDL <- RRD[rep(1:nrow(RRD),N),.(country,k,theta)]
  RDL[,id:=rep(1:N,each=nrow(RRD))]
  RDL[,rrp := rgamma(n=nrow(RDL),shape=k,scale=theta)]
  RDL[is.na(rrp),rrp:=0.0]
  RDL[,.(country,id,rrp)]
}
