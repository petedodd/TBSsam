library(discly)

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
library(HEdtree)

## prior parameters
PD0 <- read.csv(here('data/parameters.csv')) #read in

## combine different parameter types
P <- parse.parmtable(PD0)             #convert into parameter object

## for now neglect HIV
AddCFRs <- function(D,algo='WHO'){
  if(algo=='WHO'){
    D[TB=='not TB' & who.ATT==0,who.cfr:=0]
    D[TB=='not TB' & who.ATT==1,who.cfr:=0]
    D[TB=='TB' & who.ATT==0,who.cfr:=P$notx.u5$r(sum(TB=='TB' & who.ATT==0))]
    D[TB=='TB' & who.ATT==1,who.cfr:=P$ontx.u5$r(sum(TB=='TB' & who.ATT==1))]
  } else {
    ## 1 step
    D[TB=='not TB' & tbs1.ATT==0,tbs1.cfr:=0]
    D[TB=='not TB' & tbs1.ATT==1,tbs1.cfr:=0]
    D[TB=='TB' & tbs1.ATT==0,tbs1.cfr:=P$notx.u5$r(sum(TB=='TB' & tbs1.ATT==0))]
    D[TB=='TB' & tbs1.ATT==1,tbs1.cfr:=P$ontx.u5$r(sum(TB=='TB' & tbs1.ATT==1))]
    ## 2 step
    D[TB=='not TB' & tbs2.ATT==0,tbs2.cfr:=0]
    D[TB=='not TB' & tbs2.ATT==1,tbs2.cfr:=0]
    D[TB=='TB' & tbs2.ATT==0,tbs2.cfr:=P$notx.u5$r(sum(TB=='TB' & tbs2.ATT==0))]
    D[TB=='TB' & tbs2.ATT==1,tbs2.cfr:=P$ontx.u5$r(sum(TB=='TB' & tbs2.ATT==1))]
  }
}

