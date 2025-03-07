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
isoz <- c( "UGA", "ZMB")
cnz <- c("Uganda", "Zambia")
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
P <- parse.parmtable(PD0[,1:2])             #convert into parameter object
## P <- parse.parmtable(PD0[, 1:2],testdir = here("data/test")) # convert into parameter object

## NOTE rewrite any parameters specificied for SA
if (SA != "" & SA %in% names(P)) {
  cat("SA: Overwriting ", SA, " with ", LM, "!\n")
  if (LM == "LQ") {
    val <- P[[SA]]$q(0.25) # what is the LQ value?
    P[[SA]]$r <- function(n) rep(val, n) # overwrite RNG
  } else if (LM == "UQ") {
    val <- P[[SA]]$q(0.75) # what is the UQ value?
    P[[SA]]$r <- function(n) rep(val, n) # overwrite RNG
  } else {
    stop(paste0("LM supplied as ", LM, " but must be UQ or LQ!"))
  }
}



## for now neglect HIV
AddCFRs <- function(D){
  ## background SAM mortality
  D[, SAMmort := P$s.CFR.sam.noTB$r(nrow(D))]
  D[, SAMmortTBATT := P$s.CFR.sam.TBATT$r(nrow(D))]
  D[, SAMmortTBnoATT := P$s.CFR.sam.TBnoATT$r(nrow(D))]
  ## WHO version
  D[TB == "not TB" & who.ATT == 0, who.cfr := SAMmort]
  D[TB == "not TB" & who.ATT == 1, who.cfr := SAMmort]
  D[TB == "TB" & who.ATT == 0, who.cfr := SAMmortTBnoATT]
  D[TB == "TB" & who.ATT == 1, who.cfr := SAMmortTBATT]
  ## SOC
  D[TB == "not TB" & soc.ATT == 0, soc.cfr := SAMmort]
  D[TB == "not TB" & soc.ATT == 1, soc.cfr := SAMmort]
  D[TB == "TB" & soc.ATT == 0, soc.cfr := SAMmortTBnoATT]
  D[TB == "TB" & soc.ATT == 1, soc.cfr := SAMmortTBATT]
  ## 1 step
  D[TB == "not TB" & tbs1.ATT == 0, tbs1.cfr := SAMmort]
  D[TB == "not TB" & tbs1.ATT == 1, tbs1.cfr := SAMmort]
  D[TB == "TB" & tbs1.ATT == 0, tbs1.cfr := SAMmortTBnoATT]
  D[TB == "TB" & tbs1.ATT == 1, tbs1.cfr := SAMmortTBATT]
  ## 2 step
  D[TB == "not TB" & tbs2.ATT == 0, tbs2.cfr := SAMmort]
  D[TB == "not TB" & tbs2.ATT == 1, tbs2.cfr := SAMmort]
  D[TB == "TB" & tbs2.ATT == 0, tbs2.cfr := SAMmortTBnoATT]
  D[TB == "TB" & tbs2.ATT == 1, tbs2.cfr := SAMmortTBATT]
  ## no ATT
  D[TB == "not TB", nothing.cfr := SAMmort]
  D[TB == "TB", nothing.cfr := SAMmortTBnoATT]
  ## cap
  cap <- c("who.cfr", "soc.cfr", "tbs1.cfr", "tbs2.cfr","nothing.cfr")
  D[, (cap) := lapply(.SD, function(x) pmin(1, x)), .SDcols = cap]
}


## ## NOTE try to gather all stochastic things into there
## note we have a stochastic model: variables returned typically 0/1
## NOTE this is applied before TB disease status is available
getAlgoParms <- function(N,hiv=NULL){
  D <- data.table(id=1:N)
  ## coverage of elements
  D[, soc.screened := ifelse(P$s.soc.scrcov$r(nrow(D)) > runif(nrow(D)), 1, 0)]
  D[, testing.done := ifelse(P$s.soc.testingcov$r(nrow(D)) > runif(nrow(D)), 1, 0)]
  D[, xray.only := ifelse(P$s.soc.CXRonly$r(nrow(D)) > runif(nrow(D)), 1, 0)]
  D[, xpert.only := ifelse(P$s.soc.Xpertonly$r(nrow(D)) > runif(nrow(D)), 1, 0)]
  ## accuracy
  D[, s.screen.se := ifelse(P$s.soc.screen.se$r(nrow(D)) > runif(nrow(D)), 1, 0)]
  D[, s.screen.sp := ifelse(P$s.soc.screen.sp$r(nrow(D)) > runif(nrow(D)), 1, 0)]
  ## SE/SP of assessment from SAM cohort
  D[, clin.sense := ifelse(P$s.soc.clin.se$r(nrow(D)) > runif(nrow(D)), 1, 0)]
  D[, clin.spec := ifelse(P$s.soc.clin.sp$r(nrow(D)) > runif(nrow(D)), 1, 0)]
  D[, clin.senseX := ifelse(P$s.soc.clinCXR.se$r(nrow(D)) > runif(nrow(D)), 1, 0)]
  D[, clin.specX := ifelse(P$s.soc.clinCXR.sp$r(nrow(D)) > runif(nrow(D)), 1, 0)]
  D[, clin.senseU := ifelse(P$s.soc.clinGA.se$r(nrow(D)) > runif(nrow(D)), 1, 0)]
  D[, clin.specU := ifelse(P$s.soc.clinGA.sp$r(nrow(D)) > runif(nrow(D)), 1, 0)]
  D[, clin.senseXU := ifelse(P$s.soc.clinCXRGA.se$r(nrow(D)) > runif(nrow(D)), 1, 0)]
  D[, clin.specXU := ifelse(P$s.soc.clinCXRGA.sp$r(nrow(D)) > runif(nrow(D)), 1, 0)]
  ## reassessment etc
  D[, s.reassess.choice.se := ifelse(P$s.reassess.choice.se$r(nrow(D)) > runif(nrow(D)), 1, 0)]
  D[, s.reassess.choice.sp := ifelse(P$s.reassess.choice.sp$r(nrow(D)) > runif(nrow(D)), 1, 0)]
  D[, s.nonsocreassess.se := ifelse(P$s.nonsocreassess.se$r(nrow(D)) > runif(nrow(D)), 1, 0)]
  D[, s.nonsocreassess.sp := ifelse(P$s.nonsocreassess.sp$r(nrow(D)) > runif(nrow(D)), 1, 0)]
  D[, s.reassess.se := fcase(
        testing.done==0, #clinical
        ifelse(1-P$s.soc.reassessafterclin.sebar$r(nrow(D)) > runif(nrow(D)),1,0), #NOTE sebar
        testing.done==1 & xray.only==1 & xpert.only==0, #clin+CXR
        ifelse(1-P$s.soc.reassessafterclinCXR.sebar$r(nrow(D)) > runif(nrow(D)),1,0), #NOTE sebar
        testing.done==1 & xray.only==0 & xpert.only==1, #clin+Xpert
        ifelse(1-P$s.soc.reassessafterclinGA.sebar$r(nrow(D)) > runif(nrow(D)),1,0), #NOTE sebar
        testing.done==1 & xray.only==0 & xpert.only==0, #clin+CXR+Xpert
        clin.senseXU,                                   #same ans as 1st go
        default=0)]
  D[, s.reassess.sp := fcase(
        testing.done==0, #clinical
        ifelse(P$s.soc.reassessafterclin.sp$r(nrow(D)) > runif(nrow(D)),1,0),
        testing.done==1 & xray.only==1 & xpert.only==0, #clin+CXR
        ifelse(P$s.soc.reassessafterclinCXR.sp$r(nrow(D)) > runif(nrow(D)),1,0),
        testing.done==1 & xray.only==0 & xpert.only==1, #clin+Xpert
        ifelse(P$s.soc.reassessafterclinGA.sp$r(nrow(D)) > runif(nrow(D)),1,0),
        testing.done==1 & xray.only==0 & xpert.only==0, #clin+CXR+Xpert
        clin.specXU,                                    #same ans as 1st go
        default=0)]
  return(D)
}


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
