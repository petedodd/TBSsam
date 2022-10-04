## modelling the scores on the synthetic populations
rm(list=ls())
library(here)
library(glue)
library(data.table)
library(ggplot2)
gh <- function(x) glue(here(x))
cnz <- c("Cambodia","Cameroon","Côte d'Ivoire","Mozambique",
         "Sierra Leone","Uganda","Zambia")

## load dependencies
source(gh('R/utils/scores.R')) #scores are coded in here
source(gh('R/utils/costutils.R')) #cost data parser
source(here('R/utils/readyoutcomes.R')) #parameters & life-years
source(here('R/utils/HEoutputs.R')) #various outputters


## load synthetic populations
load(file=gh('data/nPOPS.Rdata'))
load(file=gh('data/nPOPS0.Rdata'))
## load(file=gh('data/POPS.Rdata'))
## load(file=gh('data/POPS0.Rdata'))
## TODO clear out old data

set.seed(2345)

## add in scores:
pop <- POPS[["SAM_notTB"]]
pop0 <- POPS0[["SAM_notTB"]]
popt <- POPS[["SAM_TB"]]
popt0 <- POPS0[["SAM_TB"]]

## --- WHO
pop <- appendWHOscores(pop)
pop0 <- appendWHOscores(pop0)
popt <- appendWHOscores(popt)
popt0 <- appendWHOscores(popt0)

## --- TB Speed
pop <- appendTBSscores(pop)
pop0 <- appendTBSscores(pop0)
popt <- appendTBSscores(popt)
popt0 <- appendTBSscores(popt0)

## check se/sp
pop[,1-mean(score_X>10)] #specificity =51%
pop[,1-mean(score_noX>10)] #specificity =87%
popt[,mean(score_X>10)] #sensitivity =71%
popt[,mean(score_noX>10)] #sensitivity =39%

## check se/sp
pop[,1-mean(TBS1S>10)] #specificity =76%
pop[,1-mean(TBS2Sa>10 & TBS2Sb>10)] #specificity =86%
popt[,mean(TBS1S>10)] #sensitivity =84%
popt[,mean(TBS2Sa>10 & TBS2Sb>10)] #sensitivity =63%

## compare
pop0[,method:='no correlation']
pop[,method:='copulas']
popt0[,method:='no correlation']
popt[,method:='copulas']
pop0[,TB:='not TB']
pop[,TB:='not TB']
popt0[,TB:='TB']
popt[,TB:='TB']
CF <- rbindlist(list(pop,popt,pop0,popt0)) #all


CFS <- CF[,.(CXR=mean(score_X),noCXR=mean(score_noX),
             CXR.sd=sd(score_X),noCXR.sd=sd(score_noX),
             TBS1S=mean(TBS1S),TBS2Sa=mean(TBS2Sa),TBS2Sb=mean(TBS2Sb),
             TBS1S.sd=sd(TBS1S),TBS2Sa.sd=sd(TBS2Sa),TBS2Sb.sd=sd(TBS2Sb)),
          by=.(TB,method)][order(TB)] #very similar
CFS

fwrite(CFS,file=here('data/compare.summary.both.csv'))



## choose method (could continue but need to remember)
CF <- CF[method=='copulas']

## grow? (for more parameter sampling)
Nfold <- 10
CF <- CF[rep(1:nrow(CF),Nfold)]
CF[,id:=1:nrow(CF)]

## === create cost data
CD <- parsecosts(gh('data/TB-Speed_SAM_Costs.csv'))
CD[,c('cost.mid','cost.sd'):=.((High+Low)/2,(High-Low)/3.92)]
## model as gamma parameters
CD[,theta:=cost.sd^2/cost.mid]
CD[,k:=cost.mid/theta]
CDL <- CD[rep(1:nrow(CD),nrow(CF)),.(NAME,country,k,theta)]
CDL[,id:=rep(1:nrow(CF),each=nrow(CD))]
CDL[,value := rgamma(n=nrow(CDL),shape=k,scale=theta)]
CDL[is.na(value),value:=0.0]
CDW <- dcast(CDL,country+id~NAME,value.var = 'value')
CDW

## extend across countries & append:
## AddAlgoParms(CF) #mainly/all for SOC
CF[,CXR.avail:=1] #code as available
AP <- getAlgoParms(max(CF$id)) #mainly/all for SOC NOTE all stochastic elts here
CF <- merge(CF,AP,by='id')

## extend across countries;
CF <- CF[rep(1:nrow(CF),length(cnz))]
CF[,country:=rep(cnz,each=nrow(CF)/length(cnz))]

## TODO stool rather than GA params?
## === WHO algorithm
CF[P$s.soc.CXRonly$r(nrow(CF))>runif(nrow(CF)),Xpert_res:=NA] #for now assume same mWRD avail as via GA in SOC

## merge in costs
CF <- merge(CF,CDW,by=c('id','country'))

## === WHO algorithm
## apply to data (appends ATT)
WHO.algorithm(CF)
## === SOC algorithm
SOC.algorithm(CF)

## === TBS algorithms
## --- TBS1S algorithm
## apply to data (appends ATT)
TBS1s.algorithm(CF)
## --- TBS2S algorithm
## apply to data (appends ATT)
TBS2s.algorithm(CF)

## ======== outcomes
AddCFRs(CF)


## NOTE
## ditch most signs for simplificty
CF <- CF[,.(country,id,TB,
            who.ATT,who.cost,who.cfr,
            soc.ATT,soc.cost,soc.cfr,
            tbs1.ATT,tbs1.cost,tbs1.cfr,
            tbs2.ATT,tbs2.cost,tbs2.cfr)] #lose lots of info for now for simplicity
summary(CF)

## se/sp of algs as a whole
CF[,.(who=mean(who.ATT),soc=mean(soc.ATT),
      tbs1=mean(tbs1.ATT),tbs2=mean(tbs2.ATT)),by=TB]


## NOTE this step resamples Npops times with popsize and calculates means
ALL <- combineHE(CF,popsize = 1e2,Npops=1e3)

## quick looks
clz <- names(ALL)
clz <- clz[-c(1,2)]
MZ <- ALL[,lapply(.SD,mean),by=country,.SDcols=clz]


## wrt SOC
MZ[,c('DC_TBS1','DC_TBS2','DC_WHO'):=.(tbs1.cost-soc.cost,tbs2.cost-soc.cost,who.cost-soc.cost)]
MZ[,c('DD_TBS1','DD_TBS2','DD_WHO'):=.(tbs1.DALYs-soc.DALYs,tbs2.DALYs-soc.DALYs,who.DALYs-soc.DALYs)]
MZ[,c('ICER_TBS1','ICER_TBS2','ICER_WHO'):=.(-DC_TBS1/DD_TBS1,-DC_TBS2/DD_TBS2,-DC_WHO/DD_WHO)]


MZ[,.(country,
      DC_TBS1,DC_TBS2,DC_WHO,
      DD_TBS1,DD_TBS2,DD_WHO,
      ICER_TBS1,ICER_TBS2,ICER_WHO)]


M <- reshapeINC(ALL)
MS <- M[,.(`DALYs averted`=mean(`DALYs averted`),
           `Incremental cost`=mean(`Incremental cost`)),
        by=.(country,algorithm)]

(GP <- CEAplots(MS))
ggsave(GP,file=here('graphs/CEhull.pdf'),h=8,w=10)


CEAC <- make.ceacs(M,seq(from=0,to=150,by=0.5))

ggplot(CEAC,aes(lambda,`Probability CE`,col=country,lty=algorithm))+
  geom_line()


## NOTE
## docs
## https://tbksp.org/en/node/2032
## https://docs.google.com/presentation/d/1dTrmzyfHa0KAja2ODXiQvRnpTYj1xmD2/edit#slide=id.p8


## TODO
## sense/spec check
## ATT despite score in TBS
## WHO reassess - worsening? - resample
## HIV
## SAM cfrs
## prevalence of RS-TB, RR-TB
## check mortality

## NOTE
## presumptive TB same w/ & w/o TB - no info on spec
