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
source(gh('R/utils/costutils.R')) #scores are coded in here
source(here('R/utils/readyoutcomes.R')) #parameters & life-years

## load synthetic populations
load(file=gh('data/POPS.Rdata'))
load(file=gh('data/POPS0.Rdata'))


set.seed(2345)

## add in scores:

## --- WHO
pop <- POPS[["WHO_notTB"]]
pop <- appendWHOscores(pop)
pop0 <- POPS0[["WHO_notTB"]]
pop0 <- appendWHOscores(pop0)
popt <- POPS[["WHO_TB"]]
popt <- appendWHOscores(popt)
popt0 <- POPS0[["WHO_TB"]]
popt0 <- appendWHOscores(popt0)

## check se/sp
pop[,1-mean(score_X>10)] #specificity =51%
pop[,1-mean(score_noX>10)] #specificity =87%
popt[,mean(score_X>10)] #sensitivity =71%
popt[,mean(score_noX>10)] #sensitivity =39%

## --- TB Speed
spop <- POPS[["TBS_notTB"]]
spop <- appendTBSscores(spop)
spop0 <- POPS0[["TBS_notTB"]]
spop0 <- appendTBSscores(spop0)
spopt <- POPS[["TBS_TB"]]
spopt <- appendTBSscores(spopt)
spopt0 <- POPS0[["TBS_TB"]]
spopt0 <- appendTBSscores(spopt0)

## check se/sp
spop[,1-mean(TBS1S>10)] #specificity =76%
spop[,1-mean(TBS2Sa>10 & TBS2Sb>10)] #specificity =86%
spopt[,mean(TBS1S>10)] #sensitivity =84%
spopt[,mean(TBS2Sa>10 & TBS2Sb>10)] #sensitivity =63%

## compare
pop0[,method:='no correlation']; spop0[,method:='no correlation']
pop[,method:='copulas']; spop[,method:='copulas']
popt0[,method:='no correlation']; spopt0[,method:='no correlation']
popt[,method:='copulas']; spopt[,method:='copulas']
pop0[,TB:='not TB']; spop0[,TB:='not TB']
pop[,TB:='not TB']; spop[,TB:='not TB']
popt0[,TB:='TB']; spopt0[,TB:='TB']
popt[,TB:='TB']; spopt[,TB:='TB']
CF <- rbindlist(list(pop,popt,pop0,popt0)) #WHO version
sCF <- rbindlist(list(spop,spopt,spop0,spopt0)) #TBS version

## summary WHO
CFS <- CF[,.(CXR=mean(score_X),noCXR=mean(score_noX),
             CXR.sd=sd(score_X),noCXR.sd=sd(score_noX)),
          by=.(TB,method)][order(TB)] #very similar
CFS

fwrite(CFS,file=here('data/compare.summary.WHO.csv'))


## summary TBS
sCFS <- sCF[,.(TBS1S=mean(TBS1S),TBS2Sa=mean(TBS2Sa),TBS2Sb=mean(TBS2Sb),
               TBS1S.sd=sd(TBS1S),TBS2Sa.sd=sd(TBS2Sa),TBS2Sb.sd=sd(TBS2Sb)),
          by=.(TB,method)][order(TB)] #very similar
sCFS

fwrite(sCFS,file=here('data/compare.summary.TBS.csv'))


## choose method (could continue but need to remember)
CF <- CF[method=='copulas']
sCF <- sCF[method=='copulas']
intersect(names(CF),names(sCF))

## === create cost data
CD <- parsecosts(gh('data/TB-Speed_SAM_Costs.csv'))
CD[,c('cost.mid','cost.sd'):=.((High+Low)/2,(High-Low)/3.92)]
## model as gamma parameters
CD[,theta:=cost.sd^2/cost.mid]
CD[,k:=cost.mid/theta]
CDL <- CD[rep(1:nrow(CD),nrow(pop)),.(NAME,country,k,theta)]
CDL[,id:=rep(1:nrow(pop),each=nrow(CD))]
CDL[,value := rgamma(n=nrow(CDL),shape=k,scale=theta)]
CDL[is.na(value),value:=0.0]
CDW <- dcast(CDL,country+id~NAME,value.var = 'value')
CDW

## extend across countries & append:
AddAlgoParms(CF) #mainly/all for SOC

## extend across countries;
CF <- CF[rep(1:nrow(CF),length(cnz))]
CF[,country:=rep(cnz,each=nrow(CF)/length(cnz))]

## TODO change para
## === WHO algorithm
CF[,CXR.avail:=1] #code as available
CF[runif(nrow(CF))<0.6,Xpert_res:=NA] #for now assume that 40% have available Xpert results

## merge in costs
CF <- merge(CF,CDW,by=c('id','country'))

## apply to data (appends ATT)
WHO.algorithm(CF)

## === SOC algorithm
SOC.algorithm(CF)

## ditch most signs for simplificty
CF <- CF[,.(country,id,TB,who.ATT,who.cost,soc.ATT,soc.cost)] #lose lots of info for now for simplicity
summary(CF)


## === TBS algorithms

## extend across countries;
sCF <- sCF[rep(1:nrow(sCF),length(cnz))]
sCF[,country:=rep(cnz,each=nrow(sCF)/length(cnz))]

## --- TBS1S algorithm
sCF[,CXR.avail:=1] #code as available
## assume Xpert available for all

## merge in costs
sCF <- merge(sCF,CDW,by=c('id','country'))

## apply to data (appends ATT)
TBS1s.algorithm(sCF)


## --- TBS2S algorithm
## apply to data (appends ATT)
TBS2s.algorithm(sCF)


sCF <- sCF[,.(country,id,TB,
              tbs1.ATT,tbs1.cost,
              tbs2.ATT,tbs2.cost)] #lose lots of info for now for simplicity

summary(sCF)


## ======== outcomes
AddCFRs(CF,algo='WHO') #includes SOC
AddCFRs(sCF,algo='TBS')

## LYS
CF <- merge(CF,LYKc[,.(country,dLYS=LYS)],by='country',all.x=TRUE)
CF <- merge(CF,LYK[,.(country,LYS)],by='country',all.x=TRUE) #undiscounted
sCF <- merge(sCF,LYKc[,.(country,dLYS=LYS)],by='country',all.x=TRUE)
sCF <- merge(sCF,LYK[,.(country,LYS)],by='country',all.x=TRUE) #undiscounted

## TODO calculate sens/spec for WHO


## ==== TB prevalence NOTE this needs thought
## TB	104
## Not TB 	431
## Total	535
tb <- runif(max(CF$id)) < 104/535
WHO <- rbind(CF[TB=='TB'][id %in% which(tb)],
             CF[TB=='not TB'][id %in% which(!tb)])
TBS <- rbind(sCF[TB=='TB'][id %in% which(tb)],
             sCF[TB=='not TB'][id %in% which(!tb)])


## ======== CEA outputs

## combined data
ALL <- merge(
  WHO[,.(country,id,
         who.cost,who.DALYs=who.cfr*dLYS,
         soc.cost,soc.DALYs=soc.cfr*dLYS)],
  TBS[,.(country,id,
         tbs1.cost,tbs1.DALYs=tbs1.cfr*dLYS,
         tbs2.cost,tbs2.DALYs=tbs2.cfr*dLYS)],
  by=c('country','id')
)


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


## NOTE
## docs
## https://tbksp.org/en/node/2032
## https://docs.google.com/presentation/d/1dTrmzyfHa0KAja2ODXiQvRnpTYj1xmD2/edit#slide=id.p8


## TODO
## sense/spec check
## ATT despite score in TBS
## RR-TB
## HIV
## SAM cfrs
## mixture of TB vs not TB
## clarify soc vs WHO & check costs
## prevalence of RS-TB, RR-TB

## NOTE
## presumptive TB same w/ & w/o TB - no info on spec
