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

Nreps <- nrow(CF)
CF[,id:=1:Nreps]

## extend across countries & append:
## AddAlgoParms(CF) #mainly/all for SOC
CF[,CXR.avail:=1] #code as available
AP <- getAlgoParms(Nreps,CF$hiv_res.factor) #mainly/all for SOC NOTE all stochastic elts here
## ## check
## AP[,hiv:=CF$hiv_res.factor]
## AP[,.(mean(cfr.noatt),mean(cfr.att)),by=hiv]
CF <- merge(CF,AP,by='id')

## extend across countries;
CF <- CF[rep(1:nrow(CF),length(cnz))]
CF[,country:=rep(cnz,each=nrow(CF)/length(cnz))]

## add in RR status
RR <- makeRRdata(Nreps)
CF <- merge(CF,RR,by=c('country','id'))

## check Xpert
CF[,.(mean(Xpert_res)),by=TB]

## === Xpert there for WHO?
## CF[P$s.soc.CXRonly$r(nrow(CF))>runif(nrow(CF)),Xpert_res:=NA] #for now assume same mWRD avail as via GA in SOC
## assume NPA/Stool make Xpert always available for WHO

## make cost data
CDW <- makeCostPSA(Nreps)

## merge in costs (read in and created in HEoutputs.R)
CF <- merge(CF,CDW,by=c('id','country'))
CF[,c.s.ATT:= rrp * c.s.rrATT + (1-rrp) * c.s.rsATT] #use a mean cost (same outcomes)
CF[,c('who.cost','soc.cost','tbs1.cost','tbs2.cost'):=0.0] #initialize costs


## === WHO algorithm
## apply to data (appends ATT)
## WHO.algorithm(CF)
## WHO.algorithm(CF,resample = TRUE) #including re-assessment via stratified resampling

ans <- WHO.algorithm(CF,resample = TRUE)
CF[,c('who.ATT','who.cost'):=ans]


## ## checks
## CF[,.(who=mean(who.ATT)),by=TB]
## CF[,.(who=mean(who.cost)),by=.(TB,reassess)]
## CF[,.(who=mean(who.ATT)),by=.(TB,reassess)]
## CF[,.(who=mean(score_X)),by=.(TB,reassess)]
## CF[is.na(Xpert_res) & itb_exp_con.factor==0,table(score_X>10,who.ATT)]



## === SOC algorithm
## SOC.algorithm(CF,resample = TRUE)
ans <- SOC.algorithm(CF,resample = TRUE)
CF[,c('soc.ATT','soc.cost'):=ans]

## ## checks
## CF[,.(soc=mean(soc.ATT)),by=TB]
## CF[,.(soc=mean(soc.cost)),by=.(TB,reassess)]
## CF[,.(soc=mean(soc.ATT)),by=.(TB,reassess)]


## === TBS algorithms
## NOTE these act by side effect rather than return
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


## merge in Life-years
CF <- merge(CF,LYKc[,.(country,dLYS=LYS)],by='country',all.x=TRUE)
CF <- merge(CF,LYK[,.(country,LYS)],by='country',all.x=TRUE) #undiscounted


## NOTE this step resamples Npops times with popsize and calculates means
ALL <- combineHE(CF,popsize = 5e2,Npops=1e3)

ALL[,c('DC_TBS1','DC_TBS2','DC_WHO'):=.(tbs1.cost-soc.cost,tbs2.cost-soc.cost,who.cost-soc.cost)]
ALL[,c('DD_TBS1','DD_TBS2','DD_WHO'):=.(tbs1.DALYs-soc.DALYs,tbs2.DALYs-soc.DALYs,who.DALYs-soc.DALYs)]

## quick looks
clz <- names(ALL)
clz <- clz[-c(1,2)]
MZ <- ALL[,lapply(.SD,mean),by=country,.SDcols=clz]
MZh <- ALL[,lapply(.SD,hi),by=country,.SDcols=clz]
MZl <- ALL[,lapply(.SD,lo),by=country,.SDcols=clz]
names(MZh)[2:ncol(MZh)] <- paste0(names(MZh)[2:ncol(MZh)],'.hi')
names(MZl)[2:ncol(MZl)] <- paste0(names(MZl)[2:ncol(MZl)],'.lo')
MZ <- merge(MZ,MZl,by='country')
MZ <- merge(MZ,MZh,by='country')

## wrt SOC
MZ[,c('ICER_TBS1','ICER_TBS2','ICER_WHO'):=.(-DC_TBS1/DD_TBS1,-DC_TBS2/DD_TBS2,-DC_WHO/DD_WHO)]


tab <- MZ[,.(country,
               `cost per child, SOC`=brkt(soc.cost,soc.cost.lo,soc.cost.hi),
               `cost per child, WHO`=brkt(who.cost,who.cost.lo,who.cost.hi),
               `cost per child, TBS1`=brkt(tbs1.cost,tbs1.cost.lo,tbs1.cost.hi),
               `cost per child, TBS2`=brkt(tbs2.cost,tbs2.cost.lo,tbs2.cost.hi),
               `incremental cost, WHO`=brkt(DC_WHO,DC_WHO.lo,DC_WHO.hi),
               `incremental cost, TBS1`=brkt(DC_TBS1,DC_TBS1.lo,DC_TBS1.hi),
               `incremental cost, TBS2`=brkt(DC_TBS2,DC_TBS2.lo,DC_TBS2.hi),
               `100x DALYs averted, WHO`=brkt(-1e2*DD_WHO,-1e2*DD_WHO.hi,-1e2*DD_WHO.lo),
               `100x DALYs averted, TBS1`=brkt(-1e2*DD_TBS1,-1e2*DD_TBS1.hi,-1e2*DD_TBS1.lo),
               `100x DALYs averted, TBS2`=brkt(-1e2*DD_TBS2,-1e2*DD_TBS2.hi,-1e2*DD_TBS2.lo),
               `ICER, WHO`=round(ICER_WHO,2),
               `ICER, TBS1`=round(ICER_TBS1,2),
               `ICER, TBS2`=round(ICER_TBS2,2)
               )]
tab

fwrite(tab,file = here('data/ICERtable.csv'))

## reshape data
keep <- c('country','id',grep('\\.',names(ALL),value = TRUE))
M <- reshapeINC(ALL[,..keep])


GP <- CEAplots(M[algorithm!='tbs2'],ring=TRUE,alph=0.05)
GP

ggsave(GP,file=here('graphs/CEhull.pdf'),h=8,w=10)


CEAC <- make.ceacs(M,seq(from=0,to=500,by=0.5))

GP <- ggplot(CEAC[algorithm!='tbs2'],aes(lambda,`Probability CE`,col=country,lty=algorithm))+
  geom_line(lwd=1)+scale_y_continuous(label=percent)+
  xlab('Cost effectiveness threshold (USD per DALY averted)')+
  ylab('Probability cost-effective')
GP

ggsave(GP,file=here('graphs/CEAC.pdf'),h=8,w=10)


## NOTE
## docs
## https://tbksp.org/en/node/2032
## https://docs.google.com/presentation/d/1dTrmzyfHa0KAja2ODXiQvRnpTYj1xmD2/edit#slide=id.p8


## TODO
## sense/spec check
## SAM cfrs
## check mortality

## NOTE
## presumptive TB same w/ & w/o TB - no info on spec
