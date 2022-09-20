## modelling the scores on the synthetic populations
library(here)
library(glue)
library(data.table)
library(ggplot2)
gh <- function(x) glue(here(x))

## load dependencies
source(gh('R/utils/scores.R')) #scores are coded in here

## load synthetic populations
load(file=gh('data/POPS.Rdata'))
load(file=gh('data/POPS0.Rdata'))

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


## --- TB Speed
spop <- POPS[["TBS_notTB"]]
spop <- appendTBSscores(spop)
spop0 <- POPS0[["TBS_notTB"]]
spop0 <- appendTBSscores(spop0)
spopt <- POPS[["TBS_TB"]]
spopt <- appendTBSscores(spopt)
spopt0 <- POPS0[["TBS_TB"]]
spopt0 <- appendTBSscores(spopt0)


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

## https://tbksp.org/en/node/2032
## https://docs.google.com/presentation/d/1dTrmzyfHa0KAja2ODXiQvRnpTYj1xmD2/edit#slide=id.p8
## 
## TODO
## cost data template
## cost & treatment outcomes
## CFRs & DALYs
## mixture of TB vs not TB
## HE outcomes




