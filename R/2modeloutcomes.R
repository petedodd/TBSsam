## modelling the scores on the synthetic populations
rm(list=ls())

args <- commandArgs(trailingOnly = TRUE)
## flags for sensitivity analyses
shell <- TRUE # whether running from shell script or not
if (shell) {
  ## running from shell
  args <- commandArgs(trailingOnly = TRUE)
  print(args)
  SA <- args[1] # none,parameter name
  LM <- args[2] # LQ or UQ
  if (!is.na(SA) && !is.na(LM) && SA != "none" && !LM %in% c("UQ", "LQ")) {
    stop(paste0("LM supplied as ", LM, " but must be UQ or LQ!"))
  }
  cat("*** Running SA analysis for ",SA,"!***\n")
  cat("*** (using ", LM, ") ***\n")
} else { # set by hand
  rm(list = ls()) # clear all
  shell <- FALSE # whether running from shell script or not
  SA <- "none"
}
if (!is.na(SA) && SA == "none") {
  SA <- ""
  LM <- ""
}
## nargs <- length(args)
## variant <- as.character(args[1])


## libraries
library(here)
library(glue)
library(data.table)
library(ggplot2)
library(readxl)
gh <- function(x) glue(here(x))

## load dependencies
source(gh('R/utils/scores.R')) #scores are coded in here
source(gh('R/utils/costutils.R')) #cost data parser
source(here('R/utils/readyoutcomes.R')) #parameters & life-years NOTE flag used to change parms
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
pop[,1-mean(score_X>10)] #specificity =46%
pop[,1-mean(score_noX>10)] #specificity =83%
popt[,mean(score_X>10)] #sensitivity =75%
popt[,mean(score_noX>10)] #sensitivity =46%

## check se/sp
pop[,1-mean(TBS1Sb>=10)] #specificity =79%
pop[,1-mean(TBS2Sa>=1 & TBS2Sb>=10, na.rm = TRUE)] #specificity =84%
popt[,mean(TBS1Sb>=10)] #sensitivity =83%
popt[,mean(TBS2Sa>=1 & TBS2Sb>=10, na.rm = TRUE)] #sensitivity =76%


## NOTE see popchecks.R around here

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

## NOTE in TBS2Sb==NA when TBS2Sa==0

CFS <- CF[,.(CXR=mean(score_X),noCXR=mean(score_noX),
             CXR.sd=sd(score_X),noCXR.sd=sd(score_noX),
             TBS1Sa=mean(TBS1Sa),TBS1Sb=mean(TBS1Sb),TBS2Sa=mean(TBS2Sa),TBS2Sb=mean(TBS2Sb),
             TBS1Sa.sd=sd(TBS1Sa),TBS1Sb.sd=sd(TBS1Sb),TBS2Sa.sd=sd(TBS2Sa),TBS2Sb.sd=sd(TBS2Sb)),
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
CF[, CXR.avail := 1] # code as available
## NOTE parameters in data/SAMparameters.csv including reassess
AP <- getAlgoParms(Nreps,CF$hiv_res.factor) #mainly/all for SOC NOTE all stochastic elts here
## ## check
## AP[,hiv:=CF$hiv_res.factor]
## AP[,.(mean(cfr.noatt),mean(cfr.att)),by=hiv]
CF <- merge(CF,AP,by='id')

## extend across countries;
CF <- CF[rep(1:nrow(CF),length(cnz))]
CF[,country:=rep(cnz,each=nrow(CF)/length(cnz))]

## add in Rif-Resistance status
RR <- makeRRdata(Nreps)
CF <- merge(CF,RR,by=c('country','id'))

## check Xpert
CF[,.(mean(Xpert_res)),by=TB]

## === Xpert there for WHO?
## CF[P$s.soc.CXRonly$r(nrow(CF))>runif(nrow(CF)),Xpert_res:=NA] #for now assume same mWRD avail as via GA in SOC
## assume NPA/Stool make Xpert always available for WHO

## make cost data
CDW <- makeCostPSA(Nreps)

## SA if cost parameter selected
if (!is.na(SA) && substr(SA, 1, 2) == "c.") { #detect a cost parm for SA
  if (SA %in% names(CDW)){
     cat("SA: Overwriting cost ", SA, " with ", LM, "!\n")
  } else{
    stop(paste0("Cost parm ", SA, " supplied for SA but not found in CDW costs!\n"))
  }
  if (LM == "LQ") {
    CDW[, c(SA) := lapply(.SD, function(x) quantile(x,0.25)), by = country, .SDcols = SA]
  } else if (LM == "UQ") {
    CDW[, c(SA) := lapply(.SD, function(x) quantile(x, 0.75)), by = country, .SDcols = SA]
  } else {
    stop(paste0("LM supplied as ", LM, " but must be UQ or LQ!"))
  }
}

## merge in costs (read in and created in HEoutputs.R)
CF <- merge(CF,CDW,by=c('id','country'))
CF[,c.s.ATT:= rrp * c.s.rrATT + (1-rrp) * c.s.rsATT] #use a mean cost (same outcomes)
CF[,c('who.cost','soc.cost','tbs1.cost','tbs2.cost'):=0.0] #initialize costs


CF0 <- copy(CF) #for easier re-running/expst
## CF <- copy(CF0)


## === WHO algorithm
## apply to data (appends ATT)
## WHO.algorithm(CF)

## ans0 <- WHO.algorithm(CF)
ans <- WHO.algorithm(CF)
CF[,c('who.ATT','who.cost'):=ans]

## summary(ans0)
## summary(ans)

## ## checks
## CF[,.(who=mean(who.ATT)),by=TB]
## CF[,.(who=mean(who.cost)),by=.(TB,reassess)]
## CF[,.(who=mean(who.ATT)),by=.(TB,reassess)]
## CF[,.(who=mean(score_X)),by=.(TB,reassess)]
## CF[is.na(Xpert_res) & itb_exp_con.factor==0,table(score_X>10,who.ATT)]



## === SOC algorithm
## SOC.algorithm(CF)
ans <- SOC.algorithm(CF)
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

## CFfull <- copy(CF)


## ## NOTE
## ## ditch most signs for simplificty
## CF <- CF[,.(country,id,TB,
##             who.ATT,who.cost,who.cfr,
##             soc.ATT,soc.cost,soc.cfr,
##             tbs1.ATT,tbs1.cost,tbs1.cfr,
##             tbs2.ATT,tbs2.cost,tbs2.cfr)] #lose lots of info for now for simplicity
## summary(CF)

## CFRs
## check: should be same
CF[, mean(soc.cfr), by = .(TB,soc.ATT)]
CF[, mean(who.cfr), by = .(TB, who.ATT)]
CF[, mean(tbs1.cfr), by = .(TB, tbs1.ATT)]
CF[, mean(tbs2.cfr), by = .(TB, tbs2.ATT)]

## mean by TB including se/sp
CF[,.(who=mean(who.cfr),soc=mean(soc.cfr),
      tbs1=mean(tbs1.cfr),tbs2=mean(tbs2.cfr)),by=TB]

## differential CFR x 1000
fac <- 1e3
CF[, .(
  dwho = fac * (mean(who.cfr) - mean(soc.cfr)), dsoc = fac * (mean(soc.cfr) - mean(soc.cfr)),
  dtbs1 = fac * (mean(tbs1.cfr) - mean(soc.cfr)), dtbs2 = fac * (mean(tbs2.cfr) - mean(soc.cfr))
)]



## se/sp of algs as a whole
SESP <- CF[, .(
  who = ifelse(TB == "TB", mean(who.ATT), mean(1 - who.ATT)),
  soc = ifelse(TB == "TB", mean(soc.ATT), mean(1 - soc.ATT)),
  tbs1 = ifelse(TB == "TB", mean(tbs1.ATT), mean(1 - tbs1.ATT)),
  tbs2 = ifelse(TB == "TB", mean(tbs2.ATT), mean(1 - tbs2.ATT))
),
by = TB
]
SESP[, qty := ifelse(TB == "TB", "Se", "Sp")]
SESP[, TB := NULL]
SESP

if(!is.na(SA) && SA=="")
  fwrite(SESP,file = here('data/SESP.csv'))



## costs of algs as a whole
CF[,.(who=mean(who.cost),soc=mean(soc.cost),
      tbs1=mean(tbs1.cost),tbs2=mean(tbs2.cost)),by=TB]
CF[,.(who=mean(who.cost),soc=mean(soc.cost),
      tbs1=mean(tbs1.cost),tbs2=mean(tbs2.cost))]


## merge in Life-years
CF <- merge(CF,LYKc[,.(country,dLYS=LYS)],by='country',all.x=TRUE)
CF <- merge(CF,LYK[,.(country,LYS)],by='country',all.x=TRUE) #undiscounted

## ## NOTE CHECK with TB prevalence set to close to 1 if run (P object will need correcting afterwards)
## P$s.TBprev$r <- function(n) rbeta(n,500,2) #overwrite
## summary(P$s.TBprev$r(1e4))                 #check
## # P <- parse.parmtable(PD0[, 1:2]) # correct afterwards

## parm names

## ## from CF
## pnmz <- c(
##   "soc.screened","testing.done","xray.only","xpert.only",
##   "s.screen.se","s.screen.sp","clin.sense","clin.spec",
##   "clin.senseX","clin.specX","clin.senseU","clin.specU",
##   "clin.senseXU","clin.specXU",
##   "s.reassess.choice.se","s.reassess.choice.sp","s.reassess.se","s.reassess.sp",
##   "rrp","SAMmort","SAMmortTBATT","SAMmortTBnoATT",
##   names(CDW)[-c(1,2)]
## )



## NOTE this step resamples Npops times with popsize and calculates means
## using 300 as ~ size per country
ALL <- combineHE(CF, popsize = 3e2, Npops = 1e3) ## ,
                 ## parnmz = pnmz)

## ## NOTE incrementals now included in combineHE

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
MZ[,c('ICER0_TBS1','ICER0_TBS2','ICER0_WHO'):=.(-DC_TBS1/DD0_TBS1,-DC_TBS2/DD0_TBS2,-DC_WHO/DD0_WHO)]
## wrt WHO
MZ[,c('wICER_TBS1','wICER_TBS2'):=.(-wDC_TBS1/wDD_TBS1,-wDC_TBS2/wDD_TBS2)]
MZ[,c('wICER0_TBS1','wICER0_TBS2'):=.(-wDC_TBS1/wDD0_TBS1,-wDC_TBS2/wDD0_TBS2)]
## wrt TBS1
MZ[,c('tICER_TBS2'):=.(-tDC_TBS2/tDD_TBS2)]
MZ[,c('tICER0_TBS2'):=.(-tDC_TBS2/tDD0_TBS2)]


## prettier table
tab <- makeTable(MZ)
tab

## transposed version
TT <- transpose(tab,make.names = TRUE)
rownames(TT) <- names(tab)[-1]
TT

## save out
if (!is.na(SA) && SA == "") {
  write.csv(TT, file = here("data/tICERtable.csv"))
  fwrite(tab, file = here("data/ICERtable.csv"))
} else {
  fwrite(TT, file = gh("data/SA/tICERtable_{SA}_{LM}.csv"))
  fwrite(tab, file = gh("data/SA/ICERtable_{SA}_{LM}.csv"))
}

## reshape data
keep <- c('country','id',grep('\\.',names(ALL),value = TRUE))
keep <- keep[1:34] # don't include extras that confuse reshapeINC if outputting parms
M <- ALL[, ..keep]
## harmonize naming:
names(M) <- gsub("s1s", "s1", names(M))
names(M) <- gsub("s2s", "s2", names(M))
M <- reshapeINC(M) #NOTE can also explore FN/FP/cfr/reassess from this data

## CE plane ---------
## GP <- CEAplots(M[algorithm!='tbs2'],ring=TRUE,alph=0.05)
GP <- CEAplots(M[country %in% c("Zambia", "Uganda")], ring = TRUE, alph = 0.5)
GP

M[,.(`DALYs averted`=mean(`DALYs averted`),
     `Incremental cost`=mean(`Incremental cost`)),
  by=.(country,algorithm)]


## rankogram ---------
GQ <- makeRankogram(ALL[country %in% c("Uganda", "Zambia")])
GQ

## GP + GQ  ---------
GB <- ggarrange(GP, GQ,
  ncol = 1, nrow = 2,
  common.legend = TRUE, heights = c(2, 1), labels = c("A", "B")
)


## CEAC  ---------
CEAC <- make.ceacs(M[country %in% c("Zambia", "Uganda")], seq(from = 0, to = 100, by = 0.5))

GC <- ggplot(
  CEAC,
  aes(lambda, `Probability CE`, col = country, lty = algorithm)
) +
  geom_line(lwd = 1) +
  scale_y_continuous(label = percent) +
  xlab("Cost effectiveness threshold (US$ per DALY averted)") +
  ylab("Probability cost-effective")+
  scale_linetype_manual(values = c("tbs1" = "dashed", "tbs2" = "dotdash", "who" = "solid")) +
  scale_color_manual(values = c("Uganda" = "darkorange3", "Zambia" = "deepskyblue3"))
GC

## saving out  ---------
if (!is.na(SA) && SA == "") {
  ggsave(GP, file = here("graphs/CEhull.png"), h = 8, w = 10)
  ggsave(GQ, file = here("graphs/Ranking.png"), h = 8, w = 10)
  ggsave(GB, file = here("graphs/Figure2.pdf"), h = 10, w = 10)
  ggsave(GB, file = here("graphs/Figure2.eps"), h = 10, w = 10)
  ggsave(GB, file = here("graphs/Figure2.png"), h = 10, w = 10)
  ggsave(GC, file = here("graphs/CEAC.png"), h = 8, w = 10)
}

## ## -------- varying prevalence

## ## NOTE this step resamples Npops times with popsize and calculates means
## ALL2 <- combineHE(CF,popsize = 5e2,Npops=1e3,
##                   prevdist = function(n) rbeta(n,2,200) #define a random prevalence via beta distribution
##                   )

## ## NOTE if this chunk gets used again - move into function
## ## quick looks
## clz <- names(ALL2)
## clz <- clz[-c(1,2)]
## MZ2 <- ALL2[,lapply(.SD,mean),by=country,.SDcols=clz]
## MZ2h <- ALL2[,lapply(.SD,hi),by=country,.SDcols=clz]
## MZ2l <- ALL2[,lapply(.SD,lo),by=country,.SDcols=clz]
## names(MZ2h)[2:ncol(MZ2h)] <- paste0(names(MZ2h)[2:ncol(MZ2h)],'.hi')
## names(MZ2l)[2:ncol(MZ2l)] <- paste0(names(MZ2l)[2:ncol(MZ2l)],'.lo')
## MZ2 <- merge(MZ2,MZ2l,by='country')
## MZ2 <- merge(MZ2,MZ2h,by='country')
## ## wrt SOC
## MZ2[,c('ICER_TBS1','ICER_TBS2','ICER_WHO'):=.(-DC_TBS1/DD_TBS1,-DC_TBS2/DD_TBS2,-DC_WHO/DD_WHO)]
## MZ2[,c('ICER0_TBS1','ICER0_TBS2','ICER0_WHO'):=.(-DC_TBS1/DD0_TBS1,-DC_TBS2/DD0_TBS2,-DC_WHO/DD0_WHO)]
## ## wrt WHO
## MZ2[,c('wICER_TBS1','wICER_TBS2'):=.(-wDC_TBS1/wDD_TBS1,-wDC_TBS2/wDD_TBS2)]
## MZ2[,c('wICER0_TBS1','wICER0_TBS2'):=.(-wDC_TBS1/wDD0_TBS1,-wDC_TBS2/wDD0_TBS2)]
## ## wrt TBS1
## MZ2[,c('tICER_TBS2'):=.(-tDC_TBS2/tDD_TBS2)]
## MZ2[,c('tICER0_TBS2'):=.(-tDC_TBS2/tDD0_TBS2)]


## tab <- makeTable(MZ2)
## tab[6:7,.(`ICER, WHO v SOC`,`ICER, TBS1 v SOC`, `ICER, TBS2 v SOC`)]
## ##at 2% prev, ICERs WHO < TBS2 < TBS1


## NOTE
## docs
## https://tbksp.org/en/node/2032
## https://docs.google.com/presentation/d/1dTrmzyfHa0KAja2ODXiQvRnpTYj1xmD2/edit#slide=id.p8


## NOTE
## presumptive TB same w/ & w/o TB - no info on spec
## unc stoch vs parm


## TODO
## remove unnecessary countries...
