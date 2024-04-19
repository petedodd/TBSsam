## modelling the scores on the synthetic populations
rm(list=ls())
library(here)
library(glue)
library(data.table)
library(ggplot2)
library(readxl)
gh <- function(x) glue(here(x))
cnz <- c("Uganda","Zambia")

## load dependencies
source(gh('R/utils/scores.R')) #scores are coded in here
source(gh('R/utils/costutils.R')) #cost data parser
source(here('R/utils/readyoutcomes.R')) #parameters & life-years
source(here('R/utils/HEoutputs.R')) #various outputters

realpopt <- read_excel("data/realpopt.xlsx")
realpop <- read_excel("data/realpop.xlsx")


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
pop[,1-mean(TBS1Sb>=10)] #specificity =79%    clinical paper: TBS1S spe: 81% [77-84]
pop[,1-mean(TBS2Sa>=1 & TBS2Sb>=10, na.rm = TRUE)] #specificity =84%     clinical paper: TBS2S spe: 86% [82-89]
popt[,mean(TBS1Sb>=10)] #sensitivity =83%     clinical paper: TBS1S sen: 86% [78-92]
popt[,mean(TBS2Sa>=1 & TBS2Sb>=10, na.rm = TRUE)] #sensitivity =76%      clinical paper: TBS2S sen: 77% [68-84] 


##
## Compare synthetic and real populations 

## add in scores to real populations

# TB pop
names(realpopt)[names(realpopt) == "pat_ide"] <- "id"

realpopt$Contact_TB<-ifelse(realpopt$Contact_TB=="Yes",1,0)
realpopt$itb_fat_2<-ifelse(realpopt$itb_fat_2=="Yes",1,0)
realpopt$itb_fev_2<-ifelse(realpopt$itb_fev_2=="Yes",1,0)
realpopt$itb_cou_2<-ifelse(realpopt$itb_cou_2=="Yes",1,0)
realpopt$itb_cou_3<-ifelse(realpopt$itb_cou_3=="Yes",1,0)
realpopt$itb_app_2<-ifelse(realpopt$itb_app_2=="Yes",1,0)
realpopt$temp_38<-ifelse(realpopt$temp_38=="Yes",1,0)
realpopt$itb_wgt.factor<-ifelse(realpopt$itb_wgt.factor=="Yes",1,0)
realpopt$tachycardia<-ifelse(realpopt$tachycardia=="Yes",1,0)
realpopt$tachypnea<-ifelse(realpopt$tachypnea=="Yes",1,0)
realpopt$ice_ind_bin.factor<-ifelse(realpopt$ice_ind_bin.factor=="Yes",1,0)
realpopt$ice_cra.factor<-ifelse(realpopt$ice_cra.factor=="Yes",1,0)
realpopt$Dep_csc<-ifelse(realpopt$Dep_csc=="Yes",1,0)
realpopt$ice_ade_bin.factor<-ifelse(realpopt$ice_ade_bin.factor=="Yes",1,0)
realpopt$cxr_pre_mil.factor<-ifelse(realpopt$cxr_pre_mil.factor=="Yes",1,0)
realpopt$cxr_pre_alv.factor<-ifelse(realpopt$cxr_pre_alv.factor=="Yes",1,0)
realpopt$cxr_pre_hil.factor<-ifelse(realpopt$cxr_pre_hil.factor=="Yes",1,0)
realpopt$cxr_pre_exc.factor<-ifelse(realpopt$cxr_pre_exc.factor=="Yes",1,0)
realpopt$cxr_pre_ple.factor<-ifelse(realpopt$cxr_pre_ple.factor=="Yes",1,0)
realpopt$cxr_pre_eff.factor<-ifelse(realpopt$cxr_pre_eff.factor=="Yes",1,0)
realpopt$cxr_pre_ple_per_eff.factor<-ifelse(realpopt$cxr_pre_ple_per_eff.factor=="Yes",1,0)
realpopt$aus_sma.factor<-ifelse(realpopt$aus_sma.factor=="Yes",1,0)
realpopt$aus_hma.factor<-ifelse(realpopt$aus_hma.factor=="Yes",1,0)
realpopt$aus_effusion<-ifelse(realpopt$aus_effusion=="Yes",1,0)
realpopt$aus_asc.factor<-ifelse(realpopt$aus_asc.factor=="Yes",1,0)

realpopt$hiv_res.factor<-ifelse(realpopt$hiv_res.factor=="Positive",1,0)
realpopt$Xpert_res<-ifelse(realpopt$Xpert_res=="Positive",1,0)

#Not TB pop
names(realpop)[names(realpop) == "pat_ide"] <- "id"

realpop$Contact_TB<-ifelse(realpop$Contact_TB=="Yes",1,0)
realpop$itb_fat_2<-ifelse(realpop$itb_fat_2=="Yes",1,0)
realpop$itb_fev_2<-ifelse(realpop$itb_fev_2=="Yes",1,0)
realpop$itb_cou_2<-ifelse(realpop$itb_cou_2=="Yes",1,0)
realpop$itb_cou_3<-ifelse(realpop$itb_cou_3=="Yes",1,0)
realpop$itb_app_2<-ifelse(realpop$itb_app_2=="Yes",1,0)
realpop$temp_38<-ifelse(realpop$temp_38=="Yes",1,0)
realpop$itb_wgt.factor<-ifelse(realpop$itb_wgt.factor=="Yes",1,0)
realpop$tachycardia<-ifelse(realpop$tachycardia=="Yes",1,0)
realpop$tachypnea<-ifelse(realpop$tachypnea=="Yes",1,0)
realpop$ice_ind_bin.factor<-ifelse(realpop$ice_ind_bin.factor=="Yes",1,0)
realpop$ice_cra.factor<-ifelse(realpop$ice_cra.factor=="Yes",1,0)
realpop$Dep_csc<-ifelse(realpop$Dep_csc=="Yes",1,0)
realpop$ice_ade_bin.factor<-ifelse(realpop$ice_ade_bin.factor=="Yes",1,0)
realpop$cxr_pre_mil.factor<-ifelse(realpop$cxr_pre_mil.factor=="Yes",1,0)
realpop$cxr_pre_alv.factor<-ifelse(realpop$cxr_pre_alv.factor=="Yes",1,0)
realpop$cxr_pre_hil.factor<-ifelse(realpop$cxr_pre_hil.factor=="Yes",1,0)
realpop$cxr_pre_exc.factor<-ifelse(realpop$cxr_pre_exc.factor=="Yes",1,0)
realpop$cxr_pre_ple.factor<-ifelse(realpop$cxr_pre_ple.factor=="Yes",1,0)
realpop$cxr_pre_eff.factor<-ifelse(realpop$cxr_pre_eff.factor=="Yes",1,0)
realpop$cxr_pre_ple_per_eff.factor<-ifelse(realpop$cxr_pre_ple_per_eff.factor=="Yes",1,0)
realpop$aus_sma.factor<-ifelse(realpop$aus_sma.factor=="Yes",1,0)
realpop$aus_hma.factor<-ifelse(realpop$aus_hma.factor=="Yes",1,0)
realpop$aus_effusion<-ifelse(realpop$aus_effusion=="Yes",1,0)
realpop$aus_asc.factor<-ifelse(realpop$aus_asc.factor=="Yes",1,0)

realpop$hiv_res.factor<-ifelse(realpop$hiv_res.factor=="Positive",1,0)
realpop$Xpert_res<-ifelse(realpop$Xpert_res=="Positive",1,0)


# --- TB Speed
setDT(realpop)
setDT(realpopt)

realpop <- appendTBSscores(realpop)
realpopt <- appendTBSscores(realpopt)


## check se/sp using Minh's variables
realpop[,1-mean(realpop$SCO_ORG_tot>=10)] #specificity =81%    clinical paper: TBS1S spe: 81%
realpop[,1-mean(realpop$SCO_SCR_tot>=1 & realpop$SCO_DIA_tot>=10)] #specificity =84%     clinical paper: TBS2S spe: 86%
realpopt[,mean(realpopt$SCO_ORG_tot>=10)] #sensitivity =86%     clinical paper: TBS1S sen: 86%
realpopt[,mean(realpopt$SCO_SCR_tot>=1 & realpopt$SCO_DIA_tot>=10)] #sensitivity =79%      clinical paper: TBS2S sen: 77%



# ## check se/sp using our score
realpop[,1-mean(TBS1Sb>=10)] #real pop sp = 81%
                           #synthetic pop sp: 81%
                           #clinical paper sp: 81% [77-84]
realpop[,1-mean(TBS2Sa>=1 & TBS2Sb>=10, na.rm = TRUE)] #real pop sp = 84%
                                                      #synthetic pop sp: 84%
                                                      #clinical paper sp: 86% [82-89]
realpopt[,mean(TBS1Sb>=10)] #real pop se = 86%
                          #synthetic pop se: 86%
                          #clinical paper se: 86% [78-92]
realpopt[,mean(TBS2Sa>=1 & TBS2Sb>=10, na.rm = TRUE)] #real pop se = 79%
                                                     #synthetic pop se: 79%
                                                     #clinical paper se: 77% [68-84]

table(realpopt$SCO_ORG_SCR_tot>=10) # 
table(realpopt$SCO_ORG_tot>=10) # 87/101
table(realpop$SCO_ORG_tot<10) # 351/434

table(realpopt$SCO_SCR_tot>=10) # 90/101
table(realpop$SCO_SCR_tot<10) # 151/434

table(realpopt$SCO_DIA_tot>=10) # 80/101
table(realpop$SCO_DIA_tot<10) # 212+151= 363/434
summary(is.na(realpop$SCO_DIA_tot))


# Compare Minh's scores and ours
all(realpopt$SCO_ORG_SCR_tot == realpopt$TBS1Sa)
all(realpop$SCO_ORG_SCR_tot == realpop$TBS1Sa)

all(realpopt$SCO_ORG_tot == realpopt$TBS1Sb)
all(realpop$SCO_ORG_tot == realpop$TBS1Sb)

all(realpopt$SCO_SCR_tot == realpopt$TBS2Sa)
all(realpop$SCO_SCR_tot == realpop$TBS2Sa)

all(realpopt$SCO_DIA_tot == realpopt$TBS2Sb, na.rm = TRUE)
all(realpop$SCO_DIA_tot == realpop$TBS2Sb, na.rm = TRUE)



## check se/sp
pop[,1-mean(score_X>10)] #specificity =47%
pop[,1-mean(score_noX>10)] #specificity =83%
popt[,mean(score_X>10)] #sensitivity =74%
popt[,mean(score_noX>10)] #sensitivity =46%



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

## ans0 <- WHO.algorithm(CF,resample = TRUE)
ans <- WHO.algorithm2(CF,resample = TRUE) #Marc version
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

CF


## NOTE
## ditch most signs for simplificty
CF <- CF[,.(country,id,TB,
            who.ATT,who.cost,who.cfr,
            soc.ATT,soc.cost,soc.cfr,
            tbs1.ATT,tbs1.cost,tbs1.cfr,
            tbs2.ATT,tbs2.cost,tbs2.cfr)] #lose lots of info for now for simplicity
summary(CF)

## CFRs
CF[,.(who=mean(who.cfr),soc=mean(soc.cfr),
      tbs1=mean(tbs1.cfr),tbs2=mean(tbs2.cfr)),by=TB]


## se/sp of algs as a whole
CF[,.(who=mean(who.ATT),soc=mean(soc.ATT),
      tbs1=mean(tbs1.ATT),tbs2=mean(tbs2.ATT)),by=TB]

## costs of algs as a whole
CF[,.(who=mean(who.cost),soc=mean(soc.cost),
      tbs1=mean(tbs1.cost),tbs2=mean(tbs2.cost)),by=TB]
CF[,.(who=mean(who.cost),soc=mean(soc.cost),
      tbs1=mean(tbs1.cost),tbs2=mean(tbs2.cost))]


## merge in Life-years
CF <- merge(CF,LYKc[,.(country,dLYS=LYS)],by='country',all.x=TRUE)
CF <- merge(CF,LYK[,.(country,LYS)],by='country',all.x=TRUE) #undiscounted


## NOTE this step resamples Npops times with popsize and calculates means
ALL <- combineHE(CF,popsize = 5e2,Npops=1e3)

## incremental wrt SOC
ALL[,c('DC_TBS1','DC_TBS2','DC_WHO'):=.(tbs1.cost-soc.cost,tbs2.cost-soc.cost,who.cost-soc.cost)]
ALL[,c('DD_TBS1','DD_TBS2','DD_WHO'):=.(tbs1.DALYs-soc.DALYs,tbs2.DALYs-soc.DALYs,who.DALYs-soc.DALYs)]
ALL[,c('DT_TBS1','DT_TBS2','DT_WHO'):=.(tbs1.ATT-soc.ATT,tbs2.ATT-soc.ATT,who.ATT-soc.ATT)]
ALL[,c('DM_TBS1','DM_TBS2','DM_WHO'):=.(tbs1.cfr-soc.cfr,tbs2.DALYs-soc.cfr,who.cfr-soc.cfr)]

## wrt WHO
ALL[,c('wDC_TBS1','wDC_TBS2'):=.(tbs1.cost-who.cost,tbs2.cost-who.cost)]
ALL[,c('wDD_TBS1','wDD_TBS2'):=.(tbs1.DALYs-who.DALYs,tbs2.DALYs-who.DALYs)]


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
## wrt WHO
MZ[,c('wICER_TBS1','wICER_TBS2'):=.(-wDC_TBS1/wDD_TBS1,-wDC_TBS2/wDD_TBS2)]


tab <- makeTable(MZ)
tab

fwrite(tab,file = here('data/ICERtable.csv'))

## reshape data
keep <- c('country','id',grep('\\.',names(ALL),value = TRUE))
M <- reshapeINC(ALL[,..keep])

#GP <- CEAplots(M[algorithm!='tbs2'],ring=TRUE,alph=0.05)
GP <- CEAplots(M[],ring=TRUE,alph=0.05)
GP

M[,.(`DALYs averted`=mean(`DALYs averted`),
     `Incremental cost`=mean(`Incremental cost`)),
  by=.(country,algorithm)]

ggsave(GP,file=here('graphs/CEhull.pdf'),h=8,w=10)


CEAC <- make.ceacs(M,seq(from=0,to=500,by=0.5))

GP <- ggplot(CEAC[algorithm!='tbs2' & country %in% c('Uganda','Zambia')],
             aes(lambda,`Probability CE`,col=country,lty=algorithm))+
  geom_line(lwd=1)+scale_y_continuous(label=percent)+
  xlab('Cost effectiveness threshold (US$ per DALY averted)')+
  ylab('Probability cost-effective')
GP

ggsave(GP,file=here('graphs/CEAC.pdf'),h=8,w=10)


## NOTE
## docs
## https://tbksp.org/en/node/2032
## https://docs.google.com/presentation/d/1dTrmzyfHa0KAja2ODXiQvRnpTYj1xmD2/edit#slide=id.p8


## TODO
## sense/spec check

## NOTE
## presumptive TB same w/ & w/o TB - no info on spec
## unc stoch vs parm
