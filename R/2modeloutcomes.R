## modelling the scores on the synthetic populations
rm(list=ls())

args <- commandArgs(trailingOnly = TRUE)
## flags for sensitivity analyses
shell <- FALSE # whether running from shell script or not
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


# install.packages("remotes")
# remotes::install_github("petedodd/Hedtree", ref="master")
# install.packages("devtools")
# devtools::install_github("petedodd/discly")

#install.packages("binom", dependencies=TRUE)
library(binom)


library(HEdtree)
library(discly)

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

#fwrite(CF, "data/CF.csv")


### Variable resampling from the TB-Speed SAM cohort

## No resampling - screening var

# SOC
CF[TB=="TB", mean(itb_cou_2 | itb_fev_2 | Contact_TB)] # se: 52% (vs se: 37% in cohort)
CF[TB!="TB", mean(itb_cou_2 == 0 & itb_fev_2== 0 & Contact_TB== 0)] # sp: 75% (vs sp: 79% in cohort)

# TBS2
CF[TB=="TB", mean(Contact_TB | itb_cou_3 | temp_38 | tachycardia | ice_ind_bin.factor |
                    ice_cra.factor | Dep_csc | ice_ade_bin.factor | hiv_res.factor)] # se: 93% (vs se: 88% in cohort)

CF[TB!="TB", mean(Contact_TB == 0 & itb_cou_3 == 0 & temp_38 == 0 & tachycardia == 0 & ice_ind_bin.factor == 0 &
                    ice_cra.factor == 0 & Dep_csc == 0 & ice_ade_bin.factor == 0 & hiv_res.factor == 0)] # sp: 32% (vs sp: 34% in cohort)

# WHO
CF[TB=="TB", mean(itb_cou_2 | itb_fev_2 | itb_fat_2 | itb_wgt_2 | itb_app_2)] # se: 97% (vs se: 79% in cohort)
CF[TB!="TB", mean(itb_cou_2 == 0 & itb_fev_2 == 0 & itb_fat_2 == 0 & itb_wgt_2 == 0 & itb_app_2 == 0)] # sp: 10% (vs sp: 28% in cohort)

CF[, lapply(.SD, mean), by = TB, .SDcols = c("itb_cou_2", "itb_fev_2", "itb_fat_2", "itb_wgt_2", "itb_app_2")]

# WHO wo weight loss
CF[TB=="TB", mean(itb_cou_2 | itb_fev_2 | itb_fat_2 | itb_app_2)] # se: 92%
CF[TB!="TB", mean(itb_cou_2 == 0 & itb_fev_2 == 0 & itb_fat_2 == 0 & itb_app_2 == 0)] # sp: 26%

# WHO wo loss appetite
CF[TB=="TB", mean(itb_cou_2 | itb_fev_2 | itb_fat_2 | itb_wgt_2)] # se: 93%
CF[TB!="TB", mean(itb_cou_2 == 0 & itb_fev_2 == 0 & itb_fat_2 == 0 & itb_wgt_2 == 0)] # sp: 17%

# WHO wo weight loss & wo loss appetite
CF[TB=="TB", mean(itb_cou_2 | itb_fev_2 | itb_fat_2)] # se: 79%
CF[TB!="TB", mean(itb_cou_2 == 0 & itb_fev_2 == 0 & itb_fat_2 == 0)] # sp: 46%

## read in
C <- as.data.table(read_excel("data/data_mod.xlsx")) #cohort data
#getwd()

# ### Resampling of all var
# 
# ## restrict & reform
# 
# C <- C[,.(Contact_TB, itb_fat_2, itb_fev_2, itb_cou_2, itb_cou_3, itb_app_2, temp_38, itb_wgt_2, itb_wgt.factor, tachycardia, tachypnea,
#           ice_ind_bin.factor, ice_cra.factor, Dep_csc, ice_ade_bin.factor, cxr_pre_mil.factor, cxr_pre_alv.factor, cxr_pre_hil.factor,
#           cxr_pre_exc.factor, cxr_pre_ple.factor, cxr_pre_eff.factor, cxr_pre_ple_per_eff.factor, aus_sma.factor, aus_hma.factor,
#           aus_effusion, aus_asc.factor, reassessment, hiv_res.factor, Xpert_res, TB_stt_bin)]
# 
# # Apply transformation
# C <- C %>%
#   mutate(across(c(Contact_TB, itb_fat_2, itb_fev_2, itb_cou_2, itb_cou_3, itb_app_2, temp_38, itb_wgt_2, itb_wgt.factor, tachycardia, tachypnea,
#                   ice_ind_bin.factor, ice_cra.factor, Dep_csc, ice_ade_bin.factor, cxr_pre_mil.factor, cxr_pre_alv.factor, cxr_pre_hil.factor,
#                   cxr_pre_exc.factor, cxr_pre_ple.factor, cxr_pre_eff.factor, cxr_pre_ple_per_eff.factor, aus_sma.factor, aus_hma.factor,
#                   aus_effusion, aus_asc.factor, reassessment),
#                 ~ ifelse(. == "Yes", 1, 0)))
# 
# C <- C %>%
#   mutate(across(c(hiv_res.factor, Xpert_res),
#                 ~ ifelse(. == "Positive", 1, 0)))
# 
# C[TB_stt_bin == "NotTB", TB_stt_bin := "not TB"]
# 
# Y <- C[TB_stt_bin=="TB", .(Contact_TB, itb_fat_2, itb_fev_2, itb_cou_2, itb_cou_3, itb_app_2, temp_38, itb_wgt_2, itb_wgt.factor, tachycardia, tachypnea,
#                            ice_ind_bin.factor, ice_cra.factor, Dep_csc, ice_ade_bin.factor, cxr_pre_mil.factor, cxr_pre_alv.factor, cxr_pre_hil.factor,
#                            cxr_pre_exc.factor, cxr_pre_ple.factor, cxr_pre_eff.factor, cxr_pre_ple_per_eff.factor, aus_sma.factor, aus_hma.factor,
#                            aus_effusion, aus_asc.factor, reassessment, hiv_res.factor, Xpert_res, TB_stt_bin)]
# X <- C[TB_stt_bin!="TB", .(Contact_TB, itb_fat_2, itb_fev_2, itb_cou_2, itb_cou_3, itb_app_2, temp_38, itb_wgt_2, itb_wgt.factor, tachycardia, tachypnea,
#                            ice_ind_bin.factor, ice_cra.factor, Dep_csc, ice_ade_bin.factor, cxr_pre_mil.factor, cxr_pre_alv.factor, cxr_pre_hil.factor,
#                            cxr_pre_exc.factor, cxr_pre_ple.factor, cxr_pre_eff.factor, cxr_pre_ple_per_eff.factor, aus_sma.factor, aus_hma.factor,
#                            aus_effusion, aus_asc.factor, reassessment, hiv_res.factor, Xpert_res, TB_stt_bin)]
# 
# names(X) <- names(Y) <- c("cont", "fat", "fev", "cou2", "cou3", "app", "temp", "wgt2",  "wgtf", "tachc", "tachp",
#                           "ind", "cra", "csc", "ade", "mil", "alv", "hil", "exc", "ple", "eff", "ple_eff", "sma",
#                           "hma", "aus_eff", "asc", "rea", "hiv", "xpe", "TB")
# 
# ## Resampling
# 
# Ytmp <- Y[sample(1:nrow(Y),nrow(CF[TB=="TB"]),replace=TRUE)]
# Xtmp <- X[sample(1:nrow(X),nrow(CF[TB!="TB"]),replace=TRUE)]
# 
# Xtmp[,id:=1:nrow(Xtmp)]
# Ytmp[,id:=1:nrow(Ytmp)]
# 
# XY <- rbind(Xtmp,Ytmp)
# 
# CF <- merge(CF,XY,by=c("id","TB"),all.x=TRUE)
# 
# CF[,c("Contact_TB", "itb_fat_2", "itb_fev_2", "itb_cou_2", "itb_cou_3", "itb_app_2",
#       "temp_38", "itb_wgt_2", "itb_wgt.factor", "tachycardia", "tachypnea",
#       "ice_ind_bin.factor", "ice_cra.factor", "Dep_csc", "ice_ade_bin.factor",
#       "cxr_pre_mil.factor", "cxr_pre_alv.factor", "cxr_pre_hil.factor",
#       "cxr_pre_exc.factor", "cxr_pre_ple.factor", "cxr_pre_eff.factor",
#       "cxr_pre_ple_per_eff.factor", "aus_sma.factor", "aus_hma.factor",
#       "aus_effusion", "aus_asc.factor", "reassessment", "hiv_res.factor",
#       "Xpert_res"):=.(cont, fat, fev, cou2, cou3, app, temp, wgt2, wgtf, tachc, tachp,
#                       ind, cra, csc, ade, mil, alv, hil, exc, ple, eff, ple_eff, sma,
#                       hma, aus_eff, asc, rea, hiv, xpe)]
# 


### Resampling of screening var

## restrict & reform
C <- C[,.(itb_cou_2,itb_fev_2,itb_fat_2,itb_wgt_2,itb_app_2, Contact_TB, itb_cou_3,
          temp_38, tachycardia, ice_ind_bin.factor, ice_cra.factor, Dep_csc, ice_ade_bin.factor, hiv_res.factor ,TB_stt_bin)]

# Apply transformation
C <- C %>%
  mutate(across(c(itb_cou_2,itb_fev_2,itb_fat_2,itb_wgt_2,itb_app_2, Contact_TB, itb_cou_3,
                  temp_38, tachycardia, ice_ind_bin.factor, ice_cra.factor, Dep_csc, ice_ade_bin.factor),
                ~ ifelse(. == "Yes", 1, 0)))

C <- C %>%
  mutate(across(c(hiv_res.factor),
                ~ ifelse(. == "Positive", 1, 0)))

C[TB_stt_bin == "NotTB", TB_stt_bin := "not TB"]


#C[, lapply(.SD, mean), by = TB_stt_bin, .SDcols = c("itb_cou_2_bi", "itb_fev_2_bi", "itb_fat_2_bi", "itb_wgt_2_bi", "itb_app_2_bi")]

Y <- C[TB_stt_bin=="TB", .(itb_cou_2,itb_fev_2,itb_fat_2,itb_wgt_2,itb_app_2, Contact_TB, itb_cou_3,
                           temp_38, tachycardia, ice_ind_bin.factor, ice_cra.factor, Dep_csc, ice_ade_bin.factor, hiv_res.factor ,TB_stt_bin)]
X <- C[TB_stt_bin!="TB", .(itb_cou_2,itb_fev_2,itb_fat_2,itb_wgt_2,itb_app_2, Contact_TB, itb_cou_3,
                           temp_38, tachycardia, ice_ind_bin.factor, ice_cra.factor, Dep_csc, ice_ade_bin.factor, hiv_res.factor ,TB_stt_bin)]

names(X) <- names(Y) <- c("cou2", "fev", "fat", "wgt", "app", "cont", "cou3", "temp", "tach","cind", "cra", "csc", "ade", "hiv", "TB")


## Resampling

# Read CSV into a data.table
# CF <- fread("CF.csv")
# set.seed(2345)

Ytmp <- Y[sample(1:nrow(Y),nrow(CF[TB=="TB"]),replace=TRUE)]
Xtmp <- X[sample(1:nrow(X),nrow(CF[TB!="TB"]),replace=TRUE)]

Xtmp[,id:=1:nrow(Xtmp)]
Ytmp[,id:=1:nrow(Ytmp)]

XY <- rbind(Xtmp,Ytmp)

CF <- merge(CF,XY,by=c("id","TB"),all.x=TRUE)

CF[,c("itb_cou_2", "itb_fev_2", "itb_fat_2", "itb_wgt_2", "itb_app_2",
      "Contact_TB", "itb_cou_3", "temp_38", "tachycardia", "ice_ind_bin.factor",
      "ice_cra.factor", "Dep_csc", "ice_ade_bin.factor", "hiv_res.factor", "TB_stt_bin"
      ):=.(cou2, fev, fat, wgt, app, cont, cou3, temp, tach, cind, cra, csc, ade, hiv, TB)]


## Check se/sp are matching between TBS and synthetic cohort - screening var

# SOC
Y[, mean(cou2 | fev | cont)] # se: 38%
Ytmp[, mean(cou2 | fev | cont)] # se: 38%
X[, mean(cou2 == 0 & fev == 0 & cont == 0)] # sp: 79%
Xtmp[, mean(cou2 == 0 & fev == 0 & cont == 0)] # sp: 79%

CF[TB=="TB", mean(cou2 | fev | cont)] # se: 37%
CF[TB=="TB", mean(itb_cou_2 | itb_fev_2 | Contact_TB)] # se: 37%
CF[TB!="TB", mean(cou2 == 0 & fev == 0 & cont == 0)] # sp: 79%
CF[TB!="TB", mean(itb_cou_2 == 0 & itb_fev_2 == 0 & Contact_TB == 0)] # sp: 79%

# TBS2
Y[, mean(cont | cou3 | temp | tach | cind | cra | csc | ade | hiv)] # se: 89%
Ytmp[, mean(cont | cou3 | temp | tach | cind | cra | csc | ade | hiv)] # se: 89%
X[, mean(cont == 0 & cou3 == 0 & temp == 0 & tach == 0 & cind == 0 & cra == 0 & csc == 0 & ade == 0 & hiv== 0)] # sp: 35%
Xtmp[, mean(cont == 0 & cou3 == 0 & temp == 0 & tach == 0 & cind == 0 & cra == 0 & csc == 0 & ade == 0 & hiv== 0)] # sp: 35%

CF[TB=="TB", mean(cont | cou3 | temp | tach | cind | cra | csc | ade | hiv)] # se: 89%
CF[TB=="TB", mean(Contact_TB | itb_cou_3 | temp_38 | tachycardia | ice_ind_bin.factor |
                    ice_cra.factor | Dep_csc | ice_ade_bin.factor | hiv_res.factor)] # se: 89%
CF[TB!="TB", mean(cont == 0 & cou3 == 0 & temp == 0 & tach == 0 & cind == 0 & cra == 0 & csc == 0 & ade == 0 & hiv== 0)] # sp: 35%
CF[TB!="TB", mean(Contact_TB == 0 & itb_cou_3 == 0 & temp_38 == 0 & tachycardia == 0 & ice_ind_bin.factor == 0 &
                    ice_cra.factor == 0 & Dep_csc == 0 & ice_ade_bin.factor == 0 & hiv_res.factor== 0)] # sp: 35%

# WHO
Y[, mean(cou2 | fev | fat | wgt | app)] # se: 80%
Ytmp[, mean(cou2 | fev | fat | wgt | app)] # se: 80%
X[, mean(cou2 == 0 & fev == 0 & fat == 0 & wgt == 0 & app == 0)] # sp: 27%
Xtmp[, mean(cou2 == 0 & fev == 0 & fat == 0 & wgt == 0 & app == 0)] # sp: 27%

CF[TB=="TB", mean(cou2 | fev | fat | wgt | app)] # se: 80%
CF[TB=="TB", mean(itb_cou_2 | itb_fev_2 | itb_fat_2 | itb_wgt_2 | itb_app_2)] # se: 80%
CF[TB!="TB", mean(cou2 == 0 & fev == 0 & fat == 0 & wgt == 0 & app == 0)] # sp: 28%
CF[TB!="TB", mean(itb_cou_2 == 0 & itb_fev_2 == 0 & itb_fat_2 == 0 & itb_wgt_2 == 0 & itb_app_2 == 0)] # sp: 28%

CF[, lapply(.SD, mean), by = TB, .SDcols = c("itb_cou_2", "itb_fev_2", "itb_fat_2", "itb_wgt_2", "itb_app_2")]


#Exact binomial CI for screening se/sp - redo for each arm and for se and for sp - estimate from X (not TB) and Y (TB)
# Given data
n_TB <- 434  # Total cases - change to 101 (TB) or 434 (not TB)
p_TB <- 0.2741935  # change value here

# Calculate number of successes (X)
X_TB <- round(p_TB * n_TB)  # Convert proportion to count

# Compute exact binomial confidence interval (Clopper-Pearson method)
ci <- binom.confint(X_TB, n_TB, conf.level = 0.95, methods = "exact")




## Append scores

setDT(CF)  # Ensure CF is a data.table
CF[, id := .I]

CF <- appendWHOscores(CF)
CF <- appendTBSscores(CF)

## check se/sp
CF[TB!="TB",1-mean(score_X>10)] #specificity =46%
CF[TB!="TB",1-mean(score_noX>10)] #specificity =82%
CF[TB=="TB",mean(score_X>10)] #sensitivity =74%
CF[TB=="TB",mean(score_noX>10)] #sensitivity =37%

## check se/sp
CF[TB!="TB",1-mean(TBS1Sb>=10)] #specificity =80%
CF[TB!="TB",1-mean(TBS2Sa>=1 & TBS2Sb>=10, na.rm = TRUE)] #specificity =85%
CF[TB=="TB",mean(TBS1Sb>=10)] #sensitivity =81%
CF[TB=="TB",mean(TBS2Sa>=1 & TBS2Sb>=10, na.rm = TRUE)] #sensitivity =72%


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

## NOTE CHECK with TB prevalence set to close to 1 if run (P object will need correcting afterwards)
# P$s.TBprev$r <- function(n) rbeta(n,101,434) #overwrite
# summary(P$s.TBprev$r(1e4))                 #check
# P <- parse.parmtable(PD0[, 1:2]) # correct afterwards

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
keep <- keep[1:24] # don't include extras that confuse reshapeINC if outputting parms
M <- ALL[, ..keep]
## harmonize naming:
names(M) <- gsub("s1s", "s1", names(M))
names(M) <- gsub("s2s", "s2", names(M))

MM <- reshapeINC(M) #NOTE can also explore FN/FP/cfr/reassess from this data

## CE plane ---------
## GP <- CEAplots(M[algorithm!='tbs2'],ring=TRUE,alph=0.05)
GP <- CEAplots(MM[country %in% c("Zambia", "Uganda")], ring = TRUE, alph = 0.5)
GP

MM[,.(`DALYs averted`=mean(`DALYs averted`),
     `Incremental cost`=mean(`Incremental cost`)),
  by=.(country,algorithm)]



## CEAC  ---------
CEAC <- make.ceacs(MM[country %in% c("Zambia", "Uganda")], seq(from = 0, to = 100, by = 0.5))

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

## ====================================

## CEAF  ---------
HZ <- getHZ(MM[,.(`DALYs averted`=mean(`DALYs averted`),
                 `Incremental cost`=mean(`Incremental cost`),
                 DALYsd=sd(`DALYs averted`),
                 COSTsd=sd(`Incremental cost`)),
               by=.(country,algorithm)])
HZ[,icer0:=c(0,icer[1:2]),by=country]
HZ[,icer1:=c(icer[1:2],400),by=country]
HZ[,algorithm:=rep(c('soc','tbs2','tbs1'),2)] #NOTE this is by hand - 'who',
MM <- reshapeINC(M,exclude.soc=FALSE) #this is vs a comparator of no intervention
CEAF <- make.ceafs(MM, seq(from = 0, to = 400, by = 0.5))

GF <- ggplot(
  CEAF,
  aes(L,P, col = algorithm)
) +
  geom_segment(data=HZ,aes(y=1,yend=1,x=icer0,xend=icer1,col=algorithm),
               lwd=2,alpha=0.5)+
  geom_vline(data=HZ,aes(xintercept=icer),col=2,lty=2)+
  #geom_text(data=HZ,aes(x=icer,y=0.95,label=txt),col=2,nudge_x=20)+
  # Adjust geom_text with conditional nudges for "130" specifically
  geom_text(data = subset(HZ, txt == "130" & country == "Uganda"),
            aes(x = icer, y = 0.95, label = txt),
            col = 2,
            nudge_x = -30  # Move "130" to the left
            ) +
  # Add other labels without specific nudges
  geom_text(data = subset(HZ, !(txt == "130" & country == "Uganda")),
            aes(x = icer, y = 0.95, label = txt),
            col = 2,
            nudge_x = 20) +

  geom_line() +
  facet_wrap(~country,ncol=1)+
  scale_y_continuous(label = percent,limits=c(0,1)) +
  xlab("Cost effectiveness threshold (US$ per DALY averted)") +
  ylab("Probability highest net benefit")+
  scale_color_manual(
    name = "Diagnostic approach",
    values = c("who" = "royalblue3", "tbs1" = "orangered2", "tbs2" = "seagreen", "soc" = "black"), 
    labels = c("who" = "WHO TDA", "tbs1" = "One-step TDA", "tbs2" = "Two-step TDA", "soc" = "SOC")) + 
  theme_bw()+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(size = 8),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        strip.background = element_blank(),
        strip.text = element_text(colour = "black")
        )
GF

ggsave(GF,file=here('graphs/CEAF3.png'),w=6,h=7)

## =======================================

## saving out  ---------
if (!is.na(SA) && SA == "") {
  ggsave(GP, file = here("graphs/CEhull.png"), h = 8, w = 10)
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


