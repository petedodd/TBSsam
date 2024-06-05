## the scores & algorithms


## function to compute WHO scores
appendWHOscores <- function(D){
  D[, c('who_scre','score_noX','score_X'):={
    who_scre <- A <- B <- C <- 0;
    ## screening step
    if(itb_cou_2==1) who_scre <- who_scre + 1;      #cough
    if(itb_fev_2==1) who_scre <- who_scre + 1;      #fever
    if(itb_fat_2==1) who_scre <- who_scre + 1;      #lethargy (fatigue, reduced playfulness, decreased activity)
    if(itb_wgt.factor==1) who_scre <- who_scre + 1;      #weight loss
    if(itb_app_2==1) who_scre <- who_scre + 1;      #loss of appetite
    ## == w CXR algorithm
    ## non-CXR variables
    if(itb_cou_3==1) A <- A + 2;      #cough
    if(itb_fev_2==1) A <- A + 5;      #fever
    if(itb_fat_2==1) A <- A + 3;      #lethargy
    if(itb_wgt.factor==1) A <- A + 3;      #weight loss
    if(haemoptysis==1) A <- A + 4;    #haemoptysis
    if(night.sweats==1) A <- A + 2;   #night sweats
    if(ice_ade_bin.factor==1) A <- A + 4; #swolen LNs
    if(tachycardia==1) A <- A + 2;    #tachycardia
    if(tachypnea==1) A <- A - 1;      #tachypnea
    ## CXR variables
    if(cxr_pre_exc.factor==1) B <- B + 6; #cavities
    if(cxr_pre_hil.factor==1) B <- B +17; #enlarged LNs
    if(cxr_pre_alv.factor==1) B <- B + 5; #opacities
    if(cxr_pre_mil.factor==1) B <- B +15; #Miliary
    if(## cxr_pre_ple.factor==1 |            #effusion NOTE var included in other
      cxr_pre_ple_per_eff.factor==1) B <- B + 8;    
    ## == no CXR algorithm
    if(itb_cou_3==1) C <- C + 5;      #cough
    if(itb_fev_2==1) C <- C +10;      #fever
    if(itb_fat_2==1) C <- C + 4;      #lethargy
    if(itb_wgt.factor==1) C <- C + 5;      #weight loss
    if(haemoptysis==1) A <- A + 9;    #haemoptysis
    if(night.sweats==1) A <- A + 6;   #night sweats
    if(ice_ade_bin.factor==1) C <- C + 7; #swolen LNs
    if(tachycardia==1) C <- C + 4;    #tachycardia
    if(tachypnea==1) C <- C + 2;      #tachypnea
    list(who_scre,C,A+B)
  }, by=id]
}

## cxr_pre_eff.factor

## TBSpeed scores
appendTBSscores <- function(D){
  D[, c('TBS1Sa', 'TBS1Sb','TBS2Sa','TBS2Sb'):={

    TBS1Sa <- TBS1Sb <- TBS2Sa <- TBS2Sb <- 0;

    ## one-step part 1
    if(Contact_TB==1) TBS1Sa <- TBS1Sa + 8;
    if(ice_ind_bin.factor==1) TBS1Sa <- TBS1Sa + 7;
    if(itb_cou_3==1) TBS1Sa <- TBS1Sa + 6;
    if(tachycardia==1) TBS1Sa <- TBS1Sa + 6;
    if(itb_app_2==1) TBS1Sa <- TBS1Sa + 2;
    if(Dep_csc==1) TBS1Sa <- TBS1Sa + 1;

    ## one-step part 2
    if(Contact_TB==1) TBS1Sb <- TBS1Sb + 8;
    if(ice_ind_bin.factor==1) TBS1Sb <- TBS1Sb + 7;
    if(itb_cou_3==1) TBS1Sb <- TBS1Sb + 6;
    if(tachycardia==1) TBS1Sb <- TBS1Sb + 6;
    if(itb_app_2==1) TBS1Sb <- TBS1Sb + 2;
    if(Dep_csc==1) TBS1Sb <- TBS1Sb + 1;
    if(cxr_pre_ple.factor==1) TBS1Sb <- TBS1Sb + 11;
    if(cxr_pre_hil.factor==1) TBS1Sb <- TBS1Sb + 6;
    if(cxr_pre_alv.factor==1) TBS1Sb <- TBS1Sb + 4;
    if(cxr_pre_eff.factor==1) TBS1Sb <- TBS1Sb - 8;
    if(Xpert_res==1) TBS1Sb <- TBS1Sb + 35;
    if(aus_hma.factor==1) TBS1Sb <- TBS1Sb + 10;
    if(aus_sma.factor==1) TBS1Sb <- TBS1Sb + 8;
    if(aus_effusion==1) TBS1Sb <- TBS1Sb + 3;
    if(aus_asc.factor==1) TBS1Sb <- TBS1Sb + 1;

    ## two-step part 1
    if(Contact_TB==1) TBS2Sa <- TBS2Sa + 1;
    if(itb_cou_3==1) TBS2Sa <- TBS2Sa + 1;
    if(temp_38==1) TBS2Sa <- TBS2Sa + 1;
    if(tachycardia==1) TBS2Sa <- TBS2Sa + 1;
    if(ice_ind_bin.factor==1) TBS2Sa <- TBS2Sa + 1;
    if(ice_cra.factor==1) TBS2Sa <- TBS2Sa + 1;
    if(Dep_csc==1) TBS2Sa <- TBS2Sa + 1;
    if(ice_ade_bin.factor==1) TBS2Sa <- TBS2Sa + 1;
    if(hiv_res.factor==1) TBS2Sa <- TBS2Sa + 1;

    ## two-step part 2
    if(Contact_TB==1) TBS2Sb <- TBS2Sb + 8;
    if(ice_ind_bin.factor==1) TBS2Sb <- TBS2Sb + 7;
    if(itb_cou_3==1) TBS2Sb <- TBS2Sb + 5;
    if(tachycardia==1) TBS2Sb <- TBS2Sb + 5;
    if(itb_app_2==1) TBS2Sb <- TBS2Sb + 2;
    if(Xpert_res==1) TBS2Sb <- TBS2Sb + 31;
    if(cxr_pre_ple.factor==1) TBS2Sb <- TBS2Sb + 10;
    if(cxr_pre_alv.factor==1) TBS2Sb <- TBS2Sb + 5;
    if(cxr_pre_hil.factor==1) TBS2Sb <- TBS2Sb + 5;
    if(cxr_pre_eff.factor==1) TBS2Sb <- TBS2Sb - 6;
    if(aus_hma.factor==1) TBS2Sb <- TBS2Sb + 10;
    if(aus_sma.factor==1) TBS2Sb <- TBS2Sb + 6;
    if(aus_effusion==1) TBS2Sb <- TBS2Sb + 4;
    TBS2Sb <- replace(TBS2Sb,TBS2Sa<1,NA);

    ## return
    list(TBS1Sa, TBS1Sb, TBS2Sa, TBS2Sb)
  }, by=id]
}



## NOTE costs must be initialized to zero
## TODO check above
## WHO algorithm
WHO.algorithm <- function(D){
  if(!is.data.table(D)) stop('Input data must be data.table!')
  cat('...',nrow(D),'\n')
  ## initial assessment
  D[,who.ATT:=0]
  D[who_scre>=1 & !is.na(Xpert_res),who.ATT:=ifelse(Xpert_res==1,1,0)] #Xpert result available
  D[who_scre>=1 & (is.na(Xpert_res) | Xpert_res==0) & Contact_TB==1,who.ATT:=1] #HH contact
  D[who_scre>=1 & (is.na(Xpert_res) | Xpert_res==0) & Contact_TB==0 & CXR.avail==1,
    who.ATT:=ifelse(score_X>10,1,0)]
  D[who_scre>=1 & (is.na(Xpert_res) | Xpert_res==0) & Contact_TB==0 & CXR.avail==0,
    who.ATT:=ifelse(score_noX>10,1,0)]
  ## reassessment (see getAlgoParms for logic)
  D[,reassess:=ifelse(who.ATT==1, #treated initially
                      0,          #no reassessment as on treatment
               ifelse(TB=='TB',s.reassess.choice.se,1-s.reassess.choice.sp))]
  D[reassess==1,who.cost:=who.cost + c.s.tbs1step.reassessCXR30]              #NOTE reassessment costs
  D[reassess==1,who.ATT:=ifelse(TB=='TB',s.reassess.se,1-s.reassess.sp)]      #treatment from reassessment
  ## costs
  D[,who.cost:=who.cost+c.s.who.scre]                                         #everyone gets
  D[who_scre>=1,who.cost:=who.cost+c.s.who.diag]                              #if presents one of the chronic symptoms
  D[who_scre>=1 & hiv_res.factor==1,who.cost:=who.cost+c.s.who.hiv.diag]      #if presents one of the chronic symptoms and is HIV+, also receive urine LAM
  D[who.ATT==1,who.cost:=who.cost + c.s.ATT]                                  #ATT costs

  return(data.table(who.ATT=D$who.ATT,who.cost=D$who.cost))
}




## TBS 1-step algorithm
## NOTE this acts by side-effect
TBS1s.algorithm <- function(D){
  if(!is.data.table(D)) stop('Input data must be data.table!')
  ## TB screening - none - all receive tbs1
  ## treatment decision
    D[,tbs1.ATT:=fcase(
    TBS1Sb>=10, 1,
    default=0
  )]
  ## reassessment
  D[,reassess:=ifelse(soc.ATT==1, #treated initially
                      0,          #no reassessment as on treatment
               ifelse(TB=='TB',s.reassess.choice.se,1-s.reassess.choice.sp))]
  D[reassess==1,soc.cost:=soc.cost + c.s.tbs1step.reassessCXR30]              #NOTE reassessment costs
  D[reassess==1,soc.ATT:=ifelse(TB=='TB',s.reassess.se,1-s.reassess.sp)]      #treatment from reassessment
  ## costs
  D[TBS1Sa>=10,tbs1.cost:=tbs1.cost+c.s.tbs1step.diag.clin]                  #clinical score only (and CXR, Xpert for non-diag purpose)
  D[TBS1Sa<10,tbs1.cost:=tbs1.cost+c.s.tbs1step.diag.test]      #clinical, CXR, Xpert, AUS scoring
  D[tbs1.ATT==1,tbs1.cost:=tbs1.cost + c.s.ATT] #ATT costs
}

## TBS 2-step algorithm
## NOTE this acts by side-effect
TBS2s.algorithm <- function(D){
  if(!is.data.table(D)) stop('Input data must be data.table!')
  ## treatment decision
  D[,tbs2.ATT:=fcase(
       TBS2Sa>=1 & TBS2Sb>=10, 1,
       default=0
     )]
  ## reassessment
  D[,reassess:=ifelse(soc.ATT==1, #treated initially
                      0,          #no reassessment as on treatment
               ifelse(TB=='TB',s.reassess.choice.se,1-s.reassess.choice.sp))]
  D[reassess==1,soc.cost:=soc.cost + c.s.tbs1step.reassessCXR30]              #NOTE reassessment costs
  D[reassess==1,soc.ATT:=ifelse(TB=='TB',s.reassess.se,1-s.reassess.sp)]      #treatment from reassessment
  ## costs
  D[,tbs2.cost:=tbs2.cost+c.s.tbs2step.scre]                             #everyone gets
  D[TBS2Sa>=1,tbs2.cost:=tbs2.cost + c.s.tbs2step.diag]                  #only those @ s2 
  D[tbs2.ATT==1,tbs2.cost:=tbs2.cost + c.s.ATT] #ATT costs
}


## SOC
SOC.algorithm <- function(D){
  if(!is.data.table(D)) stop('Input data must be data.table!')
  socvrs <- c('ptb','testing.done','xray.only','clin.senseX','clin.specX',
              'clin.sense','clin.spec','clin.senseU','clin.specU')
  cat('...',nrow(D),'\n')

  ## treatment decision
  ## NOTE this looks like a probability, but sense/spec from getAlgoParms are sampled 1/0
  D[,soc.ATT:=fcase(
       ptb==0,0,            #if not considered presumptive (screening rate=80% based on expert opinion)
       ptb==1 & testing.done==0,ifelse(TB=='TB',clin.sense,1-clin.spec), #clinical
       ptb==1 & testing.done==1 & xray.only==1,ifelse(TB=='TB',clin.senseX,1-clin.specX), #clinical+X    #MD: clin.senseX & clin.specX do not to account for Xpert result so we are missing se/sp parms in getAlgoParms for the scenario 'clinical exam + CXR + Xpert(GA)' 
       ptb==1 & testing.done==1 & xray.only==0,ifelse(TB=='TB',clin.senseU,1-clin.specU), #inc. bac
       default=0
     )]
  ## costs
  D[,soc.cost:=soc.cost+c.s.soc.scre]                             #everyone gets
  D[ptb==1 & testing.done==0 & xray.only==0,soc.cost:=soc.cost + c.s.soc.exam] #clinical
  D[ptb==1 & testing.done==0 & xray.only==1,soc.cost:=soc.cost + c.s.soc.exam + c.s.soc.CXR] #clinical+CXR
  D[ptb==1 & testing.done==1 & xray.only==0,soc.cost:=soc.cost + c.s.soc.exam + c.s.soc.xga] #clinical+Xpert
  D[ptb==1 & testing.done==1 & xray.only==1,soc.cost:=soc.cost + c.s.soc.exam + c.s.soc.CXRxga] #clinical+CXR+Xpert
  ## reassessment
  D[,reassess:=ifelse(soc.ATT==1, #treated initially
                      0,          #no reassessment as on treatment
               ifelse(TB=='TB',s.reassess.choice.se,1-s.reassess.choice.sp))]
  D[reassess==1,soc.cost:=soc.cost + c.s.tbs1step.reassessCXR30]              #NOTE reassessment costs
  D[reassess==1,soc.ATT:=ifelse(TB=='TB',s.reassess.se,1-s.reassess.sp)]      #treatment from reassessment

  ## treatment costs
  D[soc.ATT==1,soc.cost:=soc.cost + c.s.ATT] #ATT costs

  return(data.table(soc.ATT=D$soc.ATT,soc.cost=D$soc.cost))
}


## c.s.soc.exam	SOC: initial clinical assessment
## c.s.soc.CXR	SOC: CXR
## c.s.soc.CXRxga	SOC: CXR + Xpert Ultra on GA [for all] + urine LAM
## c.s.soc.reassessCXRxga	SOC: reassessment exam + CXR + Xpert Ultra on GA [for all] + urine LAM

## c.s.who.examCXR	INT: WHO algo: clinical exam + CXR
## c.s.who.exam	INT: WHO algo: clinical exam + HIV test 
## c.s.who.xns	INT: WHO algo: Xpert Ultra on NPA & stool
## c.s.who.hist	INT: WHO algo: assessment of TB contacts in the previous 12 months

## c.s.tbs2step.scre	INT: TB-Speed two-steps algo: screening: clinical exam + HIV test 
## c.s.tbs1step.diag	INT: TB-Speed one-step algo: clinical exam + HIV test + CXR + Xpert on NPA and stool + abdo US
## c.s.tbs2step.diag	INT: TB-Speed two-steps algo: diagnostic: CXR + Xpert on NPA and stool + abdo US

## c.s.rsATT	Rifampicin-sensitive anti-TB treatment in SAM children
## c.s.rrATT	Rifampicin-resistant anti-TB treatment in SAM children
