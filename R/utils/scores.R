## the scores & algorithms

## function to compute WHO scores
appendWHOscores <- function(D){
  D[, c('score_noX','score_X'):={
    A <- B <- C <- 0;
    ## == w CXR algorithm
    ## non-CXR variables
    if(itb_cou_3==1) A <- A + 2;      #cough
    if(itb_fev_2==1) A <- A + 5;      #fever
    if(itb_fat_2==1) A <- A + 3;      #lethargy
    if(itb_wgt_3==1) A <- A + 3;      #weight loss
    if(haemoptysis==1) A <- A + 4;    #haemoptysis
    if(night.sweats==1) A <- A + 2;   #night sweats
    if(ice_ade.factor==1) A <- A + 4; #swolen LNs
    if(tachycardia==1) A <- A + 2;    #tachycardia
    if(tachypnea==1) A <- A - 1;      #tachypnea
    ## CXR variables
    if(cxr_pre_exc.factor==1) B <- B + 6; #cavities
    if(cxr_pre_hil.factor==1) B <- B +17; #enlarged LNs
    if(cxr_pre_alv.factor==1) B <- B + 5; #opacities
    if(cxr_pre_mil.factor==1) B <- B +15; #Miliary
    if(## cxr_pre_ple.factor==1 |            #effusion TODO variable not found
      cxr_pre_eff.factor==1) B <- B + 8;
    ## == no CXR algorithm
    if(itb_cou_3==1) C <- C + 5;      #cough
    if(itb_fev_2==1) C <- C +10;      #fever
    if(itb_fat_2==1) C <- C + 4;      #lethargy
    if(itb_wgt_3==1) C <- C + 5;      #weight loss
    if(haemoptysis==1) A <- A + 9;    #haemoptysis
    if(night.sweats==1) A <- A + 6;   #night sweats
    if(ice_ade.factor==1) C <- C + 7; #swolen LNs
    if(tachycardia==1) C <- C + 4;    #tachycardia
    if(tachypnea==1) C <- C + 2;      #tachypnea
    list(C,A+B)
  }, by=id]
}


## TBSpeed scores
appendTBSscores <- function(D){
  D[, c('TBS1S','TBS2Sa','TBS2Sb'):={

    TBS1S <- TBS2Sa <- TBS2Sb <- 0;

    ## one-step algorithm
    if(itb_exp_con.factor==1) TBS1S <- TBS1S + 8;
    if(itb_cou_3==1) TBS1S <- TBS1S + 6;
    if(itb_app_2==1) TBS1S <- TBS1S + 2;
    if(temp_38==1) TBS1S <- TBS1S + 2;
    if(tachycardia==1) TBS1S <- TBS1S + 5;
    if(ice_ind.factor==1) TBS1S <- TBS1S + 8;
    if(ice_csc.factor==1) TBS1S <- TBS1S + 2;
    if(ice_ade.factor==1) TBS1S <- TBS1S + 12;
    if(cxr_pre_alv.factor==1) TBS1S <- TBS1S + 4;
    if(cxr_pre_hil.factor==1) TBS1S <- TBS1S + 6;
    if(cxr_pre_ple.factor==1) TBS1S <- TBS1S + 13;
    if(cxr_pre_eff.factor==1) TBS1S <- TBS1S - 9;
    if(aus_sma.factor==1) TBS1S <- TBS1S + 11;
    if(aus_effusion==1) TBS1S <- TBS1S + 5;
    if(hiv_res.factor==1) TBS1S <- TBS1S + 6;
    if(Xpert_res==1) TBS1S <- TBS1S + 40;

    ## two-step part 1
    if(itb_exp_con.factor==1) TBS2Sa <- TBS2Sa + 20;
    if(itb_fat_2==1) TBS2Sa <- TBS2Sa + 6;
    if(itb_cou_3==1) TBS2Sa <- TBS2Sa + 9;
    if(temp_38==1) TBS2Sa <- TBS2Sa + 19;
    if(ice_ind.factor==1) TBS2Sa <- TBS2Sa + 9;
    if(ice_cra.factor==1) TBS2Sa <- TBS2Sa + 9;
    if(ice_csc.factor==1) TBS2Sa <- TBS2Sa + 7;
    if(ice_ade.factor==1) TBS2Sa <- TBS2Sa + 26;
    if(hiv_res.factor==1) TBS2Sa <- TBS2Sa + 9;

    ## two-step part 2
    if(itb_exp_con.factor==1) TBS2Sb <- TBS2Sb + 9;
    if(itb_cou_3==1) TBS2Sb <- TBS2Sb + 5;
    if(itb_app_2==1) TBS2Sb <- TBS2Sb + 2;
    if(temp_38==1) TBS2Sb <- TBS2Sb + 4;
    if(ice_ind.factor==1) TBS2Sb <- TBS2Sb + 9;
    if(ice_ade.factor==1) TBS2Sb <- TBS2Sb + 14;
    if(cxr_pre_alv.factor==1) TBS2Sb <- TBS2Sb + 4;
    if(cxr_pre_hil.factor==1) TBS2Sb <- TBS2Sb + 6;
    if(cxr_pre_ple.factor==1) TBS2Sb <- TBS2Sb + 13;
    if(cxr_pre_eff.factor==1) TBS2Sb <- TBS2Sb - 10;
    if(aus_hep.factor==1) TBS2Sb <- TBS2Sb - 2;
    if(aus_sma.factor==1) TBS2Sb <- TBS2Sb + 11;
    if(aus_effusion==1) TBS2Sb <- TBS2Sb + 7;
    if(hiv_res.factor==1) TBS2Sb <- TBS2Sb + 7;
    if(Xpert_res==1) TBS2Sb <- TBS2Sb + 41;

    ## return
    list(TBS1S, TBS2Sa, TBS2Sb)
  }, by=id]
}



## WHO algorithm
## NOTE this acts by side-effect
WHO.algorithm <- function(D){
  ## treatment decision
  D[,who.ATT:=fcase(
       !is.na(Xpert_res), ifelse(Xpert_res==1,1,0), #Xpert result available
       is.na(Xpert_res) & itb_exp_con.factor==1, 1, #HH contact
       is.na(Xpert_res) & itb_exp_con.factor==0 & CXR.avail==1, ifelse(score_X>10,1,0),
       is.na(Xpert_res) & itb_exp_con.factor==0 & CXR.avail==0, ifelse(score_noX>10,1,0),
       default=0
     )]
  ## costs
  D[,who.cost:=c.s.who.exam]                             #everyone gets
  D[!is.na(Xpert_res),who.cost:=who.cost + c.s.who.xns] #NOTE assumes not available=no cost
  D[is.na(Xpert_res),who.cost:=who.cost + c.s.who.hist] #those w/o Xpert get history assesst
  D[is.na(Xpert_res) & itb_exp_con.factor==0 & CXR.avail==1,
    who.cost:=who.cost + c.s.who.examCXR]
  D[is.na(Xpert_res) & itb_exp_con.factor==0 & CXR.avail==0,
    who.cost:=who.cost + c.s.who.exam]
  D[who.ATT==1,who.cost:=who.cost + c.s.rsATT] #ATT costs
}


## TBS 1-step algorithm
## NOTE this acts by side-effect
TBS1s.algorithm <- function(D){
  ## treatment decision
  D[,tbs1.ATT:=ifelse(TBS1S>10,1,0)]       #TODO include catch-up clinical
  ## costs
  D[,tbs1.cost:=c.s.tbs1step.diag]                             #everyone gets
  D[tbs1.ATT==1,tbs1.cost:=tbs1.cost + c.s.rsATT] #ATT costs
}


## TBS 2-step algorithm
## NOTE this acts by side-effect
TBS2s.algorithm <- function(D){
  ## treatment decision
  D[,tbs2.ATT:=fcase(
       TBS2Sa>10 & TBS2Sb>10, 1,
       default=0
     )]       #TODO include catch-up clinical
  ## costs
  D[,tbs2.cost:=c.s.tbs2step.scre]                             #everyone gets
  D[TBS2Sa>10,tbs2.cost:=tbs2.cost + c.s.tbs2step.diag]        #only those @ s2 
  D[tbs2.ATT==1,tbs2.cost:=tbs2.cost + c.s.rsATT] #ATT costs
}


## SOC
## NOTE this acts by side-effect
SOC.algorithm <- function(D){
  ## treatment decision
  D[,soc.ATT:=fcase(
       ptb==0,0,                                    #if not considered presumptive
       ptb==1 & testing.done==0,ifelse(TB=='TB',clin.sense,1-clin.spec), #clinical
       ptb==1 & testing.done==1 & xray.only==1,ifelse(TB=='TB',clin.senseX,1-clin.specX), #clinical+X
       ptb==1 & testing.done==1 & xray.only==0,ifelse(TB=='TB',clin.senseU,1-clin.specU), #inc. bac
       default=0
     )]
  ## costs
  D[,soc.cost:=c.s.soc.exam]                             #everyone gets
  D[ptb==1 & testing.done==0,soc.cost:=soc.cost + 0] #clinical
  D[ptb==1 & testing.done==1 & xray.only==1,soc.cost:=soc.cost + c.s.soc.CXR] #clinical+X
  D[ptb==1 & testing.done==1 & xray.only==0,soc.cost:=soc.cost + c.s.soc.CXRxga] #inc. bac
  D[soc.ATT==1,soc.cost:=soc.cost + c.s.rsATT] #ATT costs
}

## TODO: 1) cost soc clinical only? 2) use of reassess in soc?

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
