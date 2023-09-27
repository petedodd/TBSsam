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
    list(C,A+B)
  }, by=id]
}

## cxr_pre_eff.factor

## TBSpeed scores
appendTBSscores <- function(D){
  D[, c('TBS1S','TBS2Sa','TBS2Sb'):={

    TBS1S <- TBS2Sa <- TBS2Sb <- 0;

    ## one-step algorithm
    if(Contact_TB==1) TBS1S <- TBS1S + 8;
    if(itb_cou_3==1) TBS1S <- TBS1S + 6;
    if(itb_app_2==1) TBS1S <- TBS1S + 2;
    if(tachycardia==1) TBS1S <- TBS1S + 6;
    if(ice_ind_bin.factor==1) TBS1S <- TBS1S + 7;
    if(Dep_csc==1) TBS1S <- TBS1S + 1;
    if(cxr_pre_alv.factor==1) TBS1S <- TBS1S + 4;
    if(cxr_pre_hil.factor==1) TBS1S <- TBS1S + 6;
    if(cxr_pre_ple.factor==1) TBS1S <- TBS1S + 11;
    if(cxr_pre_eff.factor==1) TBS1S <- TBS1S - 8;
    if(aus_sma.factor==1) TBS1S <- TBS1S + 8;
    if(aus_hma.factor==1) TBS1S <- TBS1S + 10;
    if(aus_effusion==1) TBS1S <- TBS1S + 3;
    if(aus_asc.factor==1) TBS1S <- TBS1S + 1;
    if(Xpert_res==1) TBS1S <- TBS1S + 35;

    ## two-step part 1
    if(Contact_TB==1) TBS2Sa <- TBS2Sa + 33;
    if(itb_fat_2==1) TBS2Sa <- TBS2Sa + 9;
    if(itb_cou_3==1) TBS2Sa <- TBS2Sa + 17;
    if(temp_38==1) TBS2Sa <- TBS2Sa + 28;
    if(tachycardia==1) TBS2Sa <- TBS2Sa + 11;
    if(ice_ind_bin.factor==1) TBS2Sa <- TBS2Sa + 16;
    if(ice_cra.factor==1) TBS2Sa <- TBS2Sa + 16;
    if(Dep_csc==1) TBS2Sa <- TBS2Sa + 10;
    if(ice_ade_bin.factor==1) TBS2Sa <- TBS2Sa + 27;
    if(hiv_res.factor==1) TBS2Sa <- TBS2Sa + 11;

    ## two-step part 2
    if(Contact_TB==1) TBS2Sb <- TBS2Sb + 8;
    if(itb_cou_3==1) TBS2Sb <- TBS2Sb + 5;
    if(itb_app_2==1) TBS2Sb <- TBS2Sb + 2;
    if(tachycardia==1) TBS2Sb <- TBS2Sb + 5;
    if(ice_ind_bin.factor==1) TBS2Sb <- TBS2Sb + 7;
    if(cxr_pre_alv.factor==1) TBS2Sb <- TBS2Sb + 5;
    if(cxr_pre_hil.factor==1) TBS2Sb <- TBS2Sb + 5;
    if(cxr_pre_ple.factor==1) TBS2Sb <- TBS2Sb + 10;
    if(cxr_pre_eff.factor==1) TBS2Sb <- TBS2Sb - 6;
    if(aus_sma.factor==1) TBS2Sb <- TBS2Sb + 6;
    if(aus_hma.factor==1) TBS2Sb <- TBS2Sb + 10;
    if(aus_effusion==1) TBS2Sb <- TBS2Sb + 4;
    if(Xpert_res==1) TBS2Sb <- TBS2Sb + 31;

    ## return
    list(TBS1S, TBS2Sa, TBS2Sb)
  }, by=id]
}


## NOTE costs must be initialized to zero



## WHO algorithm
WHO.algorithm <- function(D,resample=FALSE){
  cat('...',nrow(D),'\n')
  
  ## treatment decision
  D[,who.ATT:=fcase(
    ptb==0,0,                                    #if not considered presumptive (screening rate=80% based on expert opinion, as in SOC)
    ptb==1 & CXR.avail==1,who.ATT:=ifelse(score_X>10,1,0),
    ptb==1 & CXR.avail==0,who.ATT:=ifelse(score_noX>10,1,0),
    default=0
  )]
  
  ## treatment decision
  D[,soc.ATT:=fcase(
    ptb==0,0,                                    #if not considered presumptive (screening rate=80% based on expert opinion)
    ptb==1 & testing.done==0,ifelse(TB=='TB',clin.sense,1-clin.spec), #clinical
    ptb==1 & testing.done==1 & xray.only==1,ifelse(TB=='TB',clin.senseX,1-clin.specX), #clinical+X
    ptb==1 & testing.done==1 & xray.only==0,ifelse(TB=='TB',clin.senseU,1-clin.specU), #inc. bac
    default=0
  )]
  
  ## costs
  D[,who.cost:=who.cost+c.s.who.scre]                             #everyone gets
  D[ptb==1 & hiv_res.factor==1,who.cost:=who.cost + c.s.who.hiv.diag] #CLHIV get urine LF-LAM 
  D[ptb==1 & hiv_res.factor==0,who.cost:=who.cost + c.s.who.diag] 
  D[ptb==1 & who.ATT==1,who.cost:=who.cost + c.s.ATT] #ATT costs
  
  
  ## resample approach to reassessment
  whovrs <- c('Xpert_res','score_X','score_noX') #variables to overwrite in resmple  'who.ATT',
  if(resample){
    for(h in 0:1){                     #resample signs stratified by TB/HIV
      for(tb in c('TB','not TB')){
        cat('h=',h,' , tb=',tb,'\n')
        n <- nrow(D[who.ATT==0 & reassess==1 & TB==tb & hiv_res.factor==h])
        N <- nrow(D[TB==tb & hiv_res.factor==h])
        if(n>0){ #resample signs
          ns <- D[TB==tb & hiv_res.factor==h,..whovrs] #new sample frame
          D[who.ATT==0 & reassess==1 & TB==tb & hiv_res.factor==h,(whovrs):=ns[sample(N,n)]]
          ans <- WHO.algorithm(D[who.ATT==0 & reassess==1 & TB==tb & hiv_res.factor==h],
                               resample=FALSE) #recurse, depth 1
          D[who.ATT==0 & reassess==1 & TB==tb & hiv_res.factor==h,c('who.ATT','who.cost'):=ans]
        }
      }
    }
  }
  return(data.table(who.ATT=D$who.ATT,who.cost=D$who.cost))
}



## TBS 1-step algorithm
## NOTE this acts by side-effect
TBS1s.algorithm <- function(D){
  ## TB screening - none - all receive tbs1
  
  ## treatment decision
  D[,tbs1.ATT:=ifelse(TBS1S>10,1,0)]
  ## treatment despite score
  D[tbs1.ATT==0 & reassess==1,tbs1.cost:=tbs1.cost+0] #cost NOTE assumed zero like in SOC
  D[tbs1.ATT==0 & reassess==1 & TB=='TB',tbs1.ATT:=clin.sense]
  D[tbs1.ATT==0 & reassess==1 & TB=='not TB',tbs1.ATT:=1-clin.spec]
  ## costs
  D[,tbs1.cost:=tbs1.cost+c.s.tbs1step.diag]                             #everyone gets
  D[tbs1.ATT==1,tbs1.cost:=tbs1.cost + c.s.ATT] #ATT costs
}


## TBS 2-step algorithm
## NOTE this acts by side-effect
TBS2s.algorithm <- function(D){
  ## treatment decision
  D[,tbs2.ATT:=fcase(
       TBS2Sa>10 & TBS2Sb>10, 1,
       default=0
     )]
  ## treatment despite score
  D[tbs2.ATT==0 & reassess==1,tbs2.cost:=tbs2.cost+0] #cost NOTE assumed zero like in SOC
  D[tbs2.ATT==0 & reassess==1 & TB=='TB',tbs2.ATT:=clin.sense]
  D[tbs2.ATT==0 & reassess==1 & TB=='not TB',tbs2.ATT:=1-clin.spec]
  ## costs
  D[,tbs2.cost:=tbs2.cost+c.s.tbs2step.scre]                             #everyone gets
  D[TBS2Sa>10,tbs2.cost:=tbs2.cost + c.s.tbs2step.diag]        #only those @ s2 
  D[tbs2.ATT==1,tbs2.cost:=tbs2.cost + c.s.ATT] #ATT costs
}


## SOC
SOC.algorithm <- function(D,resample=FALSE){
  socvrs <- c('ptb','testing.done','xray.only','clin.senseX','clin.specX',
              'clin.sense','clin.spec','clin.senseU','clin.specU')
  cat('...',nrow(D),'\n')
  
  ## treatment decision
  D[,soc.ATT:=fcase(
       ptb==0,0,                                    #if not considered presumptive (screening rate=80% based on expert opinion)
       ptb==1 & testing.done==0,ifelse(TB=='TB',clin.sense,1-clin.spec), #clinical
       ptb==1 & testing.done==1 & xray.only==1,ifelse(TB=='TB',clin.senseX,1-clin.specX), #clinical+X
       ptb==1 & testing.done==1 & xray.only==0,ifelse(TB=='TB',clin.senseU,1-clin.specU), #inc. bac
       default=0
     )]
  ## costs
  D[,soc.cost:=soc.cost+c.s.soc.scre]                             #everyone gets
  D[ptb==1 & testing.done==0 & xray.only==0,soc.cost:=soc.cost + c.s.soc.exam] #clinical
  D[ptb==1 & testing.done==0 & xray.only==1,soc.cost:=soc.cost + c.s.soc.exam + c.s.soc.CXR] #clinical+X
  D[ptb==1 & testing.done==1 & xray.only==1,soc.cost:=soc.cost + c.s.soc.exam + c.s.soc.CXRxga] #inc. bac
  D[soc.ATT==1,soc.cost:=soc.cost + c.s.ATT] #ATT costs
  ## resample approach to reassessment
  if(resample){
    for(h in 0:1){                     #resample signs stratified by TB/HIV
      for(tb in c('TB','not TB')){
        cat('h=',h,' , tb=',tb,'\n')
        n <- nrow(D[soc.ATT==0 & reassess==1 & TB==tb & hiv_res.factor==h])
        N <- nrow(D[TB==tb & hiv_res.factor==h])
        if(n>0){ #resample signs
          ns <- D[TB==tb & hiv_res.factor==h,..socvrs] #new sample frame
          D[soc.ATT==0 & reassess==1 & TB==tb & hiv_res.factor==h,(socvrs):=ns[sample(N,n)]]
          ans <- SOC.algorithm(D[soc.ATT==0 & reassess==1 & TB==tb & hiv_res.factor==h],
                               resample=FALSE) #recurse, depth 1
          D[soc.ATT==0 & reassess==1 & TB==tb & hiv_res.factor==h,c('soc.ATT','soc.cost'):=ans]
        }
      }
    }
  }
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
