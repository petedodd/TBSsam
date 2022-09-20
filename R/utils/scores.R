## the scores

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





