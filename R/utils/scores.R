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
    if(Contact_with_adult_TB_case==1) TBS1S <- TBS1S + 8;
    if(Cough_more_3w==1) TBS1S <- TBS1S + 6;
    if(Loss_of_appetite_2w==1) TBS1S <- TBS1S + 2;
    if(Temperature_more_38==1) TBS1S <- TBS1S + 2;
    if(Tachycardia==1) TBS1S <- TBS1S + 5;
    if(Chest_in_drawing==1) TBS1S <- TBS1S + 8;
    if(Lack_of_consciousness==1) TBS1S <- TBS1S + 2;
    if(Cervical_or_supra_clavicular_adenopathy==1) TBS1S <- TBS1S + 12;
    if(CXR_Alveolar_opacity==1) TBS1S <- TBS1S + 4;
    if(CXR_Hila_mediastinal_lymphadenopathy==1) TBS1S <- TBS1S + 6;
    if(CXR_Pleural_effusion==1) TBS1S <- TBS1S + 13;
    if(CXR_Pericardial_effusion==1) TBS1S <- TBS1S - 9;
    if(AUS_Splenic_micro_abscesses==1) TBS1S <- TBS1S + 11;
    if(AUS_Pericardial_or_pleural_effusion==1) TBS1S <- TBS1S + 5;
    if(HIV_infection==1) TBS1S <- TBS1S + 6;
    if(Xpert_Positive==1) TBS1S <- TBS1S + 40;

    ## two-step part 1
    if(Contact_with_adult_TB_case==1) TBS2Sa <- TBS2Sa + 20;
    if(Fatigue_more_2w==1) TBS2Sa <- TBS2Sa + 6;
    if(Cough_more_3w==1) TBS2Sa <- TBS2Sa + 9;
    if(Temperature_more_38==1) TBS2Sa <- TBS2Sa + 19;
    if(Chest_in_drawing==1) TBS2Sa <- TBS2Sa + 9;
    if(Crackles_on_auscultation==1) TBS2Sa <- TBS2Sa + 9;
    if(Lack_of_consciousness==1) TBS2Sa <- TBS2Sa + 7;
    if(Cervical_or_supra_clavicular_adenopathy==1) TBS2Sa <- TBS2Sa + 26;
    if(HIV_infection_status==1) TBS2Sa <- TBS2Sa + 9;

    ## two-step part 2
    if(Contact_with_adult_TB_case==1) TBS2Sa <- TBS2Sa + 9;
    if(Cough_more_3w==1) TBS2Sa <- TBS2Sa + 5;
    if(Loss_of_appetite_2w==1) TBS2Sa <- TBS2Sa + 2;
    if(Temperature_more_38==1) TBS2Sa <- TBS2Sa + 4;
    if(Chest_in_drawing==1) TBS2Sa <- TBS2Sa + 9;
    if(Cervical_or_supra_clavicular_adenopathy==1) TBS2Sa <- TBS2Sa + 14;
    if(CXR_Alveolar_opacity==1) TBS2Sa <- TBS2Sa + 4;
    if(CXR_Hila_mediastinal_lymphadenopathy==1) TBS2Sa <- TBS2Sa + 6;
    if(CXR_Pleural_effusion==1) TBS2Sa <- TBS2Sa + 13;
    if(CXR_Pericardial_effusion==1) TBS2Sa <- TBS2Sa - 10;
    if(AUS_Hepatomegaly==1) TBS2Sa <- TBS2Sa - 2;
    if(AUS_Splenic_micro_abscesses==1) TBS2Sa <- TBS2Sa + 11;
    if(AUS_Pericardial_or_pleural_effusion==1) TBS2Sa <- TBS2Sa + 7;
    if(HIV_infection==1) TBS2Sa <- TBS2Sa + 7;
    if(Xpert_Positive==1) TBS2Sa <- TBS2Sa + 41;

    ## return
    list(TBS1S, TBS2Sa, TBS2Sb)
  }, by=id]
}





