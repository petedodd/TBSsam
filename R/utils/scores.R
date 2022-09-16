## looking to model the scores

## function to compute WHO scores
## NOTE acts by side effect
appendWHOscores <- function(D){
  D[, c('score_noX','score_X'):={
    A <- B <- C <- 0;
    ## == w CXR algorithm
    ## non-CXR variables
    if(itb_cou_3==1) A <- A + 2;      #cough
    if(itb_fev_2==1) A <- A + 5;      #fever
    if(itb_fat_2==1) A <- A + 3;      #lethargy
    if(itb_wgt_3==1) A <- A + 3;      #weight loss
    if(ice_ade.factor==1) A <- A + 4; #swolen LNs
    if(tachycardia==1) A <- A + 2;    #tachycardia
    if(tachypnea==1) A <- A + 1;      #tachypnea
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
    ## heamoptysis TODO
    ## night sweats TODO
    if(ice_ade.factor==1) C <- C + 7; #swolen LNs
    if(tachycardia==1) C <- C + 4;    #tachycardia
    if(tachypnea==1) C <- C + 2;      #tachypnea
    list(C,A+B)
  }, by=id]
}





