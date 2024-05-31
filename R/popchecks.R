
realpopt <- read_excel("data/realpopt.xlsx")
realpop <- read_excel("data/realpop.xlsx")

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
realpop[,1-mean(realpop$SCO_ORG_tot>=10)] #specificity =81%
realpop[,1-mean(realpop$SCO_SCR_tot>=1 & realpop$SCO_DIA_tot>=10)] #specificity =84%
realpopt[,mean(realpopt$SCO_ORG_tot>=10)] #sensitivity =86%
realpopt[,mean(realpopt$SCO_SCR_tot>=1 & realpopt$SCO_DIA_tot>=10)] #sensitivity =79%

# ## check se/sp using our score
realpop[,1-mean(TBS1Sb>=10)] #real pop sp = 81%
                           #synthetic pop sp: 81%
                           #clinical paper sp: 81% [77-84]
realpop[,1-mean(TBS2Sa>=1 & TBS2Sb>=10, na.rm = TRUE)] #real pop sp = 84%
                                                      #synthetic pop sp: 84%
                                                      #clinical paper sp: 84% [80-87]
realpopt[,mean(TBS1Sb>=10)] #real pop se = 86%
                          #synthetic pop se: 86%
                          #clinical paper se: 86% [78-92]
realpopt[,mean(TBS2Sa>=1 & TBS2Sb>=10, na.rm = TRUE)] #real pop se = 79%
                                                     #synthetic pop se: 79%
                                                     #clinical paper se: 79% [70-86]
# Compare Minh's scores and ours
table(realpopt$SCO_ORG_SCR_tot>=10) # 28/101
table(realpopt$SCO_ORG_tot>=10) # 87/101
table(realpop$SCO_ORG_tot<10) # 351/434
table(realpopt$TBS1Sa>=10) # 28/101
table(realpopt$TBS1Sb>=10) # 87/101
table(realpop$TBS1Sb<10) # 351/434

table(realpopt$SCO_SCR_tot>=1) # 90/101
table(realpop$SCO_SCR_tot<1) # 151/434
table(realpopt$TBS2Sa>=1) # 90/101
table(realpop$TBS2Sa<1) # 151/434

table(realpopt$SCO_DIA_tot>=10) # 80/101
table(realpop$SCO_DIA_tot<10) # 212+151= 363/434
summary(is.na(realpop$SCO_DIA_tot))
table(realpopt$TBS2Sb>=10) # 80/101
table(realpop$TBS2Sb<10) # 212+151= 363/434
summary(is.na(realpop$TBS2Sb))

all(realpopt$SCO_ORG_SCR_tot == realpopt$TBS1Sa)
all(realpop$SCO_ORG_SCR_tot == realpop$TBS1Sa)

all(realpopt$SCO_ORG_tot == realpopt$TBS1Sb)
all(realpop$SCO_ORG_tot == realpop$TBS1Sb)

all(realpopt$SCO_SCR_tot == realpopt$TBS2Sa, na.rm = TRUE) 
all(realpop$SCO_SCR_tot == realpop$TBS2Sa, na.rm = TRUE)  

all(realpopt$SCO_DIA_tot == realpopt$TBS2Sb, na.rm = TRUE)
all(realpop$SCO_DIA_tot == realpop$TBS2Sb, na.rm = TRUE)
