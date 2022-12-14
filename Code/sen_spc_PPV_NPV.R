library(here)
library(readxl)
library(tidyverse)
library(table1)
library(janitor)
library(magrittr)
library(arsenal)
library(diffdf)
library(eeptools)
library(lubridate)
library(tableone)
library(caret)

#loading clean dataset
scc_doi<-read_csv(here("Data","scc_fiveyr_clean_05-12-2022.csv"))
baseline_scc_doi<-read_csv(here("Data","clean_baseline_05-12-2022.csv"))

#Checking vectors intersted in
##Clinical Node - Prediction
## Radiographic Node - ref (gold standard) 

table(baseline_scc_doi$tab_n_clinc_stage, baseline_scc_doi$n_clinc_stage,exclude = NULL)
table(baseline_scc_doi$tab_n_radio_stage, baseline_scc_doi$n_radio_stage,exclude = NULL)

#eliminating those patients who do not have Radiographic _N_stage reported (23 patients)
CR_sen_spec<-baseline_scc_doi%>%
  select(Patient.IDID.du.patient, tab_n_clinc_stage, tab_n_radio_stage)%>%
  filter(!is.na(tab_n_radio_stage))

class(CR_sen_spec$tab_n_clinc_stage)
class(CR_sen_spec$tab_n_radio_stage)
class(CR_sen_spec$Patient.IDID.du.patient)
CR_sen_spec$tab_n_clinc_stage<-as.factor(CR_sen_spec$tab_n_clinc_stage)
CR_sen_spec$tab_n_radio_stage<-as.factor(CR_sen_spec$tab_n_radio_stage)
levels(CR_sen_spec$tab_n_clinc_stage)
levels(CR_sen_spec$tab_n_radio_stage)


confusionMatrix(CR_sen_spec$tab_n_clinc_stage, #the predicted classes
                CR_sen_spec$tab_n_radio_stage,
                dnn = c("Clinic_N_stage","Radio_N_Stage"),
                mode = "sens_spec") 

##Clinical Node - Prediction
##Pathologic Node - ref (gold standard) 

table(baseline_scc_doi$tab_n_clinc_stage, baseline_scc_doi$n_clinc_stage,exclude = NULL)
table(baseline_scc_doi$tab_n_patho_stage, baseline_scc_doi$n_patho_stage,exclude = NULL)
# baseline_scc_doi[c(which(baseline_scc_doi$n_patho_stage == "N0" & 
#                            baseline_scc_doi$tab_n_patho_stage == "All not N0")), "Patient.IDID.du.patient"] #Patient ID 100 and 195
# 
# baseline_scc_doi[c(which(baseline_scc_doi$n_patho_stage == "N0" & 
#                            baseline_scc_doi$tab_n_patho_stage == "All not N0")), "tab_n_patho_stage"]<-"N0"
# table(baseline_scc_doi$tab_n_patho_stage, baseline_scc_doi$n_patho_stage,exclude = NULL)
# 
# baseline_scc_doi[c(which(baseline_scc_doi$n_patho_stage == "Not Applicable" & 
#                            baseline_scc_doi$tab_n_patho_stage == "N0")), "Patient.IDID.du.patient"]
# scc_doi[c(which(scc_doi$Patient.IDID.du.patient %in% c(8,21,127))), c("Patient.IDID.du.patient",
#                                                                       "n_patho")] %>%  view()
# table(baseline_scc_doi$tab_n_patho_stage, baseline_scc_doi$n_patho_stage,exclude = NULL)

#eliminating those patients who do not have pathologic_N_stage reported (53 patients)
CP_sen_spec<-baseline_scc_doi%>%
  select(Patient.IDID.du.patient, tab_n_clinc_stage, tab_n_patho_stage, n_patho_stage)%>%
  filter((n_patho_stage != "Not Applicable"))

class(CP_sen_spec$tab_n_clinc_stage)
class(CP_sen_spec$tab_n_patho_stage)
class(CP_sen_spec$Patient.IDID.du.patient)
CP_sen_spec$tab_n_clinc_stage<-as.factor(CP_sen_spec$tab_n_clinc_stage)
CP_sen_spec$tab_n_patho_stage<-as.factor(CP_sen_spec$tab_n_patho_stage)
levels(CP_sen_spec$tab_n_clinc_stage)
levels(CP_sen_spec$tab_n_patho_stage)


confusionMatrix(CP_sen_spec$tab_n_clinc_stage, #the predicted classes
                CP_sen_spec$tab_n_patho_stage,
                dnn = c("Clinic_N_stage","patho_N_Stage"),
                mode = "sens_spec") 

##Radiographic Node - Prediction
##Pathologic Node - ref (gold standard) 

table(baseline_scc_doi$tab_n_radio_stage, baseline_scc_doi$n_radio_stage,exclude = NULL)
table(baseline_scc_doi$tab_n_patho_stage, baseline_scc_doi$n_patho_stage,exclude = NULL)


#eliminating those patients who do not have pathologic_N_stage reported (53 patients)
RP_sen_spec<-baseline_scc_doi%>%
  select(Patient.IDID.du.patient, tab_n_radio_stage, tab_n_patho_stage, n_patho_stage)%>%
  filter((n_patho_stage != "Not Applicable"))

class(RP_sen_spec$tab_n_radio_stage)
class(RP_sen_spec$tab_n_patho_stage)
class(RP_sen_spec$Patient.IDID.du.patient)
RP_sen_spec$tab_n_radio_stage<-as.factor(RP_sen_spec$tab_n_radio_stage)
RP_sen_spec$tab_n_patho_stage<-as.factor(RP_sen_spec$tab_n_patho_stage)
levels(RP_sen_spec$tab_n_radio_stage)
levels(RP_sen_spec$tab_n_patho_stage)


confusionMatrix(RP_sen_spec$tab_n_radio_stage, #the predicted classes
                RP_sen_spec$tab_n_patho_stage,
                dnn = c("radio_N_stage","patho_N_Stage"),
                mode = "sens_spec") 


##Clinical+radiographic Node - Prediction
##Pathologic Node - ref (gold standard) 

table1(~ tab_n_clinc_radio_stage | n_clinc_stage+n_radio_stage, data = baseline_scc_doi)
table(baseline_scc_doi$tab_n_patho_stage, baseline_scc_doi$n_patho_stage,exclude = NULL)

#eliminating those patients who do not have Radiographic _N_stage reported (23 patients)
CRP_sen_spec<-baseline_scc_doi%>%
  select(Patient.IDID.du.patient, tab_n_clinc_radio_stage, tab_n_patho_stage, n_patho_stage)%>%
  filter(n_patho_stage != "Not Applicable")

class(CRP_sen_spec$tab_n_clinc_radio_stage)
class(CRP_sen_spec$tab_n_patho_stage)
class(CRP_sen_spec$Patient.IDID.du.patient)
CRP_sen_spec$tab_n_clinc_radio_stage<-as.factor(CRP_sen_spec$tab_n_clinc_radio_stage)
CRP_sen_spec$tab_n_patho_stage<-factor(CRP_sen_spec$tab_n_patho_stage, 
                                          levels= c("All not N0","N0"),
                                          labels=c("N0+","N0"))
table(CRP_sen_spec$tab_n_patho_stage,exclude = NULL)
levels(CRP_sen_spec$tab_n_clinc_radio_stage)
levels(CRP_sen_spec$tab_n_patho_stage)


confusionMatrix(CRP_sen_spec$tab_n_clinc_radio_stage, #the predicted classes
                CRP_sen_spec$tab_n_patho_stage,
                dnn = c("Clinic__Radio_N_stage","Radio_N_Stage"),
                mode = "sens_spec") 

#Pathologic N stage - prediction
#Perinural invasion (no lymphovascular invasion) - gold standard

table(baseline_scc_doi$peri_neu_inva, baseline_scc_doi$vas_lymph_inv, exclude = NULL) #46 patients have PNI and No lymphovascular invasion
table(baseline_scc_doi$tab_n_patho_stage, baseline_scc_doi$n_patho_stage,exclude = NULL)
table(baseline_scc_doi$tab_n_patho_stage,baseline_scc_doi$peri_neu_inva, exclude = NULL)
table(baseline_scc_doi$tab_n_patho_stage, baseline_scc_doi$vas_lymph_inv, exclude = NULL)

PNI_sen_spec<-baseline_scc_doi%>%
  select(Patient.IDID.du.patient, peri_neu_inva, vas_lymph_inv, tab_n_patho_stage, n_patho_stage)%>%
  filter(n_patho_stage != "Not Applicable") #we will lose the 53 pathologic stages that are not applicable

table(PNI_sen_spec$peri_neu_inva, PNI_sen_spec$vas_lymph_inv, exclude = NULL) # from the 166 patients whose pathologic N stage
PNI_sen_spec<-PNI_sen_spec%>%
  filter(vas_lymph_inv == "No" & !is.na(peri_neu_inva)) #keeping those who do not have lymphovascular invasion and perinural invasion is reported

table(PNI_sen_spec$peri_neu_inva, PNI_sen_spec$vas_lymph_inv, exclude = NULL)

class(PNI_sen_spec$tab_n_patho_stage)
class(PNI_sen_spec$peri_neu_inva)
PNI_sen_spec$peri_neu_inva<- factor(PNI_sen_spec$peri_neu_inva, levels = c("Yes","No"))
levels(PNI_sen_spec$peri_neu_inva)
table(PNI_sen_spec$peri_neu_inva, exclude = NULL)
table(PNI_sen_spec$tab_n_patho_stage)
PNI_sen_spec$tab_n_patho_stage<-factor(PNI_sen_spec$tab_n_patho_stage, 
                                       levels= c("N0","All not N0"),
                                       labels=c("Yes","No"))
table(PNI_sen_spec$tab_n_patho_stage)

confusionMatrix(PNI_sen_spec$tab_n_patho_stage,
                PNI_sen_spec$peri_neu_inva,
                dnn = c("Patho_N_stage","PNI(no LVI)"),
                mode = "sens_spec")

#Pathologic N stage - prediction
#lymphovascular invasion (no Perinural invasion) - gold standard

table(baseline_scc_doi$peri_neu_inva, baseline_scc_doi$vas_lymph_inv, exclude = NULL) #46 patients have PNI and No lymphovascular invasion
table(baseline_scc_doi$tab_n_patho_stage, baseline_scc_doi$n_patho_stage,exclude = NULL)
table(baseline_scc_doi$tab_n_patho_stage,baseline_scc_doi$peri_neu_inva, exclude = NULL)
table(baseline_scc_doi$tab_n_patho_stage, baseline_scc_doi$vas_lymph_inv, exclude = NULL)

LVI_sen_spec<-baseline_scc_doi%>%
  select(Patient.IDID.du.patient, peri_neu_inva, vas_lymph_inv, tab_n_patho_stage, n_patho_stage)%>%
  filter(n_patho_stage != "Not Applicable") #we will lose the 53 pathologic stages that are not applicable

table( LVI_sen_spec$vas_lymph_inv,LVI_sen_spec$peri_neu_inva, exclude = NULL) # from the 166 patients whose pathologic N stage
LVI_sen_spec<-LVI_sen_spec%>%
  filter(peri_neu_inva == "No" & !is.na(vas_lymph_inv)) #keeping those who do not have perinural invasion and lymphovascular invasion is reported

table(LVI_sen_spec$vas_lymph_inv, LVI_sen_spec$peri_neu_inva, exclude = NULL)

class(LVI_sen_spec$tab_n_patho_stage)
class(LVI_sen_spec$vas_lymph_inv)
levels(factor(LVI_sen_spec$vas_lymph_inv))
LVI_sen_spec$vas_lymph_inv<- factor(LVI_sen_spec$vas_lymph_inv, levels = c("Yes","No"))
levels(LVI_sen_spec$vas_lymph_inv)
table(LVI_sen_spec$vas_lymph_inv, exclude = NULL)
table(LVI_sen_spec$tab_n_patho_stage)
LVI_sen_spec$tab_n_patho_stage<-factor(LVI_sen_spec$tab_n_patho_stage, 
                                       levels= c("N0","All not N0"),
                                       labels=c("Yes","No"))
table(LVI_sen_spec$tab_n_patho_stage)

confusionMatrix(LVI_sen_spec$tab_n_patho_stage,
                LVI_sen_spec$vas_lymph_inv,
                dnn = c("Patho_N_stage","LVI(no PNI)"),
                mode = "sens_spec")

#Pathologic N stage - prediction
#lymphovascular invasion and Perinural invasion - gold standard
table(baseline_scc_doi$peri_neu_inva, baseline_scc_doi$vas_lymph_inv, exclude = NULL) #40 patients have PNI and lymphovascular invasion. 105 do not have both
table(baseline_scc_doi$tab_n_patho_stage, baseline_scc_doi$n_patho_stage,exclude = NULL)

PNILVI_sen_spec<-baseline_scc_doi%>%
  select(Patient.IDID.du.patient, peri_neu_inva, vas_lymph_inv, tab_n_patho_stage, n_patho_stage)%>%
  filter(n_patho_stage != "Not Applicable") #we will lose the 53 pathologic stages that are not applicable

table( PNILVI_sen_spec$vas_lymph_inv,PNILVI_sen_spec$peri_neu_inva, exclude = NULL)##39 patients have PNI and lymphovascular invasion. 66 do not have both

PNILVI_sen_spec<-PNILVI_sen_spec%>%
  mutate(tab_PNILVI = case_when((peri_neu_inva == "Yes" & vas_lymph_inv == "Yes") ~ "Yes",
                                (peri_neu_inva == "No" & vas_lymph_inv == "No") ~ "No"))%>%
  filter(!is.na(tab_PNILVI))

class(PNILVI_sen_spec$tab_n_patho_stage)
class(PNILVI_sen_spec$tab_PNILVI)
levels(factor(PNILVI_sen_spec$tab_PNILVI))
PNILVI_sen_spec$tab_PNILVI<- factor(PNILVI_sen_spec$tab_PNILVI, levels = c("Yes","No"))
levels(PNILVI_sen_spec$tab_PNILVI)
table(PNILVI_sen_spec$tab_PNILVI, exclude = NULL)
table(LVI_sen_spec$tab_n_patho_stage)
PNILVI_sen_spec$tab_n_patho_stage<-factor(PNILVI_sen_spec$tab_n_patho_stage, 
                                       levels= c("N0","All not N0"),
                                       labels=c("Yes","No"))
table(PNILVI_sen_spec$tab_n_patho_stage)

confusionMatrix(PNILVI_sen_spec$tab_n_patho_stage,
                PNILVI_sen_spec$tab_PNILVI,
                dnn = c("Patho_N_stage","PNI+LVI"),
                mode = "sens_spec")
