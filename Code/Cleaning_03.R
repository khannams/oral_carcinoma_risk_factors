#continued cleaning for the recurrence data
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

#loading both datasets
scc_doi<-read_csv(here("Data","clean_scc_doi_30-11-2022.csv"))
baseline_scc_doi<- read_csv(here("Data","baseline_scc_doi_30-11-2022.csv"))


#Patients disease free till last follow-up
baseline_scc_doi<-baseline_scc_doi%>%
  rename(expired = ...289)

baseline_scc_doi%>%
  select(Patient.IDID.du.patient, recurrence,expired, metastasis)%>% view()


baseline_scc_doi%>%
  select(Patient.IDID.du.patient, surgery_type, doi_cat, doi_125, recurrence, metastasis, expired)%>% view()

baseline_scc_doi$disease_free<-NA
baseline_scc_doi[c(which(is.na(baseline_scc_doi$recurrence) & is.na(baseline_scc_doi$metastasis)&is.na(baseline_scc_doi$expired))),
                 "disease_free"]<-"disease free"


baseline_scc_doi%>%
 select(Patient.IDID.du.patient, surgery_type, doi_cat, doi_125,disease_free, recurrence,`Date of recurrence`,metastasis, expired,reason_death.y,date_expired.y, enddate )%>% view()
# #write.csv(xyz,here("Data","survivalclean.csv"))


baseline_scc_doi[c(which(baseline_scc_doi$Patient.IDID.du.patient == 393)), c("Date of surgery", "enddate")]<-ymd("2016-09-07")


#Going to go patient by patient to see their recurrence and other info
#deleting rows more than 5 year follow-up
baseline_scc_doi<-baseline_scc_doi%>% #using end-date(mix of date of surgery and date of biopsy) to generate 5 year follow-up period
  mutate(fiveyr_flw = enddate %m+% years(5))#%>% select(Patient.IDID.du.patient, Event.Name,Follow_up_Date,fiveyr_flw)%>% view()

scc_doi$fiveyr<-vector("logical",2387)
for (i in 1:nrow(scc_doi)) {
  for (j in 1:nrow(baseline_scc_doi)) {
    if (scc_doi$Patient.IDID.du.patient[i] == baseline_scc_doi$Patient.IDID.du.patient[j]){
      scc_doi$fiveyr[i]<-scc_doi$Follow_up_Date[i] <= baseline_scc_doi$fiveyr_flw [j]
    }
  }
}

table(scc_doi$fiveyr, exclude = NULL)

#eliminating the rows where follow-up is greater than 5 years
scc_doi_fiveyr<-scc_doi%>%
  filter(fiveyr%in% c(TRUE,NA))

scc_doi_fiveyr%>%
  select(Patient.IDID.du.patient, Event.Name, age,doi_125, Follow_up_Date, 
         Current.Follow.Up.StatusStatut.a.la.visite.de.suivi.actuelle,...148,
         X........If.Alive..Disease.status..........Si.Vivant..Statut.de.la.Maladie.,
         surgery_type, doi_cat, doi_125, reason_death )%>%
  group_by(Patient.IDID.du.patient)%>% view()

#patient ID 5 changed to disease free as that is what is happening in 5 years. Before this it was unknown
baseline_scc_doi[which(baseline_scc_doi$Patient.IDID.du.patient %in% c(5,24)), c("expired","reason_death.y")]<-NA
baseline_scc_doi[which(baseline_scc_doi$Patient.IDID.du.patient %in% c(5,24)), "disease_free"]<-"disease free"



#getting the max follow-up for all those disease free
xyz<-scc_doi_fiveyr%>%
  group_by(Patient.IDID.du.patient)%>%
  filter(Current.Follow.Up.StatusStatut.a.la.visite.de.suivi.actuelle == "Active")%>%
  summarise(max_follow_diseasefree=max(Follow_up_Date, na.rm=T))%>% view()

baseline_scc_doi<-merge(baseline_scc_doi, xyz, by= "Patient.IDID.du.patient", all.x = T)

#to know the exact length of follow-up for each participant
## For does who has recurrence - date of recurrence
## those who were disease free - last date of active follow-up 
## those who expired - date of expirey
## for those who show recurrence and expiry with 5 years the event that happens sooner is considered.

baseline_scc_doi$Event_final<-NA
baseline_scc_doi$final_follow_up<-as.Date(NA)

for (l in 1:nrow(baseline_scc_doi)){
   if ( baseline_scc_doi$recurrence[l] %in% "recurrence"){
     baseline_scc_doi$Event_final[l]<- "recurrence"
     baseline_scc_doi$final_follow_up[l]<-baseline_scc_doi$`Date of recurrence`[l]
   } else {
     baseline_scc_doi$Event_final[l] <- baseline_scc_doi$disease_free[l]
     baseline_scc_doi$final_follow_up[l]<- baseline_scc_doi$max_follow_diseasefree[l]
   }
  if (baseline_scc_doi$Event_final[l] %in% NA){
    baseline_scc_doi$Event_final[l]<-baseline_scc_doi$expired[l]
    baseline_scc_doi$final_follow_up[l]<-baseline_scc_doi$date_expired.y[l]
  }
  if (baseline_scc_doi$final_follow_up[l] %in% NA){
    baseline_scc_doi$final_follow_up[l]<- baseline_scc_doi$max_follow_diseasefree[l]
  }
}
  
baseline_scc_doi%>%
  select(Patient.IDID.du.patient, surgery_type, doi_cat, doi_125,disease_free, recurrence,`Date of recurrence`,metastasis, 
         expired,date_expired.y, enddate, final_follow_up, Event_final )%>% view()

#these patients do not have any follow- 
baseline_scc_doi%>%
  select(Patient.IDID.du.patient, surgery_type, doi_cat, doi_125,disease_free, recurrence,`Date of recurrence`,metastasis, 
         expired,date_expired.y, enddate, final_follow_up, Event_final )%>% filter(is.na(final_follow_up))

baseline_scc_doi[which(baseline_scc_doi$Patient.IDID.du.patient == 294), "final_follow_up"]<- ymd("2016-02-15")
baseline_scc_doi[which(baseline_scc_doi$Patient.IDID.du.patient == 244), "final_follow_up"]<- ymd("2019-01-31")
baseline_scc_doi[which(baseline_scc_doi$Patient.IDID.du.patient == 325), "final_follow_up"]<- ymd("2019-10-17")
baseline_scc_doi[which(baseline_scc_doi$Patient.IDID.du.patient == 339), "final_follow_up"]<- ymd("2020-10-29")
baseline_scc_doi[which(baseline_scc_doi$Patient.IDID.du.patient == 351), "final_follow_up"]<- ymd("2021-06-17")
baseline_scc_doi[which(baseline_scc_doi$Patient.IDID.du.patient == 366), "final_follow_up"]<- ymd("2021-08-19")
baseline_scc_doi[which(baseline_scc_doi$Patient.IDID.du.patient == 374), "final_follow_up"]<- ymd("2021-12-02")


scc_doi_fiveyr%>%
  select(Patient.IDID.du.patient, Event.Name, age,doi_125, Follow_up_Date,
         Current.Follow.Up.StatusStatut.a.la.visite.de.suivi.actuelle,...148,
         X........If.Alive..Disease.status..........Si.Vivant..Statut.de.la.Maladie.,
         reason_death, enddate)%>%
  filter(Patient.IDID.du.patient ==366)%>%view()


#Lets check if any participant followed-up for less than 2years
x<-baseline_scc_doi%>%
  group_by(Patient.IDID.du.patient)%>%
  summarise(diffdat = as.numeric((final_follow_up-enddate)/365.25))%>%
  filter(diffdat<= 2) %>% view()

baseline_scc_doi%>%
  select(Patient.IDID.du.patient,Event_final, enddate, final_follow_up)%>%
  filter(Patient.IDID.du.patient %in% c (x$Patient.IDID.du.patient))%>%#46 participants with follow-up time of less than 2 years
  filter(Event_final == "disease free")

baseline_scc_doi<-baseline_scc_doi%>%
  mutate(tab_n_clinc_radio_stage = case_when((tab_n_clinc_stage == "N0" & tab_n_radio_stage == "N0")~ "N0",
                                             (tab_n_clinc_stage != "N0" | tab_n_radio_stage != "N0") ~ "N0+"))

baseline_scc_doi$doi_125_round<- as.factor(round(baseline_scc_doi$doi_125))

#cleaning for detail of events
#Dr. hans wants details of recurrence and cause of death in the table
table(baseline_scc_doi$Event_final, exclude = NULL)
baseline_scc_doi$Event_final_det<-NA
for (i in 1:nrow(baseline_scc_doi)) {
  if (baseline_scc_doi$Event_final[i] %in% "Expired"){
    baseline_scc_doi$Event_final_det[i] <- baseline_scc_doi$reason_death.y[i]
  }else if (baseline_scc_doi$Event_final[i] %in% "recurrence"){
    baseline_scc_doi$Event_final_det[i] <- baseline_scc_doi$metastasis[i]
  }else {
    baseline_scc_doi$Event_final_det[i]<-"disease free"
  }
}
table(baseline_scc_doi$Event_final_det, exclude = NULL)


#one patient follow-up time mixed up
scc_doi%>%
  select(Patient.IDID.du.patient, Date.of.Surgery..Date.la.chirurgie...83,
         Follow_up_Date, X........If.yes..Date.of.biopsy........Si.Oui..Date.de.la.biopsie...36,
         enddate)%>%
  filter(Patient.IDID.du.patient ==406)


#changing patient 8 from true_doi to tumor thickness
scc_doi_fiveyr[c(which(scc_doi_fiveyr$Patient.IDID.du.patient==8 & 
                         scc_doi_fiveyr$Event.Name== "Initial Visit")), "doi_cat"]<-"tumor_thickness"

baseline_scc_doi[c(which(baseline_scc_doi$Patient.IDID.du.patient==8)), "doi_cat"]<-"tumor_thickness"

#Cleaning the tab_TNM_stage chart
baseline_scc_doi[c(which(baseline_scc_doi$n_patho_stage == "N0" & 
                           baseline_scc_doi$tab_n_patho_stage == "All not N0")), "Patient.IDID.du.patient"] #Patient ID 100 and 195

baseline_scc_doi[c(which(baseline_scc_doi$n_patho_stage == "N0" & 
                           baseline_scc_doi$tab_n_patho_stage == "All not N0")), "tab_n_patho_stage"]<-"N0"
table(baseline_scc_doi$tab_n_patho_stage, baseline_scc_doi$n_patho_stage,exclude = NULL)

baseline_scc_doi[c(which(baseline_scc_doi$n_patho_stage == "Not Applicable" & 
                           baseline_scc_doi$tab_n_patho_stage == "N0")), "Patient.IDID.du.patient"]
scc_doi[c(which(scc_doi$Patient.IDID.du.patient %in% c(8,21,127))), c("Patient.IDID.du.patient",
                                                                      "n_patho_stage")] %>%  view()

baseline_scc_doi[c(which(baseline_scc_doi$n_patho_stage == "Not Applicable" & 
                           baseline_scc_doi$tab_n_patho_stage == "N0")), "tab_n_patho_stage"]<-NA

table(baseline_scc_doi$tab_n_patho_stage, baseline_scc_doi$n_patho_stage,exclude = NULL)

baseline_scc_doi%>%
  #select(Patient.IDID.du.patient, surgery_type,doi_cat,doi_125_round, n_patho_stage, n_clinc_stage)%>%
  filter(doi_cat== "true_doi" & !is.na(doi_125_round) & n_clinc_stage== "N0")%>% 
  count(n_patho_stage)

baseline_scc_doi[which(baseline_scc_doi$Patient.IDID.du.patient==406), c("enddate","final_follow_up")]
baseline_scc_doi[which(baseline_scc_doi$Patient.IDID.du.patient==406), c("enddate",
                                                                         "Date of surgery",
                                                                         "X........If.yes..Date.of.biopsy........Si.Oui..Date.de.la.biopsie...36")]
scc_doi_fiveyr[which(scc_doi_fiveyr$Patient.IDID.du.patient==406), c("enddate",
                                                                         "Date.of.Surgery..Date.la.chirurgie...83",
                                                                         "X........If.yes..Date.of.biopsy........Si.Oui..Date.de.la.biopsie...36")]
#as date of surgey missing assigning date of biopsy to patient 406
baseline_scc_doi[which(baseline_scc_doi$Patient.IDID.du.patient==406), "enddate"]<-"2017-05-19"
scc_doi_fiveyr[which(scc_doi_fiveyr$Patient.IDID.du.patient==406), "enddate"]<- "2017-05-19"
                                                                     
#saving the data
 # write_csv(scc_doi_fiveyr, here("Data","scc_fiveyr_clean_05-12-2022.csv"))
 # write_csv(baseline_scc_doi, here("Data","clean_baseline_05-12-2022.csv"))

baseline_scc_doi[c(which(baseline_scc_doi$doi_125_round == 6)), "Patient.IDID.du.patient"]

print((baseline_scc_doi%>%
  select(Patient.IDID.du.patient, doi_125_round, doi_cat, surgery_type,Event_final_det, n_patho_stage)%>%
  filter(Patient.IDID.du.patient %in% 
           c(baseline_scc_doi[c(which(baseline_scc_doi$doi_125_round == 6)), "Patient.IDID.du.patient"]))), 
  quote = T)

print((baseline_scc_doi%>%
         select(Patient.IDID.du.patient, doi_42_round, doi_cat, surgery_type,Event_final_det, n_patho_stage)%>%
         filter(Patient.IDID.du.patient %in% 
                  c(baseline_scc_doi[c(which(baseline_scc_doi$doi_42_round== 6)), "Patient.IDID.du.patient"]))), 
      quote = T)

scc_doi2<-read_xlsx(here("Data","Worksheet (Finished editing).xlsx"))
scc_doi2%>%
  select(`Patient IDID du patient`,
         `---  If yes, Please Specify Depth of Invasion (tumor thickness, Unit: mm)--- Si oui, merci de preciser la profondeur de l'invasion (epaisseur de la tumeur, Unite: mm)...125`,
         `Main Surgical Procedure  Methode principale utilisee pour la chirurgie...83`,`Pathological N stage--- Pathologique - Stage N`)%>%
  filter(`---  If yes, Please Specify Depth of Invasion (tumor thickness, Unit: mm)--- Si oui, merci de preciser la profondeur de l'invasion (epaisseur de la tumeur, Unite: mm)...125` ==6)%>% view()

scc_doi2%>%
  select(`Patient IDID du patient`,
         `Main Surgical Procedure  Methode principale utilisee pour la chirurgie...83`, `--- --- If yes, Please Specify Depth of Invasion (tumor thickness, Unit: mm)--- --- Si oui, merci de preciser la profondeur de l'invasion (epaisseur de la tumeur, Unite: mm)...42`)%>%
  filter(`--- --- If yes, Please Specify Depth of Invasion (tumor thickness, Unit: mm)--- --- Si oui, merci de preciser la profondeur de l'invasion (epaisseur de la tumeur, Unite: mm)...42` ==6)


