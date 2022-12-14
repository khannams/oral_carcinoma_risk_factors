#continued cleaning after Dr. Hans comments -25th Nov 2022
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

scc_doi<- read.csv(here("Data","clean_scc_doi_04-11-2022.csv"))

##One-Note comments:-----------------
#Number of cigarettes
table(scc_doi$X........Number.of.Cigarettes.per.Day........Nombre.de.Cigarettes.par.Jour, exclude = NULL)
scc_doi[c(which(scc_doi$Event.Name == "Initial Visit" & scc_doi$Patient.IDID.du.patient %in% c(5,181,313,294))), 
        c("Patient.IDID.du.patient",
          "X........Number.of.Cigarettes.per.Day........Nombre.de.Cigarettes.par.Jour",
          "tobacco_hist","tobacco_stat_diag")]

scc_doi[c(which(scc_doi$Event.Name == "Initial Visit" & scc_doi$Patient.IDID.du.patient %in% c(5,181,313,294))), 
        c("tobacco_hist","tobacco_stat_diag")] <- "Non-Smoker"

table(scc_doi$tobacco_hist, scc_doi$X........Number.of.Cigarettes.per.Day........Nombre.de.Cigarettes.par.Jour, exclude = NULL)

scc_doi[c(which(scc_doi$Event.Name == "Initial Visit" & scc_doi$Patient.IDID.du.patient %in% c(5,181,313,294))), 
        "X........Number.of.Cigarettes.per.Day........Nombre.de.Cigarettes.par.Jour"]<-0

table1(~tobacco_hist | tobacco_stat_diag, data = (scc_doi%>% filter(Event.Name== "Initial Visit")))

scc_doi[c(which(scc_doi$Event.Name == "Initial Visit" & scc_doi$Patient.IDID.du.patient %in% c(172,438,429,21))), 
        c("Patient.IDID.du.patient",
          "X........Number.of.Cigarettes.per.Day........Nombre.de.Cigarettes.par.Jour",
          "X....Number.of.Years.Smoked....Nombre.d.annees.de.tabagisme")]%>% view()

scc_doi[c(which(scc_doi$Event.Name == "Initial Visit" & scc_doi$Patient.IDID.du.patient %in% c(172))), 
        c("X........Number.of.Cigarettes.per.Day........Nombre.de.Cigarettes.par.Jour",
          "X....Number.of.Years.Smoked....Nombre.d.annees.de.tabagisme")]<-c(10,50) #changing to 10 cigarettes for 50 years

scc_doi[c(which(scc_doi$Event.Name == "Initial Visit" & scc_doi$Patient.IDID.du.patient %in% c(429))), 
        "X........Number.of.Cigarettes.per.Day........Nombre.de.Cigarettes.par.Jour"]<-30

scc_doi[c(which(scc_doi$Event.Name == "Initial Visit" & scc_doi$Patient.IDID.du.patient %in% c(21))), 
        "X........Number.of.Cigarettes.per.Day........Nombre.de.Cigarettes.par.Jour"]<-5

#Biopsy dates
scc_doi[c(which(scc_doi$Event.Name == "Initial Visit" & scc_doi$Patient.IDID.du.patient %in% c(56,77,313,432,438))), 
        c("X....If.Biopsy..Histologic.Diagnosis.....Si.Biopsie..Diagnostic.Histologique.....37",
          "X........If.yes..Date.of.biopsy........Si.Oui..Date.de.la.biopsie...36")]

table(scc_doi$X........If.yes..Date.of.biopsy........Si.Oui..Date.de.la.biopsie...36, exclude = NULL)

scc_doi[c(which(scc_doi$Event.Name == "Initial Visit" & scc_doi$Patient.IDID.du.patient %in% c(56,77,313,432,438))), 
        c("X....If.Biopsy..Histologic.Diagnosis.....Si.Biopsie..Diagnostic.Histologique.....37",
          "X........If.yes..Date.of.biopsy........Si.Oui..Date.de.la.biopsie...36",
          "X....If.Biopsy..do.you.know.the.biopsy.date.....Si.Biopsie..connaissez.vous.la.date.de.biopsie.....35")]

#Depth of invasion
scc_doi[c(which(scc_doi$Event.Name == "Initial Visit" & scc_doi$Patient.IDID.du.patient %in% c(385))), 
        c("know_doi_41", "doi_42","doi_42_round")]

scc_doi[c(which(scc_doi$Event.Name == "Initial Visit" & scc_doi$Patient.IDID.du.patient %in% c(385))), 
        "know_doi_41"]<-"Yes"

scc_doi[c(which(scc_doi$Event.Name == "Initial Visit" & scc_doi$Patient.IDID.du.patient %in% c(385))), 
        c("doi_42","doi_42_round")]<-8

#P16 status
scc_doi[c(which(scc_doi$Event.Name == "Initial Visit" & scc_doi$Patient.IDID.du.patient %in% c(385))), 
        "P16.statusStatut.P16...43"]<-"Not Reported"


#Pathologic TNM staging
path_TNM_surgerytype<-scc_doi%>%
  select(Patient.IDID.du.patient, Event.Name, t_patho_stage, n_patho_stage, m_patho_stage,
         Was.Surgery.Completed...Une.chirurgie.a.t.elle.ete.pratiquee.,
         Date.of.Surgery..Date.la.chirurgie...82,surgery_type, final_patho_differen)%>% 
  filter(Event.Name== "Initial Visit" & Was.Surgery.Completed...Une.chirurgie.a.t.elle.ete.pratiquee. == "Yes" )%>%
  arrange(final_patho_differen)%>%view()

#loading the clean data provided by Dr. Hans
hans_path_TNM<-read.csv(here("Data","pathTNM_surgeyTypev2.csv"))

hans_path_TNM<-hans_path_TNM[-which(hans_path_TNM$Patient.IDID.du.patient%in%c(18,38,50)),-10 ]

names(hans_path_TNM)
names(path_TNM_surgerytype)

hans_path_TNM<-hans_path_TNM%>%
  rename(final_patho_differen=Degree.of.DifferentiationDegree.de.differenciation...122)

diffdf(base= hans_path_TNM,
       compare = path_TNM_surgerytype,
       keys = "Patient.IDID.du.patient")
#many differences so making the change will change the baseline data when other cleaning is complete

#number of positive nodes
scc_doi[which(scc_doi$Patient.IDID.du.patient==429&
              scc_doi$Event.Name== "Initial Visit"), c("Num_positive_nodes",
                                                       "Num_positive_nodes_fac")]<-0


#Current follow Up status
scc_doi[c(which(scc_doi$Patient.IDID.du.patient%in%c (5,11))), c("Patient.IDID.du.patient",
                                                                 "Event.Name",
                                                                 "Follow_up_Date",
                                                                 "Current.Follow.Up.StatusStatut.a.la.visite.de.suivi.actuelle")]%>% view()

scc_doi[which(scc_doi$Current.Follow.Up.StatusStatut.a.la.visite.de.suivi.actuelle == "Palliative care"), 
        "Current.Follow.Up.StatusStatut.a.la.visite.de.suivi.actuelle"]<-"Active"

scc_doi[which(scc_doi$Current.Follow.Up.StatusStatut.a.la.visite.de.suivi.actuelle == "NOTE: new liver cancer"), 
        "Current.Follow.Up.StatusStatut.a.la.visite.de.suivi.actuelle"]<-"Active"

#If expired reason for death
table(scc_doi$Current.Follow.Up.StatusStatut.a.la.visite.de.suivi.actuelle, exclude = NULL)
scc_doi%>%
  select(Patient.IDID.du.patient, Event.Name,145:148,164:167)%>%
  filter(reason_death != "Not applicable")%>% view()

table(scc_doi$...147, scc_doi$reason_death, exclude = NULL)#this needs some cleaning
scc_doi[c(which(is.na(scc_doi$reason_death))), "reason_death"]<-"Unknown"
scc_doi[c(which(is.na(scc_doi$...147))), "reason_death"]<-NA

scc_doi[c(which(scc_doi$Patient.IDID.du.patient == 31 & 
                  scc_doi$reason_death == "Unknown")), "reason_death"]<-"Head and Neck Cancer Related"

scc_doi[c(which(scc_doi$Patient.IDID.du.patient %in% c(347,321,312,307))), c("Patient.IDID.du.patient","reason_death","date_expired")]%>% view()
scc_doi[c(which(scc_doi$Patient.IDID.du.patient %in% c(347,321,312,307) &
                  scc_doi$reason_death == "Unknown")), "reason_death"]<-"Head and Neck Cancer Related"
scc_doi[c(which(scc_doi$Patient.IDID.du.patient %in% c(347) &
                  scc_doi$reason_death == "Head and Neck Cancer Related")), "date_expired"]<-"2017-08-15"
scc_doi[c(which(scc_doi$Patient.IDID.du.patient %in% c(321) &
                  scc_doi$reason_death == "Head and Neck Cancer Related")), "date_expired"]<-"2017-10-31"
scc_doi[c(which(scc_doi$Patient.IDID.du.patient %in% c(312) &
                  scc_doi$reason_death == "Head and Neck Cancer Related")), "date_expired"]<-"2017-12-08"
scc_doi[c(which(scc_doi$Patient.IDID.du.patient %in% c(307) &
                  scc_doi$reason_death == "Head and Neck Cancer Related")), "date_expired"]<-"2020-12-18"



#removing patient 43
scc_doi[which(!is.na(scc_doi$date_diag_primHN)),"Patient.IDID.du.patient"] #checking if 43 has date of first primary HN tumour diagnosis date

scc_doi_minus43<-scc_doi[-c(which(scc_doi$Patient.IDID.du.patient%in% 43)), ] #18 rows eliminated of patient 43

#Dr.Hans has requested to remove patient ID 18,38,50 as they did not get surgery done
scc_doi_minus43<-scc_doi_minus43[-c(which(scc_doi_minus43$Patient.IDID.du.patient %in% c(18,38,50))), ]

#list of patients who expired
table(scc_doi_minus43$date_expired,scc_doi_minus43$...147, exclude = NULL)
expired<-scc_doi_minus43%>%
  select(Patient.IDID.du.patient, ...147, date_expired, reason_death)%>%
  filter(`...147` %in% c("Expired","Unknown"))%>%view()

#who is unknown
scc_doi_minus43%>%
  select(Patient.IDID.du.patient, ...147)%>%
  filter(`...147` == "Unknown") #5 is unknown

baseline_scc_doi<- scc_doi_minus43%>% filter(Event.Name== "Initial Visit")

baseline_scc_doi<-merge(baseline_scc_doi, expired, by="Patient.IDID.du.patient", all.x=T)

table(baseline_scc_doi$...147.y, exclude = NULL)

#dropping the column ...147.x from baseline
names(baseline_scc_doi)
baseline_scc_doi<-baseline_scc_doi[ ,-c(148,165,167)]

 
# baseline_scc_doi<-baseline_scc_doi%>% #298 variables
#   select(-c(t_patho_stage,n_patho_stage,m_patho_stage,Was.Surgery.Completed...Une.chirurgie.a.t.elle.ete.pratiquee.,
#             Date.of.Surgery..Date.la.chirurgie...82,surgery_type,surgery_type, final_patho_differen))

##Now cleaning for Path_TNM staging CSV-------------------
#removing patient ID 43
hans_path_TNM<-hans_path_TNM[-c(which(hans_path_TNM$Patient.IDID.du.patient %in% 43)), ]
baseline_scc_doi_merge1<-merge(baseline_scc_doi, hans_path_TNM, by = "Patient.IDID.du.patient", all.x = T)

baseline_scc_doi_merge1%>%
  select(Patient.IDID.du.patient, t_patho_stage.y, t_patho_stage.x)
baseline_scc_doi_merge1[c(which(baseline_scc_doi_merge1$t_patho_stage.y == "T4")), "t_patho_stage.y"]<-"T4a"

baseline_scc_doi_merge1%>%
  select(Patient.IDID.du.patient, n_patho_stage.y, n_patho_stage.x)

baseline_scc_doi_merge1[c(which(baseline_scc_doi_merge1$n_patho_stage.y == "N2")), "n_patho_stage.y"]<-"N0"

baseline_scc_doi_merge1[c(which(baseline_scc_doi_merge1$Pathological.stage.overall....Stage.Pathologique.Global == "Stage IVA")), 
                 "Pathological.stage.overall....Stage.Pathologique.Global"]<-"Stage IV A"


#Dropping the extra columns that got added
baseline_scc_doi_merge1<-baseline_scc_doi_merge1%>%
  select(-c(Event.Name.y, t_patho_stage.x,n_patho_stage.x, m_patho_stage.x, Was.Surgery.Completed...Une.chirurgie.a.t.elle.ete.pratiquee..x,
         Date.of.Surgery..Date.la.chirurgie...82.x,surgery_type.x,final_patho_differen.x))%>%
  rename(Event.Name = Event.Name.x,
         t_patho_stage= t_patho_stage.y,
         n_patho_stage= n_patho_stage.y, 
         m_patho_stage= m_patho_stage.y, 
         Was.Surgery.Completed...Une.chirurgie.a.t.elle.ete.pratiquee.= Was.Surgery.Completed...Une.chirurgie.a.t.elle.ete.pratiquee..y,
         Date.of.Surgery..Date.la.chirurgie...82= Date.of.Surgery..Date.la.chirurgie...82.y,
         surgery_type= surgery_type.y,
         final_patho_differen= final_patho_differen.y)

baseline_scc_doi_merge1%>%
  select(Patient.IDID.du.patient, Pathological.stage.overall....Stage.Pathologique.Global)%>%
  filter(Patient.IDID.du.patient == 195) #changing stage to II as requested by Dr. Hans in file Path TNM_surgerytypev2

baseline_scc_doi_merge1[which(baseline_scc_doi_merge1$Patient.IDID.du.patient == 195), 
                        "Pathological.stage.overall....Stage.Pathologique.Global"] <- "Stage II"


##now dealing with recurrence--------------
recurrence_hans<-read_xlsx(here("Data","DOI Project- recurrence Data v4.xlsx"))

recurrence_hans<-recurrence_hans%>%
  rename(Patient.IDID.du.patient = Patient)

sum(is.na(recurrence_hans$Patient.IDID.du.patient))

#removing multiple rows for a patient ID
recurrence_hans<-recurrence_hans[-c(which(is.na(recurrence_hans$Patient.IDID.du.patient))), ]

#adding a new column saying recurrence yes
recurrence_hans$recurrence <-"recurrence"

#merging this with baseline
final_baseline<-merge(baseline_scc_doi_merge1, recurrence_hans, by = "Patient.IDID.du.patient", all.x = T)

#cleaning some variables
final_baseline[c(which(final_baseline$Local == "Yes")), "Local"]<-"Local"
final_baseline[c(which(final_baseline$Regional == "Yes")), "Regional"]<-"Regional"
final_baseline[c(which(final_baseline$`Distant mets` == "Yes")), "Distant mets"]<-"Distant"
final_baseline<-unite(final_baseline,col="metastasis",Local,Regional,`Distant mets`,sep = " and ", remove = F)

table(final_baseline$metastasis, exclude = NULL)
final_baseline$metastasis<-gsub("NA and NA and NA", NA, final_baseline$metastasis)
final_baseline$metastasis<-gsub("Local and NA and NA", "Local only", final_baseline$metastasis)
final_baseline$metastasis<-gsub("NA and Regional and Distant", "Regional and/or Distant", final_baseline$metastasis)
final_baseline$metastasis<-gsub("NA and NA and Distant", "Regional and/or Distant", final_baseline$metastasis)
final_baseline$metastasis<-gsub("NA and Regional and NA", "Regional and/or Distant", final_baseline$metastasis)
final_baseline$metastasis<-gsub("Local and NA and Distant", "Regional and/or Distant", final_baseline$metastasis)
final_baseline$metastasis<-gsub("Local and Regional and Distant", "Regional and/or Distant", final_baseline$metastasis)
final_baseline$metastasis<-gsub("Local and Regional and NA", "Regional and/or Distant", final_baseline$metastasis)
final_baseline$metastasis<-trimws(final_baseline$metastasis)
table(final_baseline$metastasis, exclude = NULL)




# write_csv(scc_doi_minus43, here("Data","clean_scc_doi_30-11-2022.csv"))
# write_csv(final_baseline, here("Data","baseline_scc_doi_30-11-2022.csv"))
# 

