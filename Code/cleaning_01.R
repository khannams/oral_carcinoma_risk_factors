library(here)
library(readxl)

#loading the dataste
scc_doi<-read_xlsx(here("Data","Worksheet (Finished editing).xlsx"))
names(scc_doi)

library(tidyverse)
library(table1)
library(janitor)
library(magrittr)
#install.packages("arsenal")
library(arsenal)
#install.packages("diffdf")
library(diffdf)
library(eeptools)
library(lubridate)


#Going through each variable to understand the data and clean
#removing all the unnecessary dates from from excel after converting to text format
scc_doi[c(which(scc_doi$Follow_up_Date=="00-01-1900")), "Follow_up_Date"]<-NA 

##Patient ID ------------------
scc_doi$`Patient IDID du patient`
summary(scc_doi$`Patient IDID du patient`)
table(factor(scc_doi$`Patient IDID du patient`), exclude = NULL)
levels(factor(scc_doi$`Patient IDID du patient`)) #224 patient? 183 NAs
scc_doi%>%
  filter(is.na(`Patient IDID du patient`)) #These could be because of the empty rows the residents left to demarcate patients
#How many completely empty rows? 
scc_doi[-which(apply(scc_doi,1,function(x) all(is.na(x)))), ]
2442-2433#9 completely empty rows
#removing the completely empty rows from the dataset
scc_doi<-scc_doi[-which(apply(scc_doi,1,function(x) all(is.na(x)))), ] #removing completely, empty rows
table(factor(scc_doi$`Patient IDID du patient`), exclude = NULL) #174 NAs

remove_empty(dat = scc_doi, which = "rows", quiet = F) #no more empty rows to remove.
#still 174 missing patient IDs

which(is.na(scc_doi$`Patient IDID du patient`))
scc_doi%$% unique(`Patient IDID du patient`) #224 patient IDs

#changing 34a to 34
which(scc_doi$`Patient IDID du patient`== "34a")
scc_doi$`Patient IDID du patient`[377]<-"34"

table(factor(scc_doi$`Patient IDID du patient`), exclude = NULL) #174 NAs
levels(factor(scc_doi$`Patient IDID du patient`)) #223 patient?
scc_doi$`Patient IDID du patient`<-as.numeric(scc_doi$`Patient IDID du patient`)

scc_doi%>%
  group_by(`Patient IDID du patient`)%>%
  arrange()%>%
  count()%>% view()


scc_doi%>%
  select(`Patient IDID du patient`, `Event Name`)%>%
  filter(is.na(`Patient IDID du patient`))%>% 
  filter(!is.na(`Event Name`))%>% view() #there are 5 rows which do not have a patient ID but have a follow-up number
which(is.na(scc_doi$`Patient IDID du patient`) & !is.na(scc_doi$`Event Name`))


scc_doi<-scc_doi%>%
  fill(`Patient IDID du patient`,.direction = "down")#lets fill down the patientIDs and see what happens
#now all rows have a patient ID

##Event name/follow-up ----------------
scc_doi$`Event Name`
levels(factor(scc_doi$`Event Name`))
table(factor(scc_doi$`Event Name`), exclude = NULL) #169 NA's

scc_doi[which(scc_doi$`Event Name` == "Follow-up"), 1] #patient ID 24 

scc_doi%>%
  filter(`Patient IDID du patient`==24) ##The problem is found with follow-up 18
scc_doi[which(scc_doi$`Event Name` == "Follow-up"), "Event Name"]<-"Follow-Up #18"
levels(factor(scc_doi$`Event Name`)) #lets change this numeric variable

scc_doi$`Event Name`<-factor(scc_doi$`Event Name`, ordered = T, levels = c ("Initial Visit","Follow-Up #1","Follow-Up #2","Follow-Up #3","Follow-Up #4","Follow-Up #5","Follow-Up #6","Follow-Up #7",
                                                       "Follow-Up #8","Follow-Up #9","Follow-Up #10","Follow-Up #11","Follow-Up #12","Follow-Up #13","Follow-Up #14",
                                                       "Follow-Up #15","Follow-Up #16", "Follow-Up #17", "Follow-Up #18","Follow-Up #19","Follow-Up #20"))

scc_doi%>%
  filter(`Event Name` == "Initial Visit")%>% #223 Initial follow-ups 
  select(`Patient IDID du patient`) #each patient has an initial follow-up

scc_doi%>%
  group_by(`Patient IDID du patient`)%>%
  summarise(min(`Event Name`, na.rm=T),max(`Event Name`))#%>% #checking how many maximum follow-ups have NA
  #filter(is.na(`max(\`Event Name\`)`))%>% view() #list of patients without final follow-up number

#Assigning them the new follow-up number
# scc_doi%>%
#   filter(`Patient IDID du patient` %in% c(x$`Patient IDID du patient`))%>% view()
scc_doi[c(which((scc_doi$`Patient IDID du patient`%in%c(294,325,339,351,366,374,403,412,420,426,427,429,435,439,424))&(is.na(scc_doi$`Event Name`)))), 2]<-"Follow-Up #1"
scc_doi[c(which((scc_doi$`Patient IDID du patient`%in%c(198,281,311,322,353,406,410,413,415))&(is.na(scc_doi$`Event Name`)))), 2]<-"Follow-Up #2"
scc_doi[c(which((scc_doi$`Patient IDID du patient`%in%c(286,372))&(is.na(scc_doi$`Event Name`)))), 2]<-"Follow-Up #3"
scc_doi[c(which((scc_doi$`Patient IDID du patient`%in%c(284,291,404))&(is.na(scc_doi$`Event Name`)))), 2]<-"Follow-Up #4"
scc_doi[c(which((scc_doi$`Patient IDID du patient`%in%c(319,344,361,390,398))&(is.na(scc_doi$`Event Name`)))), 2]<-"Follow-Up #5"
scc_doi[c(which((scc_doi$`Patient IDID du patient`%in%c(112,268,295,297,334,335,434))&(is.na(scc_doi$`Event Name`)))), 2]<-"Follow-Up #6"
scc_doi[c(which((scc_doi$`Patient IDID du patient`%in%c(438))&(is.na(scc_doi$`Event Name`)))), 2]<-"Follow-Up #7"
scc_doi[c(which((scc_doi$`Patient IDID du patient`%in%c(405))&(is.na(scc_doi$`Event Name`)))), 2]<-"Follow-Up #8"
scc_doi[c(which((scc_doi$`Patient IDID du patient`%in%c(266,298,378,381))&(is.na(scc_doi$`Event Name`)))), 2]<-"Follow-Up #9"
scc_doi[c(which((scc_doi$`Patient IDID du patient`%in%c(267,278))&(is.na(scc_doi$`Event Name`)))), 2]<-"Follow-Up #10"
scc_doi[c(which((scc_doi$`Patient IDID du patient`%in%c(1,62,98,111,386))&(is.na(scc_doi$`Event Name`)))), 2]<-"Follow-Up #11"
scc_doi[c(which((scc_doi$`Patient IDID du patient`%in%c(76,80,86))&(is.na(scc_doi$`Event Name`)))), 2]<-"Follow-Up #12"
scc_doi[c(which((scc_doi$`Patient IDID du patient`%in%c(64,75))&(is.na(scc_doi$`Event Name`)))), 2]<-"Follow-Up #13"
scc_doi[c(which((scc_doi$`Patient IDID du patient`%in%c(116,122,127,303))&(is.na(scc_doi$`Event Name`)))), 2]<-"Follow-Up #14"
scc_doi[c(which((scc_doi$`Patient IDID du patient`%in%c(10,54,55,67,128,135,263,391))&(is.na(scc_doi$`Event Name`)))), 2]<-"Follow-Up #15"
scc_doi[c(which((scc_doi$`Patient IDID du patient`%in%c(8,19,89,99))&(is.na(scc_doi$`Event Name`)))), 2]<-"Follow-Up #16"
scc_doi[c(which((scc_doi$`Patient IDID du patient`%in%c(30,77,141,257))&(is.na(scc_doi$`Event Name`)))), 2]<-"Follow-Up #17"
scc_doi[c(which((scc_doi$`Patient IDID du patient`%in% c(32,87,158,159,162,169))&(is.na(scc_doi$`Event Name`)))), 2]<-"Follow-Up #18"
scc_doi[c(which((scc_doi$`Patient IDID du patient`==5)&(is.na(scc_doi$`Event Name`)))), 2]<-"Follow-Up #20"


scc_doi[c(which((scc_doi$`Patient IDID du patient`%in% c(13))&(is.na(scc_doi$`Event Name`)))), 2]<-c("Follow-Up #12","Follow-Up #13")
scc_doi[c(which((scc_doi$`Patient IDID du patient`%in% c(316))&(is.na(scc_doi$`Event Name`)))), 2]<-c("Follow-Up #12","Follow-Up #13")
scc_doi%>%
  filter(`Patient IDID du patient`==13) #worked fine
scc_doi[c(which((scc_doi$`Patient IDID du patient`==20)&(is.na(scc_doi$`Event Name`)))), 2]<-c("Follow-Up #11","Follow-Up #12","Follow-Up #13")
scc_doi[c(which((scc_doi$`Patient IDID du patient`==21)&(is.na(scc_doi$`Event Name`)))), 2]<-c("Follow-Up #18","Follow-Up #19","Follow-Up #20")
scc_doi[c(which((scc_doi$`Patient IDID du patient`==33)&(is.na(scc_doi$`Event Name`)))), 2]<-c("Follow-Up #9","Follow-Up #10")
scc_doi[c(which((scc_doi$`Patient IDID du patient`%in%c(244))&(is.na(scc_doi$`Event Name`)))), 2]<-c("Follow-Up #1","Follow-Up #2")
scc_doi[c(which((scc_doi$`Patient IDID du patient`%in%c(253))&(is.na(scc_doi$`Event Name`)))), 2]<-c("Follow-Up #1","Follow-Up #2")
scc_doi[c(which((scc_doi$`Patient IDID du patient`%in%c(290))&(is.na(scc_doi$`Event Name`)))), 2]<-c("Follow-Up #1","Follow-Up #2")
scc_doi[c(which((scc_doi$`Patient IDID du patient`%in%c(285))&(is.na(scc_doi$`Event Name`)))), 2]<-c("Follow-Up #5","Follow-Up #6")
scc_doi[c(which((scc_doi$`Patient IDID du patient`%in%c(422))&(is.na(scc_doi$`Event Name`)))), 2]<-c("Follow-Up #7","Follow-Up #8")
scc_doi[c(which((scc_doi$`Patient IDID du patient`%in%c(299))&(is.na(scc_doi$`Event Name`)))), 2]<-c("Follow-Up #7","Follow-Up #8")
scc_doi[c(which((scc_doi$`Patient IDID du patient`%in%c(301))&(is.na(scc_doi$`Event Name`)))), 2]<-c("Follow-Up #6","Follow-Up #7")
scc_doi[c(which((scc_doi$`Patient IDID du patient`%in%c(313))&(is.na(scc_doi$`Event Name`)))), 2]<-c("Follow-Up #6","Follow-Up #7")
scc_doi[c(which((scc_doi$`Patient IDID du patient`%in%c(307))&(is.na(scc_doi$`Event Name`)))), 2]<-c("Follow-Up #15","Follow-Up #16")
scc_doi[c(which((scc_doi$`Patient IDID du patient`%in%c(314))&(is.na(scc_doi$`Event Name`)))), 2]<-c("Follow-Up #3","Follow-Up #4")
scc_doi[c(which((scc_doi$`Patient IDID du patient`%in%c(310))&(is.na(scc_doi$`Event Name`)))), 2]<-c("Follow-Up #3","Follow-Up #4")
scc_doi[c(which((scc_doi$`Patient IDID du patient`%in%c(321))&(is.na(scc_doi$`Event Name`)))), 2]<-c("Follow-Up #4","Follow-Up #5")
scc_doi[c(which((scc_doi$`Patient IDID du patient`%in%c(312))&(is.na(scc_doi$`Event Name`)))), 2]<-c("Follow-Up #4","Follow-Up #5")
scc_doi[c(which((scc_doi$`Patient IDID du patient`%in%c(340))&(is.na(scc_doi$`Event Name`)))), 2]<-c("Follow-Up #1","Follow-Up #2")
scc_doi[c(which((scc_doi$`Patient IDID du patient`%in%c(365))&(is.na(scc_doi$`Event Name`)))), 2]<-c("Follow-Up #1","Follow-Up #2")
scc_doi[c(which((scc_doi$`Patient IDID du patient`%in%c(417))&(is.na(scc_doi$`Event Name`)))), 2]<-c("Follow-Up #1","Follow-Up #2")
scc_doi[c(which((scc_doi$`Patient IDID du patient`%in%c(423))&(is.na(scc_doi$`Event Name`)))), 2]<-c("Follow-Up #1","Follow-Up #2")
scc_doi[c(which((scc_doi$`Patient IDID du patient`%in% c(347))&(is.na(scc_doi$`Event Name`)))), 2]<-c("Follow-Up #1","Follow-Up #2","Follow-Up #3")
scc_doi[c(which((scc_doi$`Patient IDID du patient`%in%c(349))&(is.na(scc_doi$`Event Name`)))), 2]<-c("Follow-Up #1","Follow-Up #2","Follow-Up #3","Follow-Up #4")
scc_doi[c(which((scc_doi$`Patient IDID du patient`%in%c(368))&(is.na(scc_doi$`Event Name`)))), 2]<-c("Follow-Up #1","Follow-Up #2","Follow-Up #3","Follow-Up #4")
scc_doi[c(which((scc_doi$`Patient IDID du patient`%in% c(400))&(is.na(scc_doi$`Event Name`)))), 2]<-c("Follow-Up #8","Follow-Up #9","Follow-Up #10")
scc_doi[c(which((scc_doi$`Patient IDID du patient`%in% c(354))&(is.na(scc_doi$`Event Name`)))), 2]<-c("Follow-Up #8","Follow-Up #9","Follow-Up #10")
scc_doi[c(which((scc_doi$`Patient IDID du patient`==395)&(is.na(scc_doi$`Event Name`)))), 2]<-c("Follow-Up #9","Follow-Up #10","Follow-Up #11")
scc_doi[c(which((scc_doi$`Patient IDID du patient`%in% c(432))&(is.na(scc_doi$`Event Name`)))), 2]<-c("Follow-Up #2","Follow-Up #3")
scc_doi[c(which((scc_doi$`Patient IDID du patient`%in% c(396))&(is.na(scc_doi$`Event Name`)))), 2]<-c("Follow-Up #2","Follow-Up #3")
scc_doi[c(which((scc_doi$`Patient IDID du patient`%in%c(397))&(is.na(scc_doi$`Event Name`)))), 2]<-c("Follow-Up #5","Follow-Up #6","Follow-Up #7")

scc_doi%>%
  group_by(`Patient IDID du patient`)%>%
  summarise(min(`Event Name`, na.rm=T),max(`Event Name`))%>% #checking how many maximum follow-ups have NA
  filter(is.na(`max(\`Event Name\`)`))%>% view()
#Patient 26, 28 and 392- waiting to hear from Dr. Hans

scc_doi%>%
  group_by(`Patient IDID du patient`)%>%
  summarise(min(`Event Name`, na.rm=T),max(`Event Name`))%>% #checking how many maximum follow-ups have NA
  view()

scc_doi%>%
  select(`Patient IDID du patient`, `Event Name`)%>%
  filter(`Patient IDID du patient` %in% c("26","28","392"))%>% view()
#patient 26 and 28 have new primary lesions, ask dr. Hans what to do with them. 

#cleaning patient 392
scc_doi[c(which((scc_doi$`Patient IDID du patient`%in%c(392))&(scc_doi$`Event Name` == "Follow-Up #3"))), 2]<-"Follow-Up #4"
scc_doi[c(which((scc_doi$`Patient IDID du patient`%in%c(392))&(scc_doi$`Event Name` == "Follow-Up #2"))), 2]<-"Follow-Up #3"
scc_doi[c(which((scc_doi$`Patient IDID du patient`%in%c(392))&(is.na(scc_doi$`Event Name`)))), 2]<-"Follow-Up #2"
scc_doi%>%
  select(`Patient IDID du patient`, `Event Name`)%>%
  filter(`Patient IDID du patient` %in% c("392"))%>% view()  #correctly assigned.

##Date of birth -----------
summary(scc_doi$`Date of birthDate de naissance`) #2210 NAs so 2210 - 2433 rows
2433-2210 #223 date of births
class(scc_doi$`Date of birthDate de naissance`)
scc_doi$`Date of birthDate de naissance`<-ymd(scc_doi$`Date of birthDate de naissance`)
scc_doi%>%
  select(`Patient IDID du patient`,`Event Name`,`Date of birthDate de naissance`,
         `Date of Surgery  Date la chirurgie...82`,
         `--- --- If yes, Date of biopsy--- --- Si Oui, Date de la biopsie...36`)%>%
  filter(`Event Name` == "Initial Visit")%>% view() #all 223 initial visits have date of birth

#18 38 and 50

scc_doi<-scc_doi%>%
  group_by(`Patient IDID du patient`)%>%
  fill(`Date of birthDate de naissance`,.direction = "down")#%>% view()


scc_doi$enddate<-NA
class(scc_doi$enddate)
scc_doi$enddate<-as.Date(scc_doi$enddate)

for (i in 1:nrow(scc_doi)) {
  if (is.na(scc_doi$`Date of Surgery  Date la chirurgie...82`)[i] == FALSE){
    scc_doi$enddate[i]<- scc_doi$`Date of Surgery  Date la chirurgie...82`[i]
  } else if (is.na(scc_doi$`Date of Surgery  Date la chirurgie...82`)[i] == TRUE){
    scc_doi$enddate[i]<-scc_doi$`--- --- If yes, Date of biopsy--- --- Si Oui, Date de la biopsie...36`[i]
  }
}
scc_doi$enddate
scc_doi<-scc_doi%>%
  group_by(`Patient IDID du patient`)%>%
  fill(enddate,.direction = "down")#%>% view()

#checking if all enddates > DOB
table(scc_doi$enddate > scc_doi$`Date of birthDate de naissance`, exclude = NULL) #all true
scc_doi$`Patient IDID du patient`[c(which(is.na(scc_doi$enddate > scc_doi$`Date of birthDate de naissance`)))]
#No NA values

# scc_doi$age<-round(age_calc(dob= scc_doi$`Date of birthDate de naissance`, 
#                                  enddate = na.omit(scc_doi$enddate), units = "years", precise = T), digits = 0)

scc_doi$age<-round(age_calc(dob= scc_doi$`Date of birthDate de naissance`, 
                                 enddate = scc_doi$enddate, units = "years", precise = T), digits = 0)
##Gender ------------
scc_doi$GenderSexe
table(factor(scc_doi$GenderSexe), exclude = NULL)
104+119 #223 participants all have gender/sex
scc_doi<-scc_doi%>%
  group_by(`Patient IDID du patient`)%>%
  fill(GenderSexe,.direction = "down")

# table1(~GenderSexe+age, data = (scc_doi%>%
#          filter(`Event Name`=="Initial Visit")))
# 
# scc_doi%>%
#   filter(`Event Name`== "Initial Visit")%>%
#   ggplot()+
#   geom_boxplot(aes(x= GenderSexe,y=age))


##Tobacco history--------------
table(factor(scc_doi$`History of tobacco useHistorique tabagique`), exclude = NULL)
scc_doi[c(which(scc_doi$`History of tobacco useHistorique tabagique` == "Unknown / Not Reported")),5]<-NA  #reassigning unknown/not reported to NA category
levels(factor(scc_doi$`History of tobacco useHistorique tabagique`))

scc_doi%>%
  filter(`Event Name`== "Initial Visit")%>%
  group_by(`History of tobacco useHistorique tabagique`)%>%
  count()

#table1(~GenderSexe+age+`History of tobacco useHistorique tabagique`, data = (scc_doi%>%
 #                                 filter(`Event Name`=="Initial Visit")))

summary(scc_doi$`--- Year of quitting--- Annee d'arret`)
class(scc_doi$`--- Year of quitting--- Annee d'arret`)
table((scc_doi%>%
         filter(`Event Name`=="Initial Visit")%>%
         select(`--- Year of quitting--- Annee d'arret`)), exclude = NULL)
223-167 #only 56 observations

table(scc_doi$`--- Number of Years Smoked--- Nombre d'annees de tabagisme`, exclude=NULL)
2433-2325 #108 observations
#barplot(table(scc_doi$`--- Number of Years Smoked--- Nombre d'annees de tabagisme`))

table((scc_doi%>%
          filter(`Event Name`=="Initial Visit")%>%
          select(`--- Main type of tobacco use--- Principal type de tabac utilise`)), exclude = NULL)
119+7#126 observations

table((scc_doi%>%
         filter(`Event Name`=="Initial Visit")%>%
         select(`--- --- Number of Cigarettes per Day--- --- Nombre de Cigarettes par Jour`)), exclude = NULL)
223-111 #112 observations where as we have 119 observations who reported smoking.
#boxplot(scc_doi$`--- --- Number of Cigarettes per Day--- --- Nombre de Cigarettes par Jour`)#shows the outlines

#Checking outliers
scc_doi%>%
  select(`Patient IDID du patient`,`Event Name`,`--- --- Number of Cigarettes per Day--- --- Nombre de Cigarettes par Jour` )%>%
  filter(`--- --- Number of Cigarettes per Day--- --- Nombre de Cigarettes par Jour`>30)

#CHanging the extreme outlier after recheckign record
scc_doi[which(scc_doi$`--- --- Number of Cigarettes per Day--- --- Nombre de Cigarettes par Jour` == 75),
        "--- --- Number of Cigarettes per Day--- --- Nombre de Cigarettes par Jour"] <- 60

table((scc_doi%>%
         filter(`Event Name`=="Initial Visit")%>%
         select(...10)), exclude = NULL)
223-120#pack years are calculated for 103 participants 
scc_doi<-scc_doi%>%
  rename(packyrs = ...10)#renaming ..10 to packyrs
scc_doi$packyrs
# boxplot((scc_doi%>%
#            filter(`Event Name`=="Initial Visit")%>%
#            select(packyrs))) #some outliers

table(scc_doi$`Status of tobacco use at time of diagnosisStatut tabagique au moment du diagnostic`, exclude = NULL)
scc_doi[c(which(scc_doi$`Status of tobacco use at time of diagnosisStatut tabagique au moment du diagnostic` == "Unknown / Not Reported")),11] <-NA  #reassigning unknown/not reported to NA category
table(scc_doi$`History of tobacco useHistorique tabagique`, scc_doi$`Status of tobacco use at time of diagnosisStatut tabagique au moment du diagnostic`, exclude = NULL)

# table1(~`History of tobacco useHistorique tabagique`|`Status of tobacco use at time of diagnosisStatut tabagique au moment du diagnostic`,
#        data=scc_doi%>%
#          filter(`Event Name`=="Initial Visit"), na.rm = F)

which(scc_doi$`History of tobacco useHistorique tabagique` == "Former Smoker" & scc_doi$`Status of tobacco use at time of diagnosisStatut tabagique au moment du diagnostic` == "Non-Smoker")

scc_doi[c(which(scc_doi$`History of tobacco useHistorique tabagique` == "Former Smoker" & scc_doi$`Status of tobacco use at time of diagnosisStatut tabagique au moment du diagnostic` == "Non-Smoker")), 11]<- "Former Smoker"

scc_doi[c(which(scc_doi$`History of tobacco useHistorique tabagique` == "Smoker" & scc_doi$`Status of tobacco use at time of diagnosisStatut tabagique au moment du diagnostic` == "Non-Smoker")), 11]<- "Former Smoker"

##Alcohol consumption history-----------
table((scc_doi%>%
        filter(`Event Name`=="Initial Visit")%>%
        select(`History of alcohol useHistorique de la consommation d'alcool`)), exclude= NULL)
scc_doi[c(which(scc_doi$`History of alcohol useHistorique de la consommation d'alcool` == "Unknown / Not Reported")),12] <-NA

scc_doi%>%
  group_by(`--- Type of User--- Type de consommateur`)%>%
  filter(`Event Name`== "Initial Visit")%>%
  count()

scc_doi%>%
  group_by(`--- --- Drinks per day--- --- Nombre de verres par jour`)%>%
  filter(`Event Name`== "Initial Visit")%>%
  count()
223-197#26 records

scc_doi%>%
  group_by(`--- --- Select the main type of Drinks:--- --- Selectionner le principal type de boisson`)%>%
  filter(`Event Name`== "Initial Visit")%>%
  count() #29 records

table(scc_doi$`History of alcohol useHistorique de la consommation d'alcool`, 
      scc_doi$`Status of alcohol use at time of diagnosisStatut de la consommation d'alcool au moment du diagnostic`,
      exclude = NULL)

scc_doi[c(which(scc_doi$`Status of alcohol use at time of diagnosisStatut de la consommation d'alcool au moment du diagnostic` == "Unknown / Not Reported")),16] <-NA

scc_doi[c(which(scc_doi$`History of alcohol useHistorique de la consommation d'alcool` == "Former User" & scc_doi$`Status of alcohol use at time of diagnosisStatut de la consommation d'alcool au moment du diagnostic` == "Non-User")), 16]<- "Former User"

scc_doi[c(which(scc_doi$`History of alcohol useHistorique de la consommation d'alcool` == "User" & scc_doi$`Status of alcohol use at time of diagnosisStatut de la consommation d'alcool au moment du diagnostic` == "Non-User")), 16]<- "Former User"
scc_doi<-scc_doi%>%
  rename(tobacco_hist= `History of tobacco useHistorique tabagique`,
         tobacco_stat_diag=`Status of tobacco use at time of diagnosisStatut tabagique au moment du diagnostic`,
         alc_hist= `History of alcohol useHistorique de la consommation d'alcool`,
         alc_stat_diag = `Status of alcohol use at time of diagnosisStatut de la consommation d'alcool au moment du diagnostic`)


##Family history of cancer-----------
scc_doi%>%
  group_by(`Any family history of cancer ?Historique de cancer dans la famille`)%>%
  filter(`Event Name`=="Initial Visit")%>%
  count()

scc_doi%>%
  group_by(`--- How many family history of cancer ?--- Combien d'historique de cancer dans la famille`)%>%
  filter(`Event Name`=="Initial Visit")%>%
  count()
48+20+5 #all 73 who were certain of family history of cancer have reported on number of members who had cancer


##Patient History of non-head and neck cancer-----------
scc_doi%>%
  group_by(`Patient history of Non Head and Neck Cancers?  Historique de cancer chez le patient, hors cancer de la Tete ou du Coup ?`)%>%
  filter(`Event Name`=="Initial Visit")%>%
  count() #35 patients with history of non-head and neck cancer


scc_doi<-scc_doi%>%
  rename(type_HN_hist = ...32,
         other_type = ...33,
         yr_diag = ...34)


scc_doi%>%
  group_by(type_HN_hist)%>%
  filter(`Event Name`=="Initial Visit")%>%
  count()
223-188 #35 observations

scc_doi%>%
  group_by(other_type)%>% filter(`Event Name`=="Initial Visit")%>%
  count()
223-216 #because of 1 overlap(skin and basal cell carcinoma)
table(scc_doi$other_type,scc_doi$type_HN_hist, exclude = NULL)
scc_doi[which(scc_doi$other_type == "Basal cell carcinoma tip of nose" & scc_doi$type_HN_hist == "Skin"), "type_HN_hist"]<-"Other"


class(scc_doi$yr_diag)
table(scc_doi$yr_diag, exclude = NULL) 
2433-2402 #only 31 participants have year of diagnosis
table(scc_doi$type_HN_hist, scc_doi$yr_diag, exclude = NULL) #Assigning these 4 patients to unknown year of diagnosis
#Checkign which 4 patient IDs
scc_doi%>%
  select(`Patient IDID du patient`, `Event Name`, type_HN_hist, other_type, yr_diag)%>%
  filter(`Event Name`=="Initial Visit" & (!is.na(type_HN_hist)) & is.na(yr_diag))%>% view() #Assigning these years to unknown


##Biopsy of non-head and neck cancer
table(scc_doi$`--- If Biopsy, do you know the biopsy date?--- Si Biopsie, connaissez-vous la date de biopsie ?...35`, exclude = NULL)
#217 know their biopsy date

class(scc_doi$`--- --- If yes, Date of biopsy--- --- Si Oui, Date de la biopsie...36`)
scc_doi$`--- --- If yes, Date of biopsy--- --- Si Oui, Date de la biopsie...36`
scc_doi$`--- --- If yes, Date of biopsy--- --- Si Oui, Date de la biopsie...36`<-ymd(scc_doi$`--- --- If yes, Date of biopsy--- --- Si Oui, Date de la biopsie...36`)
summary(scc_doi$`--- --- If yes, Date of biopsy--- --- Si Oui, Date de la biopsie...36`)
table(is.na(scc_doi$`--- --- If yes, Date of biopsy--- --- Si Oui, Date de la biopsie...36`), exclude = NULL)


#histologic diagnosis after biopsy
table(scc_doi$`--- If Biopsy, Histologic Diagnosis:--- Si Biopsie, Diagnostic Histologique ?...37`, exclude = NULL)
2433-2211 #we have 222 biopsy diagnosis but only 217 biopsy dates

#Which have biopsy diagnosis but no biopsy date??
scc_doi%>%
  select(`Patient IDID du patient`,`--- --- If yes, Date of biopsy--- --- Si Oui, Date de la biopsie...36`,
         `--- If Biopsy, Histologic Diagnosis:--- Si Biopsie, Diagnostic Histologique ?...37`)%>%
  filter(!is.na(`--- If Biopsy, Histologic Diagnosis:--- Si Biopsie, Diagnostic Histologique ?...37`) & is.na(`--- --- If yes, Date of biopsy--- --- Si Oui, Date de la biopsie...36`))

table(scc_doi$`--- --- Histologic Diagnosis Other, Specify--- --- Diagnostic histologique Autre, Specifier ?...38`, exclude = NULL)
2433-2409 #24 other diagnosis
#need to clean the variable
scc_doi[c(which(scc_doi$`--- --- Histologic Diagnosis Other, Specify--- --- Diagnostic histologique Autre, Specifier ?...38`%in% c("Carcinoma-In-Situ","Carcinoma In Situ","CIS"))),38]<-"carcinoma in situ"
scc_doi[c(which(scc_doi$`--- --- Histologic Diagnosis Other, Specify--- --- Diagnostic histologique Autre, Specifier ?...38`%in% c("DYSPLASIA","dysplasia dysplasia"))),38]<-"dysplasia"
scc_doi[c(which(scc_doi$`--- --- Histologic Diagnosis Other, Specify--- --- Diagnostic histologique Autre, Specifier ?...38`%in% c("Moderate Dysplasia"))),38]<-"Moderate dysplasia"
scc_doi[c(which(scc_doi$`--- --- Histologic Diagnosis Other, Specify--- --- Diagnostic histologique Autre, Specifier ?...38`%in% c("severe dysplasia"))),38]<-"Severe dysplasia"
table(scc_doi$`--- --- Histologic Diagnosis Other, Specify--- --- Diagnostic histologique Autre, Specifier ?...38`, exclude = NULL)

#Grade
table(scc_doi$`--- --- Grade--- --- Grade ?...39`, exclude = NULL)#none of have grades

#Biopsy differentiation
table(scc_doi$`--- If Biopsy, Differentiation--- Si Biopsie, Differenciation ?...40`, exclude = NULL)
2433-2211 #222 have differentiation of biopsy
scc_doi[c(which(scc_doi$`--- If Biopsy, Differentiation--- Si Biopsie, Differenciation ?...40`== "Not Applicable")),
        "--- If Biopsy, Differentiation--- Si Biopsie, Differenciation ?...40"]<-NA


##dept of invasion-----------
table(scc_doi$`--- If Biopsy, Known Depth of Invasion? --- Si Biopsie, la profondeur de l'invasion est-elle connue ?...41`, exclude = NULL)
scc_doi<-scc_doi%>%
  rename(know_doi_41= `--- If Biopsy, Known Depth of Invasion? --- Si Biopsie, la profondeur de l'invasion est-elle connue ?...41`,
    doi_42 =`--- --- If yes, Please Specify Depth of Invasion (tumor thickness, Unit: mm)--- --- Si oui, merci de preciser la profondeur de l'invasion (epaisseur de la tumeur, Unite: mm)...42`)


scc_doi[c(which(scc_doi$know_doi_41 == "No")), "know_doi_41"]<-"Unknown / Not Reported"
scc_doi[c(which(scc_doi$know_doi_41 == "Yes (maximum thickness)")), "know_doi_41"]<-"Yes"
table(scc_doi$know_doi_41, exclude = NULL)
120+102 #222 so 1 patient does not know depth of invasion 385
#Lets check hat is happening with Patient ID 385
scc_doi%>%
  select(`Patient IDID du patient`, `Event Name`, c(31:42))%>%
  filter(`Patient IDID du patient`== 385 & `Event Name`== "Initial Visit") #Has no history of non head and neck cancer


scc_doi%>%
  select(`Patient IDID du patient`, `Event Name`, know_doi_41, doi_42)%>%
  filter(`Event Name`== "Initial Visit" & is.na(know_doi_41))
table1(~ know_doi_41, data = (scc_doi%>% filter(`Event Name`== "Initial Visit")))


table(is.na(scc_doi$doi_42), exclude=NULL)
scc_doi%>% #only 102 patient should know depth of invasion
  select(`Patient IDID du patient`, `Event Name`, know_doi_41, doi_42)%>%
  filter(`Event Name`== "Initial Visit" & (!is.na(doi_42))) %>% view()

scc_doi[which(scc_doi$`Patient IDID du patient`==282 & scc_doi$`Event Name` == "Initial Visit"), "know_doi_41"]<-"Yes"

summary(scc_doi$doi_42) #Character vector
levels(factor(scc_doi$doi_42))
table(scc_doi$doi_42, exclude=NULL)
2433-2330

table1(~factor(doi_42), data = scc_doi)
table(scc_doi$`Patient IDID du patient`[scc_doi$doi_42 == "2-3"], exclude = NULL) #for patient ID 94 it is 2-3mm
scc_doi[which(scc_doi$doi_42 == "2-3"),"doi_42"]<-"2.5"
class(scc_doi$doi_42)
scc_doi$doi_42<-as.numeric(scc_doi$doi_42)
scc_doi$doi_42_round<-round(scc_doi$doi_42)
scc_doi$doi_42_round<-as.factor(scc_doi$doi_42)
table1(~factor(doi_42), data = scc_doi)

table(scc_doi$`P16 statusStatut P16...43`, exclude = NULL)
2211-2433

#which patient ID has missing value
scc_doi[which(scc_doi$`Event Name`== "Initial Visit" &is.na(scc_doi$`P16 statusStatut P16...43`)), "Patient IDID du patient"]

##Patient type and location of lesion------------
table(scc_doi$`Patient TypeType de patient`, exclude = NULL)

table(scc_doi$...50, exclude = NULL)#the one patient who is a recurrence or local metastasis was treated at MGH.

table(scc_doi$`Patient TypeType de patient`,scc_doi$...50, exclude = NULL)

#Checking which patient is this?
scc_doi[which(scc_doi$`Event Name`== "Initial Visit" & 
                scc_doi$`Patient TypeType de patient`== "Patient who presents with recurrent or local metastases"),
        "Patient IDID du patient"]

#need to look back in the records and add this patients primary lesion visit. As that is the requirement for this study.
scc_doi%>%
  filter(`Patient IDID du patient`== 43)%>% view()

scc_doi<-scc_doi%>%
  rename(spc_patient_type = ...50,
         date_diag_primHN = ...51,
         no_HNlesion = ...52,
         last_TT_stp = ...53,
         time_recurr_months = ...54,
         site_primHN = ...55,
         oral_or_salivary = ...56)

table(scc_doi$date_diag_primHN, exclude = NULL)
table(scc_doi$`Patient TypeType de patient`,scc_doi$date_diag_primHN, exclude = NULL)#Again the patient who has presents recurrent or local metastasis

table(scc_doi$no_HNlesion, scc_doi$last_TT_stp,exclude = NULL)

table(scc_doi$time_recurr_months, scc_doi$`Patient TypeType de patient`,exclude = NULL)

table(scc_doi$site_primHN, exclude=NULL)
2433-2210 #all 223 patients have their site information collected

#Checking the 208 in oral cavity sites
table(scc_doi$oral_or_salivary, exclude = NULL) 
2433-2225 #208 from previous have their intro oral site specified
scc_doi[which(scc_doi$`Event Name`== "Initial Visit" & scc_doi$`Patient IDID du patient`== 340),
        c("Patient IDID du patient","site_primHN","oral_or_salivary","--- --- If Cutaneous, Eye, Metastatic and NOS, Specify--- --- Si Cutane, Yeux, Metatstatique ou NOS, Specifier")]
scc_doi[which(scc_doi$oral_or_salivary == "Lower lip"), "Patient IDID du patient"] #patient 41 is already lower lip

#cleaning oral_or_salivary column
scc_doi[c(which(scc_doi$oral_or_salivary %in% c("Floor of Mouth","FOM") )),"oral_or_salivary"]<- "Floor of mouth"
scc_doi[c(which(scc_doi$oral_or_salivary %in% c("FOM/RMT", "RMT") )),"oral_or_salivary"]<- "Retromolar trigone"
scc_doi[c(which(scc_doi$oral_or_salivary %in% c("Palate") )),"oral_or_salivary"]<- "Hard Palate"
table(scc_doi$oral_or_salivary, exclude = NULL) 


scc_doi[c(which(scc_doi$`Patient IDID du patient`%in% c(52,129,155,266,271,325) & scc_doi$`Event Name`== "Initial Visit")), 
        "--- --- If Cutaneous, Eye, Metastatic and NOS, Specify--- --- Si Cutane, Yeux, Metatstatique ou NOS, Specifier"]<-"Lower Lip" #all moved to lower lip as mentioned by Dr. Hans
scc_doi[which(scc_doi$`Patient IDID du patient` == 41 & scc_doi$`Event Name`== "Initial Visit"), 
        c("site_primHN","oral_or_salivary","--- --- If Cutaneous, Eye, Metastatic and NOS, Specify--- --- Si Cutane, Yeux, Metatstatique ou NOS, Specifier")]
scc_doi%>%
  select(`Patient IDID du patient`, `Event Name`, c(55,56,59))%>%
  filter(`Event Name`== "Initial Visit")%>% view()

#Assigning NA to 41 is oral_or_salivary region
scc_doi[which(scc_doi$`Patient IDID du patient` == 41 & scc_doi$`Event Name`== "Initial Visit"), "oral_or_salivary"]<-NA

scc_doi[which(scc_doi$`Patient IDID du patient` == 41 & scc_doi$`Event Name`== "Initial Visit"),
        "--- --- If Cutaneous, Eye, Metastatic and NOS, Specify--- --- Si Cutane, Yeux, Metatstatique ou NOS, Specifier"]<-"Lower Lip"

scc_doi[which(scc_doi$`Patient IDID du patient` == 340 & scc_doi$`Event Name`== "Initial Visit"), "oral_or_salivary"]<- "Tongue"

table(scc_doi$`--- --- If Neck, Tonsil, Pharynx, Larynx, Specify--- --- Si Cou, Amygdales, Pharynx, Larynx, Specifier`, exclude = NULL)
table(scc_doi$`--- --- if Ear, Nose, Brain, Specify--- --- Si Oreilles, Nez, Cerveau, Specifier`, exclude = NULL)
table(scc_doi$`--- --- If Cutaneous, Eye, Metastatic and NOS, Specify--- --- Si Cutane, Yeux, Metatstatique ou NOS, Specifier`, exclude = NULL)

#uniting oral_or_salivary and cutaneous columns to knoexact site of all primary lesions
scc_doi<-scc_doi%>%
  unite(col = "primary_site",oral_or_salivary,`--- --- If Cutaneous, Eye, Metastatic and NOS, Specify--- --- Si Cutane, Yeux, Metatstatique ou NOS, Specifier`, 
        sep = "", na.rm = T , remove = F)

##Tumor liberality and TNM stage(clinical, radio and patho)----------
table(scc_doi$...60, exclude = NULL)
scc_doi<-scc_doi%>%
  rename(lateral_primHN = ...60,
         t_clinc_stage = ...61,
         n_clinc_stage = ...62,
         m_clinc_stage = ...63)
# scc_doi%>%
#   filter(`Event Name`== "Initial Visit")%>%
#   select(t_clinc_stage,n_clinc_stage,m_clinc_stage)%>%
#   view() 

# scc_doi<-scc_doi%>%
#   unite(t_stage,n_stage,m_stage,col = "TNM_stage_clinical",sep = "",remove = F, na.rm = T)#%>%
#   # select(t_stage,n_stage,m_stage, TNM_stage_clinical)%>%
#   # view()
#Changed my mind aboutn uniting the columns as Dr. Hans wants to view the data differently. Removing the [] brackets

scc_doi$t_clinc_stage<-gsub("[[:punct:]]", "", scc_doi$t_clinc_stage)
scc_doi$n_clinc_stage<-gsub("[[:punct:]]", "", scc_doi$n_clinc_stage)
scc_doi$m_clinc_stage<-gsub("[[:punct:]]", "", scc_doi$m_clinc_stage)

#scc_doi$TNM_stage_clinical<-trimws(scc_doi$TNM_stage_clinical, which = "both")
#table(scc_doi$TNM_stage_clinical, exclude = NULL)
2433-2210 #all 223 participants have a TNM stage_clinical
table1(~t_clinc_stage+n_clinc_stage+m_clinc_stage, data = scc_doi%>%
         filter(`Event Name`== "Initial Visit"))

table(scc_doi$...73, exclude = NULL)
2433-2234 #only 199 participants have this record
table(scc_doi$...74, exclude = NULL)
2433-2233 #only 200 participants have this record
table(scc_doi$...75, exclude = NULL)
2433-2233 #only 199 participants have this record
scc_doi%>%
  filter(`Event Name`== "Initial Visit")%>%
  select(...73,...74,...75)%>%
  view()#lets unite these columns

# scc_doi<-scc_doi%>%
#   unite(...73,...74,...75,col = "TNM_stage_radio",sep = "",remove = F, na.rm = T)#%>%
#   # select(...73,...74,...75, TNM_stage_radio)%>%
#   # view()

scc_doi<-scc_doi%>%
  rename(t_radio_stage = ...73,
         n_radio_stage = ...74,
         m_radio_stage = ...75)

scc_doi$t_radio_stage<-gsub("[[:punct:]]", "", scc_doi$t_radio_stage)
scc_doi$n_radio_stage<-gsub("[[:punct:]]", "", scc_doi$n_radio_stage)
scc_doi$m_radio_stage<-gsub("[[:punct:]]", "", scc_doi$m_radio_stage)

# scc_doi$TNM_stage_radio<-trimws(scc_doi$TNM_stage_radio, which = "both")
# table(scc_doi$TNM_stage_radio, exclude = NULL)

table1(~t_radio_stage+n_radio_stage+m_radio_stage, data = scc_doi%>%
         filter(`Event Name`== "Initial Visit"))
# scc_doi%>%
#   filter(`Event Name`== "Initial Visit")%>%
#   select(TNM_stage_clinical, TNM_stage_radio)%>% view() #a table to show the clinical and radiographic TNM staging for each patient


scc_doi<-scc_doi%>%
  rename(t_patho_stage = `Pathological T stage--- Pathologique - Stage T`,
         n_patho_stage = `Pathological N stage--- Pathologique - Stage N`,
         m_patho_stage = `Pathological M stage--- Pathologique - Stage M`)

scc_doi$t_patho_stage<-gsub("[[:punct:]]", "", scc_doi$t_patho_stage)
scc_doi$n_patho_stage<-gsub("[[:punct:]]", "", scc_doi$n_patho_stage)
scc_doi$m_patho_stage<-gsub("[[:punct:]]", "", scc_doi$m_patho_stage)

table(scc_doi$n_patho_stage, exclude = NULL) #reassigning the Not applicable
scc_doi[c(which(scc_doi$n_patho_stage == "Not Applicable")),"n_patho_stage"]<-NA

#reorganizing nodes (n) to make tables
scc_doi<-scc_doi%>%
  mutate(tab_n_clinc_stage = case_when(n_clinc_stage %in% "N0" ~ "N0",
                                       n_clinc_stage %in% c("N1","N2a","N2b","N2c") ~ "All not N0"),
         tab_n_radio_stage = case_when(n_radio_stage %in% "N0" ~ "N0",
                                       n_radio_stage %in% c("N1","N2","N2a","N2b","N2c","N3") ~ "All not N0"),
         tab_n_patho_stage = case_when(n_patho_stage %in% "N0" ~ "N0",
                                       n_patho_stage %in% c("N1","N2","N2a","N2b","N2c","N3") ~ "All not N0"))

table(scc_doi$n_clinc_stage, scc_doi$tab_n_clinc_stage, exclude = NULL) #checking if correct all working fine
table(scc_doi$n_radio_stage, scc_doi$tab_n_radio_stage, exclude = NULL)
table(scc_doi$n_patho_stage, scc_doi$tab_n_patho_stage, exclude = NULL)

table1(~t_patho_stage+n_patho_stage+m_patho_stage, data = scc_doi%>%filter(`Event Name`== "Initial Visit"))
       
# scc_doi<-scc_doi%>%
#   unite(`Pathological T stage--- Pathologique - Stage T`,`Pathological N stage--- Pathologique - Stage N`,`Pathological M stage--- Pathologique - Stage M`,
#        col = "TNM_stage_patho", sep = "", remove= F, na.rm = T)#%>%
  #select(`Pathological T stage--- Pathologique - Stage T`,`Pathological N stage--- Pathologique - Stage N`,`Pathological M stage--- Pathologique - Stage M`,
         #TNM_stage_patho, `Pathological stage overall--- Stage Pathologique Global`)%>% view()
#scc_doi$TNM_stage_patho<-gsub("[[:punct:]]", " ", scc_doi$TNM_stage_patho)
#scc_doi$TNM_stage_patho<-trimws(scc_doi$TNM_stage_patho, which = "both") #trimign white
#a table to show the clinical and radiographic, pathologic, TNM staging for each patient
# scc_doi%>%
#   filter(`Event Name`== "Initial Visit")%>%
#   select(TNM_stage_clinical, TNM_stage_radio, 
#          TNM_stage_patho, `Pathological stage overall--- Stage Pathologique Global`)%>% view() 

table(scc_doi$`Pathological stage overall--- Stage Pathologique Global`, exclude = NULL)
2433-2213 #220 patients have this information

##Treatment plan for the patient-------------
table1(~ `--- Planned treatment modality--- Forme du traitement prevu (choice=Surgery)`+
         `--- Planned treatment modality--- Forme du traitement prevu (choice=Chemotherapy)`+ 
         `--- Planned treatment modality--- Forme du traitement prevu (choice=Radiation)`+
         `--- Planned treatment modality--- Forme du traitement prevu (choice=Primary Palliation)`+                                                                                                                                                                      
         `--- Planned treatment modality--- Forme du traitement prevu (choice=Surveillance)`, 
       data = scc_doi%>%
         filter(`Event Name`== "Initial Visit"))

scc_doi<-scc_doi%>%
  rename(surgery = `--- Planned treatment modality--- Forme du traitement prevu (choice=Surgery)`,
          chemo= `--- Planned treatment modality--- Forme du traitement prevu (choice=Chemotherapy)`, 
           radiation= `--- Planned treatment modality--- Forme du traitement prevu (choice=Radiation)`,
           palliative= `--- Planned treatment modality--- Forme du traitement prevu (choice=Primary Palliation)`,                                                                                                                                                                      
           surveillance= `--- Planned treatment modality--- Forme du traitement prevu (choice=Surveillance)`)

table1(~surgery+chemo+radiation+palliative+surveillance, data = scc_doi%>%
         filter(`Event Name`== "Initial Visit"))
table(scc_doi$surgery, scc_doi$chemo, exclude = NULL)
table1(~chemo+radiation+palliative+surveillance|surgery, data = scc_doi%>%
         filter(`Event Name`== "Initial Visit"), overall = F)

#these are the important variables in this section
table(scc_doi$`Was Surgery Completed?  Une chirurgie a-t-elle ete pratiquee?`, exclude = NULL)
class(scc_doi$`Date of Surgery  Date la chirurgie...82`)
scc_doi$`Date of Surgery  Date la chirurgie...82`<-ymd(scc_doi$`Date of Surgery  Date la chirurgie...82`)
table(is.na(scc_doi$`Date of Surgery  Date la chirurgie...82`), exclude = NULL) #220 participants have date of surgey completion
 #this matches the number of dates we have.

#surgical procedure
table(scc_doi$`Main Surgical Procedure  Methode principale utilisee pour la chirurgie...83`, exclude = NULL)
2213-2433 #220 participants have information about the surgical procedure.

scc_doi<-scc_doi%>%
  mutate(surgery_type = case_when(`Main Surgical Procedure  Methode principale utilisee pour la chirurgie...83`%in% c("Neck dissection only",
                                                                                                                      "Resection, neck dissection, free flap",
                                                                                                                      "Resection, neck dissection, pedicle flap",
                                                                                                                      "Resection, neck dissection, primary closure or skin graft")~ "Neck dissection",
                                  `Main Surgical Procedure  Methode principale utilisee pour la chirurgie...83` %in% c("Resection primary closure or skin graft") ~ "No neck dissection"))


scc_doi%>%
  select(`Patient IDID du patient`, `Event Name`, t_patho_stage, n_patho_stage, m_patho_stage, 82,83, surgery_type,
         `Degree of DifferentiationDegree de differenciation...122`)%>%
  filter(`Event Name`== "Initial Visit")%>% 
  arrange(surgery_type, `Degree of DifferentiationDegree de differenciation...122`)%>%view()
#write_csv(xy, here("pathTNM_surgeyType.csv")) 

##Histologic diagnosis (grade, differentiation)---------------
table(scc_doi$`Histologic Diagnosis:Diagnostic Histologique ?...119`, exclude = NULL)#all 223 were squamous cell carcinoma
table(scc_doi$`--- Histologic Diagnosis Other, Specify--- Diagnostic histologique Autre, Specifier ?...120`, exclude = NULL)

table(scc_doi$`--- Grade--- Grade ?...121`, exclude = NULL)
table(scc_doi$`Degree of DifferentiationDegree de differenciation...122`, exclude = NULL)
2433-2213 #220 patients have degree of differentiation
#Patient 98, 158, 181,379, 380,410, 13 under went neck dissection and still differentiation not applicable. The remaining are no neck dissection.

scc_doi[c(which(scc_doi$`Degree of DifferentiationDegree de differenciation...122`== "Not Applicable")),
        "Degree of DifferentiationDegree de differenciation...122"]<-NA

scc_doi<-scc_doi%>%
  rename(final_patho_differen = "Degree of DifferentiationDegree de differenciation...122",
         biopsy_differen = "--- If Biopsy, Differentiation--- Si Biopsie, Differenciation ?...40")

##Number of positive nodes---------
table(scc_doi$...123, exclude = NULL)
scc_doi<-scc_doi%>%
  rename(Num_positive_nodes = ...123)
2433-2215 #218 participants have information about the number of nodes involved.
scc_doi%>%
  select(`Patient IDID du patient`, `Event Name`, Num_positive_nodes, surgery_type, n_patho_stage)%>% 
  filter(`Event Name` == "Initial Visit" & is.na(Num_positive_nodes))%>% view()

scc_doi$Num_positive_nodes_fac<-as.factor(scc_doi$Num_positive_nodes)

##Depth of invasion and true DOI or tumour thickness------------
table(scc_doi$`Known Depth of Invasion?Profondeur de l'invasion connue ?...124`, exclude= NULL)
160+60
scc_doi<-scc_doi%>%
  rename(know_doi_124 = `Known Depth of Invasion?Profondeur de l'invasion connue ?...124`,
         doi_125 =`---  If yes, Please Specify Depth of Invasion (tumor thickness, Unit: mm)--- Si oui, merci de preciser la profondeur de l'invasion (epaisseur de la tumeur, Unite: mm)...125`)

scc_doi%>%
  select(`Patient IDID du patient`, `Event Name`, know_doi_41,doi_42, know_doi_124,doi_125, ...126 )%>%
  filter(`Event Name` == "Initial Visit")%>% 
  arrange(know_doi_124)%>%view()

scc_doi[which(scc_doi$`Patient IDID du patient` == 176 & scc_doi$`Event Name` == "Initial Visit"), "doi_125"]<- 7.5 #7 to 8
scc_doi[which(scc_doi$`Patient IDID du patient` == 340 & scc_doi$`Event Name` == "Initial Visit"), "doi_125"]<- 1.5 #<2
scc_doi[which(scc_doi$`Patient IDID du patient` %in% c(435,439) & scc_doi$`Event Name` == "Initial Visit"), "doi_125"]<- 10 #>10

table(scc_doi$doi_125,scc_doi$know_doi_124,exclude = NULL)

scc_doi$`Patient IDID du patient`[which(scc_doi$know_doi_124 == "No" & scc_doi$doi_125 == 14)] #patient 94 says no DOI but reports it, so making this change
scc_doi[which(scc_doi$`Patient IDID du patient` == 94 & scc_doi$`Event Name` == "Initial Visit"), "know_doi_124"]<- "Yes"

scc_doi%>%
  select(`Patient IDID du patient`, `Event Name`, know_doi_124,doi_125, ...126 )%>%
  filter(`Event Name` == "Initial Visit")%>% view()

table1(~doi_125, data = (scc_doi%>% filter(`Event Name`== "Initial Visit")))

table(scc_doi$...126, exclude = NULL)
scc_doi<-scc_doi%>%
  mutate(doi_cat = case_when(...126 %in% c("tumour thickness (depth of invasion)")~ "true_doi",
                             ...126 %in% c("(tumor thickness)",
                                           "(tumour size)",
                                           "(tumour thickness)",
                                           "tumour size",
                                           "tumour thickness") ~ "tumor_thickness"))

table(scc_doi$...126, scc_doi$doi_cat, exclude = NULL)

scc_doi[c(which(is.na(scc_doi$doi_cat) & scc_doi$`Event Name`== "Initial Visit")), "doi_cat"]<- "true_doi"
table(scc_doi$...126, scc_doi$doi_cat, exclude = NULL)

##Margins of lesion----------------
table(scc_doi$`Margins (microscopic)Marges (microscopique)...127`, exclude = NULL) #information on all 220
#does not applicable mean missing? 
2433-2213
table(scc_doi$`Margins (microscopic)Marges (microscopique)...127`, scc_doi$`Was Surgery Completed?  Une chirurgie a-t-elle ete pratiquee?`, exclude= NULL)

table(scc_doi$`--- If Margin Negative, Please select the closest distance:--- Si Marges Negatives, Selectionner la distance la plus proche:...128`, exclude = NULL)
which(scc_doi$`--- If Margin Negative, Please select the closest distance:--- Si Marges Negatives, Selectionner la distance la plus proche:...128`== ">5")

#editing the extra category
scc_doi[which(scc_doi$`--- If Margin Negative, Please select the closest distance:--- Si Marges Negatives, Selectionner la distance la plus proche:...128`== ">5"),"--- If Margin Negative, Please select the closest distance:--- Si Marges Negatives, Selectionner la distance la plus proche:...128"]<- "> 5"

table(scc_doi$`Margins (microscopic)Marges (microscopique)...127`, 
      scc_doi$`--- If Margin Negative, Please select the closest distance:--- Si Marges Negatives, Selectionner la distance la plus proche:...128`, exclude = NULL)
79+101+25
table(scc_doi$`Margins (microscopic)Marges (microscopique)...127`,
      scc_doi$`--- If Margins Positive, Please specify:--- Si Marges Positives, Specifier:...129`, exclude = NULL)

##extra cellular spread of lymphnodes------------
table(scc_doi$`Extracapsular Spread of Lymph NodesPropagation Extrascapulaire des Ganglions Lymphatiques...130`, exclude = NULL)
2433-2213 #information on 220 participants who underwent surgery

##Perineural invasion----------
table(scc_doi$`Perineural InvasionInvasion Preneurale...131`, exclude = NULL)
table(scc_doi$`Extracapsular Spread of Lymph NodesPropagation Extrascapulaire des Ganglions Lymphatiques...130`, scc_doi$`Perineural InvasionInvasion Preneurale...131`, exclude = NULL)

##Vascular invasion------------
table(scc_doi$`Vascular/Lymphatic InvasionInvasion Vasculaire/Lymphatique...132`, exclude = NULL)

##Bone and cartilage invasion------
table(scc_doi$`Bone/Cartilage InvasionInvasion des Os/Cartilages...133`, exclude = NULL)

##p16 status of patient (HPV)------
table(scc_doi$`P16 statusStatut P16...134`, exclude = NULL)

scc_doi<-scc_doi%>%
  rename(margins = `Margins (microscopic)Marges (microscopique)...127`,
         neg_margins =`--- If Margin Negative, Please select the closest distance:--- Si Marges Negatives, Selectionner la distance la plus proche:...128`,
         extra_capsu= `Extracapsular Spread of Lymph NodesPropagation Extrascapulaire des Ganglions Lymphatiques...130`,
         peri_neu_inva= `Perineural InvasionInvasion Preneurale...131`,
         vas_lymph_inv= `Vascular/Lymphatic InvasionInvasion Vasculaire/Lymphatique...132`,
         bone_inv= `Bone/Cartilage InvasionInvasion des Os/Cartilages...133`,
         p16_status= `P16 statusStatut P16...134`)


#table(scc_doi$margins, exclude = NULL)
scc_doi[c(which(scc_doi$margins == "Not Applicable")), "margins"]<-NA

#table(scc_doi$neg_margins, exclude = NULL)
#scc_doi[c(which(scc_doi$neg_margins == "> 5")), "neg_margins"]<-">5"
#scc_doi[c(which(is.na(scc_doi$neg_margins))), 129]<-"Unknown"

#table(scc_doi$extra_capsu, exclude = NULL)
scc_doi[c(which(scc_doi$extra_capsu == "Not Applicable")), "extra_capsu"]<-NA

#table(scc_doi$peri_neu_inva, exclude = NULL)
scc_doi[c(which(scc_doi$peri_neu_inva == "Not Applicable")), "peri_neu_inva"]<-NA

#table(scc_doi$vas_lymph_inv, exclude = NULL)
scc_doi[c(which(scc_doi$vas_lymph_inv == "Not Applicable")), "vas_lymph_inv"]<-NA

#table(scc_doi$bone_inv, exclude = NULL)
scc_doi[c(which(scc_doi$bone_inv == "Not Applicable")), "bone_inv"]<-NA

#table(scc_doi$p16_status, exclude = NULL
scc_doi[c(which(is.na(scc_doi$p16_status))), "p16_status"]<-"Not Reported"

##Was patient given radio therapy------------
table(scc_doi$`Was there Radiotherapy?  Une Radiotherapie a-t-elle administree ?`, exclude = NULL)

##was there chemotherapy ------------
table(scc_doi$`Was there Chemotherapy?  Une Chimiotherapie a-t-elle administree ?`, exclude = NULL)


##Date of follow-up -----------------
scc_doi%>%
  select(`Patient IDID du patient`, `Event Name`, Follow_up_Date)%>%
  filter(`Event Name` == "Initial Visit")  #All initial visits NA, replacing this with enddate column used calculate age of participants
 

class(scc_doi$Follow_up_Date)
scc_doi$Follow_up_Date<-dmy(scc_doi$Follow_up_Date)

class(scc_doi$enddate)

for (j in 1:nrow(scc_doi)) {
  if (scc_doi$`Event Name`[j]== "Initial Visit" & (!is.na(scc_doi$`Event Name`[j]))){
    scc_doi$Follow_up_Date[j]<- scc_doi$enddate[j]
  }
}

scc_doi%>% #loop is working fine
  select(`Patient IDID du patient`, `Event Name`, Follow_up_Date, enddate)%>% view()


#For those who were lost to follow-up the dates have been removed and NA has been assigned
levels(factor(scc_doi$`Current Follow Up StatusStatut a la visite de suivi actuelle`))
for (k in i:nrow(scc_doi)) {
  if (scc_doi$`Current Follow Up StatusStatut a la visite de suivi actuelle`[k] == "Lost to follow up" &
      (!is.na(scc_doi$`Current Follow Up StatusStatut a la visite de suivi actuelle`[k]))){
    scc_doi$Follow_up_Date[k]<-NA
  }
}

scc_doi%>%
  select(`Patient IDID du patient`, `Event Name`, Follow_up_Date,
         `Current Follow Up StatusStatut a la visite de suivi actuelle`, ...147,
         `--- --- If Alive, Disease status: --- --- Si Vivant, Statut de la Maladie:`)%>% view()
levels(factor(scc_doi$`Current Follow Up StatusStatut a la visite de suivi actuelle`))  
table(scc_doi$`Current Follow Up StatusStatut a la visite de suivi actuelle`, exclude = NULL)  

scc_doi%>%
  select(`Patient IDID du patient`, `Event Name`, Follow_up_Date,
         `Current Follow Up StatusStatut a la visite de suivi actuelle`, ...147,
         `--- --- If Alive, Disease status: --- --- Si Vivant, Statut de la Maladie:`)%>% 
  filter(`Current Follow Up StatusStatut a la visite de suivi actuelle` %in% c("NOTE: new liver cancer", "Palliative care"))

##Vital status
table(scc_doi$...147, exclude = NULL) #which is unknown
scc_doi%>%
  select(`Patient IDID du patient`, `Event Name`, Follow_up_Date,
         `Current Follow Up StatusStatut a la visite de suivi actuelle`, ...147,
         `--- --- If Alive, Disease status: --- --- Si Vivant, Statut de la Maladie:`)%>% 
  filter(...147 == "Unknown")
  

##Length of follow-up------------
class(scc_doi$Follow_up_Date)
scc_doi%>%
  group_by(`Patient IDID du patient`)%>%
  summarise(min(Follow_up_Date, na.rm=T), max(Follow_up_Date, na.rm = T),
            len_flw_mon= as.numeric(`max(Follow_up_Date, na.rm = T)`-`min(Follow_up_Date, na.rm = T)`)/(365.25/12))%>%
  summarise(mean=mean(len_flw_mon), SD=sd(len_flw_mon), 
            median= median(len_flw_mon), min=min(len_flw_mon),
            max=max(len_flw_mon),IQR = IQR(len_flw_mon))

#If expired reason for death ------------

scc_doi<-scc_doi%>%
  rename(date_expired= ...164,
         disease_status_when_expired= ...165,
         reason_death= ...166,
         other_reason_death= ...167)

table(scc_doi$...147, exclude = NULL) #64 participants expired and 1 unknown

scc_doi%>%
  select(...147, date_expired, disease_status_when_expired, reason_death, other_reason_death)%>%
  filter(...147 == "Expired")%>% view()

table(scc_doi$reason_death, exclude = NULL)
scc_doi[c(which(scc_doi$reason_death %in% c("Non- Head and Neck Cancer Related","Non Head and Neck Cancer Related"))), 
        "reason_death"]<-"Non-head and neck cancer related"
scc_doi[c(which(scc_doi$reason_death %in% c("unknown"))),"reason_death"]<-"Unknown"

scc_doi$`Patient IDID du patient`[which(scc_doi$reason_death == "Esophageal cancer")]
scc_doi[which(scc_doi$reason_death == "Esophageal cancer"), "reason_death"]<-"Non-head and neck cancer related"

table(scc_doi$other_reason_death, exclude = NULL)#no other reasons

#The ones who did not die have been assigned not applicable
for (l in 1:nrow(scc_doi)) {
  if(is.na(scc_doi$...147[l]) | scc_doi$...147[l] != "Expired"){
    scc_doi$reason_death[l]<-"Not applicable"
  }
}

table(scc_doi$reason_death, exclude = NULL)

scc_doi%>%
  filter(`Event Name`== "Initial Visit")%>%
  count(reason_death)%>% view()

#Fill down demographic variables--------
scc_doi<-scc_doi%>%
  group_by(`Patient IDID du patient`)%>%
  fill(c(age, enddate, GenderSexe, tobacco_hist, tobacco_stat_diag, alc_hist, alc_stat_diag,
         t_clinc_stage,n_clinc_stage, t_patho_stage,surgery_type,
         n_patho_stage, `Pathological stage overall--- Stage Pathologique Global`, Num_positive_nodes_fac),.direction = "down")%>%
  fill(c(`Was there Radiotherapy?  Une Radiotherapie a-t-elle administree ?`,
         `Was there Chemotherapy?  Une Chimiotherapie a-t-elle administree ?`),.direction = "updown")%>%
  ungroup()
#Disease status----
table(scc_doi$`--- --- If Alive, Disease status: --- --- Si Vivant, Statut de la Maladie:`, exclude=NULL)


scc_doi%>%
  select(`Patient IDID du patient`, `Event Name`, Follow_up_Date,
         `Current Follow Up StatusStatut a la visite de suivi actuelle`, ...147,
         `--- --- If Alive, Disease status: --- --- Si Vivant, Statut de la Maladie:`)%>% 
  filter(`Patient IDID du patient` %in% c(45,105,112))%>% view()

MK_recurData<-scc_doi%>%
  select(`Patient IDID du patient`, `Event Name`,Follow_up_Date, ...147, `--- --- If Alive, Disease status: --- --- Si Vivant, Statut de la Maladie:`)%>%
  filter(!is.na(`--- --- If Alive, Disease status: --- --- Si Vivant, Statut de la Maladie:`) & 
           (`--- --- If Alive, Disease status: --- --- Si Vivant, Statut de la Maladie:` != "Disease free"))%>% view()

#write_csv(MK_recurData, here("Data","Recurrence_chart_before_cleaning.csv"))

#Saving the clean dataset
#write_csv(scc_doi, here("Data","clean_scc_doi_04-11-2022.csv"))

#If any changes to the above clean dataset, then re-read the basline dataset, or else run line 21 onwards
baseline_scc_doi<- scc_doi%>% filter(`Event Name`== "Initial Visit")
#write_csv(baseline_scc_doi, here("Data","baseline_scc_doi_04-11-2022.csv"))

#Cleaning this variable using the information coming from the new table provided by Dr. Hans
hans_recurData<-read_xlsx(here("Data","DOI Project- Recurrence Data v3.xlsx"))

scc_doi%>%
  select(`Patient IDID du patient`, `Event Name`,Follow_up_Date, ...147,
         `--- --- If Alive, Disease status: --- --- Si Vivant, Statut de la Maladie:`)%>%
  filter(`Patient IDID du patient` %in% c(recurData$Patient)) %>% view()





#Introducing the new variables from the recurrence Data set provided by Dr. Hans-----------------
hans_recurData<-hans_recurData%>%
  fill(Patient,.direction = "down")

