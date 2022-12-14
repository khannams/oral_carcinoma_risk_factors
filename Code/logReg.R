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

baseline_scc_doi<- read_csv(here("Data","clean_baseline_05-12-2022.csv"))
class(baseline_scc_doi$doi_125_round)
baseline_scc_doi$doi_125_round<-factor(baseline_scc_doi$doi_125_round)

baseline_scc_doi%>%
  #select(Patient.IDID.du.patient, surgery_type,doi_cat,doi_125_round, n_patho_stage, n_clinc_stage)%>%
  filter(doi_cat== "true_doi" & !is.na(doi_125_round) & n_clinc_stage== "N0")%>% 
  count(n_patho_stage) #leaves us with 82 participants

baseline_scc_doi%>%
  select(Patient.IDID.du.patient, surgery_type,doi_cat,doi_125_round, n_patho_stage, n_clinc_stage)%>%
  filter(doi_cat== "true_doi" & !is.na(doi_125_round) & n_clinc_stage== "N0")%>% 
  group_by(doi_125_round)%>% arrange(doi_125_round) %>%view() #add filter of removing those for whom path_n stage not applicable
  
baseline_scc_doi%>%
  select(Patient.IDID.du.patient, surgery_type,doi_cat,doi_125_round, n_patho_stage, n_clinc_stage)%>%
  filter(doi_cat== "true_doi" & !is.na(doi_125_round) & n_clinc_stage== "N0" & n_patho_stage != "Not Applicable")%>% 
  count(n_patho_stage)

log.reg<-baseline_scc_doi%>%
  select(Patient.IDID.du.patient, surgery_type,doi_cat,doi_125,doi_125_round, n_patho_stage, n_clinc_stage)%>%
  filter(doi_cat== "true_doi" & !is.na(doi_125_round) & n_clinc_stage== "N0" & n_patho_stage != "Not Applicable")%>% 
  group_by(doi_125_round)%>% arrange(doi_125_round) %>% view()

log.reg$occultmet<-NA
for (i in 1:nrow(log.reg)) {
  if (log.reg$n_patho_stage[i] != log.reg$n_clinc_stage[i]){
    log.reg$occultmet[i]<- 1 #meaning yes occult metastasis seen
  } else{
    log.reg$occultmet[i]<-0 #meaning no occult metastasis
  }
}

#write.table(log.reg, here("Data","log.reg.txt"), sep = ",")

#ploting the two variables before logistic regression
class(log.reg$doi_125_round)
log.reg$doi_125_round_num<-as.numeric(as.character(log.reg$doi_125_round))#changing back to numeric
table(log.reg$doi_125_round, log.reg$doi_125_round_num, exclude = NULL)
boxplot(doi_125_round_num ~ occultmet, data = log.reg, xlab = "Occult metastasis", ylab = "DOI",
        main = "Distribution of occult metastasis by DOI for patients who reported TRUE_DOI and clinic stage was N0") # few outliers


##Models predicting occult metastasis--------------------
##ROUNDED DOI-------------------------
#fitting the model
lr.fit<-glm(occultmet ~ doi_125_round_num, data = log.reg, family = binomial)
summary(lr.fit)
confint(lr.fit)


#getting odds ratio with confidence interval
exp(lr.fit$coefficients)
exp(confint(lr.fit))
#odds ratio for DOI is 1.02. This means the odds of occult metastasis increase by 2% for each mm increase in DOI


#getting predicted values
predicted_data<-data.frame(doi_125_round_num = seq(min(log.reg$doi_125_round_num), 
                                                   max(log.reg$doi_125_round_num),
                                                   len = 500))

predicted_data$occultmet<-predict(lr.fit, predicted_data, type = "response") #geting the predicted values
  
predit_se<-predict(lr.fit,  predicted_data, type = "response", se.fit = T) #se.fitting to get CI for predicted values

predicted_data$lowerCI<-predit_se$fit-1.96*predit_se$se.fit
predicted_data$upperCI<-predit_se$fit+1.96*predit_se$se.fit


ggplot(data = log.reg,
       aes(x=doi_125_round_num,
           y=occultmet))+
  geom_point()+
  stat_smooth(method = "glm", se =TRUE, method.args = list(family=binomial))+
  xlab(label = "Depth of invasion(mm)- continous: rounded")+
  ylab(label = "Probability fo occult lymphnode metstasis")+
  theme_light()

##NOT ROUNDED DOI-------------------
log.reg_doi<-baseline_scc_doi%>%
  select(Patient.IDID.du.patient, surgery_type,doi_cat,doi_125, n_patho_stage, n_clinc_stage)%>%
  filter(doi_cat== "true_doi" & !is.na(doi_125) & n_clinc_stage== "N0" & n_patho_stage != "Not Applicable")%>% 
  group_by(doi_125)%>% arrange(doi_125) %>% view()

log.reg_doi$occultmet<-NA
for (i in 1:nrow(log.reg_doi)) {
  if (log.reg_doi$n_patho_stage[i] != log.reg_doi$n_clinc_stage[i]){
    log.reg_doi$occultmet[i]<- 1 #meaning yes occult metastasis seen
  } else{
    log.reg_doi$occultmet[i]<-0 #meaning no occult metastasis
  }
}

class(log.reg_doi$doi_125)
boxplot(doi_125 ~ occultmet, data = log.reg_doi, xlab = "Occult metastasis", ylab = "DOI_cont",
        main = "Distribution of occult metastasis by DOI for patients who reported TRUE_DOI and clinic stage was N0") # few outliers

#fitting the model
lr.fit_doicont<-glm(occultmet ~ doi_125, data = log.reg_doi, family = binomial)
summary(lr.fit_doicont)
confint(lr.fit_doicont)


#getting odds ratio with confidence interval
round(exp(lr.fit_doicont$coefficients), digits = 2)
round(exp(confint(lr.fit_doicont)), digits = 2)
#odds ratio for DOI is 1.02. This means the odds of occult metastasis increase by 2% for each mm increase in DOI


#getting predicted values
predicted_data_doicont<-data.frame(doi_125 = seq(min(log.reg_doi$doi_125), 
                                                   max(log.reg_doi$doi_125),
                                                   len = 500))

predicted_data_doicont$occultmet<-predict(lr.fit_doicont, predicted_data_doicont, type = "response") #getting the predicted values

predit_doicon_se<-predict(lr.fit_doicont,  predicted_data_doicont, type = "response", se.fit = T) #se.fitting to get CI for predicted values

predicted_data_doicont$lowerCI<-predit_doicon_se$fit-1.96*predit_doicon_se$se.fit
predicted_data_doicont$upperCI<-predit_doicon_se$fit+1.96*predit_doicon_se$se.fit


ggplot(data = log.reg_doi,
       aes(x=doi_125,
           y=occultmet))+
  geom_point()+
  stat_smooth(method = "glm", se =TRUE, method.args = list(family=binomial))+
  xlab(label = "Depth of invasion(mm)-continous: decimal point")+
  ylab(label = "Probability fo occult lymphnode metstasis")+
  theme_light()


#Perinural invasion--------------------------
baseline_scc_doi%>%
  #select(Patient.IDID.du.patient, surgery_type,doi_cat,doi_125_round, n_patho_stage, n_clinc_stage)%>%
  filter(n_clinc_stage== "N0")%>% 
  count(n_patho_stage) #leaves us with 82 participants


PNI_log.reg<-baseline_scc_doi%>%
  select(Patient.IDID.du.patient, surgery_type,doi_cat,doi_125_round, peri_neu_inva,n_patho_stage, n_clinc_stage)%>%
  filter(n_clinc_stage== "N0" & n_patho_stage != "Not Applicable", !is.na(peri_neu_inva))%>% view()
#127 participants for this model

PNI_log.reg$occultmet<-NA
for (j in 1:nrow(PNI_log.reg)) {
  if (PNI_log.reg$n_patho_stage[j] != PNI_log.reg$n_clinc_stage[j]){
    PNI_log.reg$occultmet[j]<- 1 #meaning yes occult metastasis seen
  } else{
    PNI_log.reg$occultmet[j]<-0 #meaning no occult metastasis
  }
}  

table(PNI_log.reg$occultmet, PNI_log.reg$peri_neu_inva, exclude = NULL)

pni.lr.fit<-glm(occultmet~peri_neu_inva, data = PNI_log.reg, family = binomial)
summary(pni.lr.fit)
exp(pni.lr.fit$coefficients)
exp(confint(pni.lr.fit))


#Grade of differentiation------------------------------------
table(baseline_scc_doi$biopsy_differen, exclude = NULL)
DD_log.reg<-baseline_scc_doi%>%
  select(Patient.IDID.du.patient, surgery_type,doi_cat,doi_125_round, peri_neu_inva,biopsy_differen,n_patho_stage, n_clinc_stage)%>%
  filter(n_clinc_stage== "N0" & n_patho_stage != "Not Applicable", !is.na(biopsy_differen))%>% view()
#110 participants for this model

DD_log.reg$occultmet<-NA
for (k in 1:nrow(DD_log.reg)) {
  if (DD_log.reg$n_patho_stage[k] != DD_log.reg$n_clinc_stage[k]){
    DD_log.reg$occultmet[k]<- 1 #meaning yes occult metastasis seen
  } else{
    DD_log.reg$occultmet[k]<-0 #meaning no occult metastasis
  }
}  

table(DD_log.reg$occultmet, DD_log.reg$biopsy_differen, exclude = NULL)
class(DD_log.reg$biopsy_differen)
DD_log.reg$biopsy_differen<-factor(DD_log.reg$biopsy_differen, levels=c("Well","Moderate","Poorly"))
table(DD_log.reg$occultmet, DD_log.reg$biopsy_differen, exclude = NULL)

DD.lr.fit<-glm(occultmet~biopsy_differen, data = DD_log.reg, family = binomial)
summary(DD.lr.fit)
exp(DD.lr.fit$coefficients)
exp(confint(DD.lr.fit))


#Making the line of best fit our requested by Dr. Hans-------------------------------------
x<-log.reg%>%
  group_by(doi_125_round_num)%>%
  summarise(occultsum = sum(occultmet))
y<-log.reg%>%
  group_by(doi_125_round_num)%>%
  count()
prob <-merge(x,y, by="doi_125_round_num")
prob$prob <- round(prob$occultsum/prob$n, digits = 2)
ggplot(data = prob, 
       aes(x= doi_125_round_num,
           y= prob))+
  geom_line(lwd =1, col="blue")+
  theme_minimal()
plot(prob~doi_125_round_num, data = prob)


##DOI-42 -----------------------------------------
class(baseline_scc_doi$doi_42)
baseline_scc_doi%>%
  select(Patient.IDID.du.patient, surgery_type,doi_cat,doi_42, n_patho_stage, n_clinc_stage)%>%
  filter(!is.na(doi_42) & n_clinc_stage== "N0")%>% 
  count(n_patho_stage) #leaves us with 86 participants

baseline_scc_doi%>%
  select(Patient.IDID.du.patient, surgery_type,doi_cat,doi_42, n_patho_stage, n_clinc_stage)%>%
  filter(!is.na(doi_42) & n_clinc_stage== "N0")%>% 
  group_by(doi_42)%>% arrange(doi_42) %>%view() #add filter of removing those for whom path_n stage not applicable

baseline_scc_doi%>%
  select(Patient.IDID.du.patient, surgery_type,doi_cat,doi_42, n_patho_stage, n_clinc_stage)%>%
  filter(!is.na(doi_42) & n_clinc_stage== "N0" & n_patho_stage != "Not Applicable")%>% 
  count(n_patho_stage)#67 patients

log.reg.42<-baseline_scc_doi%>%
  select(Patient.IDID.du.patient, surgery_type,doi_cat,doi_42, n_patho_stage, n_clinc_stage)%>%
  filter(!is.na(doi_42) & n_clinc_stage== "N0" & n_patho_stage != "Not Applicable")%>% 
  group_by(doi_42)%>% arrange(doi_42) %>% view()

log.reg.42$occultmet<-NA
for (i in 1:nrow(log.reg.42)) {
  if (log.reg.42$n_patho_stage[i] != log.reg.42$n_clinc_stage[i]){
    log.reg.42$occultmet[i]<- 1 #meaning yes occult metastasis seen
  } else{
    log.reg.42$occultmet[i]<-0 #meaning no occult metastasis
  }
}

#write.table(log.reg, here("Data","log.reg.txt"), sep = ",")

#ploting the two variables before logistic regression
class(log.reg.42$doi_42)
# log.reg$doi_125_round_num<-as.numeric(as.character(log.reg$doi_125_round))#changing back to numeric
# table(log.reg$doi_125_round, log.reg$doi_125_round_num, exclude = NULL)
boxplot(doi_42~ occultmet, data = log.reg.42, xlab = "Occult metastasis", ylab = "DOI",
        main = "Distribution of occult metastasis by DOI for patients who reported clinic stage was N0") # few outliers


##Models predicting occult metastasis--------------------
#fitting the model
lr.fit.42<-glm(occultmet ~ doi_42, data = log.reg.42, family = binomial)
summary(lr.fit.42)
confint(lr.fit.42)


#getting odds ratio with confidence interval
exp(lr.fit.42$coefficients)
exp(confint(lr.fit.42))
#odds ratio for DOI is 1.02. This means the odds of occult metastasis increase by 2% for each mm increase in DOI


#getting predicted values
predicted_data_42<-data.frame(doi_42 = seq(0, max(log.reg.42$doi_42),
                                                   len = 100))

predicted_data_42$occultmet<-predict(lr.fit.42, predicted_data_42, type = "response") #geting the predicted values

predit_se_42<-predict(lr.fit.42,  predicted_data_42, type = "response", se.fit = T) #se.fitting to get CI for predicted values

predicted_data_42$lowerCI<-predit_se_42$fit-1.96*predit_se_42$se.fit
predicted_data_42$upperCI<-predit_se_42$fit+1.96*predit_se_42$se.fit


ggplot(data = log.reg.42,
       aes(x=doi_42,
           y=occultmet))+
  geom_point()+
  stat_smooth(method = "glm", se =TRUE, method.args = list(family=binomial))+
  xlab(label = "Depth of invasion(mm) [42]")+
  ylab(label = "Probability fo occult lymphnode metstasis")+
  theme_light()



baseline_scc_doi%>%
  select(Patient.IDID.du.patient, surgery_type,doi_cat,doi_125,doi_125_round, n_patho_stage, n_clinc_stage,primary_site)%>%
  filter(doi_cat== "true_doi" & !is.na(doi_125_round) & n_clinc_stage== "N0" & n_patho_stage != "Not Applicable")%>% 
  group_by(primary_site) %>% count()%>%view()


