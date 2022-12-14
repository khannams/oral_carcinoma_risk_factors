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

surv_tab<-baseline_scc_doi%>%
  select(Patient.IDID.du.patient, Event_final, enddate, final_follow_up,
         Pathological.stage.overall....Stage.Pathologique.Global, reason_death.y,
         surgery_type)%>%
  rename(global_patho_stage = Pathological.stage.overall....Stage.Pathologique.Global )

class(surv_tab$enddate)
class(surv_tab$final_follow_up)
#calculating follow-up time in months
surv_tab$time<- round((as.numeric(surv_tab$final_follow_up - surv_tab$enddate))*0.032855, digits = 2) #changing days to months
summary(surv_tab$time)

surv_tab[c(which(surv_tab$time>60)), ]%>% view()


#now the event of interest
table(surv_tab$Event_final, exclude = NULL)
table(surv_tab$reason_death.y, exclude = NULL)
surv_tab<-surv_tab%>%
  mutate(bin_event_final= case_when(surv_tab$Event_final %in% "disease free" ~ 0,
                                    surv_tab$Event_final %in% c("recurrence", "Expired") ~1))

table(surv_tab$Event_final, surv_tab$bin_event_final, exclude = NULL)

unique(surv_tab$global_patho_stage)

class(surv_tab$global_patho_stage)
surv_tab$global_patho_stage<-as.factor(surv_tab$global_patho_stage)
levels(surv_tab$global_patho_stage)


#fitting the simple survival model
library(survival)
s.obj<-Surv(time= surv_tab$time, event = surv_tab$bin_event_final)
s.obj
km.fit<-survfit(s.obj~1)

tiff(here::here("Graphs","01_surv_curve.tiff"),height = 20, width = 36, units = "cm", res=300)
plot(km.fit, mark.time = T, col="blue", lwd=2, xlab= "Months", ylab = "Proportion disease free",
     main= "Disease free survival time")
abline(h=0.5, col="red",lty=2,lwd=2)
dev.off()

#fitting the survival model by pathologic stage
#table to see how many recurrences in each category
xtabs(~bin_event_final+global_patho_stage, data = surv_tab )
library(survival)
km.fit2<-survfit(s.obj~surv_tab$global_patho_stage)

tiff(here::here("Graphs","01_surv_curve_patho_stage.tiff"),height = 20, width = 36, units = "cm", res=300)
plot(km.fit2, mark.time = T, lwd=2, xlab= "Months", ylab = "Proportion disease free",
     col = c("blue","darkviolet","green","sienna2","deeppink","black"),
     cex.axis=1.5, cex.lab=1.5, main= "Disease free survival time by pathologic stage")
abline(h=0.5, col="red",lty=2,lwd=4)
legend("bottom",legend = c("Stage I", "Stage II", "Stage III", "Stage IV A", "Stage IV B", "Stage IV C"),
       lty = c(1,1),lwd = c(2,2), col = c("blue","darkviolet","green","sienna2","deeppink","black"), cex = 1,
       ncol = 2, title = "Pathologic stage")
dev.off()




##Time to event analysis
#running a log rank test to see if the survival curves between the two groups ie. neck dissection and no neck dissection is different
class(surv_tab$surgery_type)
surv_tab$surgery_type<-factor(surv_tab$surgery_type)
levels(surv_tab$surgery_type)

xtabs(~bin_event_final+surgery_type, data = surv_tab )
km.fit3<-survfit(s.obj~surv_tab$surgery_type)

tiff(here::here("Graphs","01_surv_curve_surgeryType.tiff"),height = 20, width = 36, units = "cm", res=300)
plot(km.fit3, mark.time = T, lwd=2, xlab= "Months", ylab = "Proportion disease free",
     col = c("blue","green"),
     cex.axis=1.5, cex.lab=1.5, main= "Disease free survival time by type of surgery")
abline(h=0.5, col="red",lty=2,lwd=4)
legend("bottom",legend = c("Neck dissection","No neck dissection"),
       lty = c(1,1),lwd = c(2,2), col = c("blue","green"), cex = 1,
       ncol = 2, title = "Type of Surgery")
dev.off()

#to check difference between groups
survdiff(s.obj~surv_tab$surgery_type)
# Our p-value is p = 0.004. If we test at level = 0.05, then we would reject the null hypothesis. So we have evidence that 
#the survival curves are different for patients who underwent neck dissection and those who did not.

#now lets see the magnitude of difference between he curves using cox proportional model
cph.fit<-coxph(s.obj~surv_tab$surgery_type)
summary(cph.fit)

#The hazard rate for patients who did not undergo neck dissection is 0.36 (CI 95% 0.17 to 0.75) times as the ones who underwent neck dissection.
