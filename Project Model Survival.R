#Syntax R untuk Analisis Deskriptif
library(dplyr)
library(psych)
library(survminer)

# Read the data
data <- read.csv("heart_failure_clinical_records_dataset.csv") 

head(data)

# Summary statistics
summary(data)

# Descriptive statistics for each variable
describe(data)

# Frequency table for a categorical variable
table(data$categorical_variable)

# Histogram for a numeric variable
hist(data$numeric_variable)

# Box plot for a numeric variable
boxplot(data$numeric_variable)

# Correlation matrix
cor(data)

# Scatter plot for two numeric variables
plot(data$numeric_variable1, data$numeric_variable2)

ggsurvplot(St.Z1, conf.int=TRUE, pval=TRUE, risk.table=TRUE, legend.labs=c("0=age","1=anaemia","2=creatinine_phosphokinase","3=diabetes","4=ejection_fraction","5=high_blood_pressure","6=platelets","7=serum_creatinine","8=serum_sodium
","9=sex","10=smoking","11=time","12=DEATH_EVENT"),legend.title="Z1",palette=c("dodgerblue1","orchid1"),title="", risk.table.height=.3)

library(survival)
library(KMsurv)
library(survminer)
library(MASS)
km_model <- data %>%
  mutate(
    smoking = factor(ifelse(smoking == 0, "non-smoking", "smoking"))
  ) %>%
  survfit(Surv(time, DEATH_EVENT) ~ smoking, data = .)

ggsurvplot(km_model, data = data, risk.table = TRUE, 
           break.time.by = 10, size = 0.3, tables.height = 0.20)


library(survival)
library(KMsurv)
library(survminer)
library(MASS)
km_model <- data %>%
  mutate(
    smoking = factor(ifelse(diabetes == 0, "non-diabetes", "diabetes"))
  ) %>%
  survfit(Surv(time, DEATH_EVENT) ~ diabetes, data = .)

ggsurvplot(km_model, data = data, risk.table = TRUE, 
           break.time.by = 10, size = 0.3, tables.height = 0.20)


library(survival)
library(KMsurv)
library(survminer)
library(MASS)
km_model <- data %>%
  mutate(
    smoking = factor(ifelse(anaemia == 0, "non-anaemia", "anaemia"))
  ) %>%
  survfit(Surv(time, DEATH_EVENT) ~ anaemia, data = .)

ggsurvplot(km_model, data = data, risk.table = TRUE, 
           break.time.by = 10, size = 0.3, tables.height = 0.20)

#Syntax R untuk Regresi Cox - PH
library(tidyverse)
library(survival)
library(survminer)

#import dataset
df <- heart_failure_clinical_records_dataset

#Ubah variabel yang sebelumnya boolean (0,1) menjadi variabel kategorik
#dan set levelnya (yang jadi base factor) untuk model fitting yang baik
df <- df %>% 
  mutate(
    anaemia = factor(ifelse(anaemia == 1, "anaemic", "non-anaemic"), levels = c("non-anaemic", "anaemic")),
    diabetes = factor(ifelse(diabetes == 1, "diabetic", "non-diabetic"), levels = c("non-diabetic", "diabetic")),
    high_blood_pressure = factor(ifelse(high_blood_pressure == 1, "high-bp", "non-high-bp"), levels = c("non-high-bp", "high-bp")),
    sex = factor(ifelse(sex == 0, "female", "male"), levels = c("female", "male")),
    smoking = factor(ifelse(smoking == 0, "non-smoker", "smoker"), levels = c("non-smoker", "smoker")),
    platelets = platelets/1e4, 
    creatinine_phosphokinase = creatinine_phosphokinase/1e3
  )

#cek data
df %>% head

# Cox proportional hazard model
cox_model <- coxph(Surv(time, DEATH_EVENT) ~ age + anaemia + creatinine_phosphokinase + diabetes + ejection_fraction + 
                     high_blood_pressure + platelets + serum_creatinine + serum_sodium + smoking + sex, 
                   data = df)
summary(cox_model)

# Karena model telah fit, kita dapat menggunakannya untuk memplot probabilitas survival kumulatif dari suatu populasi.
ggsurvplot(survfit(cox_model), data = df, risk.table = TRUE, break.time.by = 10)
#Syntax R untuk Pengecekan Asumsi Proportional Hazard
df<-read.csv("C:\\Users\\rifqi\\Documents\\Semester 4\\Model Survival\\heart_failure_clinical_records_dataset.csv")



library(survival)
library(KMsurv)
library(survminer)
library(MASS)

#import dataset


#Ubah variabel yang sebelumnya boolean (0,1) menjadi variabel kategorik
#dan set levelnya (yang jadi base factor) untuk model fitting yang baik
df <- df %>% 
  mutate(
    anaemia = factor(ifelse(anaemia == 1, "anaemic", "non-anaemic"), levels = c("non-anaemic", "anaemic")),
    diabetes = factor(ifelse(diabetes == 1, "diabetic", "non-diabetic"), levels = c("non-diabetic", "diabetic")),
    high_blood_pressure = factor(ifelse(high_blood_pressure == 1, "high-bp", "non-high-bp"), levels = c("non-high-bp", "high-bp")),
    sex = factor(ifelse(sex == 0, "female", "male"), levels = c("female", "male")),
    smoking = factor(ifelse(smoking == 0, "non-smoker", "smoker"), levels = c("non-smoker", "smoker")),
    platelets = platelets/1e4, 
    creatinine_phosphokinase = creatinine_phosphokinase/1e3
  )

#cek data
df %>% head

# Cox proportional hazard model
cox_model <- coxph(Surv(time, DEATH_EVENT) ~ age + anaemia + serum_creatinine + serum_sodium +creatinine_phosphokinase + diabetes + ejection_fraction + 
                     high_blood_pressure + platelets + smoking + sex, 
                   data = df)
summary(cox_model)

# Plot the survival for a population with mean value of covariates
ggsurvplot(survfit(cox_model), data = df, risk.table = TRUE, break.time.by = 10)


fit.df<- survfit(Surv(time, DEATH_EVENT) ~ smoking, data = df)
plot(fit.df,fun="cloglog",lty=1:2,col=1:2, mark.time=FALSE,xlab="Waktu Survival (T)", ylab="log(H(t))",)

fit2.df<- survfit(Surv(time, DEATH_EVENT) ~ anaemia, data = df)
plot(fit2.df,fun="cloglog",lty=1:2,col=1:2, mark.time=FALSE,xlab="Waktu Survival (T)", ylab="log(H(t))")

fit3.df<- survfit(Surv(time, DEATH_EVENT) ~ diabetes, data = df)
plot(fit3.df,fun="cloglog",lty=1:2,col=1:2, mark.time=FALSE,xlab="Waktu Survival (T)", ylab="log(H(t))")




options(repr.plot.width = 18, repr.plot.height = 12)
ggcoxzph(cox.zph(cox_model))
cox.zph(cox_model)
