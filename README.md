---
title: "ConnectionsStudy"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Load stats packages
```{r}
library(Amelia)
library(prettyR)
library(nlme)
library(descr)
library(foreign)
library(lme4)
library(sjstats)
library(MissMech)
library(ggplot2)
library(psych)
```
Loading the data and reading it in
Creating living where seperate to merge later from 6-month because I need to match the data and it is in wide form.
Then get just baseline data for GPRA and code variables (double check my coding)
Then grab all the variables that I want and put into a seperate data set, because if I na.omit whole data set it will result in no data.
Then get rid of anyone who is not eligible for a 6-month to reduce missing data
```{r}
setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/ConnectionsPaperData")
GPRAAll = read.csv("ConnGPRA.csv", header = TRUE, na.strings = c(-9, -8, -7, -1, "", " ", "NULL", NULL, NA, "NA")) 
# subet the data based on 1's for baseline and 2's for six-month.  Then write as CSV's, then merge together.
# Create living where follow up variable to merge later so need ID and living where variable
LivingWhere_follow = subset(GPRAAll, InterviewType ==2)
LivingWhere_follow =data.frame(ClientID = LivingWhere_follow$ClientID, LivingWhere_follow = LivingWhere_follow$LivingWhere)
GPRAAll = subset(GPRAAll, InterviewType ==1)
GPRAAll$InterviewDate = as.Date(GPRAAll$InterviewDate, format = "%m/%d/%Y")
describe.factor(GPRAAll$LivingWhere)
dim(GPRAAll)
dim(LivingWhere_follow)
### Merge files
GPRAAll = merge(GPRAAll, LivingWhere_follow, by = "ClientID", all.y = TRUE)
dim(GPRAAll)
#######
GPRAAll$ClientID == LivingWhere_follow$ClientID

#### Get rid of those with housing before the program started
GPRAAll = subset(GPRAAll,LivingWhere !=4)
dim(GPRAAll)
describe.factor(GPRAAll$LivingWhere_follow)
GPRAAll$LivingWhere_follow = ifelse(GPRAAll$LivingWhere_follow == 4, 1,0)

GPRAAll = subset(GPRAAll, Gender <= 2)
describe.factor(GPRAAll$Gender)
dim(GPRAAll)
# Try getting rid of this variable
GPRAAll$Gender = ifelse(GPRAAll$Gender == 1, 1,0)
GPRAAll$EducationYears = ifelse(GPRAAll$EducationYears > 11, 1, 0)
GPRAAll$County = ifelse(GPRAAll$County == "Monroe", 1, 0)

### Employment status
#EmployStatus
#1 = Employed Full Time (35+ hours per week, or would have been)2 = Employed Part Time 3 = Unemployed, looking for work4 = Unemployed, disabled5 = Unemployed, volunteer work6 = Unemployed, retired 7 = Unemployed, not looking for work0 = Other-7 = Refused-8 = Don't Know-9 =Missing Data
describe.factor(GPRAAll$EmployStatus)
part_full_employ = ifelse(GPRAAll$EmployStatus == 1, 1, ifelse(GPRAAll$EmployStatus == 2, 1, 0))
#Then I am subsetting the data for only those at the time of analysis that are eligible for reassessment.  Because the reassessment takes place every 6 months and the date for this analysis was 8-1-2018 anyone who entered the program later than 2-1-2018 would not be eligible for reassessments so they are not included.  InterviewDate.x equals the intake or baseline date.

describe.factor(GPRAAll$NrCrimes)
describe.factor(GPRAAll$ArrestedDays)
describe.factor(GPRAAll$ParoleProbation)

ConnPaper = data.frame(ClientID = GPRAAll$ClientID, InterviewDate = GPRAAll$InterviewDate, LivingWhere = GPRAAll$LivingWhere,  HealthStatus = GPRAAll$HealthStatus, Age = GPRAAll$Age, EducationYears = GPRAAll$EducationYears, Gender = GPRAAll$Gender, DAUseIllegDrugsDays = GPRAAll$DAUseIllegDrugsDays, County = GPRAAll$County, ERPhysical = GPRAAll$ERPhysical, ERMental = GPRAAll$ERMental, ERAlcoholSA = GPRAAll$ERAlcoholSA, Ncrimes = GPRAAll$NrCrimes, ParoleProbation = GPRAAll$ParoleProbation, InpatientPhysical= GPRAAll$InpatientPhysical, InpatientMental = GPRAAll$InpatientMental, InpatientAlcoholSA = GPRAAll$InpatientAlcoholSA, OutpatientPhysical = GPRAAll$OutpatientPhysical, OutpatientMental = GPRAAll$OutpatientMental, OutpatientAlcoholSA = GPRAAll$OutpatientAlcoholSA, LivingWhere_follow = GPRAAll$LivingWhere_follow, part_full_employ = part_full_employ)


dim(ConnPaper)
#Here I am loading up both the PHQ9 and GAD7 baselines and 6-month reassessments together.  I am using the merge function, which allows me to combine different data sets using a unique identifier common to both data sets.

PHQ9Base = read.spss("S:/Indiana Research & Evaluation/Indiana Connections/Data/PHQ9/PHQ9 Baseline.sav", to.data.frame = TRUE)
describe.factor(PHQ9Base$PHQ9Total) 
GAD7Base = read.spss("S:/Indiana Research & Evaluation/Indiana Connections/Data/GAD7/GAD7 Baseline.sav", to.data.frame = TRUE)

#Now from PHQ9 and GAD7 data sets, I am grabbing only the total scores from both assessments along with the id's and changing the name to ClientID so we can merge it with the GPRA data set.
PHQ9Connections = data.frame(ClientID = PHQ9Base$ParticipantID,PHQ9Base= PHQ9Base$PHQ9Total)
GAD7Connections = data.frame(ClientID = GAD7Base$ParticipantID, GAD7Base = GAD7Base$GAD7Total)

PHQ9_GAD7 = merge(PHQ9Connections, GAD7Connections, by = "ClientID", all = TRUE)
#Now I am merging the ConnPaper data set with the GPRA data set using the ClientID.  all = TRUE means that all ClientID's will be included from both datasets.
# Only including those people from the baseline who are eligible for 6-month so missing data is not inflated
dim(PHQ9_GAD7)
dim(ConnPaper)
ConnPaper = merge(ConnPaper, PHQ9_GAD7, by = "ClientID", all.x = TRUE)
dim(ConnPaper)


ConnPaper = subset(ConnPaper, InterviewDate < "2018-02-01")
dim(ConnPaper)
sum(is.na(ConnPaper$LivingWhere_follow))

## Combine the ER visits
describe.factor(ConnPaper$ERPhysical)
describe.factor(ConnPaper$ERMental)
describe.factor(ConnPaper$PHQ9Base)


describe.factor(ConnPaper$ER_visit)




ConnPaper$acute = ifelse(ConnPaper$InpatientPhysical == 1, 1, ifelse(ConnPaper$InpatientMental == 1,1, ifelse(ConnPaper$InpatientAlcoholSA == 1, 1,ifelse(ConnPaper$ERPhysical == 1, 1, ifelse(ConnPaper$ERMental == 1, 1,ifelse(ConnPaper$ERAlcoholSA == 1,1,0))))))
describe.factor(ConnPaper$acute)

ConnPaper$Hospital_out = ifelse(ConnPaper$OutpatientPhysical== 1, 1, ifelse(ConnPaper$OutpatientMental ==1,1, ifelse(ConnPaper$OutpatientAlcoholSA  == 1, 1,0)))
describe.factor(ConnPaper$Hospital_out)

head(ConnPaper)

Conn_Base = ConnPaper[c("LivingWhere_follow", "HealthStatus", "Age", "EducationYears", "Gender", "DAUseIllegDrugsDays", "County", "PHQ9Base", "Ncrimes", "ParoleProbation", "acute", "Hospital_out", "part_full_employ")]
describe.factor(Conn_Base$PHQ9Base)
```
Just get the baseline
Check VIFS and get descriptives for this
Check the interactions
```{r}
head(Conn_Base)
Conn_Base = data.frame(Conn_Base)
write.csv(Conn_Base, "Conn_Base.csv", row.names = FALSE)
Conn_Base = read.csv("Conn_Base.csv", header = TRUE)

#TestMCARNormality(Conn_Base)
Conn_Base_complete = na.omit(Conn_Base)
dim(Conn_Base_complete)
1-(dim(Conn_Base_complete)[1] /dim(Conn_Base)[1])  
Conn_Base_complete$PHQ9Base = as.numeric(Conn_Base_complete$PHQ9Base)
```
Get descirptives
```{r}
describe(Conn_Base_complete)
describe.factor(Conn_Base_complete$LivingWhere_follow)
describe.factor(Conn_Base_complete$EducationYears)
describe.factor(Conn_Base_complete$County)
describe.factor(Conn_Base_complete$ER_visit)
#describe.factor(Conn_Base_complete$Hospital)


```
Now run the model
```{r}
library(rstanarm)
head(Conn_Base_complete)
Conn_Base_complete$part_full_employ = NULL
bayes_logit_model = stan_glm(LivingWhere_follow~ ., data = Conn_Base_complete, family = binomial (link = "logit"))
bayes_sum =round(bayes_logit_model$stan_summary[,c(1,3,4,10, 11)],4)
bayes_sum = round(data.frame(bayes_sum[,1:2], odd_ratio = exp(bayes_sum[,1]), Odds_Lower = exp(bayes_sum[,3]), Odds_Upper = exp(bayes_sum[,4]), Eff = bayes_sum[,5]),3)
bayes_sum 
```
Look at diagnostics
```{r}
### Bayesian R^2
launch_shinystan(bayes_logit_model)
median(bayes_R2(bayes_logit_model))
```
Test interactions
```{r}
library(rstanarm)

### Health status
bayes_logit_model = stan_glm(LivingWhere_follow~ HealthStatus*DAUseIllegDrugsDays + Age + EducationYears + Gender + County + DAUseIllegDrugsDays + PHQ9Base + ER_visit + Ncrimes + ParoleProbation + Hospital + part_full_employ, data = Conn_Base_complete, family = binomial (link = "logit"))
summary(bayes_logit_model)


bayes_logit_model = stan_glm(LivingWhere_follow~ HealthStatus*PHQ9Base + DAUseIllegDrugsDays + Age + EducationYears + Gender + County + PHQ9Base + ER_visit + Ncrimes + ParoleProbation + Hospital + part_full_employ, data = Conn_Base_complete, family = binomial (link = "logit"))
summary(bayes_logit_model)


bayes_logit_model = stan_glm(LivingWhere_follow~ HealthStatus*ER_visit + PHQ9Base + DAUseIllegDrugsDays + Age + EducationYears + Gender + County  + Ncrimes + ParoleProbation + Hospital + part_full_employ, data = Conn_Base_complete, family = binomial (link = "logit"))
summary(bayes_logit_model)


bayes_logit_model = stan_glm(LivingWhere_follow~ HealthStatus*Ncrimes + ER_visit + PHQ9Base + DAUseIllegDrugsDays + Age + EducationYears + Gender + County + ParoleProbation + Hospital + part_full_employ, data = Conn_Base_complete, family = binomial (link = "logit"))
summary(bayes_logit_model)

bayes_logit_model = stan_glm(LivingWhere_follow~ HealthStatus*Ncrimes + ER_visit + PHQ9Base + DAUseIllegDrugsDays + Age + EducationYears + Gender + County + ParoleProbation + Hospital + part_full_employ, data = Conn_Base_complete, family = binomial (link = "logit"))
summary(bayes_logit_model)


bayes_logit_model = stan_glm(LivingWhere_follow~ HealthStatus*ParoleProbation + Ncrimes + ER_visit + PHQ9Base + DAUseIllegDrugsDays + Age + EducationYears + Gender + County +  + Hospital + part_full_employ, data = Conn_Base_complete, family = binomial (link = "logit"))
summary(bayes_logit_model)

bayes_logit_model = stan_glm(LivingWhere_follow~ HealthStatus*Hospital + ParoleProbation + Ncrimes + ER_visit + PHQ9Base + DAUseIllegDrugsDays + Age + EducationYears + Gender + County + part_full_employ, data = Conn_Base_complete, family = binomial (link = "logit"))
summary(bayes_logit_model)


bayes_logit_model = stan_glm(LivingWhere_follow~ HealthStatus*part_full_employ + Hospital + ParoleProbation + Ncrimes + ER_visit + PHQ9Base + DAUseIllegDrugsDays + Age + EducationYears + Gender + County , data = Conn_Base_complete, family = binomial (link = "logit"))
summary(bayes_logit_model)


## AGe
bayes_logit_model = stan_glm(LivingWhere_follow~ Age*DAUseIllegDrugsDays + HealthStatus + EducationYears + Gender + County + DAUseIllegDrugsDays + PHQ9Base + ER_visit + Ncrimes + ParoleProbation + Hospital + part_full_employ, data = Conn_Base_complete, family = binomial (link = "logit"))
summary(bayes_logit_model)


bayes_logit_model = stan_glm(LivingWhere_follow~ Age*PHQ9Base + DAUseIllegDrugsDays + HealthStatus + EducationYears + Gender + County + PHQ9Base + ER_visit + Ncrimes + ParoleProbation + Hospital + part_full_employ, data = Conn_Base_complete, family = binomial (link = "logit"))
summary(bayes_logit_model)


bayes_logit_model = stan_glm(LivingWhere_follow~ Age*ER_visit + PHQ9Base + DAUseIllegDrugsDays + HealthStatus + EducationYears + Gender + County  + Ncrimes + ParoleProbation + Hospital + part_full_employ, data = Conn_Base_complete, family = binomial (link = "logit"))
summary(bayes_logit_model)

bayes_logit_model = stan_glm(LivingWhere_follow~ Age*Ncrimes + ER_visit + PHQ9Base + DAUseIllegDrugsDays + HealthStatus + EducationYears + Gender + County + ParoleProbation + Hospital + part_full_employ, data = Conn_Base_complete, family = binomial (link = "logit"))
summary(bayes_logit_model)

bayes_logit_model = stan_glm(LivingWhere_follow~ Age*Ncrimes + ER_visit + PHQ9Base + DAUseIllegDrugsDays + HealthStatus + EducationYears + Gender + County + ParoleProbation + Hospital + part_full_employ, data = Conn_Base_complete, family = binomial (link = "logit"))
summary(bayes_logit_model)


bayes_logit_model = stan_glm(LivingWhere_follow~ Age*ParoleProbation + Ncrimes + ER_visit + PHQ9Base + DAUseIllegDrugsDays + HealthStatus + EducationYears + Gender + County +  + Hospital + part_full_employ, data = Conn_Base_complete, family = binomial (link = "logit"))
summary(bayes_logit_model)

bayes_logit_model = stan_glm(LivingWhere_follow~ Age*Hospital + ParoleProbation + Ncrimes + ER_visit + PHQ9Base + DAUseIllegDrugsDays + HealthStatus + EducationYears + Gender + County + part_full_employ, data = Conn_Base_complete, family = binomial (link = "logit"))
summary(bayes_logit_model)


bayes_logit_model = stan_glm(LivingWhere_follow~ Age*part_full_employ + Hospital + ParoleProbation + Ncrimes + ER_visit + PHQ9Base + DAUseIllegDrugsDays + HealthStatus + EducationYears + Gender + County , data = Conn_Base_complete, family = binomial (link = "logit"))
summary(bayes_logit_model)

### Edu
bayes_logit_model = stan_glm(LivingWhere_follow~ EducationYears*DAUseIllegDrugsDays + HealthStatus + Age+ Gender + County + DAUseIllegDrugsDays + PHQ9Base + ER_visit + Ncrimes + ParoleProbation + Hospital + part_full_employ, data = Conn_Base_complete, family = binomial (link = "logit"))
summary(bayes_logit_model)


bayes_logit_model = stan_glm(LivingWhere_follow~ EducationYears*PHQ9Base + DAUseIllegDrugsDays + HealthStatus + Age+ Gender + County + PHQ9Base + ER_visit + Ncrimes + ParoleProbation + Hospital + part_full_employ, data = Conn_Base_complete, family = binomial (link = "logit"))
summary(bayes_logit_model)


bayes_logit_model = stan_glm(LivingWhere_follow~ EducationYears*ER_visit + PHQ9Base + DAUseIllegDrugsDays + HealthStatus + Age+ Gender + County  + Ncrimes + ParoleProbation + Hospital + part_full_employ, data = Conn_Base_complete, family = binomial (link = "logit"))
summary(bayes_logit_model)


bayes_logit_model = stan_glm(LivingWhere_follow~ EducationYears*Ncrimes + ER_visit + PHQ9Base + DAUseIllegDrugsDays + HealthStatus + Age+ Gender + County + ParoleProbation + Hospital + part_full_employ, data = Conn_Base_complete, family = binomial (link = "logit"))
summary(bayes_logit_model)

bayes_logit_model = stan_glm(LivingWhere_follow~ EducationYears*Ncrimes + ER_visit + PHQ9Base + DAUseIllegDrugsDays + HealthStatus + Age+ Gender + County + ParoleProbation + Hospital + part_full_employ, data = Conn_Base_complete, family = binomial (link = "logit"))
summary(bayes_logit_model)


bayes_logit_model = stan_glm(LivingWhere_follow~ EducationYears*ParoleProbation + Ncrimes + ER_visit + PHQ9Base + DAUseIllegDrugsDays + HealthStatus + Age+ Gender + County +  + Hospital + part_full_employ, data = Conn_Base_complete, family = binomial (link = "logit"))
summary(bayes_logit_model)

bayes_logit_model = stan_glm(LivingWhere_follow~ EducationYears*Hospital + ParoleProbation + Ncrimes + ER_visit + PHQ9Base + DAUseIllegDrugsDays + HealthStatus + Age+ Gender + County + part_full_employ, data = Conn_Base_complete, family = binomial (link = "logit"))
summary(bayes_logit_model)


bayes_logit_model = stan_glm(LivingWhere_follow~ EducationYears*part_full_employ + Hospital + ParoleProbation + Ncrimes + ER_visit + PHQ9Base + DAUseIllegDrugsDays + HealthStatus + Age+ Gender + County , data = Conn_Base_complete, family = binomial (link = "logit"))
summary(bayes_logit_model)

## Gender
bayes_logit_model = stan_glm(LivingWhere_follow~ Gender*DAUseIllegDrugsDays + HealthStatus + Age+ EducationYears+ County + DAUseIllegDrugsDays + PHQ9Base + ER_visit + Ncrimes + ParoleProbation + Hospital + part_full_employ, data = Conn_Base_complete, family = binomial (link = "logit"))
summary(bayes_logit_model)


bayes_logit_model = stan_glm(LivingWhere_follow~ Gender*PHQ9Base + DAUseIllegDrugsDays + HealthStatus + Age+ EducationYears+ County + PHQ9Base + ER_visit + Ncrimes + ParoleProbation + Hospital + part_full_employ, data = Conn_Base_complete, family = binomial (link = "logit"))
summary(bayes_logit_model)


bayes_logit_model = stan_glm(LivingWhere_follow~ Gender*ER_visit + PHQ9Base + DAUseIllegDrugsDays + HealthStatus + Age+ EducationYears+ County  + Ncrimes + ParoleProbation + Hospital + part_full_employ, data = Conn_Base_complete, family = binomial (link = "logit"))
summary(bayes_logit_model)


bayes_logit_model = stan_glm(LivingWhere_follow~ Gender*Ncrimes + ER_visit + PHQ9Base + DAUseIllegDrugsDays + HealthStatus + Age+ EducationYears+ County + ParoleProbation + Hospital + part_full_employ, data = Conn_Base_complete, family = binomial (link = "logit"))
summary(bayes_logit_model)

bayes_logit_model = stan_glm(LivingWhere_follow~ Gender*Ncrimes + ER_visit + PHQ9Base + DAUseIllegDrugsDays + HealthStatus + Age+ EducationYears+ County + ParoleProbation + Hospital + part_full_employ, data = Conn_Base_complete, family = binomial (link = "logit"))
summary(bayes_logit_model)


bayes_logit_model = stan_glm(LivingWhere_follow~ Gender*ParoleProbation + Ncrimes + ER_visit + PHQ9Base + DAUseIllegDrugsDays + HealthStatus + Age+ EducationYears+ County +  + Hospital + part_full_employ, data = Conn_Base_complete, family = binomial (link = "logit"))
summary(bayes_logit_model)

bayes_logit_model = stan_glm(LivingWhere_follow~ Gender*Hospital + ParoleProbation + Ncrimes + ER_visit + PHQ9Base + DAUseIllegDrugsDays + HealthStatus + Age+ EducationYears+ County + part_full_employ, data = Conn_Base_complete, family = binomial (link = "logit"))
summary(bayes_logit_model)


bayes_logit_model = stan_glm(LivingWhere_follow~ Gender*part_full_employ + Hospital + ParoleProbation + Ncrimes + ER_visit + PHQ9Base + DAUseIllegDrugsDays + HealthStatus + Age+ EducationYears+ County , data = Conn_Base_complete, family = binomial (link = "logit"))
summary(bayes_logit_model)

### County
bayes_logit_model = stan_glm(LivingWhere_follow~ County*DAUseIllegDrugsDays + HealthStatus + Age+ EducationYears+ Gender+ DAUseIllegDrugsDays + PHQ9Base + ER_visit + Ncrimes + ParoleProbation + Hospital + part_full_employ, data = Conn_Base_complete, family = binomial (link = "logit"))
summary(bayes_logit_model)


bayes_logit_model = stan_glm(LivingWhere_follow~ County*PHQ9Base + DAUseIllegDrugsDays + HealthStatus + Age+ EducationYears+ Gender+ PHQ9Base + ER_visit + Ncrimes + ParoleProbation + Hospital + part_full_employ, data = Conn_Base_complete, family = binomial (link = "logit"))
summary(bayes_logit_model)


bayes_logit_model = stan_glm(LivingWhere_follow~ County*ER_visit + PHQ9Base + DAUseIllegDrugsDays + HealthStatus + Age+ EducationYears+ Gender + Ncrimes + ParoleProbation + Hospital + part_full_employ, data = Conn_Base_complete, family = binomial (link = "logit"))
summary(bayes_logit_model)


bayes_logit_model = stan_glm(LivingWhere_follow~ County*Ncrimes + ER_visit + PHQ9Base + DAUseIllegDrugsDays + HealthStatus + Age+ EducationYears+ Gender+ ParoleProbation + Hospital + part_full_employ, data = Conn_Base_complete, family = binomial (link = "logit"))
summary(bayes_logit_model)

bayes_logit_model = stan_glm(LivingWhere_follow~ County*Ncrimes + ER_visit + PHQ9Base + DAUseIllegDrugsDays + HealthStatus + Age+ EducationYears+ Gender+ ParoleProbation + Hospital + part_full_employ, data = Conn_Base_complete, family = binomial (link = "logit"))
summary(bayes_logit_model)


bayes_logit_model = stan_glm(LivingWhere_follow~ County*ParoleProbation + Ncrimes + ER_visit + PHQ9Base + DAUseIllegDrugsDays + HealthStatus + Age+ EducationYears+ Gender+  + Hospital + part_full_employ, data = Conn_Base_complete, family = binomial (link = "logit"))
summary(bayes_logit_model)

bayes_logit_model = stan_glm(LivingWhere_follow~ County*Hospital + ParoleProbation + Ncrimes + ER_visit + PHQ9Base + DAUseIllegDrugsDays + HealthStatus + Age+ EducationYears+ Gender+ part_full_employ, data = Conn_Base_complete, family = binomial (link = "logit"))
summary(bayes_logit_model)


bayes_logit_model = stan_glm(LivingWhere_follow~ County*part_full_employ + Hospital + ParoleProbation + Ncrimes + ER_visit + PHQ9Base + DAUseIllegDrugsDays + HealthStatus + Age+ EducationYears+ Gender, data = Conn_Base_complete, family = binomial (link = "logit"))
summary(bayes_logit_model)



```



