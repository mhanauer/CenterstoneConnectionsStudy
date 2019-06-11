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
library(BaylorEdPsych)
library(ggplot2)
library(HLMdiag)
library(psych)
library(MuMIn)
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
describe.factor(LivingWhere)
GPRAAll = subset(GPRAAll, InterviewType ==1)
GPRAAll$InterviewDate = as.Date(GPRAAll$InterviewDate, format = "%m/%d/%Y")
describe.factor(GPRAAll$LivingWhere)
dim(GPRAAll)

### Merge files
GPRAAll = merge(GPRAAll, LivingWhere_follow, by = "ClientID", all.x = TRUE)
dim(GPRAAll)
#### Get rid of those with housing before the program started
GPRAAll = subset(GPRAAll,LivingWhere !=4)
dim(GPRAAll)
describe.factor(GPRAAll$LivingWhere)
GPRAAll$LivingWhere_follow = ifelse(GPRAAll$LivingWhere_follow == 4, 1,0)

GPRAAll = subset(GPRAAll, Gender <= 2)
describe.factor(GPRAAll$Gender)
dim(GPRAAll)
# Try getting rid of this variable
GPRAAll$Gender = ifelse(GPRAAll$Gender == 1, 1,0)
GPRAAll$EducationYears = ifelse(GPRAAll$EducationYears > 11, 1, 0)
GPRAAll$County = ifelse(GPRAAll$County == "Monroe", 1, 0)


#Then I am subsetting the data for only those at the time of analysis that are eligible for reassessment.  Because the reassessment takes place every 6 months and the date for this analysis was 8-1-2018 anyone who entered the program later than 2-1-2018 would not be eligible for reassessments so they are not included.  InterviewDate.x equals the intake or baseline date.

describe.factor(GPRAAll$NrCrimes)
describe.factor(GPRAAll$ArrestedDays)
describe.factor(GPRAAll$ParoleProbation)

ConnPaper = data.frame(ClientID = GPRAAll$ClientID, InterviewDate = GPRAAll$InterviewDate, LivingWhere = GPRAAll$LivingWhere,  HealthStatus = GPRAAll$HealthStatus, Age = GPRAAll$Age, EducationYears = GPRAAll$EducationYears, Gender = GPRAAll$Gender, DAUseIllegDrugsDays = GPRAAll$DAUseIllegDrugsDays, County = GPRAAll$County, ERPhysical = GPRAAll$ERPhysical, ERMental = GPRAAll$ERMental, Ncrimes = GPRAAll$NrCrimes, ParoleProbation = GPRAAll$ParoleProbation, InpatientPhysical= GPRAAll$InpatientPhysical, InpatientMental = GPRAAll$InpatientMental, InpatientAlcoholSA = GPRAAll$InpatientAlcoholSA, OutpatientPhysical = GPRAAll$OutpatientPhysical, OutpatientMental = GPRAAll$OutpatientMental, OutpatientAlcoholSA = GPRAAll$OutpatientAlcoholSA, LivingWhere_follow = GPRAAll$LivingWhere_follow)


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
ConnPaper = merge(ConnPaper, PHQ9_GAD7, by = "ClientID", all = TRUE)
ConnPaper = subset(ConnPaper, InterviewDate < "2018-02-01")
dim(ConnPaper)
sum(is.na(ConnPaper$LivingWhere))
sum(is.na(ConnPaper$LivingWhere_follow))

## Combine the ER visits
describe.factor(ConnPaper$ERPhysical)
describe.factor(ConnPaper$ERMental)
describe.factor(ConnPaper$PHQ9Base)
ConnPaper$ER_visit = ifelse(ConnPaper$ERPhysical == 1, 1, ifelse(ConnPaper$ERMental == 1, 1,0))
describe.factor(ConnPaper$ER_visit)

## Combine the hosptial
ConnPaper$Hospital = ifelse(ConnPaper$InpatientPhysical == 1, 1, ifelse(ConnPaper$InpatientMental == 1,1, ifelse(ConnPaper$InpatientAlcoholSA == 1, 1, ifelse(ConnPaper$OutpatientPhysical== 1, 1, ifelse(ConnPaper$OutpatientMental ==1,1, ifelse(ConnPaper$OutpatientAlcoholSA  == 1, 1, 0))))))
describe.factor(ConnPaper$Hospital)



Conn_Base = ConnPaper[c("LivingWhere_follow", "HealthStatus", "Age", "EducationYears", "Gender", "DAUseIllegDrugsDays", "County", "PHQ9Base", "ER_visit", "Ncrimes", "ParoleProbation", "Hospital")]

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
TestMCARNormality(Conn_Base)
Conn_Base_complete = na.omit(Conn_Base)
dim(Conn_Base_complete)
1-(dim(Conn_Base_complete)[1] /dim(Conn_Base)[1])  
describe.factor(Conn_Base_complete$PHQ9Base)
Conn_Base_complete$PHQ9Base = as.numeric(Conn_Base_complete$PHQ9Base)
```
Get descirptives
```{r}
describe(Conn_Base_complete)
describe.factor(Conn_Base_complete$LivingWhere_follow)
describe.factor(Conn_Base_complete$EducationYears)
describe.factor(Conn_Base_complete$County)
describe.factor(Conn_Base_complete$ER_visit)
describe.factor(Conn_Base_complete$Hospital)
```
Now run the model
```{r}
library(MCMCpack)
bayes_logit_model = MCMClogit(LivingWhere_follow~ ., data = Conn_Base_complete)
summary(bayes_logit_model)

sum_model_bayes = summary(bayes_logit_model)
quant_exp= exp(sum_model_bayes$quantiles)
quant_exp

```
Look at diagnostics
```{r}
plot(bayes_logit_model)
```

