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
```


Load up PHQ-9 and GAD-7 Data
```{r}
PHQ9Base = read.spss("S:/Indiana Research & Evaluation/Indiana Connections/Data/PHQ9/PHQ9 Baseline.sav", to.data.frame = TRUE)
PHQ96month = read.spss("S:/Indiana Research & Evaluation/Indiana Connections/Data/PHQ9/PHQ9 6 Month.sav", to.data.frame = TRUE)
PHQ9All = merge(PHQ9Base, PHQ96month, by = "ParticipantID", all = TRUE)
GAD7Base = read.spss("S:/Indiana Research & Evaluation/Indiana Connections/Data/GAD7/GAD7 Baseline.sav", to.data.frame = TRUE)
GAD76month = read.spss("S:/Indiana Research & Evaluation/Indiana Connections/Data/GAD7/GAD7 6 Month.sav", to.data.frame = TRUE)
GAD7All = merge(GAD7Base, GAD76month, by = "ParticipantID", all = TRUE)
```
Load up the GPRA data and get the measures that Jon is interested in, the intake, 6-month, and housing variables.  
```{r}
setwd("S:/Indiana Research & Evaluation/Indiana Connections/Data/GPRA")
GPRAAll = read.csv("ConnGPRA.csv", header = TRUE) 
# subet the data based on 1's for baseline and 2's for six-month.  Then write as CSV's, then merge together.
GPRAConBase = subset(GPRAAll, InterviewType ==1)
GPRAConMonth6 = subset(GPRAAll, InterviewType == 2)
GPRAAll = merge(GPRAConBase, GPRAConMonth6, by = "ClientID", all = TRUE)
write.csv(GPRAAll, "GPRAAll.csv", row.names = FALSE)
GPRAAll = read.csv("GPRAAll.csv", header = TRUE, na.strings = c(-9, -8, -7, -1, "", " ", "NULL", NULL, NA, "NA"))
GPRAAll$InterviewDate.x = as.Date(GPRAAll$InterviewDate.x, format = "%m/%d/%Y") 
GPRAAll$InterviewDate.y = as.Date(GPRAAll$InterviewDate.y, format = "%m/%d/%Y")
head(GPRAAll)
```
Looks like these variables are the winners
age, education, sex, income
Add PHQ9 and GAD7
```{r}
ConnPaper = data.frame(ClientID = GPRAAll$ClientID, InterviewDate.x = GPRAAll$InterviewDate.x, Depression.x = GPRAAll$Depression.x, 	Anxiety.x = GPRAAll$Anxiety.x,	BrainFunction.x = GPRAAll$BrainFunction.x,	ViolentBehavior.x = GPRAAll$ViolentBehavior.x,	PhysicallyHurt.x = GPRAAll$PhysicallyHurt.x,	InteractFamilyFriends.x = GPRAAll$InteractFamilyFriends.x, Depression.y = GPRAAll$Depression.y, Anxiety.y=	GPRAAll$Anxiety.y,	BrainFunction.y = GPRAAll$BrainFunction.y,	ViolentBehavior.y = GPRAAll$ViolentBehavior.y,	PhysicallyHurt.y = GPRAAll$PhysicallyHurt.y,	InteractFamilyFriends.y = GPRAAll$InteractFamilyFriends.y, Employment.x = GPRAAll$Employment.x, Employment.y = GPRAAll$Employment.y,ArrestedDays.x = GPRAAll$ArrestedDays.x, ArrestedDays.y = GPRAAll$ArrestedDays.y,LivingWhere.x = GPRAAll$LivingWhere.x, LivingWhere.y = GPRAAll$LivingWhere.y, HealthStatus.x = GPRAAll$HealthStatus.x,HealthStatus.y = GPRAAll$HealthStatus.y, IncomeWages.x = GPRAAll$IncomeWages.x, Age.x = GPRAAll$Age.x, EducationYears.x = GPRAAll$EducationYears.x, Gender.x = GPRAAll$Gender.x)
dim(ConnPaper)
#summary(ConnPaper)
ConnPaperComplete = na.omit(ConnPaper)
dim(ConnPaperComplete)

## Geting the people that are eligible for reassessments
ConnPaper = subset(ConnPaper, InterviewDate.x < "2018-02-01")
dim(ConnPaper)

1-(dim(ConnPaperComplete)[1]/(dim(ConnPaper)[1]))
#summary(ConnPaperComplete)
ConnGPRA = ConnPaper
```
Load PHQ9 and GAD7 into one data set with client ID.  Then put them together with the GPRA data and see how much is missing.
```{r}
PHQ9Connections = data.frame(ClientID = PHQ9All$ParticipantID,PHQ9Base= PHQ9All$PHQ9Total.x, PHQ9Month6= PHQ9All$PHQ9Total.y)


GAD7Connections = data.frame(ClientID = GAD7All$ParticipantID, GAD7Base = GAD7All$GAD7Total.x, GAD7Month6 = GAD7All$GAD7Total.y)
PHQ9_GAD7 = merge(PHQ9Connections, GAD7Connections, by = "ClientID", all = TRUE)
head(PHQ9_GAD7)
```

Combine PHQ9 and GAD7, with interview data so we can exclude intake where no reassessment is due
```{r}
PHQ9_GAD7 = merge(ConnPaper, PHQ9_GAD7, by = "ClientID", all = TRUE)
dim(PHQ9_GAD7)
PHQ9_GAD7 = data.frame(ClientID = PHQ9_GAD7$ClientID, InterviewDate.x = PHQ9_GAD7$InterviewDate.x, PHQ9_GAD7[,23:30], )
dim(PHQ9_GAD7)
PHQ9_GAD7 = subset(PHQ9_GAD7, InterviewDate.x < "2018-02-01")
dim(PHQ9_GAD7)

PHQ9_GAD7Complete = na.omit(PHQ9_GAD7)
dim(PHQ9_GAD7Complete)
1-(dim(PHQ9_GAD7Complete)[1]/(dim(PHQ9_GAD7)[1]))

```
Put everything into long format for GPRA and PHQ9 and GAD7
```{r}
ConnGPRA = reshape(ConnGPRA, varying = list(c("Depression.x", "Depression.y"), c("Anxiety.x", "Anxiety.y"), c("BrainFunction.x", "BrainFunction.y"), c("ViolentBehavior.x", "ViolentBehavior.y"), c("PhysicallyHurt.x", "PhysicallyHurt.y"), c("Employment.x", "Employment.y"), c("ArrestedDays.x", "ArrestedDays.y"), c("LivingWhere.x", "LivingWhere.y"), c("HealthStatus.x", "HealthStatus.y")), times = c(0,1), direction = "long")


PHQ9_GAD7 = reshape(PHQ9_GAD7, varying = list(c("PHQ9Base", "PHQ9Month6"), c("GAD7Base", "GAD7Month6")), times = c(0,1), direction = "long")
```
Ok look dichotomize the house variable
```{r}

```



Ok maybe try with complete data just looking for those who were housed and not if there were changes across several outcomes
PHQ9First
```{r}
ConnPaperPHQ9 = data.frame(ClientID = ConnPaperAll$ClientID)

```




############### Here I have extra data describing how I can come to different conclusions ####################
These are the variables that Jon is most interested in: HealthStatus, LifeQuality, EnoughEnergyForEverydayLife, PerformDailyActivitiesSatisfaction, SelfSatisfaction, RelationshipSatisfaction, intake date, reassesment date, housing, LivingWhere ==  4 housed
```{r}
GPRAAll$InterviewDate.x = as.Date(GPRAAll$InterviewDate.x, format = "%m/%d/%Y") 
GPRAAll$InterviewDate.y = as.Date(GPRAAll$InterviewDate.y, format = "%m/%d/%Y")
GPRAJons = data.frame(ClientID = GPRAAll$ClientID, InterviewDate.x =GPRAAll$InterviewDate.x, InterviewDate.y = GPRAAll$InterviewDate.y, HealthStatus.x =  GPRAAll$HealthStatus.x,HealthStatus.y = GPRAAll$HealthStatus.y, LifeQuality.x = GPRAAll$LifeQuality.x, LifeQuality.y = GPRAAll$LifeQuality.y, EnoughEnergyForEverydayLife.x = GPRAAll$EnoughEnergyForEverydayLife.x, EnoughEnergyForEverydayLife.y = GPRAAll$EnoughEnergyForEverydayLife.y, PerformDailyActivitiesSatisfaction.x = GPRAAll$PerformDailyActivitiesSatisfaction.x, PerformDailyActivitiesSatisfaction.y = GPRAAll$PerformDailyActivitiesSatisfaction.y, SelfSatisfaction.x = GPRAAll$SelfSatisfaction.x, SelfSatisfaction.y = GPRAAll$SelfSatisfaction.y, RelationshipSatisfaction.x = GPRAAll$RelationshipSatisfaction.x, RelationshipSatisfaction.y = GPRAAll$RelationshipSatisfaction.y)

#check how much missing data there is for each variable
GPRAJonsMissingPrep = GPRAJons
GPRAJonsMissingPrep$InterviewDate.x = NULL
GPRAJonsMissingPrep$InterviewDate.y = NULL
GPRAJonsMissingPrep$ClientID = NULL
write.csv(GPRAJonsMissingPrep, "GPRAJonsMissingPrep.csv", row.names = FALSE)
GPRAJonsMissingPrep = read.csv("GPRAJonsMissingPrep.csv", header = TRUE)
GRPAllMissingCheck =  amelia(GPRAJonsMissingPrep)
summary(GRPAllMissingCheck)

# Ok so only health status enough data everything else is missing about 50%.

# Subseting any intake data that would not be eligible for reassessment so six months prior to 8-1-2018 so any intake after 2-1-2018 is no good.
# What if we get rid of missing data.  Then we figure out what people are not eligible so we don't exclude any early people. 
dim(GPRAJons)
GPRAJonsCompleteInelig = na.omit(GPRAJons)
dim(GPRAJonsCompleteInelig)

#GPRAJons_House
```
