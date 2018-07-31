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
These are the variables in the GPRA that have the least missing data
```{r}
ConnPaper = data.frame(ClientID = GPRAAll$ClientID, InterviewDate.x = GPRAAll$InterviewDate.x, Depression.x = GPRAAll$Depression.x, 	Anxiety.x = GPRAAll$Anxiety.x,	BrainFunction.x = GPRAAll$BrainFunction.x,	ViolentBehavior.x = GPRAAll$ViolentBehavior.x,	PhysicallyHurt.x = GPRAAll$PhysicallyHurt.x,	InteractFamilyFriends.x = GPRAAll$InteractFamilyFriends.x, Depression.y = GPRAAll$Depression.y, Anxiety.y=	GPRAAll$Anxiety.y,	BrainFunction.y = GPRAAll$BrainFunction.y,	ViolentBehavior.y = GPRAAll$ViolentBehavior.y,	PhysicallyHurt.y = GPRAAll$PhysicallyHurt.y,	InteractFamilyFriends.y = GPRAAll$InteractFamilyFriends.y, Employment.x = GPRAAll$Employment.x, Employment.y = GPRAAll$Employment.y,ArrestedDays.x = GPRAAll$ArrestedDays.x, ArrestedDays.y = GPRAAll$ArrestedDays.y,LivingWhere.x = GPRAAll$LivingWhere.x, LivingWhere.y = GPRAAll$LivingWhere.y, HealthStatus.x = GPRAAll$HealthStatus.x,HealthStatus.y = GPRAAll$HealthStatus.y, IncomeWages.x = GPRAAll$IncomeWages.x, Age.x = GPRAAll$Age.x, EducationYears.x = GPRAAll$EducationYears.x, Gender.x = GPRAAll$Gender.x, DAUseIllegDrugsDays.x = GPRAAll$DAUseIllegDrugsDays.x, DAUseIllegDrugsDays.y = GPRAAll$DAUseIllegDrugsDays.y)
dim(ConnPaper)

# Getting a complete data set for comparision.  Then gettting rid of the those who are not eligiable
ConnPaperComplete = na.omit(ConnPaper)
ConnPaperComplete = subset(ConnPaperComplete, InterviewDate.x < "2018-02-01")
dim(ConnPaperComplete)

## Geting the people that are eligible for reassessments
ConnPaper = subset(ConnPaper, InterviewDate.x < "2018-02-01")
dim(ConnPaper)

1-(dim(ConnPaperComplete)[1]/(dim(ConnPaper)[1]))
#summary(ConnPaperComplete)
ConnGPRA =  ConnPaper
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

Load PHQ9 and GAD7 into one data set with client ID.  Then put them together with the GPRA data and see how much is missing.
```{r}
PHQ9Connections = data.frame(ClientID = PHQ9All$ParticipantID,PHQ9Base= PHQ9All$PHQ9Total.x, PHQ9Month6= PHQ9All$PHQ9Total.y)


GAD7Connections = data.frame(ClientID = GAD7All$ParticipantID, GAD7Base = GAD7All$GAD7Total.x, GAD7Month6 = GAD7All$GAD7Total.y)
PHQ9_GAD7 = merge(PHQ9Connections, GAD7Connections, by = "ClientID", all = TRUE)
head(PHQ9_GAD7)
```

Combine PHQ9 and GAD7, with interview date so we can exclude intake where no reassessment is due and housing
```{r}
PHQ9_GAD7 = merge(ConnPaper, PHQ9_GAD7, by = "ClientID", all = TRUE)
dim(PHQ9_GAD7)
PHQ9_GAD7 = data.frame(ClientID = PHQ9_GAD7$ClientID, InterviewDate.x = PHQ9_GAD7$InterviewDate.x, LivingWhere.y= PHQ9_GAD7$LivingWhere.y, PHQ9_GAD7[,23:32])
dim(PHQ9_GAD7)
PHQ9_GAD7 = subset(PHQ9_GAD7, InterviewDate.x < "2018-02-01")
dim(PHQ9_GAD7)

PHQ9_GAD7Complete = na.omit(PHQ9_GAD7)
dim(PHQ9_GAD7Complete)
1-(dim(PHQ9_GAD7Complete)[1]/(dim(PHQ9_GAD7)[1]))
```
Data Cleaning: Changing housing to yes or no for both data sets PHQ9_GAD7 and GPRA.  Only care about the 6 month reassessment, because we want to know if they housed eventually
```{r}
describe.factor(PHQ9_GAD7$LivingWhere.y)
PHQ9_GAD7$LivingWhere.y = ifelse(PHQ9_GAD7$LivingWhere.y == 4, 1, 0)
ConnGPRA$LivingWhere.y = ifelse(ConnGPRA$LivingWhere.y == 4,1,0)
describe.factor(PHQ9_GAD7$LivingWhere.y)


describe.factor(ConnGPRA$LivingWhere.y)
```


Put everything into long format for GPRA and PHQ9 and GAD7
```{r}
ConnGPRALong = reshape(ConnGPRA, varying = list(c("Depression.x", "Depression.y"), c("Anxiety.x", "Anxiety.y"), c("BrainFunction.x", "BrainFunction.y"), c("ViolentBehavior.x", "ViolentBehavior.y"), c("PhysicallyHurt.x", "PhysicallyHurt.y"), c("Employment.x", "Employment.y"), c("ArrestedDays.x", "ArrestedDays.y"), c("HealthStatus.x", "HealthStatus.y"), c("DAUseIllegDrugsDays.x", "DAUseIllegDrugsDays.y")), times = c(0,1), direction = "long")
head(ConnGPRALong)

PHQ9_GAD7Long = reshape(PHQ9_GAD7, varying = list(c("PHQ9Base", "PHQ9Month6"), c("GAD7Base", "GAD7Month6"), c("DAUseIllegDrugsDays.x", "DAUseIllegDrugsDays.y")), times = c(0,1), direction = "long")

```
Ok look dichotomize the house variable just trying the PHQ9 and GAD7 continous variable
Then simple multilevel model 
```{r}
PHQ9_GAD7AnalysisLong = na.omit(PHQ9_GAD7Long)
write.csv(PHQ9_GAD7AnalysisLong, "PHQ9_GAD7AnalysisLong.csv", row.names = FALSE)
PHQ9_GAD7AnalysisLong = read.csv("PHQ9_GAD7AnalysisLong.csv", header = TRUE)

modelPHQ9Multi = lme(PHQ9Base ~ LivingWhere.y*time, random = ~1 | ClientID, data = PHQ9_GAD7AnalysisLong)
summary(modelPHQ9Multi)

#Not enough data for random slopes and intercepts
#modelPHQ9MultiRanBoth = lme(PHQ9Base ~ LivingWhere.y*time, random = ~time | ClientID, data = PHQ9_GAD7AnalysisLong)

modelGAD7Multi = lme(GAD7Base ~ LivingWhere.y*time, random = ~1 | ClientID, data = PHQ9_GAD7AnalysisLong)
summary(modelGAD7Multi)

```
Data Cleaning: Getting the ready for t.tests and linear regression so need wide format
Data Analysis: Running t-tests, testing assumptions of normality, and linear regression with housing at the covariate; 
Variables: Dependent = PHQ9, GAD7; Independent: Housing
```{r}
PHQ9_GAD7DiffLin = PHQ9_GAD7
PHQ9_GAD7DiffLin = na.omit(PHQ9_GAD7DiffLin)
write.csv(PHQ9_GAD7DiffLin, "PHQ9_GAD7DiffLin.csv", row.names = FALSE)
PHQ9_GAD7DiffLin = read.csv("PHQ9_GAD7DiffLin.csv", header = TRUE)
PHQ9_GAD7DiffLin$PHQ9Diff = PHQ9_GAD7DiffLin$PHQ9Month6-PHQ9_GAD7DiffLin$PHQ9Base
PHQ9_GAD7DiffLin$LivingWhere.y = as.factor(PHQ9_GAD7DiffLin$LivingWhere.y)

### Analyses PHQ9
hist(PHQ9_GAD7DiffLin$PHQ9Diff)
t.test(PHQ9Diff ~ LivingWhere.y, data  =PHQ9_GAD7DiffLin)

modelPHQ9_GAD7DiffLin = lm(PHQ9Diff ~ LivingWhere.y, data = PHQ9_GAD7DiffLin)
summary(modelPHQ9_GAD7DiffLin)


### Analyses GAD7
GAD7_GAD7DiffLin = PHQ9_GAD7
GAD7_GAD7DiffLin = na.omit(GAD7_GAD7DiffLin)
write.csv(GAD7_GAD7DiffLin, "GAD7_GAD7DiffLin.csv", row.names = FALSE)
GAD7_GAD7DiffLin = read.csv("GAD7_GAD7DiffLin.csv", header = TRUE)
GAD7_GAD7DiffLin$GAD7Diff = GAD7_GAD7DiffLin$GAD7Month6-GAD7_GAD7DiffLin$GAD7Base
hist(GAD7_GAD7DiffLin$GAD7Diff)
compmeans(GAD7_GAD7DiffLin$GAD7Diff, GAD7_GAD7DiffLin$LivingWhere.y)
t.test(GAD7Diff ~ LivingWhere.y, data  =GAD7_GAD7DiffLin)

modelGAD7_GAD7DiffLin = lm(GAD7Diff ~ LivingWhere.y, data = GAD7_GAD7DiffLin)
summary(modelGAD7_GAD7DiffLin)

### Analyses drugs
```
Data cleaning: Multilevel modeling not working create difference scores for linear regression
Depression.x, Anxiety.x, Employment.x, ArrestedDays.x, HealthStatus.x, DAUseIllegDrugsDays.x

So I think a difference score of absolute terms in the days should make the dependent variable normal 
We need to take the post minus the pre to get the decrease for everything besides heatlh status

Figure out employment later probably just use the post score
```{r}
ConnGPRA$DepressionDiff = ConnGPRA$Depression.y-ConnGPRA$Depression.x 
ConnGPRA$AnxietyDiff =   ConnGPRA$Anxiety.y - ConnGPRA$Anxiety.x
ConnGPRA$ArrestedDaysDiff = ConnGPRA$ArrestedDays.y- ConnGPRA$ArrestedDays.x
ConnGPRA$HealthStatusDiff = ConnGPRA$HealthStatus.y - ConnGPRA$HealthStatus.x
ConnGPRA$DAUseIllegDrugsDaysDiff = ConnGPRA$DAUseIllegDrugsDays.y-ConnGPRA$DAUseIllegDrugsDays.x


mean(ConnGPRA$DepressionDiff, na.rm = TRUE)
hist(ConnGPRA$DepressionDiff)

summary(ConnGPRA)
describe.factor(ConnGPRA$DepressionDiff)
```
Now try linear regression with each of the above
```{r}

ConnGPRAAnalysis = na.omit(ConnGPRA)
describe.factor(ConnGPRAAnalysis$LivingWhere.y)
dim(ConnGPRAAnalysis)

modelLinearDepress = lm(DepressionDiff ~ LivingWhere.y, data = ConnGPRAAnalysis)
summary(modelLinearDepress)
robust(modelLinearDepress)

modelLinearAnxeity = lm(AnxietyDiff ~ LivingWhere.y, data = ConnGPRAAnalysis)
summary(modelLinearAnxeity)
robust(modelLinearAnxeity)

modelLinearArrested = lm(ArrestedDaysDiff ~ LivingWhere.y, data = ConnGPRAAnalysis)
summary(modelLinearArrested)
robust(modelLinearArrested)

hist(ConnGPRAAnalysis$HealthStatusDiff)
modelLinearHealthStatusDiff = lm(HealthStatusDiff ~ LivingWhere.y, data = ConnGPRAAnalysis)
summary(modelLinearHealthStatusDiff)
robust(modelLinearHealthStatusDiff)

ConnGPRALongAnalysis$Employment.x


describe.factor(ConnGPRAAnalysis$ArrestedDaysDiff)
```


Data Analysis: Try multiple outcomes with the gpra for multilevel
Depression.x
```{r}
head(ConnGPRALong)
ConnGPRALongAnalysis = na.omit(ConnGPRALong)
dim(ConnGPRALongAnalysis)
write.csv(ConnGPRALong, "ConnGPRALong.csv", row.names = FALSE)
ConnGPRALong = read.csv("ConnGPRALong.csv", header = TRUE)




### Multi not working so try linear regression

### Try t-tests


## Shows time is significant so that is good, but cannot hanlde the interaction effect
modelDepress = glmer(Depression.x ~ LivingWhere.y*time + (1 | ClientID), data  = ConnGPRALongAnalysis, family = "poisson")
summary(modelDepress)

modelTimeDepress = glmer(Depression.x ~ time + (1 | ClientID), data  = ConnGPRALongAnalysis, family = "poisson")
summary(modelTimeDepress)

modelTimeAnxeity = glmer(Anxiety.x ~ time + (1 | ClientID), data  = ConnGPRALongAnalysis, family = "poisson")
summary(modelTimeAnxeity)

modelTimeArrested  = glmer(ArrestedDays.x ~ time + (1 | ClientID), data  = ConnGPRALongAnalysis, family = "poisson")
summary(modelTimeArrested)

modelTimeHealthStatus  = lme(HealthStatus.x ~ time, random =~ 1 | ClientID, data  = ConnGPRALongAnalysis)
summary(modelTimeHealthStatus)





# Not enough data to run these models
describe.factor(ConnGPRALongAnalysis$IncomeWages.x)
modelWages = glmer(IncomeWages.x~ LivingWhere.y*time + (1 | ClientID), data  = ConnGPRALongAnalysis, family = "poisson")
summary(modelWages)


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








