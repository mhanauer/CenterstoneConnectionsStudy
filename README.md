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

Getting rid of people who were housed at the start the program

Need to change living where, with 4 being housed for both pre and post 
Need to change employment with 1 being employed and 2 not employed
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

dim(GPRAAll)

# Getting rid of people who were originally housed
GPRAAll = subset(GPRAAll,LivingWhere.x !=4)
GPRAAll$LivingWhere.y = ifelse(GPRAAll$LivingWhere.y == 4, 1,0)
GPRAAll$LivingWhere.x = ifelse(GPRAAll$LivingWhere.x == 4, 1,0)
GPRAAll$Employment.x = ifelse(GPRAAll$Employment.x == 1, 1,0)
GPRAAll$Employment.y = ifelse(GPRAAll$Employment.y == 1, 1,0)
GPRAAll = subset(GPRAAll, Gender.x <= 2)
GPRAAll = subset(GPRAAll, Gender.y <= 2)
GPRAAll$Gender.x = ifelse(GPRAAll$Gender.x == 1, 1,0)
GPRAAll$Gender.y = ifelse(GPRAAll$Gender.y == 1, 1,0)
```
These are the variables in the GPRA that have the least missing data
Add interview date for post and then create a time in intervention
```{r}
ConnPaper = data.frame(ClientID = GPRAAll$ClientID, InterviewDate.x = GPRAAll$InterviewDate.x, InterviewDate.y = GPRAAll$InterviewDate.y, Depression.x = GPRAAll$Depression.x, 	Anxiety.x = GPRAAll$Anxiety.x,	BrainFunction.x = GPRAAll$BrainFunction.x,	ViolentBehavior.x = GPRAAll$ViolentBehavior.x,	PhysicallyHurt.x = GPRAAll$PhysicallyHurt.x,	InteractFamilyFriends.x = GPRAAll$InteractFamilyFriends.x, Depression.y = GPRAAll$Depression.y, Anxiety.y=	GPRAAll$Anxiety.y,	BrainFunction.y = GPRAAll$BrainFunction.y,	ViolentBehavior.y = GPRAAll$ViolentBehavior.y,	PhysicallyHurt.y = GPRAAll$PhysicallyHurt.y,	InteractFamilyFriends.y = GPRAAll$InteractFamilyFriends.y, Employment.x = GPRAAll$Employment.x, Employment.y = GPRAAll$Employment.y,ArrestedDays.x = GPRAAll$ArrestedDays.x, ArrestedDays.y = GPRAAll$ArrestedDays.y,LivingWhere.x = GPRAAll$LivingWhere.x, LivingWhere.y = GPRAAll$LivingWhere.y, HealthStatus.x = GPRAAll$HealthStatus.x,HealthStatus.y = GPRAAll$HealthStatus.y, IncomeWages.x = GPRAAll$IncomeWages.x, Age.x = GPRAAll$Age.x, EducationYears.x = GPRAAll$EducationYears.x, Gender.x = GPRAAll$Gender.x, DAUseIllegDrugsDays.x = GPRAAll$DAUseIllegDrugsDays.x, DAUseIllegDrugsDays.y = GPRAAll$DAUseIllegDrugsDays.y)
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
PHQ9_GAD7 = data.frame(ClientID = PHQ9_GAD7$ClientID, InterviewDate.x = PHQ9_GAD7$InterviewDate.x,InterviewDate.y = PHQ9_GAD7$InterviewDate.y,LivingWhere.x= PHQ9_GAD7$LivingWhere.x, LivingWhere.y= PHQ9_GAD7$LivingWhere.y, InteractFamilyFriends.x = PHQ9_GAD7$InteractFamilyFriends.x, InteractFamilyFriends.y = PHQ9_GAD7$InteractFamilyFriends.y, Employment.x = PHQ9_GAD7$Employment.x, Employment.y = PHQ9_GAD7$Employment.y, Depression.x = PHQ9_GAD7$Depression.x, Depression.y = PHQ9_GAD7$Depression.y, PHQ9_GAD7[,23:33])
dim(PHQ9_GAD7)
PHQ9_GAD7 = subset(PHQ9_GAD7, InterviewDate.x < "2018-02-01")
dim(PHQ9_GAD7)


PHQ9_GAD7Complete = na.omit(PHQ9_GAD7)
dim(PHQ9_GAD7Complete)
1-(dim(PHQ9_GAD7Complete)[1]/(dim(PHQ9_GAD7)[1]))
```
Data Cleaning: Changing housing to yes or no for both data sets PHQ9_GAD7 and GPRA.  Only care about the 6 month reassessment, because we want to know if they housed eventually

Need to change living where for both 
```{r}
describe.factor(PHQ9_GAD7$LivingWhere.x)
describe.factor(PHQ9_GAD7$LivingWhere.y)
describe.factor(ConnGPRA$LivingWhere.y)
describe.factor(ConnGPRA$Employment.y)
```
Put everything into long format for GPRA and PHQ9 and GAD7
```{r}

ConnGPRALong = reshape(ConnGPRA, varying = list(c("Depression.x", "Depression.y"), c("Anxiety.x", "Anxiety.y"), c("BrainFunction.x", "BrainFunction.y"), c("ViolentBehavior.x", "ViolentBehavior.y"), c("PhysicallyHurt.x", "PhysicallyHurt.y"), c("Employment.x", "Employment.y"), c("ArrestedDays.x", "ArrestedDays.y"), c("HealthStatus.x", "HealthStatus.y"), c("DAUseIllegDrugsDays.x", "DAUseIllegDrugsDays.y"), c("InterviewDate.x", "InterviewDate.y"), c("LivingWhere.x", "LivingWhere.y"), c("InteractFamilyFriends.x", "InteractFamilyFriends.y")), times = c(0,1), direction = "long")
head(ConnGPRALong)



PHQ9_GAD7Long = reshape(PHQ9_GAD7, varying = list(c("PHQ9Base", "PHQ9Month6"), c("GAD7Base", "GAD7Month6"), c("DAUseIllegDrugsDays.x", "DAUseIllegDrugsDays.y"), c("InterviewDate.x", "InterviewDate.y"), c("LivingWhere.x", "LivingWhere.y"), c("InteractFamilyFriends.x", "InteractFamilyFriends.y"), c("Employment.x", "Employment.y"), c("Depression.x", "Depression.y")), times = c(0,1), direction = "long")

PHQ9_GAD7AnalysisLong = na.omit(PHQ9_GAD7Long)
write.csv(PHQ9_GAD7AnalysisLong, "PHQ9_GAD7AnalysisLong.csv", row.names = FALSE)
PHQ9_GAD7AnalysisLong = read.csv("PHQ9_GAD7AnalysisLong.csv", header = TRUE)


ConnGPRALongAnalysis = na.omit(ConnGPRALong)
dim(ConnGPRALongAnalysis)
write.csv(ConnGPRALong, "ConnGPRALong.csv", row.names = FALSE)
ConnGPRALong = read.csv("ConnGPRALong.csv", header = TRUE)

ConnGPRALongMissing = ConnGPRALong
ConnGPRALongMissing$InterviewDate.x = NULL
PHQ9_GAD7AnalysisLong$InterviewDate.x = NULL
summary(ConnGPRALongMissing)
```

Data Analysis: Try multiple outcomes by housing not enough data
```{r}

modelTimeHouseDepress= glmer(Depression.x ~ LivingWhere.x*time + (1 | ClientID), data  = ConnGPRALongAnalysis, family = "poisson")
summary(modelTimeHouseDepress)
```
Data cleaning for logisitic analysis
Need to change education at some point
Need to change the gender variable as well
Education: 1 = less than high school so less high < 12, 0 is great than (Maybe change this later to be more specific)  
Gender: 1 = feamle, 0 = male, get rid of two people who are neither (original data is 1 = male, 2 = female)
```{r}
describe.factor(ConnGPRALongAnalysis$EducationYears.x)
ConnGPRALongAnalysis$EducationYears.x = ifelse(ConnGPRALongAnalysis$EducationYears.x <= 12, 1, 0)
describe.factor(ConnGPRALongAnalysis$EducationYears.x)
```
Center everything besides categorical variables
```{r}
PHQ9_GAD7AnalysisLongBinary = data.frame(ClientID = PHQ9_GAD7AnalysisLong$ClientID, Gender.x = PHQ9_GAD7AnalysisLong$Gender.x, 
```


Final model with logistic regression living where
```{r}

modelLogit12 = glmer(LivingWhere.x ~ Employment.x + GAD7Base + (1 | ClientID), data  = PHQ9_GAD7AnalysisLong, family = "binomial")
summary(modelLogit12)
```



Data Analysis: Now try logisitic regression for factors related to those in housing and not in housing




```{r}

## Too many variables
modelLogit  = glmer(LivingWhere.x ~  time + Employment.x +HealthStatus.x + Gender.x + Age.x + EducationYears.x + (1 | ClientID), data  = ConnGPRALongAnalysis, family = "binomial")
summary(modelLogit)

### Try just time and move up from there, time not significant 
modelLogit1  = glmer(LivingWhere.x ~  time + (1 | ClientID), data  = ConnGPRALongAnalysis, family = "binomial")
summary(modelLogit1)

## Try employment employment not significant
modelLogit2  = glmer(LivingWhere.x ~  Employment.x + (1 | ClientID), data  = ConnGPRALongAnalysis, family = "binomial")
summary(modelLogit2)

## Try HealthStatus.x, not significant
modelLogit3  = glmer(LivingWhere.x ~  HealthStatus.x + Employment.x + (1 | ClientID), data  = ConnGPRALongAnalysis, family = "binomial")
summary(modelLogit3)

## ArrestedDays.x
modelLogit4  = glmer(LivingWhere.x ~  ArrestedDays.x + Employment.x + (1 | ClientID), data  = ConnGPRALongAnalysis, family = "binomial")
summary(modelLogit4)

## Try EducationYears.x, not significant
modelLogit5 = glmer(LivingWhere.y ~  EducationYears.x + (1 | ClientID), data  = ConnGPRALongAnalysis, family = "binomial")
summary(modelLogit5)

### Try Depression.x, not significant 
modelLogit6 = glmer(LivingWhere.x ~  Depression.x + Employment.x  + (1 | ClientID), data  = ConnGPRALongAnalysis, family = "binomial")
summary(modelLogit6)


### Try Anxiety.x, not significant
modelLogit7 = glmer(LivingWhere.x ~  Anxiety.x  + Employment.x  +  (1 | ClientID), data  = ConnGPRALongAnalysis, family = "binomial")
summary(modelLogit7)

### Try  Gender.x, not significant
modelLogit8 = glmer(LivingWhere.x ~ Gender.x + Employment.x  +  + (1 | ClientID), data  = ConnGPRALongAnalysis, family = "binomial")
summary(modelLogit8)

### DAUseIllegDrugsDays.x, not significant
modelLogit9 = glmer(LivingWhere.x ~  DAUseIllegDrugsDays.x + Employment.x  + (1 | ClientID), data  = ConnGPRALongAnalysis, family = "binomial")
summary(modelLogit9)

### Try PHQ9, not significant 
modelLogit10 = glmer(LivingWhere.x ~  PHQ9Base + Employment.x + (1 | ClientID), data  = PHQ9_GAD7AnalysisLong, family = "binomial")
summary(modelLogit10)


### Try GAD7Base, to much correlation between variables probably
modelLogit11 = glmer(LivingWhere.x ~  GAD7Base+ Employment.x + Depression.x + (1 | ClientID), data  = PHQ9_GAD7AnalysisLong, family = "binomial")
summary(modelLogit11)

### Try GAD7Base
modelLogit12 = glmer(LivingWhere.x ~ Employment.x + Depression.x + (1 | ClientID), data  = PHQ9_GAD7AnalysisLong, family = "binomial")
summary(modelLogit12)
```
For logisitic model, could just see if baseline factor are related to being housed.  Maybe it won't make a difference?
```{r}

## Try employment employment not significant
modelLogit2  = glm(LivingWhere.y ~  Employment.x, data  = ConnGPRAAnalysis, family = "binomial")
summary(modelLogit2)

## Try HealthStatus.x, not significant
modelLogit3  = glm(LivingWhere.y ~  HealthStatus.x, data  =ConnGPRAAnalysis , family = "binomial")
summary(modelLogit3)

## ArrestedDays.x
modelLogit4  = glm(LivingWhere.y ~  ArrestedDays.x, data  = ConnGPRAAnalysis, family = "binomial")
summary(modelLogit4)

## Try EducationYears.x, not significant
modelLogit5 = glm(LivingWhere.y ~  EducationYears.x, data  = ConnGPRAAnalysis, family = "binomial")
summary(modelLogit5)

### Try Depression.x, not significant 
modelLogit6 = glm(LivingWhere.y ~  Depression.x, data  = ConnGPRAAnalysis, family = "binomial")
summary(modelLogit6)

### Try Depression.x, not significant 
modelLogit6 = glm(LivingWhere.y ~  Depression.x, data  = ConnGPRAAnalysis, family = "binomial")
summary(modelLogit6)

### Try Anxiety.x, not significant
modelLogit7 = glm(LivingWhere.y ~  Anxiety.x, data  = ConnGPRAAnalysis, family = "binomial")
summary(modelLogit7)

### Try  Gender.x, not significant
modelLogit8 = glm(LivingWhere.y ~ Gender.x, data  = ConnGPRALongAnalysis, family = "binomial")
summary(modelLogit8)

### DAUseIllegDrugsDays.x, not significant
modelLogit9 = glm(LivingWhere.y ~  DAUseIllegDrugsDays.x, data  = ConnGPRAAnalysis, family = "binomial")
summary(modelLogit9)

### Get rid of missing values for PHQ9_GAD7
PHQ9_GAD7Analysis = na.omit(PHQ9_GAD7)
write.csv(PHQ9_GAD7Analysis, "PHQ9_GAD7Analysis.csv", row.names = FALSE)
PHQ9_GAD7Analysis = read.csv("PHQ9_GAD7Analysis.csv", header = TRUE)
### Try PHQ9, not significant 
modelLogit10 = glm(LivingWhere.y ~  PHQ9Base, data  = PHQ9_GAD7Analysis, family = "binomial")
summary(modelLogit10)


### Try GAD7Base
modelLogit11 = glm(LivingWhere.y ~  GAD7Base, data  = PHQ9_GAD7Analysis, family = "binomial")
summary(modelLogit11)
```



















