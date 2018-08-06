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
```
Loading the data and reading it in
I am then subsetting the data for only those at baseline (InterviewType == 1) and 6 months (InterviewType == 2)
Then I am merging those datasets together back into one data set
Then I am writing and reading them back into R so I can set the missing values indicators
Then I am transforming the both the interview dates into date formats that R can read
Then I am getting rid of people who were housed at the start (LivingWhere.x == 4)
Because when LivingWhere is 4 that is the only value indicating that people are housed.  Then I am changing both variables to 1 and 0 where 1 is when LivingWhere equals 4 and 0 otherwise
Because when employment equals 1 that means that they are employed at all, I am making that one and all else zero
I am subetting the gender variable where gender is greater than 2 is excluded, because there are not enough non male or female gender identities to run any statistics on
Then for gender I am changing them to 1 for male and 0 for female
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

GPRAAll = subset(GPRAAll,LivingWhere.x !=4)
GPRAAll$LivingWhere.y = ifelse(GPRAAll$LivingWhere.y == 4, 1,0)
GPRAAll$LivingWhere.x = ifelse(GPRAAll$LivingWhere.x == 4, 1,0)
GPRAAll$Employment.x = ifelse(GPRAAll$Employment.x == 1, 1,0)
GPRAAll$Employment.y = ifelse(GPRAAll$Employment.y == 1, 1,0)
GPRAAll = subset(GPRAAll, Gender.x <= 2)
GPRAAll = subset(GPRAAll, Gender.y <= 2)
GPRAAll$Gender.x = ifelse(GPRAAll$Gender.x == 1, 1,0)
GPRAAll$Gender.y = ifelse(GPRAAll$Gender.y == 1, 1,0)
GPRAAll$EducationYears.x = ifelse(GPRAAll$EducationYears.x <= 12, 1, 0)
GPRAAll$EducationYears.y = ifelse(GPRAAll$EducationYears.y <= 12, 1, 0)
```
Here I am trying to get the variables that have the most data and the most relevance to research by subsetting the GPRA for only those questions.

Then I am subsetting the data for only those at the time of analysis that are eligible for reassessment.  Because the reassessment takes place every 6 months and the date for this analysis was 8-1-2018 anyone who entered the program later than 2-1-2018 would not be eligible for reassessments so they are not included.  InterviewDate.x equals the intake or baseline date.
```{r}
ConnPaper = data.frame(ClientID = GPRAAll$ClientID, InterviewDate.x = GPRAAll$InterviewDate.x, InterviewDate.y = GPRAAll$InterviewDate.y, Depression.x = GPRAAll$Depression.x, Depression.y = GPRAAll$Depression.y, 	Anxiety.x = GPRAAll$Anxiety.x, Anxiety.y = GPRAAll$Anxiety.y, Employment.x = GPRAAll$Employment.x, Employment.y = GPRAAll$Employment.y,ArrestedDays.x = GPRAAll$ArrestedDays.x, ArrestedDays.y = GPRAAll$ArrestedDays.y,LivingWhere.x = GPRAAll$LivingWhere.x, LivingWhere.y = GPRAAll$LivingWhere.y, HealthStatus.x = GPRAAll$HealthStatus.x,HealthStatus.y = GPRAAll$HealthStatus.y, IncomeWages.x = GPRAAll$IncomeWages.x, IncomeWages.y = GPRAAll$IncomeWages.y, Age.x = GPRAAll$Age.x, Age.y = GPRAAll$Age.y, EducationYears.x = GPRAAll$EducationYears.x, EducationYears.y = GPRAAll$EducationYears.y, Gender.x = GPRAAll$Gender.x, Gender.y = GPRAAll$Gender.y, DAUseIllegDrugsDays.x = GPRAAll$DAUseIllegDrugsDays.x, DAUseIllegDrugsDays.y = GPRAAll$DAUseIllegDrugsDays.y)

ConnPaper = subset(ConnPaper, InterviewDate.x < "2018-02-01")
```
Here I am loading up both the PHQ9 and GAD7 baselines and 6-month reassessments together.  I am using the merge function, which allows me to combine different data sets using a unique identifier common to both data sets.
```{r}
PHQ9Base = read.spss("S:/Indiana Research & Evaluation/Indiana Connections/Data/PHQ9/PHQ9 Baseline.sav", to.data.frame = TRUE)
PHQ96month = read.spss("S:/Indiana Research & Evaluation/Indiana Connections/Data/PHQ9/PHQ9 6 Month.sav", to.data.frame = TRUE)
PHQ9All = merge(PHQ9Base, PHQ96month, by = "ParticipantID", all = TRUE)
GAD7Base = read.spss("S:/Indiana Research & Evaluation/Indiana Connections/Data/GAD7/GAD7 Baseline.sav", to.data.frame = TRUE)
GAD76month = read.spss("S:/Indiana Research & Evaluation/Indiana Connections/Data/GAD7/GAD7 6 Month.sav", to.data.frame = TRUE)
GAD7All = merge(GAD7Base, GAD76month, by = "ParticipantID", all = TRUE)
```
Now from PHQ9 and GAD7 data sets, I am grabbing only the total scores from both assessments along with the id's and changing the name to ClientID so we can merge it with the GPRA data set.
```{r}
PHQ9Connections = data.frame(ClientID = PHQ9All$ParticipantID,PHQ9Base= PHQ9All$PHQ9Total.x, PHQ9Month6= PHQ9All$PHQ9Total.y)
GAD7Connections = data.frame(ClientID = GAD7All$ParticipantID, GAD7Base = GAD7All$GAD7Total.x, GAD7Month6 = GAD7All$GAD7Total.y)
PHQ9_GAD7 = merge(PHQ9Connections, GAD7Connections, by = "ClientID", all = TRUE)
head(PHQ9_GAD7)
```
Now I am merging the PHQ9_GAD7 data set with the GPRA data set using the ClientID.  all = TRUE means that all ClientID's will be included from both datasets.
```{r}
PHQ9_GAD7 = merge(ConnPaper, PHQ9_GAD7, by = "ClientID", all = TRUE)
PHQ9_GAD7 = subset(PHQ9_GAD7, InterviewDate.x < "2018-02-01")
dim(PHQ9_GAD7)
head(PHQ9_GAD7)
```
Now I am using the reshape function to change everything from wide format (i.e. Depression.x, Depression.y) into long format (Depression over time points 0 (baseline) and 1 (6-month)).  Changing to wide format is kind of like stacking the variables where 6-month goes below the baseline and we use a time variable to indicate whether a row is baseline (time = 0) or 6-month (time = 1).
```{r}
PHQ9_GAD7Long = reshape(PHQ9_GAD7, varying = list(c("PHQ9Base", "PHQ9Month6"), c("GAD7Base", "GAD7Month6"), c("DAUseIllegDrugsDays.x", "DAUseIllegDrugsDays.y"), c("InterviewDate.x", "InterviewDate.y"), c("LivingWhere.x", "LivingWhere.y"), c("Employment.x", "Employment.y"), c("Depression.x", "Depression.y"), c("HealthStatus.x", "HealthStatus.y"), c("Gender.x", "Gender.y"), c("EducationYears.x", "EducationYears.y"), c("IncomeWages.x", "IncomeWages.y"), c("ArrestedDays.x", "ArrestedDays.y"), c("Anxiety.x", "Anxiety.y"), c("Age.x", "Age.y")), times = c(0,1), direction = "long")
```
Now I am trying to get the percentage of missing data.  I do this by creating two data sets one with complete data and another with all the data.  Then I get the number of observations in each data set using the dim function and grabbing the first column and dividing the data set with no missing values by the data set that includes missing values minus one to get the percentage of missing data.  Then I just write and read in the data set again, because that tends to solve problems later (not sure why, but reading and writing data sets seems to solve most problems).   
```{r}
dim(PHQ9_GAD7Long)
PHQ9_GAD7AnalysisLong = na.omit(PHQ9_GAD7Long)
dim(PHQ9_GAD7AnalysisLong)

1-(dim(PHQ9_GAD7AnalysisLong)[1]/dim(PHQ9_GAD7Long)[1])

write.csv(PHQ9_GAD7AnalysisLong, "PHQ9_GAD7AnalysisLong.csv", row.names = FALSE)
PHQ9_GAD7AnalysisLong = read.csv("PHQ9_GAD7AnalysisLong.csv", header = TRUE)
```
We are getting the total number of observations across both time points.  Then getting the number of people at baseline and the number of people at follow up.
```{r}
dim(PHQ9_GAD7AnalysisLong)[1]
nSampleBase = subset(PHQ9_GAD7AnalysisLong, time == 0)
dim(nSampleBase)[1]
nSampleMonth6 = subset(PHQ9_GAD7AnalysisLong, time == 1)
dim(nSampleMonth6)[1]
```
Kelsey don't worry about this

Checking to see if the data is missing at random.  Get rid of any data that would mess it up, which includes the dates, id's and arrested days which has almost all zeroes.
```{r}
PHQ9_GAD7Long$InterviewDate.x = NULL
PHQ9_GAD7Long$InterviewDate.y = NULL
PHQ9_GAD7Long$id = NULL
PHQ9_GAD7Long$ClientID = NULL
PHQ9_GAD7Long$ArrestedDays.x = NULL

LittleMCAR(PHQ9_GAD7Long)
```

Center everything besides categorical, date, and count (centering count variables could result in negative and not whole numbers).  So create two data sets one for the categorical outcomes and one for the continuous ones that will be centered.  Then recombine them into one data set overwriting the old one.
```{r}
head(PHQ9_GAD7AnalysisLong)

PHQ9_GAD7AnalysisLongBinary = data.frame(ClientID = PHQ9_GAD7AnalysisLong$ClientID, time = PHQ9_GAD7AnalysisLong$time, Gender.x = PHQ9_GAD7AnalysisLong$Gender.x, LivingWhere.x = PHQ9_GAD7AnalysisLong$LivingWhere.x, Employment.x = PHQ9_GAD7AnalysisLong$Employment.x, EducationYears.x = PHQ9_GAD7AnalysisLong$EducationYears.x, DAUseIllegDrugsDays.x = PHQ9_GAD7AnalysisLong$DAUseIllegDrugsDays.x, Depression.x = PHQ9_GAD7AnalysisLong$Depression.x, InterviewDate.x = PHQ9_GAD7AnalysisLong$InterviewDate.x, ArrestedDays.x = PHQ9_GAD7AnalysisLong$ArrestedDays.x, Anxiety.x = PHQ9_GAD7AnalysisLong$Anxiety.x)

PHQ9_GAD7AnalysisLongCon = data.frame(HealthStatus.x = PHQ9_GAD7AnalysisLong$HealthStatus.x, IncomeWages.x = PHQ9_GAD7AnalysisLong$IncomeWages.x, Age.x = PHQ9_GAD7AnalysisLong$Age.x, PHQ9Base = PHQ9_GAD7AnalysisLong$PHQ9Base, GAD7Base = PHQ9_GAD7AnalysisLong$GAD7Base)
summary(PHQ9_GAD7AnalysisLongCon)

write.csv(PHQ9_GAD7AnalysisLongCon, "PHQ9_GAD7AnalysisLongCon.csv", row.names = FALSE)
PHQ9_GAD7AnalysisLongCon = read.csv("PHQ9_GAD7AnalysisLongCon.csv", header = TRUE)

PHQ9_GAD7AnalysisLongCon = scale(PHQ9_GAD7AnalysisLongCon, center = TRUE, scale = FALSE)

PHQ9_GAD7AnalysisLongCenter = data.frame(PHQ9_GAD7AnalysisLongBinary, PHQ9_GAD7AnalysisLongCon)
write.csv(PHQ9_GAD7AnalysisLongCenter, "PHQ9_GAD7AnalysisLongCenter.csv", row.names = FALSE)
PHQ9_GAD7AnalysisLongCenter = read.csv("PHQ9_GAD7AnalysisLongCenter.csv", header = TRUE)

summary(PHQ9_GAD7AnalysisLongCenter)
```
Ok Kelsey you can stop here.

### Final models
Two competing models cannot be combined, because they they loss their significance.  Present results as step regression so include the covariates that were included
```{r}
modelLogit12 = glmer(LivingWhere.x ~ Employment.x + Depression.x + (1 | ClientID), data  = PHQ9_GAD7AnalysisLong, family = "binomial")
summary(modelLogit12)

modelLogit13 = glmer(LivingWhere.x ~ Employment.x + GAD7Base + (1 | ClientID), data  = PHQ9_GAD7AnalysisLongCenter, family = "binomial")
summary(modelLogit12)

```


Extra data analysis
Data Analysis: Now try logisitic regression for factors related to those in housing and not in housing
```{r}

## Too many variables
modelLogit  = glmer(LivingWhere.x ~  time + Employment.x +HealthStatus.x + Gender.x + Age.x + EducationYears.x + (1 | ClientID), data  = PHQ9_GAD7AnalysisLongCenter, family = "binomial")
summary(modelLogit)

### Try just time and move up from there, time not significant 
modelLogit1  = glmer(LivingWhere.x ~  time + (1 | ClientID), data  = PHQ9_GAD7AnalysisLongCenter, family = "binomial")
summary(modelLogit1)

## Try employment employment not significant
modelLogit2  = glmer(LivingWhere.x ~  Employment.x + (1 | ClientID), data  = PHQ9_GAD7AnalysisLongCenter, family = "binomial")
summary(modelLogit2)

## Try HealthStatus.x, not significant
modelLogit3  = glmer(LivingWhere.x ~  HealthStatus.x + Employment.x + (1 | ClientID), data  = PHQ9_GAD7AnalysisLongCenter, family = "binomial")
summary(modelLogit3)

## ArrestedDays.x
modelLogit4  = glmer(LivingWhere.x ~  ArrestedDays.x + Employment.x + (1 | ClientID), data  = PHQ9_GAD7AnalysisLongCenter, family = "binomial")
summary(modelLogit4)

## Try EducationYears.x, not significant
modelLogit5 = glmer(LivingWhere.x ~  EducationYears.x +Employment.x  +  (1 | ClientID), data  = PHQ9_GAD7AnalysisLongCenter, family = "binomial")
summary(modelLogit5)

### Try Depression.x, not significant 
modelLogit6 = glmer(LivingWhere.x ~  Depression.x + Employment.x  + (1 | ClientID), data  = PHQ9_GAD7AnalysisLongCenter, family = "binomial")
summary(modelLogit6)


### Try Anxiety.x, not significant
modelLogit7 = glmer(LivingWhere.x ~  Anxiety.x  + Employment.x  +  (1 | ClientID), data  = PHQ9_GAD7AnalysisLongCenter, family = "binomial")
summary(modelLogit7)

### Try  Gender.x, not significant
modelLogit8 = glmer(LivingWhere.x ~ Gender.x + Employment.x   + (1 | ClientID), data  = PHQ9_GAD7AnalysisLongCenter, family = "binomial")
summary(modelLogit8)

### DAUseIllegDrugsDays.x, not significant
modelLogit9 = glmer(LivingWhere.x ~  DAUseIllegDrugsDays.x + Employment.x  + (1 | ClientID), data  = PHQ9_GAD7AnalysisLongCenter, family = "binomial")
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


### Try GAD7Base without depression, 
modelLogit13 = glmer(LivingWhere.x ~ Employment.x  + GAD7Base + (1 | ClientID), data  = PHQ9_GAD7AnalysisLong, family = "binomial")
summary(modelLogit13)
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
Ok look look at interaction effect for difference score
I think that only 
```{r}

write.csv(PHQ9_GAD7, "PHQ9_GAD7.csv", row.names = FALSE)
PHQ9_GAD7 = read.csv("PHQ9_GAD7.csv", header = TRUE)
PHQ9_GAD7$PHQ9Diff= PHQ9_GAD7$PHQ9Month6 - PHQ9_GAD7$PHQ9Base
PHQ9_GAD7$GAD7Diff = PHQ9_GAD7$GAD7Month6 - PHQ9_GAD7$GAD7Base
```
Now looking at the analysis for just baseline and difference score
```{r}
PHQ9_GAD7Analysis = na.omit(PHQ9_GAD7)
dim(PHQ9_GAD7Analysis)

modelLinerPHQ9 = lm(PHQ9Diff ~ LivingWhere.y, data = PHQ9_GAD7Analysis)
summary(modelLinerPHQ9)

modelLinerGAD7 = lm(GAD7Diff ~ LivingWhere.y, data = PHQ9_GAD7Analysis)
summary(modelLinerGAD7)
```

Data Analysis: Try interaction effect by housing not enough data
```{r}

modelTimeHouseDepress= glmer(Depression.x ~ LivingWhere.x*time + (1 | ClientID), data  = PHQ9_GAD7AnalysisLong, family = "poisson")
summary(modelTimeHouseDepress)
```















