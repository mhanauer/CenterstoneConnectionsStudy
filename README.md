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
setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/ConnectionsPaperData")
GPRAAll = read.csv("ConnGPRA.csv", header = TRUE) 
# subet the data based on 1's for baseline and 2's for six-month.  Then write as CSV's, then merge together.
GPRAConBase = subset(GPRAAll, InterviewType ==1)
GPRAConMonth6 = subset(GPRAAll, InterviewType == 2)
GPRAAll = merge(GPRAConBase, GPRAConMonth6, by = "ClientID", all = TRUE)
write.csv(GPRAAll, "GPRAAll.csv", row.names = FALSE)
GPRAAll = read.csv("GPRAAll.csv", header = TRUE, na.strings = c(-9, -8, -7, -1, "", " ", "NULL", NULL, NA, "NA"))

GPRAAll$InterviewDate.x = as.Date(GPRAAll$InterviewDate.x, format = "%m/%d/%Y") 
GPRAAll$InterviewDate.y = as.Date(GPRAAll$InterviewDate.y, format = "%m/%d/%Y")
dim(GPRAAll)
GPRAAll = subset(GPRAAll,LivingWhere.x !=4)
dim(GPRAAll)
GPRAAll$LivingWhere.y = ifelse(GPRAAll$LivingWhere.y == 4, 1,0)
GPRAAll$LivingWhere.x = ifelse(GPRAAll$LivingWhere.x == 4, 1,0)
GPRAAll$Employment.x = ifelse(GPRAAll$Employment.x == 1, 1,0)
GPRAAll$Employment.y = ifelse(GPRAAll$Employment.y == 1, 1,0)
GPRAAll = subset(GPRAAll, Gender.x <= 2)
describe.factor(GPRAAll$Gender.x)
dim(GPRAAll)
# Try getting rid of this variable
GPRAAll$Gender.y = NULL
GPRAAll$Gender.x = ifelse(GPRAAll$Gender.x == 1, 1,0)
GPRAAll$EducationYears.x = ifelse(GPRAAll$EducationYears.x < 12, 1, 0)
GPRAAll$EducationYears.y = ifelse(GPRAAll$EducationYears.y < 12, 1, 0)
GPRAAll$County.x = ifelse(GPRAAll$County.x == "Monroe", 1, 0)
GPRAAll$County.y = ifelse(GPRAAll$County.y == "Monroe", 1, 0)


#Then I am subsetting the data for only those at the time of analysis that are eligible for reassessment.  Because the reassessment takes place every 6 months and the date for this analysis was 8-1-2018 anyone who entered the program later than 2-1-2018 would not be eligible for reassessments so they are not included.  InterviewDate.x equals the intake or baseline date.

ConnPaper = data.frame(ClientID = GPRAAll$ClientID, InterviewDate.x = GPRAAll$InterviewDate.x, InterviewDate.y = GPRAAll$InterviewDate.y, Depression.x = GPRAAll$Depression.x, Depression.y = GPRAAll$Depression.y, 	Anxiety.x = GPRAAll$Anxiety.x, Anxiety.y = GPRAAll$Anxiety.y, Employment.x = GPRAAll$Employment.x, Employment.y = GPRAAll$Employment.y,ArrestedDays.x = GPRAAll$ArrestedDays.x, ArrestedDays.y = GPRAAll$ArrestedDays.y,LivingWhere.x = GPRAAll$LivingWhere.x, LivingWhere.y = GPRAAll$LivingWhere.y, HealthStatus.x = GPRAAll$HealthStatus.x,HealthStatus.y = GPRAAll$HealthStatus.y, IncomeWages.x = GPRAAll$IncomeWages.x, IncomeWages.y = GPRAAll$IncomeWages.y, Age.x = GPRAAll$Age.x, Age.y = GPRAAll$Age.y, EducationYears.x = GPRAAll$EducationYears.x, EducationYears.y = GPRAAll$EducationYears.y, Gender.x = GPRAAll$Gender.x, DAUseIllegDrugsDays.x = GPRAAll$DAUseIllegDrugsDays.x, DAUseIllegDrugsDays.y = GPRAAll$DAUseIllegDrugsDays.y, County.x = GPRAAll$County.x, County.y = GPRAAll$County.y)

ConnPaper = subset(ConnPaper, InterviewDate.x < "2018-02-01")
dim(ConnPaper)
#Here I am loading up both the PHQ9 and GAD7 baselines and 6-month reassessments together.  I am using the merge function, which allows me to combine different data sets using a unique identifier common to both data sets.

PHQ9Base = read.spss("S:/Indiana Research & Evaluation/Indiana Connections/Data/PHQ9/PHQ9 Baseline.sav", to.data.frame = TRUE)
PHQ96month = read.spss("S:/Indiana Research & Evaluation/Indiana Connections/Data/PHQ9/PHQ9 6 Month.sav", to.data.frame = TRUE)
PHQ9All = merge(PHQ9Base, PHQ96month, by = "ParticipantID", all = TRUE)
GAD7Base = read.spss("S:/Indiana Research & Evaluation/Indiana Connections/Data/GAD7/GAD7 Baseline.sav", to.data.frame = TRUE)
GAD76month = read.spss("S:/Indiana Research & Evaluation/Indiana Connections/Data/GAD7/GAD7 6 Month.sav", to.data.frame = TRUE)
GAD7All = merge(GAD7Base, GAD76month, by = "ParticipantID", all = TRUE)

#Now from PHQ9 and GAD7 data sets, I am grabbing only the total scores from both assessments along with the id's and changing the name to ClientID so we can merge it with the GPRA data set.

PHQ9Connections = data.frame(ClientID = PHQ9All$ParticipantID,PHQ9Base= PHQ9All$PHQ9Total.x, PHQ9Month6= PHQ9All$PHQ9Total.y)
GAD7Connections = data.frame(ClientID = GAD7All$ParticipantID, GAD7Base = GAD7All$GAD7Total.x, GAD7Month6 = GAD7All$GAD7Total.y)
PHQ9_GAD7 = merge(PHQ9Connections, GAD7Connections, by = "ClientID", all = TRUE)
head(PHQ9_GAD7)

#Now I am merging the PHQ9_GAD7 data set with the GPRA data set using the ClientID.  all = TRUE means that all ClientID's will be included from both datasets.

PHQ9_GAD7 = merge(ConnPaper, PHQ9_GAD7, by = "ClientID", all = TRUE)
PHQ9_GAD7 = subset(PHQ9_GAD7, InterviewDate.x < "2018-02-01")
dim(PHQ9_GAD7)
head(PHQ9_GAD7)
sum(is.na(PHQ9_GAD7$LivingWhere.x))

#Data Cleaning: Changing housing to yes or no for both data sets PHQ9_GAD7 and GPRA. Only care about the 6 month reassessment, because we want to know if they housed eventually

# Get data into long format
PHQ9_GAD7$PHQ9Base = ifelse(PHQ9_GAD7$PHQ9Base == "<NA>", NA, PHQ9_GAD7$PHQ9Base)

```

Just get the baseline
Check VIFS and get descriptives for this
Check the interactions
```{r}
head(PHQ9_GAD7)

#Conn_Base = PHQ9_GAD7[c("Employment.x", "Employment.y", "LivingWhere.y", "HealthStatus.x", "HealthStatus.y", "Age.x", "Age.y", "EducationYears.x", "Gender.x", "DAUseIllegDrugsDays.x", "DAUseIllegDrugsDays.y", "County.x", "PHQ9Base", "GAD7Base", "PHQ9Month6", "GAD7Month6", "Anxiety.x", "Anxiety.y", "Depression.x","Depression.y")]

#Conn_Base = PHQ9_GAD7[c("Employment.x", "Employment.y", "LivingWhere.y", "HealthStatus.x", "HealthStatus.y", "Age.x", "Age.y", "EducationYears.x", "Gender.x", "DAUseIllegDrugsDays.x", "DAUseIllegDrugsDays.y", "County.x", "Anxiety.x", "Anxiety.y", "Depression.x","Depression.y")]

#Conn_Base = PHQ9_GAD7[c("Employment.x", "LivingWhere.y", "HealthStatus.x", "Age.x", "EducationYears.x", "Gender.x", "DAUseIllegDrugsDays.x", "County.x", "Anxiety.x", "Depression.x")]

Conn_Base = PHQ9_GAD7[c("Employment.x", "LivingWhere.y", "HealthStatus.x", "Age.x", "EducationYears.x", "Gender.x", "DAUseIllegDrugsDays.x", "County.x", "GAD7Base", "PHQ9Base")]

dim(Conn_Base)
TestMCARNormality(Conn_Base)
Conn_Base_complete = na.omit(Conn_Base)
dim(Conn_Base_complete)

Conn_Base_complete$PHQ9Base = as.numeric(Conn_Base_complete$PHQ9Base)
Conn_Base_complete$GAD7Base = as.numeric(Conn_Base_complete$GAD7Base)

```
Get descirptives
```{r}
describe(Conn_Base_complete)
describe.factor(Conn_Base_complete$Employment.x)
describe.factor(Conn_Base_complete$LivingWhere.y)
describe.factor(Conn_Base_complete$EducationYears.x)
describe.factor(Conn_Base_complete$County.x)
```

Now run the model
```{r}
logit_model = glm(LivingWhere.y ~ ., data = Conn_Base_complete)
step_sum = step(logit_model, scope =. ~ .^2)
summary(step_sum)
library(car)
library(DescTools)
PseudoR2(logit_model)
vif(logit_model)
```



Getting descriptives for baseline and 6-months later
LivingWhere.x  Employment.x+ Depression.x + HealthStatus.x + Gender.x + EducationYears.x + Age.x + DAUseIllegDrugsDays.x + County.x 
```{r}
PHQ9_GAD7CatBase = subset(PHQ9_GAD7LongComplete, time == 0)
#PHQ9_GAD7CatBase = data.frame(PHQ9_GAD7CatBase$Employment.x, PHQ9_GAD7CatBase$LivingWhere.x, PHQ9_GAD7CatBase$HealthStatus.x, PHQ9_GAD7CatBase$EducationYears.x, PHQ9_GAD7CatBase$Gender.x, PHQ9_GAD7CatBase$County.x)

apply(PHQ9_GAD7CatBase, 2, function(x){describe.factor(x)})
describe(PHQ9_GAD7CatBase)
apply(PHQ9_GAD7CatBase, 2, sd)


PHQ9_GAD7CatMonth6 = subset(PHQ9_GAD7LongComplete, time == 1)
#PHQ9_GAD7CatMonth6 = data.frame(PHQ9_GAD7CatMonth6$Employment.x, PHQ9_GAD7CatMonth6$LivingWhere.x, PHQ9_GAD7CatMonth6$HealthStatus.x, PHQ9_GAD7CatMonth6$EducationYears.x, PHQ9_GAD7CatMonth6$Gender.x, PHQ9_GAD7CatMonth6$County.x)

apply(PHQ9_GAD7CatMonth6, 2, function(x){describe.factor(x)})
describe(PHQ9_GAD7CatMonth6)
apply(PHQ9_GAD7CatMonth6, 2, sd)
range(PHQ9_GAD7CatMonth6$Age.x)
```

Center continious variables
```{r}
PHQ9_GAD7LongComplete$HealthStatusScaled.x = scale(PHQ9_GAD7LongComplete$HealthStatus.x, center = TRUE, scale = FALSE)

PHQ9_GAD7LongComplete$AgeScaled.x = scale(PHQ9_GAD7LongComplete$Age.x, center = TRUE, scale = FALSE)
```
###Final Model####
```{r}
model1 = glmer(LivingWhere.x ~ Employment.x+ Depression.x + HealthStatus.x + Gender.x + EducationYears.x + Age.x + DAUseIllegDrugsDays.x + County.x  + (1 | ClientID), data  = PHQ9_GAD7LongComplete, family = "binomial")

summary(model1)
r.squaredGLMM(model1)
test =  summary(model1)
hist(test$residuals)
range(test$residuals)

install.packages("dfoptim")
library(dfoptim)
fm1.all <- allFit(model1)
summary(fm1.all)
ss <- summary(fm1.all)
ss
ss$which.OK
model1Summary =  summary(model1)

?convergence

exp(confint.merMod(model1, parm = "beta_", method = "Wald"))

```
Test interactions of time varying with time
```{r}
model_employ = glmer(LivingWhere.x ~ Employment.x+ Depression.x + HealthStatusScaled.x*time + Gender.x + EducationYears.x + AgeScaled.x + DAUseIllegDrugsDays.x + County.x  + (1 | ClientID), data  = PHQ9_GAD7LongComplete, family = "binomial")

summary(model_employ)
```


Cannot get time in the model won't run right 
```{r}
model1 = glmer(LivingWhere.x ~ Employment.x+ Depression.x + HealthStatusScaled.x + Gender.x + EducationYears.x + AgeScaled.x + DAUseIllegDrugsDays.x + County.x  + (1 | ClientID), data  = PHQ9_GAD7LongComplete, family = "binomial")
summary(model1)


model2 = glmer(LivingWhere.x ~ Employment.x+ Depression.x  + Gender.x  + AgeScaled.x  + County.x + (1 | ClientID), data  = PHQ9_GAD7LongComplete, family = "binomial")
summary(model2)



```
Try just a wide format

Not sure really sure what to do with this
```{r}
head(PHQ9_GAD7)
PHQ9_GAD7$ClientID = NULL
PHQ9_GAD7$InterviewDate.x = NULL
PHQ9_GAD7$InterviewDate.y = NULL
PHQ9_GAD7$Anxiety.x = NULL
PHQ9_GAD7$Anxiety.y = NULL
PHQ9_GAD7$ArrestedDays.x = NULL
PHQ9_GAD7$ArrestedDays.y = NULL
PHQ9_GAD7$IncomeWages.x = NULL
PHQ9_GAD7$IncomeWages.y = NULL
PHQ9_GAD7$PHQ9Base = NULL
PHQ9_GAD7$PHQ9Month6 = NULL
PHQ9_GAD7$GAD7Base = NULL
PHQ9_GAD7$GAD7Month6 = NULL




PHQ9_GAD7Complete = na.omit(PHQ9_GAD7)
dim(PHQ9_GAD7Complete)


PHQ9_GAD7Complete$HealthStatus.x = scale(PHQ9_GAD7Complete$HealthStatus.x, center = TRUE, scale = FALSE) 
PHQ9_GAD7Complete$HealthStatus.y = scale(PHQ9_GAD7Complete$HealthStatus.y, center = TRUE, scale = FALSE) 

PHQ9_GAD7CompleteCenter = apply(PHQ9_GAD7Complete, 2, function(x){scale(x, center = TRUE, scale = FALSE)})
PHQ9_GAD7CompleteCenter = data.frame(PHQ9_GAD7CompleteCenter)

modelLogit1 = glm(LivingWhere.y ~ Employment.x+ Depression.x + HealthStatus.x + Gender.x + EducationYears.x + Age.x + DAUseIllegDrugsDays.x + County.x, data  = PHQ9_GAD7Complete, family = "binomial")

summary(modelLogit1)


# Make everything besides edu years and gender y's 
modelLogit2 = glm(LivingWhere.y ~ Employment.y + Depression.y + HealthStatus.y + Gender.x + EducationYears.x + Age.x  + DAUseIllegDrugsDays.y + County.x, data  = PHQ9_GAD7Complete, family = "binomial")

summary(modelLogit2)



# Try centering everything but it has the same results
# Try full model, expect gender and county
modelLogit3 = glm(PHQ9_GAD7Complete$LivingWhere.y ~ Employment.x + Employment.y +  Depression.x+ Depression.y + HealthStatus.x + HealthStatus.y  + Gender.x + EducationYears.y + EducationYears.x + Age.x + Age.y + DAUseIllegDrugsDays.x + DAUseIllegDrugsDays.y + County.x, data  = PHQ9_GAD7CompleteCenter, family = "binomial")

summary(modelLogit3)

```
