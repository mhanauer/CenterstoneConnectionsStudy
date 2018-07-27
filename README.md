---
title: "ConnectionsStudy"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
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


Need to the total number of people at baseline and across follow up.
```{r}
PHQ9Connections = data.frame(PHQ9Base= PHQ9All$PHQ9Total.x, PHQ9Month6= PHQ9All$PHQ9Total.y)
PHQ9Connections
PHQ9ConnectionsMissing = na.omit(PHQ9Connections)
dim(PHQ9ConnectionsMissing)[1]
dim(PHQ9Connections)[1]
1-(dim(PHQ9ConnectionsMissing)[1]/dim(PHQ9Connections)[1])

GAD7Connections = data.frame(GAD7Base = GAD7All$GAD7Total.x, GAD7Month6 = GAD7All$GAD7Total.y)
dim(GAD7Connections)
GAD7ConnectionsMissing = na.omit(GAD7Connections)
dim(GAD7Connections)[1]
dim(GAD7ConnectionsMissing)[1]
1-(dim(GAD7ConnectionsMissing)[1]/dim(GAD7Connections)[1])

```

