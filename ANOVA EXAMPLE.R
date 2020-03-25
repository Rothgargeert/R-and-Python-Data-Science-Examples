install.packages("car")

install.packages("ggplot2")

install.packages("multcomp")

install.packages("pastecs")

library (car)
library(ggplot2)
library(multcomp)
library(pastecs)

#create the data in the R data frame
libido<-c(3,2,1,1,4,5,2,4,2,3,7,4,5,3,6)
dose<-gl(3,5, labels=c("Placebo", "Low Dose", "High Dose"))
viagraData<-data.frame(dose, libido)
viagraData

#anova analysis: newModel<-aov(outcome~predictor(s), data=dataFrame)
viagraModel<-aov(libido~dose, data=viagraData)
summary(viagraModel)
#Df         Sum   Sq  Mean Sq   F value Pr(>F)  
#dose         2  20.13  10.067   5.119  0.0247 *
#Residuals   12  23.60   1.967                 
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#Source of variation  DF  Sum Sq  Mean Sq   F value   Pr(>F)
#       Model         K-1 SSm     SSm/(K-1) MSm/MSR   p-value
#       Residuals     N-K SSR     SSR/(N-K)
#       Total         N-1         SST
#SST=sSM+SSR
#K=#of groups (levels of categorical predictor)
#N=Total no. of observations

#descriptive statistics for different levels of categorical
#variable
#by(vaiable, group, output), to get Grand mean
dose_averages<-by(viagraData$libido, viagraData$dose, mean)
dose_averages
#viagraData$dose: Placebo
#[1] 2.2
#--------------------------------------------------- 
#  viagraData$dose: Low Dose
#[1] 3.2
#--------------------------------------------------- 
#  viagraData$dose: High Dose
#[1] 5

#Mean of means=Grand mean
mean(c(dose_averages[1], dose_averages[2], dose_averages[3]))
#[1] 3.466667

#calculate p value from F-distribution
pf(5.12, df1=2, df2=12, lower.tail=FALSE)
#[1] 0.02467623