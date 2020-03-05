#ANOVA-Y is continuous/interval and X is categorical
# multiple linear regression-
#Y is continuous and X can be categorical and continuous

library(car)
#create the data in a R data frame
libido <- c(3,2,1,1,4,5,2,4,2,3,7,4,5,3,6)
dose <- gl(3,5,labels=c("Placebo","Low Dose","High Dose"))
viagraData <- data.frame(dose,libido)

#perform Levene's test
leveneTest(viagraData$libido, viagraData$dose, center=median)
#Levene's Test for Homogeneity of Variance (center = median)
#        Df F value Pr(>F)
#group   2  0.1176   0.89
#       12   
#Since p>??, we can't reject H0, we have support that the
#variances among libido measures for the three different
#groups(dose levels) may be equal; no support that the
#variances among the libido measures for the different dosage
#groups are not different; we proceed with ANOVA

#ANOVA test for Viagra test; ANOVA as
#regression model: libidoi = dosei + errori
viagraModel <- aov(libido~dose, data=viagraData)
summary(viagraModel)
#              Df Sum Sq Mean Sq F value Pr(>F)  
#dose         2  20.13  10.067   5.119 0.0247 *
#Residuals   12  23.60   1.967                 
#---
  #Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Post-hoc tests will determine exactly what doses (groups) 
#have significantly different mean libido levels

#Bonferroni tests for Viagra Data; The general form of the 
#Bonferroni post-hoc tests in R is pairwise.t.test(outcome, 
#predictor, paired=FALSE, p.adjust.method="method"), 
#pp.447-448 Discovering Stats in R book
pairwise.t.test(viagraData$libido, viagraData$dose, 
p.adjust.method="bonferroni")
#Pairwise comparisons using t tests with pooled SD 

#data: viagraData$libido and viagraData$dose 

#           Placebo Low Dose
#Low Dose    0.845   -       
#High Dose   0.025   0.196   

#P value adjustment method: bonferroni 
#The p-value for this test is 0.025; p-value ????? (reject H0 
#and accept H1)
#so we have statistical support that the mean libido levels 
#are different betweeen patients
#who took the Placebo and patients who took a 
#High Dose of Viagra