# ANOVA in R for Experimental and Quasi-Experimental Design 

Hi All,

Below is R code for ANOVA analysis with results for my Experimental and Quasi-Experimental Design course. Some of the code has been modified from the instructor because it was missing or a specific step was added. Any follow ups/help would be appreciated if you see that there should be any changes. Thanks. Roger

plot(density(ANOVAExample$Valuation))
#install moments package
library(moments)
agostino.test(ANOVAExample$Valuation)
#D'Agostino skewness test
#data:  ANOVAExample$Valuation
#skew = 0.10977, z = 0.71253, p-value = 0.4761
#alternative hypothesis: data have a skewness
anscombe.test(ANOVAExample$Valuation)
#Anscombe-Glynn kurtosis test
#data:  ANOVAExample$Valuation
#kurt = 2.3083, z = -3.2073, p-value = 0.00134
#alternative hypothesis: kurtosis is not equal to 3
shapiro.test(ANOVAExample$Valuation)
#Shapiro-Wilk normality test
##data:  ANOVAExample$Valuation
#W = 0.97406, p-value = 0.0002225
ANOVAExample$Valuation<-log(ANOVAExample$Valuation + 2)
#Log,book explanation
bartlett.test(ANOVAExample$Valuation, ANOVAExample$Condition)
#Bartlett test of homogeneity of variances
#data:  ANOVAExample$Valuation and ANOVAExample$Condition
#Bartlett's K-squared = 6.5638, df = 2, p-value = 0.03756
tapply(ANOVAExample$Valuation, ANOVAExample$Condition, var)
#Apply a Function Over a Ragged Array
#Apply a function to each cell of a ragged array, that is to each 
#(non-empty) group of values given by a unique combination of the levels 
#of certain factors.
#0         1         2 
#0.2664030 0.3316206 0.1854722 
#A general rule of thumb is that if we have balanced data 
#(the # of observations in each level is equal) then small violations
#(largest variance/smallest variance < 4) we are ok with ANOVA.
#Orthogonal contrasts to see where differences are between which variables
summary(aov(Valuation ~ factor(Condition), data = ANOVAExample))
#Df Sum Sq Mean Sq F value   Pr(>F)    
#factor(Condition)   2   9.97   4.985   19.09 2.06e-08 ***
#Residuals         237  61.90   0.261                     
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#Doing so we find a significant effect of Condition, F(2, 237) = 19.09, 
#p < .001. But where is the difference?
#One way to get to the bottom of why an ANOVA is significant is to perform 
#contrasts: Compare the treatment effects to one another.
#We have to do orthogonal contrasts, or we have to correct for repeated 
#comparison (p inflation).
#Orthogonal contrasts for analysis of variance are independent linear comparisons 
#between the groups of a factor with at least three fixed levels. ... With this 
#numbering system, two contrasts 
#are orthogonal to each other if the products of their coefficients sum to zero.
c1 <- c(0,-1, 1)
c2 <- c(2,-1, -1)
mat <- cbind(c1,c2)
ANOVAExample$Condition <- factor(ANOVAExample$Condition)
contrasts(ANOVAExample$Condition) <- mat 
model <- aov(Valuation ~ Condition, data = ANOVAExample)
summary (model, split=list(Condition= list("Buyer vs Seller" = 1, "Chooser vs B/S" = 2)))
#Df Sum Sq Mean Sq F value   Pr(>F)    
#Condition                      2   9.97   4.985  19.087 2.06e-08 ***
  #Condition: Buyer vs Seller   1   9.56   9.559  36.600 5.62e-09 ***
  #Condition: Chooser vs B/S    1   0.41   0.411   1.575    0.211    
#Residuals                    237  61.90   0.261                     
#---
  #Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
TukeyHSD(model)
#Create a set of confidence intervals on the differences between the means of the levels 
#of a factor with the specified family-wise probability of coverage. The intervals 
#are based on the Studentized range statistic, 
#Tukey's 'Honest Significant Difference' method.
#Tukey multiple comparisons of means

#95% family-wise confidence level

#Fit: aov(formula = Valuation ~ Condition, data = ANOVAExample)

#$Condition
#diff         lwr        upr     p adj
#1-0 -0.3322336 -0.52281032 -0.1416569 0.0001598
#2-0  0.1566055 -0.03397124  0.3471822 0.1303141
#2-1  0.4888391  0.29826236  0.6794158 0.0000000
#P values just tell us there is a difference but don't tell us the size of 
#the difference (i.e., the effect).
#Many researchers focus on effect sizes since large effects are more likely 
#to be robust (replicable).
#You will want to install two packages: "pastecs" and "compute.es"
library(pastecs)
#pastecs= Regularisation, decomposition and analysis of space-time series.
library(compute.es)
#compute.es=Compute Effect Sizes in R
#This package provides a comprehensive set of tools/functions to easily 
#derive and/or convert statistics generated from one's study (or from those 
#reported in a published study) to all of the common effect size estimates, 
#along with their variances, confidence intervals, and p-values. Several 
#additional statistics are generated, including NNT (number needed to treat),
#U3 (Cohen's U3 distribution overlap statistic), CLES (Common Language Effect Size) 
#and Cliff's Delta (success rate difference). 

#First will get the relevant stats for each of our factor levels 
#(only relevant output pasted below:by
Subzero=subset(ANOVAExample, Condition=="0")
Subone=subset(ANOVAExample, Condition=="1")
Subtwo=subset(ANOVAExample, Condition=="2")
#stat.desc=Descriptive statistics on a data frame or time series
#Compute a table giving various descriptive statistics about the 
#series in a data frame or in a single/multiple time series
stat.desc(Subzero)
#Valuation ProductorGamble Condition
#nbr.val       80.00000000              NA        NA
#nbr.null       0.00000000              NA        NA
#nbr.na         0.00000000              NA        NA
#min            0.69314718              NA        NA
#max            2.70805020              NA        NA
#range          2.01490302              NA        NA
#sum          163.13206103              NA        NA
#median         2.19722458              NA        NA
#mean           2.03915076              NA        NA
#SE.mean        0.05770647              NA        NA
#CI.mean.0.95   0.11486186              NA        NA
#var            0.26640296              NA        NA
#std.dev        0.51614238              NA        NA
#coef.var       0.25311634              NA        NA
stat.desc(Subone)
#Valuation ProductorGamble Condition
#nbr.val       80.00000000              NA        NA
#nbr.null       0.00000000              NA        NA
#nbr.na         0.00000000              NA        NA
#min            0.69314718              NA        NA
#max            2.70805020              NA        NA
#range          2.01490302              NA        NA
#sum          163.13206103              NA        NA
#median         2.19722458              NA        NA
#mean           2.03915076              NA        NA
#SE.mean        0.05770647              NA        NA
#CI.mean.0.95   0.11486186              NA        NA
#var            0.26640296              NA        NA
#std.dev        0.51614238              NA        NA
#coef.var       0.25311634              NA        NA
stat.desc(Subtwo)
#Valuation ProductorGamble Condition
#nbr.val       80.00000000              NA        NA
#nbr.null       0.00000000              NA        NA
#nbr.na         0.00000000              NA        NA
#min            0.69314718              NA        NA
#max            2.89037176              NA        NA
#range          2.19722458              NA        NA
#sum          175.66050006              NA        NA
#median         2.30258509              NA        NA
#mean           2.19575625              NA        NA
#SE.mean        0.04814979              NA        NA
#CI.mean.0.95   0.09583976              NA        NA
#var            0.18547220              NA        NA
#std.dev        0.43066484              NA        NA
#coef.var       0.19613508              NA        NA

#only relevant output pasted below:
#Condition 0		 Condition 1		 Condition 2
#n   M     SD		 n	M	  SD	  	  n	  M	    SD
#80  2.03  .52		80	1.71  .56		  80	  2.19 .43
#Then we can use these values to compute a few standard measures of 
#effect size for any pair of interest (will compare 1 and 2 ~ Buyers vs 
#Sellers known as the endowment effect).
#mes=Means to Effect Size
#Converts raw mean scores to an effect size of d (mean difference), 
#g (unbiased estimate of d), r (correlation coefficient), z' (Fisher's z),
#and log odds ratio. The variances, confidence intervals and p-values of 
#these estimates are also computed, along with NNT (number needed to treat), 
#U3 (Cohen's U(3) overlapping proportions of distributions), CLES 
#(Common Language Effect Size) and Cliff's Delta.
mes(2.195, 1.706, .575, .430, 80, 80)
#Cohen's d = .96, Hedge's g = .96, r = .46

#These are large effects: <=.2 is small, .5 is medium, .8 is large, 
#> 1 very large to huge.

#Mean Differences ES: 
  
  #d [ 95 %CI] = 0.96 [ 0.63 , 1.29 ] 
#var(d) = 0.03 
#p-value(d) = 0 
#U3(d) = 83.23 % 
#CLES(d) = 75.21 % 
#Cliff's Delta = 0.5 

#g [ 95 %CI] = 0.96 [ 0.63 , 1.29 ] 
#var(g) = 0.03 
#p-value(g) = 0 
#U3(g) = 83.11 % 
#CLES(g) = 75.11 % 

#Correlation ES: 

#r [ 95 %CI] = 0.43 [ 0.3 , 0.55 ] 
#var(r) = 0 
#p-value(r) = 0 

#z [ 95 %CI] = 0.46 [ 0.31 , 0.62 ] 
#var(z) = 0.01 
#p-value(z) = 0 

#Odds Ratio ES: 

#OR [ 95 %CI] = 5.74 [ 3.15 , 10.44 ] 
#3p-value(OR) = 0 

#Log OR [ 95 %CI] = 1.75 [ 1.15 , 2.35 ] 
#var(lOR) = 0.09 
#p-value(Log OR) = 0 

#Other: 

#NNT = 2.87 
#Total N = 160

#Our final assumption for ANOVA (also in regression) is that our 
#residuals are normally distributed.
#We first want to look at these visually using normal probability plots
qqnorm(model$residuals)
qqline(model$residuals)
#We want to see our dots on that line, or at least
#no clear pattern of the dots. This doesnt look
#so good.
#We can check how bad this is using Shapiro
#shapiro.test=test of normality
shapiro.test(model$residuals)
#W = 0.94428, p-value = 6.202e-08
#Our model is violating the final assumption.
#This is probably the least important assumption and you
#will never find perfect residuals. However, it does hint that 
#other predictors should be thought about and included

#Factorial Designs
#Running our example in R is as simple as adding an * and our new factor.
model <- aov(Valuation ~ Condition*ProductorGamble, data = ANOVAExample)
summary (model)
#                          Df Sum Sq Mean Sq F value   Pr(>F)    
#Condition                   2   9.97   4.985  22.539 1.12e-09 ***
#ProductorGamble             1   9.35   9.352  42.284 4.73e-10 ***
#Condition:ProductorGamble   2   0.79   0.396   1.789    0.169    
#Residuals                 234  51.75   0.221                     
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Perspective and item type are significant, their interaction is not. 
#Lets plot the interaction anyways.
interaction.plot(ANOVAExample$Condition, ANOVAExample$ProductorGamble, 
                 ANOVAExample$Valuation)
#Note we can also make inferences about our main effects
#from this as well: Products > Gambles, Sellers > Buyers




                                                                 
