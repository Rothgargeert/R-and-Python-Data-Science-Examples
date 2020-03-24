#The first thing will note is that Ad is coded numerically, 
#but we need to treat it as a categorical predictor so we should 
#factor it right away before we forget and get ourselves into trouble
RegressionExample$Ad <- factor(RegressionExample$Ad)
#Next, since we know we will be including an interaction involving a continuous variable 
#(Age) we know we need to mean center that predictor to make sure it is orthogonal to 
#the main effects.
RegressionExample$CAge <- scale(RegressionExample$Age, scale = FALSE)

#Next we should check the range of our predictors (i.e., do the run the full range/have some variance).
tabulate(RegressionExample$Ad)
#[1] 167 167 166

#It looks ok. We have ~ equal assignment to the three ads and ages range from ~20 to 60. 
hist.default(RegressionExample$Age)

#Now we check our dependents distribution to see if its mostly normal 
#(might we have outliers).
#It looks like we might have an outlier somewhere near/at 0. 
#Will see if that is the case later on, for now its ok.
plot(density(RegressionExample$Rating))

#Now we should check to see if we have a linear relationship between Age and Rating.
#install car then library (car)
library(car)
#This looks very linear, but note also a lot of observations falling
#quite far off the line again suggesting some outliers (see the box plots), 
#but they look fairly random and matched on each side.
scatterplot(RegressionExample$Age, RegressionExample$Rating)

#Next we see if we have a relationship between Ad and Age -
#this is a good place for ANOVA
model <- aov(RegressionExample$Age~RegressionExample$Ad) 
summary(model)
#p<2e-16 ***

tapply(RegressionExample$Age, RegressionExample$Ad, mean)
#0        1        2 
#32.23952 35.79641 40.58434
#There is a relationship, but it looks minor. 
#Will check more later after we run our model

#Now for the rest of the checks we have to have our model first 
#so will go ahead and run it
model <- lm(Rating ~ Ad*CAge, data = RegressionExample)
summary(model)
#    Min      1Q  Median      3Q     Max 
#-61.852  -4.003  -0.058   4.147  44.626 

#Coefficients:
 # Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 60.40939    0.65783  91.832  < 2e-16 ***
 # Ad1          1.94056    0.87579   2.216 0.027162 *  
 # Ad2          3.25108    0.95500   3.404 0.000717 ***
#  CAge         1.43460    0.07975  17.988  < 2e-16 ***
  #Ad1:CAge     0.22707    0.11782   1.927 0.054526 .  
#Ad2:CAge     0.05051    0.11771   0.429 0.668017    
---
  #Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 7.458 on 494 degrees of freedom
#Multiple R-squared:  0.7311,	Adjusted R-squared:  0.7284 
#F-statistic: 268.6 on 5 and 494 DF,  p-value: < 2.2e-16
#We can see a few things. First Ad 1 is > Ad 0 as is Ad 2. Age is 
#a significant with increases in Age leading to increased Ratings.
#None of the interactions are significant (one is marginal).
#It is possible that the one interaction would be significant 
#if we look into the possibility of outliers or high leverage value observations.

qqnorm(model$residuals)
qqline(model$residuals)
shapiro.test(model$residuals)
#Shapiro-Wilk normality test

#data:  model$residuals
#W = 0.8466, p-value < 2.2e-16
#We clearly do not have good fit here. Lets use Cooks Distance
#to see which data points are having a big impact on our model

cutoff <- 4/((nrow(RegressionExample)-length(model$coefficients)-1)) 
plot(model, which=4, cook.levels=cutoff)
#It appears observations 140, 249, and 421 are having a
#big impact on our model relative to other observations.
#Lets check them out in our data (140 rating = 2, 249
#rating = 3, and 421 rating = 33)

#For 140 and 249 these points seem to make sense as they are 
#ratings which are much lower than the average ratings (recall 
#our normal density plot suggested these as outliers to begin with).
#Lets now check for outliers:
outlierTest(model)
#rstudent unadjusted p-value Bonferroni p
#249 -8.970361         6.1786e-18   3.0893e-15
#119  6.233434         9.8075e-10   4.9038e-07
#421 -5.824005         1.0373e-08   5.1866e-06
#140 -5.135732         4.0540e-07   2.0270e-04
#333 -4.492803         8.7663e-06   4.3832e-03
#313  4.472160         9.6203e-06   4.8101e-03
#186  3.982162         7.8580e-05   3.9290e-02
#146  3.969937         8.2591e-05   4.1295e-02

#It appears that the three data points identified as having a large 
#impact on our model are also outliers.
#Lets re-run our model excluding these observations and see if our 
#model now fits better.
RegressionExample$number = row.names(RegressionExample)

model1.2 <- lm(Rating ~ Ad*CAge,data=subset(RegressionExample,
    number != 140 & number != 249 & number != 421))
#We find with the removal of those three data points the interaction that was marginal 
#is now far from significant indicating the interactions really aren't meaningful.
#We could remove them but we don't need to here (we aim to build a predictive model

qqnorm(model1.2$residuals)
qqline(model1.2$residuals)
shapiro.test(model1.2$residuals)
#Shapiro-Wilk normality test

#data:  model1.2$residuals
#W = 0.92509, p-value = 4.909e-15
#The plot looks much better and the W value is much 
#closer to 1 but its still showing some departures at the
#extremes.
#As we are not trying to build a predictive model
#this is probably good enough (note you can use the
#plot(model) command to get diagnostic plots for all residual assumptions in one go)

#Recall that our regression only compared Ad 1 and Ad 2 to Ad 0 and 
#in both cases we find a significant difference.
#Now we are also interested in whether Ad 1 and Ad 2 differ (looking 
#at their respective coefficients its likely they will differ but we need to be sure)
#We need the "multcomp" package:
install.packages("multcomp")
library(multcomp)
tukey <- glht(model1.2, linfct = mcp(Ad = "Tukey"))
summary(tukey, test = adjusted())
#Simultaneous Tests for General Linear Hypotheses
#Multiple Comparisons of Means: Tukey Contrasts

#Fit: lm(formula = Rating ~ Ad * CAge, data = subset(RegressionExample, 
#number != 140 & number != 249 & number != 421))

#Linear Hypotheses:
#  Estimate Std. Error t value Pr(>|t|)    
#1 - 0 == 0   1.6184     0.7630   2.121 0.086438 .  
#2 - 0 == 0   3.2376     0.8321   3.891 0.000321 ***
#  2 - 1 == 0   1.6192     0.7832   2.067 0.097537 .  
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#(Adjusted p values reported -- single-step method)
#We can see 2 and 1 don't differ significantly and likely wouldn't 
#even with p values being unadjusted. Thus, in this case it looks like 
#Ad 2 is the best Ad (always be sure to get your means)
tapply(RegressionExample$Rating, RegressionExample$Ad, mean)
#0        1        2 
#54.73054 61.68263 70.17470 