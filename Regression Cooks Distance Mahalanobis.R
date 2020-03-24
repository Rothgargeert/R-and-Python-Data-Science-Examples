install.packages("QuantPsyc")

###set working directory
setwd("~/OneDrive - Missouri State University/TEACHING/745 Grad Statistics/notes/fall 15")
options(scipen = 999)

##import the datafile
library(haven)
regdata = read_spss("c7 regression.sav")
c6_liar = as.data.frame(c6_liar)
regdata<-c7_regression
master = regdata[ , c(8:10)]
model1 = lm(CESD_total ~ PIL_total + AUDIT_TOTAL_NEW, data = master)
mahal = mahalanobis(master, 
                    colMeans(master), 
                    cov(master))
cutmahal = qchisq(1-.001, ncol(master))
cutmahal
badmahal = as.numeric(mahal > cutmahal)
table(badmahal)
badmahal
0   1 
266   1
k=3
leverage = hatvalues(model1)
cutleverage = (2*k+2) / nrow(master)
cutleverage
badleverage = as.numeric(leverage > cutleverage)
table(badleverage)
badleverage
0   1 
251  16
cooks = cooks.distance(model1)
cutcooks = 4 / (nrow(master) - k - 1)
cutcooks
badcooks = as.numeric(cooks > cutcooks)
table(badcooks)
totalout = badmahal + badleverage + badcooks
table(totalout)
totalout
0   1   2   3 
240  21   5   1 
model2 = lm(CESD_total ~ PIL_total + AUDIT_TOTAL_NEW, data = noout)
noout = subset(master, totalout < 2)
noout
summary(model2, correlation = TRUE)
Call:
  lm(formula = CESD_total ~ PIL_total + AUDIT_TOTAL_NEW, data = noout)

Residuals:
  Min      1Q  Median      3Q     Max 
-13.535  -5.172  -1.545   3.168  29.820 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)     54.31164    4.16332  13.045   <2e-16 ***
  PIL_total       -0.37281    0.03603 -10.346   <2e-16 ***
  AUDIT_TOTAL_NEW  0.00120    0.08315   0.014    0.988    
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 7.422 on 258 degrees of freedom
Multiple R-squared:  0.298,	Adjusted R-squared:  0.2926 
F-statistic: 54.77 on 2 and 258 DF,  p-value: < 2.2e-16

Correlation of Coefficients:
  (Intercept) PIL_total
PIL_total       -0.98                
AUDIT_TOTAL_NEW -0.28        0.15    
standardized = rstudent(model2)
fitted = scale(model2$fitted.values)
qqnorm(standardized)
abline(0,1)
hist(standardized)
plot(fitted, standardized)
abline(0,0)
abline(v = 0)
summary(model2)
Call:
  lm(formula = CESD_total ~ PIL_total + AUDIT_TOTAL_NEW, data = noout)

Residuals:
  Min      1Q  Median      3Q     Max 
-13.535  -5.172  -1.545   3.168  29.820 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)     54.31164    4.16332  13.045   <2e-16 ***
  PIL_total       -0.37281    0.03603 -10.346   <2e-16 ***
  AUDIT_TOTAL_NEW  0.00120    0.08315   0.014    0.988    
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 7.422 on 258 degrees of freedom
Multiple R-squared:  0.298,	Adjusted R-squared:  0.2926 
F-statistic: 54.77 on 2 and 258 DF,  p-value: < 2.2e-16
library(QuantPsyc)
lm.beta(model2)
PIL_total AUDIT_TOTAL_NEW 
-0.5458229582    0.0007613226 