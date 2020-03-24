library(fpp2)
#extracts all data from 1995 onward
window(ausbeer, start=1995)
#subset() allws more types of subsetting;
#allows the use of indices to use subset;
#extracts last 5 years of observations
subset(ausbeer, start=length(ausbeer)-4*5)
#Extracts first quarter for all years
subset(ausbeer, quarter=1)
#head and tail useful for extracting first few or
#last observations; last 5 years below
tail(ausbeer, 4*5)

#Qtr1 Qtr2 Qtr3 Qtr4
#1995  426  408  416  520
#1996  409  398  398  507
#1997  432  398  406  526
#1998  428  397  403  517
#1999  435  383  424  521
#32000  421  402  414  500
#2001  451  380  416  492
#2002  428  408  406  506
#2003  435  380  421  490
#2004  435  390  412  454
#2005  416  403  408  482
#2006  438  386  405  491
#2007  427  383  394  473
#2008  420  390  410  488
#2009  415  398  419  488
#2010  414  374          
#> subset(ausbeer, start=length(ausbeer)-4*5)
#Qtr1 Qtr2 Qtr3 Qtr4
#2005       403  408  482
#2006  438  386  405  491
#2007  427  383  394  473
#2008  420  390  410  488
#2009  415  398  419  488
#2010  414  374          
#> subset(ausbeer, quarter=1)
#Time Series:
 # Start = 1956 
#End = 2010 
#Frequency = 1 
#[1] 284 262 272 261 286 295 279 294 313 331 335 353 393 383 387 410
#[17] 419 458 465 500 510 486 515 503 513 548 493 475 453 464 459 481
#[33] 474 467 485 464 443 433 449 426 409 432 428 435 421 451 428 435
#[49] 435 416 438 427 420 415 414
#> #head and tail useful for extracting first few or
 # > #last observations; last 5 years below
 # > tail(ausbeer, 4*5)
#Qtr1 Qtr2 Qtr3 Qtr4
#2005            408  482
#2006  438  386  405  491
#2007  427  383  394  473
#2008  420  390  410  488
#2009  415  398  419  488
#2010  414  374          

#Forecasts for quarterly beer production
beer2<-window(ausbeer, start=1992, end=c(2007,4))
beerfit1<-meanf(beer2,h=10)
beerfit2<-rwf(beer2,h=10)
beerfit3<-snaive(beer2,h=10)

autoplot(window(ausbeer,start=1992))+
autolayer(beerfit1,series="Mean", PI=FALSE)+
autolayer(beerfit2,series="Naive", PI=FALSE)+
autolayer(beerfit3,series="Seasonal naive", PI=FALSE)+
xlab("Year")+ylab("Megalitres")+
ggtitle("Forecasts for quarterly beer production")+
guides(colour=guide_legend(title="Forecast"))

#Forecast accuracies; code doesn't work below
beer3 <- window(ausbeer, start=2008)
accuracy(beerfit1, beer3)
#               ME     RMSE      MAE        MPE     MAPE     MASE
#Training set   0.000 43.62858 35.23438 -0.9365102 7.886776 2.463942
#Test set     -13.775 38.44724 34.82500 -3.9698659 8.283390 2.435315
#ACF1 Theil's U
#Training set -0.10915105        NA
#Test set     -0.06905715  0.801254
accuracy(beerfit2, beer3)
#                     ME     RMSE      MAE         MPE     MAPE
#Training set   0.4761905 65.31511 54.73016  -0.9162496 12.16415
#Test set     -51.4000000 62.69290 57.40000 -12.9549160 14.18442
#MASE        ACF1 Theil's U
#Training set 3.827284 -0.24098292        NA
#Test set     4.013986 -0.06905715  1.254009
accuracy(beerfit3, beer3)
#                   ME     RMSE  MAE        MPE     MAPE      MASE
#Training set -2.133333 16.78193 14.3 -0.5537713 3.313685 1.0000000
#Test set      5.200000 14.31084 13.4  1.1475536 3.168503 0.9370629
#ACF1 Theil's U
#Training set -0.2876333        NA
#Test set      0.1318407  0.298728
                #RMSE 	MAE 	MAPE 	MASE
#Mean method 	38.45 	34.83 	8.28 	2.44
#Naïve method 	62.69 	57.40 	14.18 	4.01
#Seasonal naïve method 	14.31 	13.40 	3.17 	0.94
#It is obvious from the graph that the seasonal naïve method
#is best for these data, although it can still be improved, 
#as we will discover later. Sometimes, different accuracy 
#measures will lead to different results as to which forecast 
#method is best. However, in this case, all of the results 
#point to the seasonal naïve method as the best of these 
#three methods for this data set.


googfc1 <- meanf(goog200, h=40)
googfc2 <- rwf(goog200, h=40)
googfc3 <- rwf(goog200, drift=TRUE, h=40)
autoplot(subset(goog, end = 240)) +
  autolayer(googfc1, PI=FALSE, series="Mean") +
  autolayer(googfc2, PI=FALSE, series="Naïve") +
  autolayer(googfc3, PI=FALSE, series="Drift") +
  xlab("Day") + ylab("Closing Price (US$)") +
  ggtitle("Google stock price (daily ending 6 Dec 13)") +
  guides(colour=guide_legend(title="Forecast"))

googtest <- window(goog, start=201, end=240)
accuracy(googfc1, googtest)
#                        ME   RMSE       MAE        MPE     MAPE
#Training set -4.296286e-15  36.91961  26.86941 -0.6596884  5.95376
#Test set      1.132697e+02 114.21375 113.26971 20.3222979 20.32230
#MASE      ACF1 Theil's U
#Training set  7.182995 0.9668981        NA
#Test set     30.280376 0.8104340  13.92142
accuracy(googfc2, googtest)
#                   ME      RMSE       MAE       MPE      MAPE
#Training set  0.6967249  6.208148  3.740697 0.1426616 0.8437137
#Test set     24.3677328 28.434837 24.593517 4.3171356 4.3599811
#MASE        ACF1 Theil's U
#Training set 1.000000 -0.06038617        NA
#Test set     6.574582  0.81043397  3.451903
accuracy(googfc3, googtest)
#                       ME      RMSE       MAE         MPE      MAPE
#Training set -5.998536e-15  6.168928  3.824406 -0.01570676 0.8630093
#Test set      1.008487e+01 14.077291 11.667241  1.77566103 2.0700918
#MASE        ACF1 Theil's U
#Training set 1.022378 -0.06038617        NA
#Test set     3.119002  0.64732736  1.709275

                #RMSE 	MAE 	MAPE 	  MASE
#Mean method 	114.21 	113.27 	20.32 	30.28
#Naïve method 	28.43 	24.59 	4.36 	6.57
#Drift method 	14.08 	11.67 	2.07 	3.12
#Here, the best method is the drift method 
#(regardless of which accuracy measure is used)

e<-tsCV(goog200, rwf, drift=TRUE, h=1)
sqrt(mean(e^2, na.rm=TRUE))
#[1] 6.233245
sqrt(mean(residuals(rwf(goog200, drift=TRUE))^2, na.rm=TRUE))
#[1] 6.168928

#As expected, the RMSE from the residuals is smaller, as the 
#corresponding "forecasts" are based on a model 
#The code below evaluates the forecasting performance of 1- to 
#8-step-ahead naïve forecasts with tsCV(), using MSE as the 
#forecast error measure. The plot shows that the forecast error 
#increases as the forecast horizon increases, as we would expect
e <- tsCV(goog200, forecastfunction=naive, h=8)
# Compute the MSE values and remove missing values
mse <- colMeans(e^2, na.rm = T)
# Plot the MSE values against the forecast horizon
data.frame(h = 1:8, MSE = mse) %>%
  ggplot(aes(x = h, y = MSE)) + geom_point()
