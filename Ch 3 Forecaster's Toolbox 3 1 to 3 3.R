library (fpp2)
#set training data from 1992 to 2007
beer2<-window(ausbeer,start=1992, end=c(2007,4))
#plot some forecasts for quarterly beer production
autoplot(beer2) +
autolayer(meanf(beer2,h=11),
series="Mean", PI=FALSE)+
autolayer(naive(beer2, h=11),
series="Naive",PI=FALSE)+
autolayer(snaive(beer2, h=11),
series="Seasonal naive",PI=FALSE)+
ggtitle("Forecasts for quarterly beer production")+
xlab("Year")+ylab("Megalitres")+
guides(colour=guide_legend(title="Forecast"))

#non-seaonal methods to series of 200 days
#of hte Google daily closing stock price
autoplot(goog200)+
autolayer(meanf(goog200,h=40),
series="Mean", PI=FALSE)+
autolayer(rwf(goog200,h=40),
series="Naive", PI=FALSE)+
autolayer(rwf(goog200, drift=TRUE, h=40),
series="Drift", PI=FALSE)+
ggtitle("Google stock (daily ending 6 Dec 2013)")+
xlab("Day")+ylab("Closing Price(US$)")+
guides(colour=guide_legend(title="Forecast"))

#monthdays() will remove variation before fitting
#forecasting model
dframe<-cbind(Monthly=milk,
DailyAverage=milk/monthdays(milk))

autoplot(dframe,facet=TRUE)+
xlab("Years")+ylab("Pounds")+
ggtitle("Milk production per cow")

#Box-Cox.lambda()
(lambda<-BoxCox.lambda(elec))
autoplot(BoxCox(elec,lambda))    

#Bias adjusted. Bias adjustment is not done by default in 
#the forecast package. If you want your forecasts to be 
#means rather than medians, use the argument bias
#adj=TRUE when you select your Box-Cox transformation 
#parameter
fc<-rwf(eggs, drift=TRUE, lambda=0, h=50,level=80)
fc2<-rwf(eggs, drift=TRUE, lambda=0, h=50, level=80,
biasadj=TRUE)

autoplot(eggs)+
autolayer(fc,series="Simple back transformation")+
autolayer(fc2, series="Bias adjusted", PI=FALSE)+
guides(colour=guide_legend(title="Forecast"))

autoplot(goog200)+
xlab("Day")+ylab("Closing Price(US$)")+
ggtitle("Google Stock (daily ending 6 December 2013)")
#Residuals using naive method. Good forecasting method:
#1)Resids are uncorrelated; if correlations then information
#left in resids which should be used in computing forecasts;
#2)resids have zero mean; if resids have mean other than 
#zero, then forecasts are biased
res<-residuals(naive(goog200))

autoplot(res)+xlab("Day")+ylab("")+
ggtitle("Residuals from naive method")

gghistogram(res)+ggtitle("Histogram of residuals")
#Right tail of histogram too long for normal distribution
ggAcf(res)+ggtitle("ACF of residuals")
#lack of correlation in ACF graph suggests forecasts are good

#Box test
#Box.test(res,lag=10,fitdf=0)
#Box-Pierce test
#data:  res
#X-squared = 10.611, df = 10, p-value = 0.3886
Box.test(res,lag=10, fitdf=0, type="Lj")
#Box-Ljung test
#data:  res
#X-squared = 11.031, df = 10, p-value = 0.3551
#For both Q and Q*, the results are not significant(not >.05)

#checkresiduals(): time plot, ACF plot, histogram of resids
#with normal distribution overlay, and Ljung-Box test with
#correct degrees of freedom
checkresiduals(naive(goog200))
