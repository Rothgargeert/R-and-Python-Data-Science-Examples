install.packages("fpp2")

library(fpp2)

#Time plot airline passengers
autoplot(melsyd[,"Economy.Class"]) +
  ggtitle("Economy class passengers: Melbourne-Sydney") +
  xlab("Year") +
  ylab("Thousands")

#Time plot antidiabetic drugs
autoplot(a10) +
  ggtitle("Antidiabetic drug sales") +
  ylab("$ million") +
  xlab("Year")

#Seasonal plot antidiabetic drugs
ggseasonplot(a10, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("$ million") +
  ggtitle("Seasonal plot: antidiabetic drug sales")

#Polar seasonal plot antidiabetic drugs
ggseasonplot(a10, polar=TRUE) +
  ylab("$ million") +
  ggtitle("Polar seasonal plot: antidiabetic drug sales")

#Seasonal subseries antidiabetic drugs
ggsubseriesplot(a10) +
  ylab("$ million") +
  ggtitle("Seasonal subseries plot: antidiabetic drug sales")

#Time series trend half-hourly electric demand and temperature
autoplot(elecdemand[,c("Demand","Temperature")], facets=TRUE) +
  xlab("Year: 2014") + ylab("") +
  ggtitle("Half-hourly electricity demand: Victoria, Australia")

#Scatterplot electric and temp
qplot(Temperature, Demand, data=as.data.frame(elecdemand)) +
  ylab("Demand (GW)") + xlab("Temperature (Celsius)")

#Time series quarterly visitor nights
autoplot(visnights[,1:5], facets=TRUE) +
  ylab("Number of visitor nights each quarter (millions)")

#Scatterplot matrices
install.packages("GGally")
library(GGally)
GGally::ggpairs(as.data.frame(visnights[,1:5]))

#Lagplot beer production
beer2 <- window(ausbeer, start=1992)
gglagplot(beer2)

#Corellogram beer production
ggAcf(beer2)

#Time series electric demand
aelec <- window(elec, start=1980)
autoplot(aelec) + xlab("Year") + ylab("GWh")
#Corellogram electric demand
ggAcf(aelec, lag=48)

#White Noise series
set.seed(30)
y <- ts(rnorm(50))
autoplot(y) + ggtitle("White noise")
#White noise series corellogram
ggAcf(y)
