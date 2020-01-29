#Assignemnt 3
file.choose()
load("/Users/daniellehatt/Desktop/Ecologyworkshop/ARIMA_Workshop.RData")
install.packages("zoo")
install.packages("tseries")
install.packages("fracdiff")
install.packages("forecast")
install.packages("xts")
library(zoo) 
library(tseries)
library(fracdiff)
library(forecast) 
library(xts)
#Create timeseries object
nee <- ts( mangroves$nee, start= 1, frequency=30)
nee
#Visualize data
par(mfrow=c(1,1), mai=c(0.25,0.8,0.1, 0.1)) 
plot( nee, typ="l", ylab= "NEE", xlab="")
#removing outliers
plot(nee)
lines(tsclean(nee), col="red")
nee <- tsclean(nee)
#Decompose timeseries
nee.d <- decompose(nee, 'multiplicative') 
plot(nee.d)
#Test for stationarity (assumptions)
adf.test(nee)
#Detecting Autocorrelation
acf(nee, lag.max=45)
pacf(nee, lag.max=45)
#Fitting ARIMA Model
arima.nee1 <-auto.arima(nee, trace=TRUE)
tsdisplay(residuals(arima.nee1), lag.max=45)
arima.nee2 <-arima(nee , order=c(10,1,3), seasonal= list(order=c(2,0,2)))
sdisplay(residuals(arima.nee2), lag.max= 30)
#Minimizing AIC
AIC(arima.nee1, arima.nee2)
par(mfrow=c(1,1))
plot(nee , typ="l"); lines(fitted(arima.nee2),col="red")
checkresiduals(arima.nee2, lag=36)
par(mfrow=c(1,1))
plot(nee , typ="l"); lines(fitted(arima.nee2),col="red")
plot(forecast(arima.nee2, h=30))

#mydata
file.choose()
Tab.DK=read.csv("/Users/daniellehatt/Desktop/BSC 6926/Data.DK.csv", header = TRUE)
Tab.DK
nee1 <- ts( Tab.DK$Biomass, start= 1, frequency=6)
nee1
par(mfrow=c(1,1), mai=c(0.25,0.8,0.1, 0.1)) 
plot( nee1, typ="l", ylab= "Biomass", xlab="")
plot(nee1)
lines(tsclean(nee1), col="red")
nee1 <- tsclean(nee1)
nee1
nee.d1 <- decompose(nee1, 'multiplicative') 
plot(nee.d1)
adf.test(nee1) #ts is satationary:null hyp not rejected
acf(nee1, lag.max=45)
pacf(nee1, lag.max=45)
arima.nee2 <-auto.arima(nee1, trace=TRUE)
tsdisplay(residuals(arima.nee2), lag.max=50)
AIC(arima.nee2)
par(mfrow=c(1,1))
plot(nee1 , typ="l"); lines(fitted(arima.nee2),col="red")
checkresiduals(arima.nee2, lag=36)
plot(forecast(arima.nee2, h=12))

#Explanatory series for temperature
Tab.DK=read.csv("/Users/daniellehatt/Desktop/BSC 6926/Data.DK.csv", header = TRUE)
Tab.DK
temp <- ts(Tab.DK$Temperature, start= 1, frequency=6)
par(mfrow=c(1,1), mai=c(0.25,0.8,0.1, 0.1)) 
plot(temp, typ="l", ylab= "Temperature", xlab="")
lines(tsclean(temp) , col="red")
temp <- tsclean(temp)
temp.d <- decompose(temp, 'multiplicative') 
plot(temp.d)
adf.test(temp)
ccf( diff(temp),nee1, na.action = na.pass, lag.max=40, plot=TRUE)
arima.nee3 <-auto.arima(nee1, xreg=c(diff(temp),0), trace=TRUE)
AIC(arima.nee2, arima.nee3)
temp.i <- temp
temp.i[temp.i > 28 ]<- 0 
temp.i[temp.i >= 28 ]<- 1
plot(temp.i)
arima.nee4 <-auto.arima(nee1, xreg=temp.i, trace=TRUE)
AIC(arima.nee2,arima.nee4 )
checkresiduals(arima.nee4, lag=36)
par(mfrow=c(1,1))
plot(nee1 , typ="l"); lines(fitted(arima.nee4),col="red")

#Explanatory series for salinity
Tab.DK=read.csv("/Users/daniellehatt/Desktop/BSC 6926/Data.DK.csv", header = TRUE)
Tab.DK
sal <- ts(Tab.DK$Salinity, start= 1, frequency=6)
par(mfrow=c(1,1), mai=c(0.25,0.8,0.1, 0.1)) 
plot(sal , typ="l", ylab= "Salinity", xlab="")
lines(tsclean(sal) , col="red")
sal <- tsclean(sal)
sal.d <- decompose(sal, 'multiplicative') 
plot(sal.d)
adf.test(sal)
ccf( diff(sal),nee1, na.action = na.pass, lag.max=40, plot=TRUE)
arima.nee5 <-auto.arima(nee1, xreg=c(diff(sal),0), trace=TRUE)
AIC(arima.nee2, arima.nee5 )
sal.i <- sal 
sal.i[sal.i < 25 ]<- 0 
sal.i[sal.i >= 25 ]<- 1
plot(sal.i)
arima.nee6 <-auto.arima(nee1, xreg=sal.i, trace=TRUE)
AIC(arima.nee2,arima.nee6 )
checkresiduals(arima.nee6, lag=36)
par(mfrow=c(1,1))
plot(nee1 , typ="l"); lines(fitted(arima.nee6),col="red")

file.choose()






