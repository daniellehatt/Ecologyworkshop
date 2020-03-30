#Final Project

file.choose()
install.packages("zoo")
install.packages("tseries")
install.packages("fracdiff")
install.packages("forecast")
install.packages("xts")
library(ggplot2)
library(zoo) 
library(tseries)
library(fracdiff)
library(forecast) 
library(xts)

#Sprigger Bank: Time Series
file.choose()
Tab.SB=read.csv("/Users/daniellehatt/Desktop/BSC 6926/Final Project Datasets/LTER.SB.csv", header = TRUE)
Tab.SB
SB1 <- ts( Tab.SB$Biomass, start= 1, frequency=4)
SB1
par(mfrow=c(1,1), mai=c(1,1,1,1))
plot( SB1, typ="l", ylab= "Biomass"~"("~"g/m"^{2}~"per quadrat"~")", xlab="Time" ,xaxt="none", main="Figure 1 showing biomass of Penicillus at SB from 2007-2017")
axis(1,at=seq(1,11,1),  labels=seq(2007,2017,1))
lines(tsclean(SB1), col="red")
SB1 <- tsclean(SB1)
plot(SB1, ylab= "Biomass"~"("~"g/m"^{2}~"per quadrat"~")", xlab="Time" ,xaxt="none", main="Biomass at Sprigger Bank from 2007-2017")
axis(1,at=seq(1,12,1),  labels=seq(2007,2018,1))
SB.d1 <- decompose(SB1, 'multiplicative') 
par(mfrow=c(1,1), mai=c(0.5,0.5,0.5, 0.5)) 
plot(SB.d1, xaxt="none", xlab="")
axis(1,at=seq(1,12,1),  labels=seq(2007,2018,1))
adf.test(SB1) #ts is not stationary:Reject the null hyp
adf.test(diff(SB1,1))
acf(diff(SB1, 1), lag.max=4)
pacf(diff(SB1, 1), lag.max=4)
arima.SB2 <-auto.arima((SB1), trace=TRUE)
SB2= diff(SB1, differences = 2, lag = 4)
adf.test(SB2)
acf((SB2), lag.max=4)
pacf((SB2), lag.max=4)
plot(SB2)
arima.SB2 <-auto.arima((SB2), trace=TRUE)
tsdisplay(residuals(arima.SB2), lag.max=4)
AIC(arima.SB2)
par(mfrow=c(1,1), mai=c(1,1,1,1))
plot(SB2 , typ="l", ylab= "Biomass"~"("~"g/m"^{2}~"per quadrat"~")", xlab="Time", xaxt="none", main="ARIMA model fitted to biomass data for Sprigger Bank"); lines(fitted(arima.SB2),col="red")
axis(1,at=seq(1,12,1),  labels=seq(2007,2018,1))
checkresiduals(arima.SB2, lag=4)
plot(forecast(arima.SB2, h=20), xaxt="none", ylab= "Biomass"~"("~"g/m"^{2}~"per quadrat"~")", xlab="Time")
axis(1,at=seq(1,21,1),  labels=seq(2007,2027,1))
summary(Tab.SB$Temperature)
summary(Tab.DK$Temperature)
summary(Tab.BA$Temperatire)
mean(Tab.SB$Temperatire)
sd(Tab.SB$Temperature)
summary(Tab.SB$Biomass)
summary(Tab.DK$Biomass)
summary(Tab.BA$Biomass)


#Explanatory series for Sprigger Bank
#Explanatory series: Temperature
Tab.SB
Tab.SB$Temperature
temp <- ts(Tab.SB$Temperature, start= 1, frequency=4)
par(mfrow=c(1,1), mai=c(0.9,0.9,0.9, 0.9)) 
plot(temp, typ="l", ylab= "Temperature", xlab="Time")
lines(tsclean(temp) , col="red")
temp <- tsclean(temp)
temp.d <- decompose(temp, 'multiplicative') 
plot(temp.d)
adf.test(temp)
temp2.SB= diff(temp, differences = 2, lag = 4)
temp2.SB
adf.test(temp2.SB)
acf(temp2.SB)
pacf(temp2.SB)
plot(temp2.SB)
ccf(temp2.SB,SB2, na.action = na.pass, lag.max=4, plot=TRUE)
arima.SB3 <-auto.arima(SB2, xreg=c(temp2.SB), trace=TRUE)
AIC(arima.SB2, arima.SB3)
plot(SB2 , typ="l", ylab="Biomass", xlab="Time"); lines(fitted(arima.SB3),col="red")
temp.i <- temp
temp.i[temp.i < 29 ]<- 0 
temp.i[temp.i >= 29 ]<- 1
plot(temp.i, ylab="Temperature", xlab="Time")
temp.i=diff(temp.i, differences= 2, lag =4)
arima.SB4 <-auto.arima(SB2, xreg=temp.i, trace=TRUE)
AIC(arima.SB2,arima.SB4 )
AIC(arima.SB3,arima.SB4 )
checkresiduals(arima.SB3, lag=36)
par(mfrow=c(1,1))
plot(SB2 , typ="l", ylab= "Biomass"~"("~"g/m"^{2}~"per quadrat"~")", xlab="Time", xaxt="none"); lines(fitted(arima.SB3),col="red")

#Explanatory series for salinity
Tab.SB
sal <- ts(Tab.SB$Salinity, start= 1, frequency=4)
par(mfrow=c(1,1), mai=c(0.25,0.8,0.1, 0.1)) 
plot(sal , typ="l", ylab= "Salinity", xlab="Time")
lines(tsclean(sal) , col="red")
sal <- tsclean(sal)
sal.d <- decompose(sal, 'multiplicative') 
plot(sal.d)
adf.test(sal)
sal2= diff(sal, differences = 2, lag = 4)
sal2
adf.test(sal2)
acf(sal2)
pacf(sal2)
plot(sal2)
ccf(sal2,SB2, na.action = na.pass, lag.max=4, plot=TRUE)
plot(sal2)
arima.SB5 <-auto.arima(SB2, xreg=sal2, trace=TRUE)
AIC(arima.SB2, arima.SB5 )
sal.i <- sal 
sal.i[sal.i < 24 ]<- 0 
sal.i[sal.i >= 24 ]<- 1
plot(sal.i)
sal.i=diff(sal.i, differences= 2, lag =4)
arima.SB6 <-auto.arima(SB2, xreg=sal.i, trace=TRUE)
AIC(arima.SB2,arima.SB6 )
checkresiduals(arima.SB6, lag=4)
par(mfrow=c(1,1), mai=c(0.5,0.5,0.5, 0.5))
plot(SB1 , typ="l", ylab="", xlab="", xaxt="none", main=c("ARIMA model with extreme salinity", "as a driver of biomass at Sprigger Bank")); lines(fitted(arima.SB5),col="red")
axis(1,at=seq(1,12,1),  labels=seq(2007,2018,1))

#explanatory variable: salinity and temperature
x=c(temp2.SB,sal2)
x
y=cbind(temp2,sal2)
adf.test(x)
arima.SB7 <-auto.arima(SB2, xreg=y, trace=TRUE)
AIC(arima.SB2,arima.SB7)
AIC(arima.SB3,arima.SB7)
par(mfrow=c(1,1), mai=c(1,1,1,1))
plot(SB2 , typ="l", ylab="", xlab="", xaxt="none", main=c("ARIMA model with temperature", "and salinity as a drivers of biomass at Sprigger Bank"));
lines(fitted(arima.SB7),col="red")
axis(1,at=seq(1,12,1),  labels=seq(2007,2018,1))

z=c(temp.i,sal.i)
z
y=cbind(temp.i,sal.i)
adf.test(z)
adf.test(diff(z))
arima.SB8 <-auto.arima(SB1, xreg=y, trace=TRUE)
AIC(arima.SB2,arima.SB8 )
plot(SB1 , typ="l", ylab="", xlab=""); lines(fitted(arima.SB7),col="red")

#Duck Key: Time Series
file.choose()
Tab.DK=read.csv("/Users/daniellehatt/Desktop/BSC 6926/Final Project Datasets/LTERData.DK.csv", header = TRUE)
Tab.DK
DK1 <- ts( Tab.DK$Biomass, start= 1, frequency=4)
DK1
par(mfrow=c(1,1), mai=c(1,1,1,1)) 
plot( DK1, typ="l", ylab= "Biomass"~"("~"g/m"^{2}~"per quadrat"~")", xlab="Time", xaxt="none", main="Biomass at Duck Key from 2007-2017")
axis(1,at=seq(1,12,1),  labels=seq(2007,2018,1))
lines(tsclean(DK1), col="red")
DK1 <- tsclean(DK1)
plot(DK1, ylab= "Biomass"~"("~"g/m"^{2}~"per quadrat"~")", xlab="Time", xaxt="none", main="Biomass at Duck Key from 2007-2017")
axis(1,at=seq(1,12,1),  labels=seq(2007,2018,1))
DK.d1 <- decompose(DK1, 'multiplicative') 
par(mfrow=c(1,1), mai=c(.6,.6,.6,.6)) 
plot(DK.d1, xaxt="none", xlab="")
axis(1,at=seq(1,12,1),  labels=seq(2007,2018,1))
adf.test(DK1) #ts is satationary:null hyp not rejected
DK2= diff(DK1, differences = 2, lag = 4)
plot(DK2)
acf(DK2, lag.max=4)
pacf(DK2, lag.max=4)
arima.DK1 <-auto.arima(DK2, trace=TRUE)
tsdisplay(residuals(arima.DK1), lag.max=4)
AIC(arima.DK1)
par(mfrow=c(1,1), mai=c(1,1,1,1)) 
plot(DK2 , typ="l", ylab= "Biomass"~"("~"g/m"^{2}~"per quadrat"~")", xlab="Time", xaxt="none", main="ARIMA model fitted to biomass data for Duck Key"); lines(fitted(arima.DK1),col="red")
axis(1,at=seq(1,12,1),  labels=seq(2007,2018,1))
checkresiduals(arima.DK1, lag=4)
plot(forecast(arima.DK1, h=20), xaxt="none",  ylab= "Biomass"~"("~"g/m"^{2}~"per quadrat"~")", xlab="Time")
axis(1,at=seq(1,15,1),  labels=seq(2007,2021,1))

#Explanatory series for Duck Key
#Explanatory series: Temperature
Tab.DK
Tab.DK$Temperature
temp <- ts(Tab.DK$Temperature, start= 1, frequency=4)
par(mfrow=c(1,1), mai=c(0.9,0.9,0.9, 0.9)) 
plot(temp, typ="l", ylab= "Temperature", xlab="Time")
lines(tsclean(temp) , col="red")
temp <- tsclean(temp)
temp.d <- decompose(temp, 'multiplicative') 
plot(temp.d)
adf.test(temp)
temp2.DK= diff(temp, differences = 2, lag = 4)
temp2.DK
adf.test(temp2.DK)
acf(temp2.DK)
pacf(temp2.DK)
plot(temp2.DK)
ccf(temp2.DK,DK2, na.action = na.pass, lag.max=4, plot=TRUE)
arima.DK3 <-auto.arima(DK2, xreg=c(temp2.DK), trace=TRUE)
AIC(arima.DK1, arima.DK3)
plot(DK2 , typ="l", ylab= "Biomass"~"("~"g/m"^{2}~"per quadrat"~")", xlab="Time", xaxt="none"); lines(fitted(arima.DK3),col="red")
axis(1,at=seq(1,12,1),  labels=seq(2007,2018,1))
temp.i <- temp2
temp.i[temp.i < 29 ]<- 0 
temp.i[temp.i >= 29 ]<- 1
plot(temp.i, ylab="Temperature", xlab="Time")
arima.DK4 <-auto.arima(DK2, xreg=temp.i, trace=TRUE)
AIC(arima.DK1,arima.DK4 )
AIC(arima.DK3,arima.DK4 )
checkresiduals(arima.DK3, lag=4)
par(mfrow=c(1,1))
plot(DK2 , typ="l", ylab= "Biomass"~"("~"g/m"^{2}~"per quadrat"~")", xlab="Time", xaxt="none"); lines(fitted(arima.DK3),col="red")
axis(1,at=seq(1,12,1),  labels=seq(2007,2018,1))

#Explanatory series for salinity
Tab.DK
sal <- ts(Tab.DK$Salinity, start= 1, frequency=4)
sal
par(mfrow=c(1,1), mai=c(0.25,0.8,0.1, 0.1)) 
plot(sal, typ="l", ylab= "Salinity", xlab="Time")
lines(tsclean(sal) , col="red")
sal <- tsclean(sal)
sal.d <- decompose(sal, 'multiplicative') 
plot(sal.d)
adf.test(sal)
sal2= diff(sal, differences = 2, lag = 4)
sal2
adf.test(sal2)
acf(sal2)
pacf(sal2)
plot(sal2)
ccf(sal2,DK2, na.action = na.pass, lag.max=4, plot=TRUE)
plot(sal2)
arima.DK5 <-auto.arima(DK2, xreg=sal2, trace=TRUE)
AIC(arima.DK1, arima.DK5 )
sal.i <- sal 
sal.i[sal.i < 35 ]<- 0 
sal.i[sal.i >= 35 ]<- 1
plot(sal.i)
sal.i= diff(sal, differences = 2, lag = 4)
arima.DK6 <-auto.arima(DK2, xreg=sal.i, trace=TRUE)
AIC(arima.DK2,arima.DK6 )
AIC(arima.DK2,arima.DK6 )
checkresiduals(arima.DK6, lag=4)
par(mfrow=c(1,1), mai=c(0.5,0.5,0.5, 0.5))
plot(DK1 , typ="l", ylab="", xlab="", xaxt="none", main=c("ARIMA model with extreme salinity", "as a driver of biomass at Sprigger Bank")); lines(fitted(arima.DK5),col="red")
axis(1,at=seq(1,12,1),  labels=seq(2007,2018,1))

#explanatory variable: salinity and temperature
x=c(temp2,sal2)
x
y=cbind(temp2,sal2)
adf.test(x)
arima.DK7 <-auto.arima(DK2, xreg=y, trace=TRUE)
AIC(arima.DK1,arima.DK7)
AIC(arima.DK3,arima.DK7)
par(mfrow=c(1,1), mai=c(1,1,1,1))
plot(DK2 , typ="l", ylab="", xlab="", xaxt="none", main=c("ARIMA model with temperature", "and salinity as a drivers of biomass at Sprigger Bank"));
lines(fitted(arima.DK7),col="red")
axis(1,at=seq(1,12,1),  labels=seq(2007,2018,1))


#Bob Allen: Time Series
file.choose()
Tab.BA=read.csv("/Users/daniellehatt/Desktop/BSC 6926/Final Project Datasets/LTER.BA.csv", header = TRUE)
Tab.BA
BA1 <- ts( Tab.BA$Biomass, start= 1, frequency=4)
BA1
par(mfrow=c(1,1), mai=c(1,1,1,1)) 
plot(BA1, typ="l", ylab= "Biomass"~"("~"g/m"^{2}~"per quadrat"~")", xlab="Time", xaxt="none", main="Biomass at Bob Allen Keys from 2007-2017")
axis(1,at=seq(1,12,1),  labels=seq(2007,2018,1))
lines(tsclean(BA1), col="red")
BA1 <- tsclean(BA1)
plot(BA1, ylab= "Biomass"~"("~"g/m"^{2}~"per quadrat"~")", xlab="Time", xaxt="none")
axis(1,at=seq(1,12,1),  labels=seq(2007,2018,1))
par(mfrow=c(1,1), mai=c(.6,.6,.6,.6)) 
BA.d1 <- decompose(BA1, 'multiplicative') 
plot(BA.d1, xaxt="none", xlab="")
axis(1,at=seq(1,12,1),  labels=seq(2007,2018,1))
adf.test(BA1) #ts is satationary:null hyp not rejected
BA2= diff(DK1, differences = 1, lag = 4)
plot(BA2)
acf(BA2, lag.max=4)
pacf(BA2, lag.max=4)
arima.BA2 <-auto.arima(BA2, trace=TRUE)
tsdisplay(residuals(arima.BA2), lag.max=50)
AIC(arima.BA2)
par(mfrow=c(1,1))
plot(BA2 , typ="l", ylab= "Biomass"~"("~"g/m"^{2}~"per quadrat"~")", xlab="Time", xaxt="none", main="Figure 4 showing ARIMA model fitted to Biomass data for Penicillus");
lines(fitted(arima.BA2),col="red")
axis(1,at=seq(1,12,1),  labels=seq(2007,2018,1))
checkresiduals(arima.BA2, lag=4)
plot(forecast(arima.BA2, h=20), xaxt="none")
axis(1,at=seq(1,15,1),  labels=seq(2007,2021,1))

#Explanatory series for Bob Allen
#Explanatory series: Temperature
Tab.BA
Tab.BA$Temperatire
temp <- ts(Tab.BA$Temperatire, start= 1, frequency=4)
par(mfrow=c(1,1), mai=c(0.9,0.9,0.9, 0.9)) 
plot(temp, typ="l", ylab= "Temperature", xlab="Time")
lines(tsclean(temp) , col="red")
temp <- tsclean(temp)
temp.d <- decompose(temp, 'multiplicative') 
plot(temp.d)
adf.test(temp)
temp2= diff(temp, differences = 1, lag = 4)
temp2
adf.test(temp2)
acf(temp2)
pacf(temp2)
plot(temp2)
ccf(temp2,BA2, na.action = na.pass, lag.max=4, plot=TRUE)
arima.BA3 <-auto.arima(BA2, xreg=c(temp2), trace=TRUE)
AIC(arima.BA2, arima.BA3)
plot(BA2 , typ="l", ylab= "Biomass"~"("~"g/m"^{2}~"per quadrat"~")", xlab="Time", xaxt="none"); lines(fitted(arima.BA3),col="red")
axis(1,at=seq(1,12,1),  labels=seq(2007,2018,1))
temp.i <- temp2
temp.i[temp.i < 29 ]<- 0 
temp.i[temp.i >= 29 ]<- 1
plot(temp.i, ylab="Temperature", xlab="Time")
arima.BA4 <-auto.arima(BA2, xreg=temp.i, trace=TRUE)
AIC(arima.BA2,arima.BA4 )
AIC(arima.BA3,arima.BA4 )
checkresiduals(arima.BA3, lag=4)
par(mfrow=c(1,1))
plot(BA2 , typ="l", ylab= "Biomass"~"("~"g/m"^{2}~"per quadrat"~")", xlab="Time", xaxt="none"); lines(fitted(arima.BA3),col="red")
axis(1,at=seq(1,12,1),  labels=seq(2007,2018,1))

#Explanatory series for salinity
Tab.BA
sal <- ts(Tab.BA$Salinity, start= 1, frequency=4)
sal
par(mfrow=c(1,1), mai=c(0.25,0.8,0.1, 0.1)) 
plot(sal, typ="l", ylab= "Salinity", xlab="Time")
lines(tsclean(sal) , col="red")
sal <- tsclean(sal)
sal.d <- decompose(sal, 'multiplicative') 
plot(sal.d)
adf.test(sal)
sal2= diff(sal, differences = 1, lag = 4)
sal2
adf.test(sal2)
acf(sal2)
pacf(sal2)
plot(sal2)
ccf(sal2,BA2, na.action = na.pass, lag.max=4, plot=TRUE)
plot(sal2)
arima.BA5 <-auto.arima(BA2, xreg=sal2, trace=TRUE)
AIC(arima.BA2, arima.BA5 )
sal.i <- sal 
sal.i[sal.i < 35 ]<- 0 
sal.i[sal.i >= 35 ]<- 1
plot(sal.i)
sal.i= diff(sal, differences = 1, lag = 4)
arima.BA6 <-auto.arima(BA2, xreg=sal.i, trace=TRUE)
AIC(arima.BA2,arima.BA6 )
AIC(arima.BA5,arima.BA6 )
checkresiduals(arima.BA6, lag=4)
par(mfrow=c(1,1), mai=c(0.5,0.5,0.5, 0.5))
plot(BA1 , typ="l", ylab="", xlab="", xaxt="none", main=c("ARIMA model with extreme salinity", "as a driver of biomass at Sprigger Bank")); lines(fitted(arima.BA5),col="red")
axis(1,at=seq(1,12,1),  labels=seq(2007,2018,1))

#explanatory variable: salinity and temperature
x=c(temp2,sal2)
x
y=cbind(temp2,sal2)
adf.test(x)
arima.BA7 <-auto.arima(BA2, xreg=y, trace=TRUE)
AIC(arima.BA2,arima.BA7)
AIC(arima.BA3,arima.BA7)
par(mfrow=c(1,1), mai=c(1,1,1,1))
plot(BA2 , typ="l", ylab="", xlab="", xaxt="none", main=c("ARIMA model with temperature", "and salinity as a drivers of biomass at Sprigger Bank"));
lines(fitted(arima.BA7),col="red")
axis(1,at=seq(1,12,1),  labels=seq(2007,2018,1))


#Biomass for each site
dev.off()
par(mfrow=c(1,3))
plot(SB1, ylab= "Biomass"~"("~"g/m"^{2}~"per quadrat"~")", xlab="" ,xaxt="none", main="Biomass at Sprigger Bank from 2007-2017")
axis(1,at=seq(1,12,1),  labels=seq(2007,2018,1))
plot(BA1, ylab= "", xaxt="none", xlab="Time" ,main="Biomass of Penicillus at Bob Allen Keys from 2007-2017")
axis(1,at=seq(1,12,1),  labels=seq(2007,2018,1))
plot(DK1, ylab= "", xlab="", xaxt="none", main="Biomass at Duck Key from 2007-2017")
axis(1,at=seq(1,12,1),  labels=seq(2007,2018,1))

#Decomposition for each site
par(mfrow=c(1,3))
p1=plot(SB.d1, xaxt="none", xlab="")
axis(1,at=seq(1,12,1),  labels=seq(2007,2018,1))
p1
plot(BA.d1, xaxt="none", xlab="")
axis(1,at=seq(1,12,1),  labels=seq(2007,2018,1))
plot(DK.d1, xaxt="none", xlab="")
axis(1,at=seq(1,12,1),  labels=seq(2007,2018,1))

#ARIMA model for each site
par(mfrow=c(1,3))
plot(SB2 , typ="l", ylab= "Biomass"~"("~"g/m"^{2}~"per quadrat"~")", xlab="", xaxt="none", main="ARIMA model fitted to biomass data for Sprigger Bank"); lines(fitted(arima.SB2),col="red")
axis(1,at=seq(1,12,1),  labels=seq(2007,2018,1))
plot(BA2 , typ="l", ylab= "", xlab="Time", xaxt="none", main="ARIMA model fitted to biomass data for Bob Allen Keys"); lines(fitted(arima.BA2),col="red")
axis(1,at=seq(1,12,1),  labels=seq(2007,2018,1))
plot(DK2 , typ="l", ylab= "", xlab="", xaxt="none", main="ARIMA model fitted to biomass data for Duck Key"); lines(fitted(arima.DK1),col="red")
axis(1,at=seq(1,12,1),  labels=seq(2007,2018,1))

#Forecasting for each site
par(mfrow=c(1,3))
plot(forecast(arima.SB3, xreg=temp2.SB, h=5), xaxt="none", ylab= "Biomass"~"("~"g/m"^{2}~"per quadrat"~")", xlab="")
axis(1,at=seq(1,25,1),  labels=seq(2007,2031,1))
plot(forecast(arima.BA2, h=36), xaxt="none", ylab= "", xlab="Time")
axis(1,at=seq(1,25,1),  labels=seq(2007,2031,1))
plot(forecast(arima.DK3, xreg=temp2.DK, h=20), xaxt="none",  ylab= "", xlab="")
axis(1,at=seq(1,25,1),  labels=seq(2007,2031,1))

#Explanatory series for each site
par(mfrow=c(1,2))
AIC(arima.SB2, arima.SB3)
plot(SB2, typ="l", ylab= "Biomass"~"("~"g/m"^{2}~"per quadrat"~")", xlab="Time", xaxt="none", main=c("ARIMA model with temperature", "as a driver of biomass at Sprigger Bank")); lines(fitted(arima.SB3),col="red")
axis(1,at=seq(1,12,1),  labels=seq(2007,2018,1))
AIC(arima.DK1, arima.DK3)
plot(DK2 , typ="l", ylab= "Biomass"~"("~"g/m"^{2}~"per quadrat"~")", xlab="Time", xaxt="none",  main=c("ARIMA model with temperature", "as a driver of biomass at Duck Key")); lines(fitted(arima.DK3),col="red")
axis(1,at=seq(1,12,1),  labels=seq(2007,2018,1))
