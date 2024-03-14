######################################################################
########################### ARIMA GAS FOTRECASTING ########################
######################################################################
#Loading the required libraries
library('ggplot2')
library('forecast')
library('tseries')
library('MLmetrics')


#Setting the working directory
setwd("C:/2020/RProgramming/TimeSeries_Forecasting/")

data<- forecast::gas

dim(data)
class(data)
str(data)
summary(data)
head(data)
tail(data)
frequency(data)

#Checking for NA's
anyNA(data)

#Convert data to time series data and plot it
#Data is already a time series
autoplot(data) + 
  xlab("Year") + ylab("Aussie Gas production")

#Selecting the dataset only from 1970-Jan

gasTS <- window(data, start=c(1970,1), frequency=12)

#Plotting Again
autoplot(gasTS) + 
  xlab("Year") + ylab("Aussie Gas production")

# Plotting for monthplot and seasonplot & Boxplot
monthplot(gasTS, main = "Aussie Gas Production  - Monthplot", xlab = "Month", ylab = "Count")

seasonplot(gasTS, year.labels = TRUE, year.labels.left=TRUE, col=1:40, pch=19, main = "Aussie Gas Production - Seasonplot", xlab = "Month", ylab = "Count")

cycle(gasTS)
boxplot(gasTS ~ cycle(gasTS), xlab = "Month", ylab = "Count", main = "Aussie Gas Production - Boxplot")

# Check for Seasonality & remove it ?
decom1 = decompose(gasTS,type="additive")
decom1$seasonal
plot(decom1)

decom2 = decompose(gasTS,type="multiplicative")
decom2$seasonal
plot(decom2)

# Using stl() function to desesaonalize
decom_stl <- stl(gasTS, s.window="p") #constant seasonality
plot(decom_stl)
decom_stl

#Adding the Trend & Reminder to deseanalise
series_names <- c('Deseasoned_Gas', 'Actual_Gas')
Deseason_gasTS <- (decom_stl$time.series[,2]+decom_stl$time.series[,3]) 
ts.plot(gasTS, Deseason_gasTS, col=c("red", "blue"), main="Actual Demand vs De-seasoned Gas Demand")

#Partitioning the dataset- We will use the dataset only from 1970-Jan
gasTSTrain <- window(Deseason_gasTS, start=c(1970,1), end=c(1993,12), frequency=12) 
gasTSTest <- window(Deseason_gasTS, start=c(1994,1), frequency=12)

head(gasTSTrain)
head(gasTSTest)

#Check for stationarity using the Augmented Dickey-Fuller test on the train dataset

adf.test(gasTS, alternative = "stationary")
#Series is stationary since p-value is less than  0.05

adf.test(gasTSTrain, alternative = "stationary")
#Series is non-stationary since p-value is more than 0.05

adf.test(gasTSTest, alternative = "stationary")
#Series is non-stationary since p-value is more than 0.05

# Also using nsdiffs() to check if differencing is required 
nsdiffs(gasTSTrain)

#Differencing for train dataset for trend
gasTSTrain_d1 = diff(gasTSTrain, differences = 1)
autoplot(gasTSTrain_d1)+
  xlab("Year") + ylab("Gas Production")

#Check for stationarity again
adf.test(gasTSTrain_d1, alternative = "stationary")
#Differenced demand is stationary

########### Model Building ######################

#Check autocorrelation plots
#acf and pacf for dif time series. ARIMA(p,d,q)
acf(gasTSTrain_d1, main="ACF for differences series")#--->q
pacf(gasTSTrain_d1, main="PACF for differences series") #-->p

#There are significant autocorrelations with many lags in our demand series, as shown by 
#the ACF plot at intervals of 6. 

#PACF plot shows that there could be significance at lag 4 and yearly seasonality since 
#the plot also peaks at #intervals of 6

#acf and pacf for dif time series. ARIMA(p,d,q)


#So here for manual we will use the combinations of pq as (4,6) & (6,6) 

#Fitting an ARIMA model (manual) d=1 since we use the non-differenced dataset here 
#and that would need 1 order of differencing
gasARIMA_4p6q = Arima(gasTSTrain, order=c(4,1,6))
gasARIMA_4p6q
head(gasARIMA_4p6q)

#gasARIMA_6p6q = Arima(gasTSTrain, order=c(6,1,6))
#head(gasARIMA_6p6q)
#Residual analysis using Ljung-Box test
#H0: Residuals are independent
#Ha: Residuals are not independent
#Check the residual analysis for both the manual arima and auto arima
library(stats)

# Plotting the train dataset actual Vs forecast using Arima-Manual
plot(gasARIMA_4p6q$x,col="blue") 
lines(gasARIMA_4p6q$fitted,col="red",main="Gas prod'n Manual: Actual vs Forecast ")
MAPE(gasARIMA_4p6q$fitted,gasARIMA_4p6q$x)
Box.test(gasARIMA_4p6q$residuals,type = "Ljung-Box")
tsdisplay(residuals(gasARIMA_4p6q), lag.max=24, main='Model Residuals_4p6q')

#Validate the manually fitted ARIMA models with test data for 20 periods
#Model performance on test
gasARIMA_4p6q_test <- forecast(gasARIMA_4p6q, h=20)
Vec2 <- cbind(gasTSTest,gasARIMA_4p6q_test)
#Vec2
#par(mfrow=c(1,1), mar=c(2, 2, 2, 2), mgp=c(3, 1, 0), las=0)
ts.plot(Vec2[,1],Vec2[,2], col=c("blue","red"),xlab="year", ylab="Gas production", main="Gas Production Manual Arima Test: Actual vs Forecast")
Box.test(gasARIMA_4p6q_test$residuals,type = "Ljung-Box")
tsdisplay(residuals(gasARIMA_4p6q_test), lag.max=24, main='Model Residuals_4p6q_test')
MAPE(gasARIMA_4p6q_test$fitted,gasARIMA_4p6q_test$x)

#Fitting with Auto ARIMA. 
nsdiffs(gasTSTrain)
fit<-auto.arima(gasTSTrain,seasonal = FALSE,stepwise = FALSE, approximation = FALSE)
fit
plot(fit$x,col="blue") 
lines(fit$fitted,col="red",main="Demand A: Actual vs Forecast ")
MAPE(fit$fitted,fit$x)
Box.test(fit$residuals,type = "Ljung-Box")
tsdisplay(residuals(fit), lag.max=24, main='Auto ARIMA Model Residuals')

#Validate the auto fitted ARIMA models with test data for 20 periods
#Model performance on test
gasARIMA_auto_test <- forecast(fit, h=20)
Vec3 <- cbind(gasTSTest,gasARIMA_auto_test)
ts.plot(Vec3[,1],Vec3[,2], col=c("blue","red"),xlab="year", ylab="Gas production", main="Gas Production Auto Arima Test: Actual vs Forecast")
Box.test(gasARIMA_auto_test$residuals,type = "Ljung-Box")
tsdisplay(residuals(gasARIMA_auto_test), lag.max=24, main='Model Residuals_4p6q_test')
MAPE(gasARIMA_auto_test$fitted,gasARIMA_4p6q_test$x)

#Accuracy of the forecast-Manual ARIMA with hold out(test) data
f_manual=forecast(gasARIMA_4p6q)
accuracy(f_manual, gasTSTest)

#Accuracy of auto arima model with hold out(test) data
f_auto=forecast(fit)
accuracy(f_auto, gasTSTest)

#acf(fit$residuals)
#pacf(fit$residuals)

###############################
#Finalizing the model and doing predictions for next 12 months starting from Aug-1995.
#The best model was Manual model with p=4 and q=6
#Manual
fcast_final <- forecast(gasARIMA_4p6q, h=32)
plot(fcast_final)
fcast_final

#Auto
fcast_final_2 <- forecast(fit, h=32)
plot(fcast_final_2)

# We will go with the Manual Arima model.