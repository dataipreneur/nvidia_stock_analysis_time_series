#Installation and loading of libraries
install.packages('quantmod')
install.packages('TTR')
install.packages('forecast')
install.packages('fBasics')
install.packages('lmtest')
library(fpp2)
library(fBasics)
library(quantmod)
library(TTR)
library(forecast)
library(fBasics)
require(graphics)
library(tseries)
library(lmtest)


#LOADING DATA
# Setting the working directory
#setwd("/Users/devxvishal/Desktop/R Project/")
setwd("/Users/priestleyfernandes/Desktop/Financial econometrics/Assignment1")
#Loading the stock data of csv
stock <- read.csv('NVDA.csv')
end_date = '2023-03-14'


#Loading data from yahoo finance 
stock_data <- getSymbols('NVDA',src='yahoo',auto.assign =FALSE,to = end_date)
stock_data_monthly <- to.monthly(stock_data)


#STORING DATA 

#1.Collecting prices data

#Daily Prices
nvda_daily = stock_data$NVDA.Adjusted
print(head(nvda_daily))
#Monthly Prices
nvda_monthly = to.monthly(nvda_daily,indexAt='Lastof',OHLC=FALSE)
print(head(nvda_monthly))
#Quarterly Prices
nvda_quarterly = to.quarterly(nvda_daily,indexAt='Lastof',OHLC=FALSE)
print(head(nvda_quarterly))
#Annual Prices
nvda_yearly = to.yearly(nvda_daily,indexAt='Lastof',OHLC=FALSE)
print(head(nvda_yearly))


#2.Collecting returns data
nvda_daily_return = diff(nvda_daily)
nvda_monthly_return = diff(nvda_monthly)

#3. Logarthmic (Log proces)
nvda_daily_log = log(nvda_daily)
nvda_monthly_log = log(nvda_monthly)
nvda_quarterly_log = log(nvda_quarterly)
nvda_yearly_log = log(nvda_yearly)


#4. Log retruns data
nvda_daily_log_return = diff(nvda_daily_log)
nvda_monthly_log_return = diff(nvda_monthly_log)

#5. Collecting volume 
nvda_daily_volume = stock_data$NVDA.Volume
nvda_monthly_volume  = to.monthly(nvda_daily_volume,indexAt='Lastof',OHLC=FALSE)


#BASIC STATISTICAL EXAMINATION

#Removing the missing values columns if any
nvda_daily_return <- na.omit(nvda_daily_return)
nvda_monthly_return <- na.omit(nvda_monthly_return)
nvda_daily_log_return <- na.omit(nvda_daily_log_return)
nvda_monthly_log_return <- na.omit(nvda_monthly_log_return)

#Summary of the stock data
summary(stock_data)
summary(stock_data_monthly)

#Basic stats
#Prices
basicStats(nvda_daily)
basicStats(nvda_monthly)

#Returns
basicStats(nvda_daily_log_return)
basicStats(nvda_monthly_log_return)

#Volume
basicStats(nvda_daily_volume)
basicStats(nvda_monthly_volume)


#Performing t-test for mean return being zero

#Prices
t.test(as.vector(nvda_daily))
t.test(as.vector(nvda_monthly))

#Returns
t.test(as.vector(nvda_daily_log_return))
t.test(as.vector(nvda_monthly_log_return))

#Volume
t.test(as.vector(nvda_daily_volume))
t.test(as.vector(nvda_monthly_volume))


#Performing normality test using the Jaque-Bera method.
#Prices
normalTest(nvda_daily,method="jb")
normalTest(nvda_monthly,method="jb")

#Returns
normalTest(nvda_daily_log_return,method="jb")
normalTest(nvda_monthly_log_return,method="jb")

#Volume
normalTest(nvda_daily_volume,method="jb")
normalTest(nvda_monthly_volume,method="jb")


#Performing skewness and kurtosis tests.

#Prices
#Skewness
s3=skewness(nvda_daily);
T <- length(nvda_daily); 
tst = abs(s3/sqrt(6/T))  # t-ratio for testing skewness
pv <- 2*(1-pnorm(tst)) # calculate p value for 2-tailed value 2(1-cdf(|t|))
pv
#Kurtosis
k4 <- kurtosis(nvda_daily)  # already subtracts 3 from kurtosis 
tst <-abs( k4/sqrt(24/T))  # test excess kurtosis
pv <- 2*(1-pnorm(tst)) # calculate p value for 2-tailed value 2(1-df(|t|))
pv
#Skewness
s3=skewness(nvda_monthly);
T <- length(nvda_monthly); 
tst = abs(s3/sqrt(6/T))  # t-ratio for testing skewness
pv <- 2*(1-pnorm(tst)) # calculate p value for 2-tailed value 2(1-cdf(|t|))
pv
#Kurtosis
k4 <- kurtosis(nvda_monthly)  # already subtracts 3 from kurtosis 
tst <-abs( k4/sqrt(24/T))  # test excess kurtosis
pv <- 2*(1-pnorm(tst)) # calculate p value for 2-tailed value 2(1-df(|t|))
pv



#Returns
#Skewness
s3=skewness(nvda_daily_log_return);
T <- length(nvda_daily_log_return); 
tst = abs(s3/sqrt(6/T))  # t-ratio for testing skewness
pv <- 2*(1-pnorm(tst)) # calculate p value for 2-tailed value 2(1-cdf(|t|))
pv
#Kurtosis
k4 <- kurtosis(nvda_daily_log_return)  # already subtracts 3 from kurtosis 
tst <-abs( k4/sqrt(24/T))  # test excess kurtosis
pv <- 2*(1-pnorm(tst)) # calculate p value for 2-tailed value 2(1-df(|t|))
pv

#Skewness
s3=skewness(nvda_monthly_log_return);
T <- length(nvda_monthly_log_return); 
tst = abs(s3/sqrt(6/T))  # t-ratio for testing skewness
pv <- 2*(1-pnorm(tst)) # calculate p value for 2-tailed value 2(1-cdf(|t|))
pv
#Kurtosis
k4 <- kurtosis(nvda_monthly_log_return)  # already subtracts 3 from kurtosis 
tst <-abs( k4/sqrt(24/T))  # test excess kurtosis
pv <- 2*(1-pnorm(tst)) # calculate p value for 2-tailed value 2(1-df(|t|))
pv


#Volume
#Skewness
s3=skewness(nvda_daily_volume);
T <- length(nvda_daily_volume); 
tst = abs(s3/sqrt(6/T))  # t-ratio for testing skewness
pv <- 2*(1-pnorm(tst)) # calculate p value for 2-tailed value 2(1-cdf(|t|))
pv
#Kurtosis
k4 <- kurtosis(nvda_daily_volume)  # already subtracts 3 from kurtosis 
tst <-abs( k4/sqrt(24/T))  # test excess kurtosis
pv <- 2*(1-pnorm(tst)) # calculate p value for 2-tailed value 2(1-df(|t|))
pv

#Skewness
s3=skewness(nvda_monthly_volume);
T <- length(nvda_monthly_volume); 
tst = abs(s3/sqrt(6/T))  # t-ratio for testing skewness
pv <- 2*(1-pnorm(tst)) # calculate p value for 2-tailed value 2(1-cdf(|t|))
pv
#Kurtosis
k4 <- kurtosis(nvda_monthly_volume)  # already subtracts 3 from kurtosis 
tst <-abs( k4/sqrt(24/T))  # test excess kurtosis
pv <- 2*(1-pnorm(tst)) # calculate p value for 2-tailed value 2(1-df(|t|))
pv



#ljung box test 
log(T)  # check what should be m --> m ~ log(T) 
#Prices
Box.test(nvda_daily, lag = 10, type = "Ljung")
Box.test(nvda_monthly, lag = 10, type = "Ljung")

#Returns
Box.test(nvda_daily_log_return, lag = 10, type = "Ljung")
Box.test(nvda_monthly_log_return, lag = 10, type = "Ljung")

#Volume
Box.test(nvda_daily_volume, lag = 10, type = "Ljung")
Box.test(nvda_monthly_volume, lag = 10, type = "Ljung")


#VISUAL EXAMINATION
#Time series plots

#Prices
par(mfrow=c(2,2))
ts.plot(nvda_daily)
ts.plot(nvda_monthly)

#Returns
par(mfrow=c(2,2))
ts.plot(nvda_daily_log_return)
ts.plot(nvda_monthly_log_return)

#Volume
par(mfrow=c(2,2))
ts.plot(nvda_daily_volume)
ts.plot(nvda_monthly_volume)

dev.off()
#Histogram plots
#Prices
plot(hist(nvda_daily))
plot(hist(nvda_monthly))

#Returns
plot(hist(nvda_daily_log_return))
plot(hist(nvda_monthly_log_return))

#Volume
plot(hist(nvda_daily_volume))
plot(hist(nvda_monthly_volume))

dev.off()

#ACF and PACF plots
#Prices
par(mfrow=c(2,2))
acf(nvda_daily)
pacf(nvda_daily)
acf(nvda_monthly)
pacf(nvda_monthly)

#Returns
par(mfrow=c(2,2))
acf(nvda_daily_log_return)
pacf(nvda_daily_log_return)
acf(nvda_monthly_log_return)
pacf(nvda_monthly_log_return)

#Volume
par(mfrow=c(2,2))
acf(nvda_daily_volume)
pacf(nvda_daily_volume)
acf(nvda_monthly_volume)
pacf(nvda_monthly_volume)


#UNIT ROOT TEST

#Prices
adf.test(nvda_daily)
adf.test(nvda_monthly)

#Returns
adf.test(nvda_daily_log_return)
adf.test(nvda_monthly_log_return)
dev.off()

#ARIMA MODEL 

#Checking Seasonality 

#1. Original Prices
par(mfrow=c(2,2))
ts.plot(nvda_daily)
ts.plot(nvda_monthly)
ts.plot(nvda_quarterly)
ts.plot(nvda_yearly)

#ACF of all prices log
par(mfrow=c(2,2))
acf(nvda_daily)
acf(nvda_monthly)
acf(nvda_quarterly)
acf(nvda_yearly)

#PACF of all prices log
par(mfrow=c(2,2))
pacf(nvda_daily)
pacf(nvda_monthly)
pacf(nvda_quarterly)
pacf(nvda_yearly)

#2.Log prices
#Plotting the logarithmic plots
par(mfrow=c(2,2))
ts.plot(nvda_daily_log)
ts.plot(nvda_monthly_log)
ts.plot(nvda_quarterly_log)
ts.plot(nvda_yearly_log)

#ACF of all prices log
par(mfrow=c(2,2))
acf(nvda_daily_log)
acf(nvda_monthly_log)
acf(nvda_quarterly_log)
acf(nvda_yearly_log)

#PACF of all prices log
par(mfrow=c(2,2))
pacf(nvda_daily_log)
pacf(nvda_monthly_log)
pacf(nvda_quarterly_log)
pacf(nvda_yearly_log)

dev.off()


#3. 1 Differencing Prices 
#Checking for seasonal variation
par(mfrow=c(4,2))
ts.plot(diff(nvda_daily))
ts.plot(diff(nvda_monthly))
ts.plot(diff(nvda_monthly,lag=12))
ts.plot(diff(nvda_quarterly))
ts.plot(diff(nvda_quarterly,lag=4))
ts.plot(diff(nvda_yearly))
ts.plot(diff(nvda_yearly,lag=1))
dev.off()

#ACF of all 1 price differnces
par(mfrow=c(3,2))
acf(na.omit(diff(nvda_daily)))
acf(na.omit(diff(nvda_monthly)))
acf(na.omit(diff(nvda_monthly,lag=12)))
acf(na.omit(diff(nvda_quarterly)))
acf(na.omit(diff(nvda_quarterly,lag=4)))
acf(na.omit(diff(nvda_yearly, lag=1)))

#PACF of all 1 price differnces
par(mfrow=c(3,2))
pacf(na.omit(diff(nvda_daily)))
pacf(na.omit(diff(nvda_monthly)))
pacf(na.omit(diff(nvda_monthly,lag=12)))
pacf(na.omit(diff(nvda_quarterly)))
pacf(na.omit(diff(nvda_quarterly,lag=4)))
pacf(na.omit(diff(nvda_yearly)))

#3. 1 Differencing (Returns)
#Checking for seasonal variation
par(mfrow=c(4,2))
ts.plot(diff(nvda_daily_log))
ts.plot(diff(nvda_monthly_log))
ts.plot(diff(nvda_monthly_log,lag=12))
ts.plot(diff(nvda_quarterly_log))
ts.plot(diff(nvda_quarterly_log,lag=4))
ts.plot(diff(nvda_yearly_log))
ts.plot(diff(nvda_yearly_log,lag=1))
dev.off()

#ACF of all 1 price differnces
par(mfrow=c(3,2))
acf(na.omit(diff(nvda_daily_log)))
acf(na.omit(diff(nvda_monthly_log)))
acf(na.omit(diff(nvda_monthly_log,lag=12)))
acf(na.omit(diff(nvda_quarterly_log)))
acf(na.omit(diff(nvda_quarterly_log,lag=4)))
acf(na.omit(diff(nvda_yearly_log)))

#PACF of all 1 price differnces
par(mfrow=c(3,2))
pacf(na.omit(diff(nvda_daily_log)))
pacf(na.omit(diff(nvda_monthly_log)))
pacf(na.omit(diff(nvda_monthly_log,lag=12)))
pacf(na.omit(diff(nvda_quarterly_log)))
pacf(na.omit(diff(nvda_quarterly_log,lag=4)))
pacf(na.omit(diff(nvda_yearly_log)))


#Checking for results after differencing
dev.off()
#Prices
par(mfrow=c(3,2))
acf(nvda_daily)
pacf(nvda_daily)
acf(na.omit(diff(nvda_daily)))
pacf(na.omit(diff(nvda_daily)))
acf(na.omit(diff(diff(nvda_daily))))
pacf(na.omit(diff(diff(nvda_daily))))


#Returns
par(mfrow=c(3,2))
acf(nvda_daily_log_return)
pacf(nvda_daily_log_return)
acf(na.omit(diff(nvda_daily_log_return)))
pacf(na.omit(diff(nvda_daily_log_return)))
acf(na.omit(diff(diff(nvda_daily_log_return))))
pacf(na.omit(diff(diff(nvda_daily_log_return))))


#Model ARIMA analysis

library(TSA)
#EACF
eacf(nvda_daily)


#The loop for all cases between 1 and 6
for(i in 1:6){
  for(j in 1:6){
    print(paste("Model",i,j))
    print(arima(nvda_daily,order=c(i,1,j))$aic)
  }
}


#Based on AIC, five models are selected as below:
#Case1
m1 = arima(nvda_daily, order=c(3,1,4))
m1

#Case2
m2 = arima(nvda_daily, order=c(4,1,5))
m2

#Case3
m3 = arima(nvda_daily, order=c(5,1,6))
m3


#Case4
m4 = arima(nvda_daily, order=c(6,1,5))
m4

#Case5
# we could have identifies a AR(1) and seasonal AR(1) too 
m5 = arima(nvda_daily, order=c(4,1,3))
m5

#Case6
# we could have identifies a AR(1) and seasonal AR(1) too 
m6 = arima(nvda_daily, order=c(4,1,6))
m6



#Check Residuals
# check model 
res1 = checkresiduals(m1, lag=10)
res2 = checkresiduals(m2, lag=10)
res3 = checkresiduals(m3, lag=10)
res4 = checkresiduals(m4, lag=10)
res5 = checkresiduals(m5, lag=10)
res6 = checkresiduals(m6, lag=10)



#Analysis of log returns
eacf(nvda_daily_log_return)

#The loop for all cases between 1 and 6
for(i in 0:6){
  for(j in 0:6){
    print(paste("Model",i,j))
    print(arima(nvda_daily_log_return,order=c(i,0,j))$aic)
  }
}


#Performing Ljung Box test and ADF test on returns of prices i.e diff(nvda_daily)
#to check for nature of autocorrelations and stationarity

Box.test(diff(nvda_daily), lag = 10, type = "Ljung")
adf.test(na.omit(diff(nvda_daily)))

#Based on AIC, five models are selected as below:
#Case1
m1 = arima(nvda_daily_log_return, order=c(1,0,0))
m1

#Case2
m2 = arima(nvda_daily_log_return, order=c(2,0,0))
m2

#Case3
m3 = arima(nvda_daily_log_return, order=c(3,0,4))
m3


#Case4
m4 = arima(nvda_daily_log_return, order=c(4,0,0))
m4

#Case5
 
m5 = arima(nvda_daily_log_return, order=c(5,0,0))
m5

#Case6

m6 = arima(nvda_daily_log_return, order=c(1,0,8))
m6

#Case7

m7 = arima(nvda_daily_log_return, order=c(8,0,1))
m7

#Case8

m8 = arima(ts(nvda_daily_log_return), order=c(2,0,2))
m8




#Check Residuals
# check model 
res1 = checkresiduals(m1, lag=10)
res2 = checkresiduals(m2, lag=10)
res3 = checkresiduals(m3, lag=10)
res4 = checkresiduals(m4, lag=10)
res5 = checkresiduals(m5, lag=10)
res6 = checkresiduals(m6, lag=10)
res7 = checkresiduals(m7, lag=10)
res8 = checkresiduals(m8, lag=10)



#Adding price or log price for selected model
#Experimenting and checking for various models after initial models failed 
m9 = arima(ts(nvda_daily_log), order=c(0,1,8))
m9
coeftest(m9) 

m9 = arima(ts(nvda_daily_log), order=c(0,1,8), fixed=c(0,0,0,0,0,0,0,NA))
res9 = checkresiduals(m9, lag=10)
m9

m9 = arima(ts(nvda_daily_log), order=c(0,1,9))
m9 = arima(ts(nvda_daily_log), order=c(0,1,9) ,fixed=c(NA,0,0,0,0,0,0,NA,0))
coeftest(m9) 
res9 = checkresiduals(m9, lag=10)

m9 = arima(ts(nvda_daily_log), order=c(9,1,0))
m9 = arima(ts(nvda_daily_log), order=c(9,1,0) ,fixed=c(NA,0,0,0,0,0,0,NA,0))
coeftest(m9) 
res9 = checkresiduals(m9, lag=10)


#Final model which gave some reliable results
m10 = arima(nvda_daily_log, order=c(8,1,0))
m10<-arima(nvda_daily_log, order=c(8,1,0) ,fixed=c(NA,0,0,0,0,0,NA,NA))
res10 = checkresiduals(m10, lag=10)
coeftest(m10) 
m10

m11 = arima(nvda_daily_log_return, order=c(8,0,0))
m11<-arima(nvda_daily_log_return, order=c(8,0,0) ,fixed=c(NA,0,0,0,0,0,NA,NA,NA))
res11 = checkresiduals(m11, lag=10)
coeftest(m11) 
m11

class(m10)

library(TSA)

#Performing Ljung Box test and ADF test on returns of prices i.e diff(nvda_daily)
#to check for nature of autocorrelations and stationarity

Box.test(diff(nvda_daily), lag = 10, type = "Ljung")
adf.test(na.omit(diff(nvda_daily)))



#Auto Arima Results
# auto.arima prices
m_auto1 = auto.arima(ts(nvda_daily))
m_auto1

# auto.arima prices
m_auto2 = auto.arima(diff(nvda_daily))
m_auto2

# auto.arima log
m_auto3 = auto.arima(nvda_daily_log_return)
m_auto3

# auto.arima log
m_auto4 = auto.arima(diff(nvda_daily_log_return))
m_auto4

m_auto4 = auto.arima(diff(nvda_daily_log_return))
m_auto4
class(m_auto4)

dev.off()


#FORECAST

print(m7)
library(forecast)

#Forecast
#Prices
nvidia_forward_prices = forecast(m10,h=200,level=c(90,95))
plot(nvidia_forward_prices)


forecast = predict(m10,n.ahead = 260, ci=0.95)
plot(forecast)


nvidia_forward_returns = forecast(m11, h=200,level=c(90,95))
plot(nvidia_forward_returns)



