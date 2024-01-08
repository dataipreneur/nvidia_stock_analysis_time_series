library(fpp2)
library(fBasics)
library(quantmod)
library(TTR)
library(forecast)
library(fBasics)
require(graphics)
library(tseries)
library(TSA)
library(rugarch)
library(lmtest)
install.packages('tseries')
require(tseries)

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
nvda_daily_log_return <- diff(nvda_daily_log)
nvda_monthly_log_return = diff(nvda_monthly_log)
nvda22 <- na.omit(nvda_daily_log_return)

#5. Collecting volume 
nvda_daily_volume = stock_data$NVDA.Volume
nvda_monthly_volume  = to.monthly(nvda_daily_volume,indexAt='Lastof',OHLC=FALSE)
na.omit(nvda_daily_log_return)
nvdats <- ts(nvda_daily_log_return)

#Based on AIC, two models are selected as below:
#Case1
m1 = arima(nvda_daily, order=c(4,1,5))
m1

#Case2
m2 = arima(nvda_daily, order=c(5,1,6))
m2

#Case3
m3 = arima(nvda_daily, order=c(4,1,6))
m3


#Returns
#MA seems to be a feaisble model here
#Order 8 and 1 can be selected

#Case3
m4 = arima(nvda22, order=c(0,0,1))
m4

#Case4
m5 = arima(ts(nvda22), order=c(8,1,0))
#case5
m5= arima(ts(nvda22), order=c(8,1,0) ,fixed=c(NA,0,0,0,0,0,NA,NA))
res5 = checkresiduals(m5, lag=10)
#coeftest(m10)

#Case6
# we could have identifies a AR(1) and seasonal AR(1) too 
m6 = arima(ts(nvda22), order=c(8,0,1),fixed=c(NA,0,0,0,0,0,NA,NA))
m6
coeftest(m6)
#case7
m7 = arima(ts(nvda22), order=c(8,0,0),fixed=c(NA,0,0,0,0,0,NA,NA,NA))
m7
coeftest(m7)

aic_r <- list(m1$aic, m2$aic, m3$aic, m4$aic, m5$aic, m6$aic)
print(aic_r)


dev.off()

#Check Residuals
# check model 
res1 = checkresiduals(m1, lag=10)
res2 = checkresiduals(m2, lag=10)
res3 = checkresiduals(m3, lag=10)
res4 = checkresiduals(m4, lag=10)
res5 = checkresiduals(m5, lag=10)
res6 = checkresiduals(m6, lag=10)
res7 = checkresiduals(m7, lag=10)
#Ljung box test for residuals
Box.test(m7$residuals, lag = 10, type = "Ljung")
qqnorm(m7$residuals)

forecast_r <- forecast(m6, h=10,level=c(90,95))
plot(forecast_r)

#ARCH_GARCH#

#For log returns#

#Plot the returns time series, ACF and PACF
plot(ts(nvda22, frequency=12,start=c(2007,1)))

acf(na.omit(diff(nvda22)))
pacf(na.omit(diff(nvda22)))

#chosen model

m6= arima(ts(nvda22), order=c(8,0,1) ,fixed=c(NA,0,0,0,0,0,NA,NA))
res6 = checkresiduals(m6, lag=10)

#case7
m7 = arima(ts(nvda22), order=c(8,0,0),fixed=c(NA,0,0,0,0,0,NA,NA,NA))
m7
res7 = checkresiduals(m7, lag=10)
coeftest(m7)
nvdaret = resid(m7)


res6 = checkresiduals(m6, lag=10)
nvdaret1 = resid(m6)
res7 = checkresiduals(m7, lag=10)



#ACF & PACF of absolute and Squared Residuals
nvdasq= nvdaret^2
nvdaabs = abs(nvdaret)

nvdasq1 = nvdaret1^2
nvdaabs1 = abs(nvdaret1)

par(mfrow=c(1,2))
acf(as.vector(nvdasq),main="ACF of Squared Residuals")
pacf(as.vector(nvdasq),main="PACF of Squared Residuals")

par(mfrow=c(1,2))
acf(as.vector(nvdaabs),main="ACF of Absolute Residuals") 
pacf(as.vector(nvdaabs),main="PACF of Absolute Residuals")


#ARCH test on returns 
library(MTS)

archTest(nvdasq)

archTest(nvdaabs)

#p value lesser than 0.05, thus garch effect present

acf(nvdasq)
pacf(nvdasq)
eacf(nvdasq)
eacf(nvdaabs)

library(rugarch)

for(i in 5:5){
  for(j in 1:5){
    print(paste("Model",i,j))
    m1 = garch(nvdaret,order=c(i,j))
    #print(summary(m1))
    print(paste('AIC',AIC(m1)))
  }
}

m1 = garch(nvdaret,order=c(1,1))
summary(m1)
coeftest(m1)
Box.test(residuals(m1),lag=12,type='Ljung')


m2 = garch(nvdaret,order=c(2,1))
summary(m2)
coeftest(m2)
Box.test(residuals(m2),lag=12,type='Ljung')

m3 = garch(nvdaret,order=c(2,2))
summary(m3)
coeftest(m3)
Box.test(nvdaret,lag=12,type='Ljung')



m4 = garch(nvdaret,order=c(3,1))
m4 = garch(nvdaret,order=c(3,1), fixed= c(NA,NA,NA,0,NA))
summary(m4)
coeftest(m4)
Box.test(residuals(m4),lag=12,type='Ljung')

m5 = garch(nvdaret,order=c(3,2))

summary(m5)
coeftest(m5)
Box.test(residuals(m5),lag=12,type='Ljung')



###############################################################
m7 = garch(ts(nvda22),order=c(5,1))
summary(m7)
coeftest(m1)
Box.test(residuals(m1),lag=12,type='Ljung')

m8 = garch(ts(nvda22),order=c(5,2))
summary(m8)
coeftest(m2)
Box.test(residuals(m2),lag=12,type='Ljung')

m9 = garch(ts(nvda22),order=c(5,3))
summary(m9)
coeftest(m3)
Box.test(residuals(m3),lag=12,type='Ljung')

m10 = garch(ts(nvda22),order=c(5,4))
summary(m10)
coeftest(m4)
Box.test(residuals(m4),lag=12,type='Ljung')

m11 = garch(ts(nvda22),order=c(5,5))
summary(m11)
coeftest(m5)
Box.test(residuals(m5),lag=12,type='Ljung')

m12 = garch(ts(nvda22),order=c(5,6))
summary(m12)
coeftest(m6)
Box.test(residuals(m6),lag=12,type='Ljung')


# checking normality assumption 

par(mfrow=c(3,2))
dev.off()
qqnorm(residuals(m1));
qqline(residuals(m1));

qqnorm(residuals(m2));
qqline(residuals(m2));

qqnorm(residuals(m3));
qqline(residuals(m3));

qqnorm(residuals(m4));
qqline(residuals(m4));

qqnorm(residuals(m5));
qqline(residuals(m5));



#qqline(residuals(m6))


  
Box.test(residuals(g01),lag=12,type='Ljung')



##Fitting our chosen GARCH(3,1) model with chosen ARMA(8,0) mean model
#Specify the mean and garch models
spec <- ugarchspec(mean.model = list(armaOrder = c(8,0)), 
                   variance.model = list(model = "sGARCH", garchOrder = c(3,1)), distribution.model="norm" )
def.fit1 = ugarchfit(spec = spec, data = na.omit(nvda_daily_log_return))
print(def.fit1)

spec <- ugarchspec(mean.model = list(armaOrder = c(8,0)), 
                   variance.model = list(model = "sGARCH", garchOrder = c(3,1)), distribution.model="std" )
def.fit2 = ugarchfit(spec = spec, data = na.omit(nvda_daily_log_return))
print(def.fit2)

spec <- ugarchspec(mean.model = list(armaOrder = c(8,0)), 
                   variance.model = list(model = "sGARCH", garchOrder = c(3,1)), distribution.model="ged" )
def.fit3 = ugarchfit(spec = spec, data = na.omit(nvda_daily_log_return))
print(def.fit3)

spec <- ugarchspec(mean.model = list(armaOrder = c(8,0)), 
                   variance.model = list(model = "sGARCH", garchOrder = c(3,1)), distribution.model="sstd" )
def.fit4 = ugarchfit(spec = spec, data = na.omit(nvda_daily_log_return))
print(def.fit4)


#Plots to check volatility and distribution
par(mfrow=c(2,2))
plot(def.fit1,which=9)
plot(def.fit2,which=9)
plot(def.fit3,which=9)
plot(def.fit4,which=9)


##Fitting our chosen GARCH(3,1) model with chosen ARMA(8,0) mean model
#Specify the mean and garch models
spec <- ugarchspec(mean.model = list(armaOrder = c(8,0)), 
                   variance.model = list(model = "sGARCH", garchOrder = c(1,1)), distribution.model="norm" )
def.fit1 = ugarchfit(spec = spec, data = na.omit(nvda_daily_log_return))
print(def.fit1)

spec <- ugarchspec(mean.model = list(armaOrder = c(8,0)), 
                   variance.model = list(model = "sGARCH", garchOrder = c(1,1)), distribution.model="std" )
def.fit2 = ugarchfit(spec = spec, data = na.omit(nvda_daily_log_return))
print(def.fit2)

spec <- ugarchspec(mean.model = list(armaOrder = c(8,0)), 
                   variance.model = list(model = "sGARCH", garchOrder = c(1,1)), distribution.model="ged" )
def.fit3 = ugarchfit(spec = spec, data = na.omit(nvda_daily_log_return))
print(def.fit3)

spec <- ugarchspec(mean.model = list(armaOrder = c(8,0)), 
                   variance.model = list(model = "sGARCH", garchOrder = c(1,1)), distribution.model="sstd" )
def.fit4 = ugarchfit(spec = spec, data = na.omit(nvda_daily_log_return))
print(def.fit4)


#Plots to check volatility and distribution
par(mfrow=c(2,2))
plot(def.fit1,which=9)
plot(def.fit2,which=9)
plot(def.fit3,which=9)
plot(def.fit4,which=9)

#No sign bias, joint effect present

## fitting different grach models 
# step1 
spec1 = ugarchspec(variance.model=list(model="fGARCH",submodel = "TGARCH", garchOrder=c(3,1)), mean.model=list(armaOrder=c(8,0), include.mean=TRUE),  distribution.model="std" ) #the empty function specifies the default model. 
def.fit1 = ugarchfit(spec = spec1, data = (na.omit(nvda_daily_log_return)))
print(def.fit1)
res_spec1 <- residuals(def.fit1,standardize = TRUE)
jarque.bera.test(res_spec1)

#egarch
spec2 = ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(3,1)), mean.model=list(armaOrder=c(8,0), include.mean=TRUE),  distribution.model="std" ) #the empty function specifies the default model. 
def.fit2 = ugarchfit (spec = spec2, data = (na.omit(nvda_daily_log_return)))
print(def.fit2)
res_spec2 <- residuals(def.fit2,standardize = TRUE)
jarque.bera.test(res_spec2)

#Plots to check volatility and distribution
plot(def.fit1,which=9)
plot(def.fit2,which=9)

#QQ plot suggests student t distribution
#GARCH-M
spec5 = ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(3,1)), mean.model=list(armaOrder=c(8,0), include.mean=TRUE, archm = TRUE),  distribution.model="std") 
print(spec5)
def.fit5 = ugarchfit(spec = spec5, data = (na.omit(nvda_daily_log_return)))
print(def.fit5)
#ARCH m is not significant


#Forecasting returns using bootstrap method
set.seed(12345)

bootnvda=ugarchboot(def.fit2,method=c("Partial","Full")[1],n.ahead = 12,n.bootpred=1000,n.bootfit=1000)
bootnvda

par(mfrow=c(2,1))
plot(bootnvda,which=3) #Volatility Plot

plot(bootnvda,which=2) #Returns Plot

