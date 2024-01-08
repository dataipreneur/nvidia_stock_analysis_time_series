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
install.packages('MTS')
library(MTS)

start_date = '2007-01-03'
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


#SnP Index 
snp_data <- getSymbols('^GSPC',src='yahoo',auto.assign =FALSE,from = start_date, to = end_date)
print(head(snp_data))


nvda_daily_2 <- data.frame(Date = index(nvda_daily), Close = as.numeric(nvda_daily$NVDA.Adjusted))
print(head(nvda_daily_2))

nvda_daily_log_2 <- data.frame(Date = index(nvda_daily), Close = as.numeric(log(nvda_daily)))
print(head(nvda_daily_log_2))



#Daily Prices of SNP 
snp_daily = snp_data$GSPC.Adjusted
print(head(snp_daily))

snp_daily_2 <- data.frame(Date = index(snp_daily), Close = as.numeric(snp_daily$GSPC.Adjusted))
print(head(snp_daily_2))

snp_daily_log_2 = data.frame(Date = index(snp_daily), Close = as.numeric(log(snp_daily)))
print(head(snp_daily_log_2))


#Multi time series plot
plot(nvda_daily_2,type='l',col="red" )  
par(new = TRUE)
plot(snp_daily_2,type='l', col="green" )  

par(mfrow=c(2,1))
plot(nvda_daily_2,type='l',col="red" )  
plot(snp_daily_2,type='l', col="green" ) 


#Checking the number of cols
print(ncol(nvda_daily_2))
print(ncol(snp_daily_2))


#Applying regression

#Prices
m1=lm(unlist(nvda_daily_2)~unlist(snp_daily_2))
summary(m1)
wt = m1$residuals
print(wt)
print(length(wt))
print(ncol(wt))
plot(wt, type = 'l')


#Returns
nvda_log_ret  <- na.omit(diff(log(nvda_daily)))
nvda_log_ret
snp_log_ret  <- na.omit(diff(log(snp_daily)))
snp_log_ret

m1_1=lm(nvda_log_ret ~ snp_log_ret)
summary(m1_1)
wt1 = m1_1$residuals
plot(wt1, type = 'l')



#VAR Analysis by combining two series

snp_nvda = cbind(nvda_daily_2$Close, snp_daily_2$Close)
print(head(snp_nvda))

snp_nvda_log <-  cbind(nvda_daily_log_2$Close, snp_daily_log_2$Close)
print(head(snp_nvda_log))

xt1 = snp_nvda[,1:2]
print(head(xt1))
MTSplot(xt1)
ccm(xt1)

xt2 = snp_nvda_log[,1:2]
print(head(xt2))
rt2=diffM(log(xt2))
print(head(rt2))


#Multi series plot

MTSplot(xt2)
MTSplot(rt2)

#The correlarion matrix
ccm(xt2)
ccm(rt2)


#Model fitting
#Calcaulating orders 
m1 = VARorder(xt1)
m2 = VARorder(rt2)


#Models
m3_1 <- VAR(rt2, p=11) ### Fit a VAR(11) model
m3_1
summary(m3_1)
res = m3_1$residuals
mq(res,adj=0)
mq(res,lag =13,adj=0)



m3_2=VAR(rt2,p = 10) ### Fit a VAR(10) model
m3_2
summary(m3_2)
res = m3_2$residuals
mq(res,adj=0)
mq(res,lag =13,adj=0)



m3_3=VAR(rt2,p = 1) ### Fit a VAR(2) model
m3_3
summary(m3_3)
res = m3_3$residuals
mq(res,adj=0)
mq(res,lag =13,adj=0)


#Model checking
MTSdiag(m3_1)
MTSdiag(m3_2)
MTSdiag(m3_3)


#Using RefVar
m1=refVAR(m3_1,thres = 1.96) # threshhold is the t -stat
m1
m1$coef

m2=refVAR(m3_2,thres = 1.96) # threshhold is the t -stat
m2
m2$coef

m3=refVAR(m3_3,thres = 1.96) # threshhold is the t -stat
m3
m3$coef


#Co-integration test
m6=ca.jo(rt2, K=2)
summary(m6)
m7=ca.jo(xt2, K=2)
summary(m7)


#Forecasts
forec = VARpred(m1,8)
forec = VARpred(m3,8)
plot(forec)



library(vars)
m1=VAR(rt2,p = 11) ### Fit a VAR(11) model
m1 = restrict(m1,thresh=1.96)
m1
forecasts1 <- predict(m1,n.ahead = 260, ci=0.95)
plot(forecasts1)


m2=VAR(rt2,p = 10)
m2 = restrict(m2,thresh=1.96)
m2
forecasts2 <- predict(m2,n.ahead = 260, ci=0.95)
plot(forecasts2)


#Model Checking
rm(list=ls())


