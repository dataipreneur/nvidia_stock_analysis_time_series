setwd("~/Dropbox/1.WORK/1. courses econometrics SIT/Fn620/All Data")
da=read.table("d-ibm-0110.txt",header=T)
head(da)
ibm=log(da$return+1)
nibm=-ibm    ## loss value

### RiskMetrics #########
source("RMfit.R")
RMfit(nibm)
### One can use default parameter beta = 0.96 wihtout estimation with the command
RMfit(nibm,estim=F)

### Econometric modeling
library(fGarch)
m1=garchFit(~garch(1,1), data = nibm,trace=FALSE)
#library(tseries); library(TSA)
#m1=garch(nibm,order=c(1,1))
summary(m1)
pm1=predict(m1,10)
pm1
source("RMeasure.R")
RMeasure(-.0006,.00782)

#### 10-day VaR
names(pm1)
v1=sqrt(sum(pm1$standardDeviation^2))
RMeasure(-0.006,v1)
m2=garchFit(~garch(1,1),data=nibm,trace=F,cond.dist="std")
summary(m2)
pm2=predict(m2,1)
pm2
RMeasure(-.000411,.0081,cond.dist="std",df=5.751)

### Empirical quantile and quantile regression
quantile(nibm,c(0.95,0.99,0.999))
da1=read.table("d-ibm-rq.txt",header=T)
require(quantreg)
m3=rq(nibm~vol+vix,data=da1,tau=0.95)
summary(m3)
ts.plot(nibm)
VaR_quant =  -0.00104 + 1.17724*da1$vol[length(da1$vol)]+ 0.00028*da1$vix[length(da1$vix)]
lines(m3$fitted.values,col="red")

### Extreme value theory
require("evir")
m4=gev(nibm,block=21)
m4
source("evtVaR.R")
evtVaR(0.2517,0.0103,0.0297)



### codes for other methods in the book
### Peaks over threshold
m4a=pot(nibm,thres=0.01)
plot(m4a)
riskmeasures(m4a,c(0.95,0.99,0.999))

### generalized Pareto distribution
m5=gpd(nibm,0.01)
m5
plot(m5)
riskmeasures(m5,c(0.95,0.99,0.999))