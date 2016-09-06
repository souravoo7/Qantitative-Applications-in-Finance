#script to perform the following functions:
# 1. read data and calculate the log returns (written on a output file)
# 2. Normality of returns via multiple methods-JB test, QQ-plot etc
# 3. Technical indicator of volume-cum-returns
# 4. Stationarity test of price and returns
# 5. HAC
#========================================================================
# READING THE DATA
data<-read.csv('acc.csv');
#attach(data);
#library('GLDEX');
#CALCULATING LOG RETURNS
log_returns <- diff(log(data$LAST_PRICE), lag=1);
returns<-diff(data$LAST_PRICE, lag=1);# not relevant
volume<-diff(log(data$VOLUME), lag=1);
#NORMALITY TEST
#plot the density
par(mfrow=c(2,3));
plot(density(log_returns));
n<-length(log_returns);
n_data<-rnorm(length(log_returns), mean=mean(log_returns), sd=sd(log_returns));
lines(density(n_data), col='red');
#qqplot
require(graphics);
qqnorm(log_returns);
qqline(log_returns);
#J-B TEST
library('tseries');
library('moments');
jb<-jarque.bera.test(log_returns);
da<-agostino.test(log_returns);
#TECHNICAL INDIACTOR (LOG_VOL/LOG_RETURNS)
vr_ratio<-volume*log_returns;
#now can comment on the significance of the estimated parameter
#STATIONARITY TEST
#(a) LOG_RETURNS
acf(log_returns);
pacf(log_returns);
a<-adf.test(log_returns, alternative=c("stationary"));
#(b) PRICES
acf(data$LAST_PRICE);
pacf(data$LAST_PRICE);
b<-adf.test(data$LAST_PRICE, alternative=c("stationary"));
#ARMAX fitting: comparing four models
library('TSA');
library('forecast');
aa<-auto.arima(log_returns);
m1<-arimax(log_returns, c(0,0,1), xreg=lag(vr_ratio,1));
m2<-arima(log_returns, c(0,0,1));
m3<-arimax(log_returns, c(0,0,1), xreg=vr_ratio);
# HAC INFERENCES
library('lmtest');
s1<-lrtest(m1,m2);
s2<-lrtest(m3,m2);