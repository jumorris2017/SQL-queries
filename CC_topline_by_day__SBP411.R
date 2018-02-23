## Topline CC Prediction ##
## forecasting basic time trend ##

##USING PROPHET

#load libraries
library(data.table)
library(ggplot2)
library(forecast)
library(zoo)
library(prophet)
library(mFilter)
library(censReg)
library(stringr)
set.seed(98115)

#load data
cct <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc_topline_by_day.csv")

#set names as lower case for ease
setnames(cct, tolower(names(cct)))

#update variable classes
cct[, caldate := as.Date(caldate, format = "%Y-%m-%d")]
cct[, ccscore := as.numeric(ccscore)]
#create a separate db that is just the latest 3 weeks for validating
cctforcheck <- cct[caldate>'2018-01-13']
cctforcheck <- cctforcheck[, .(caldate,ccscore)]

#only keep data post august 2015
cct <- cct[caldate>='2015-09-01']

#omit the latest 3 weeks for validating
cct <- cct[caldate<='2018-01-13']

#recode holiday as binary
cct[holidayflag=='N', holiday := 0];cct[holidayflag=='Y', holiday := 1]
cct[, holidayflag := NULL]

#create dataset for prophet
pcc <- copy(cct)
setnames(pcc,c("caldate","ccscore"),c("ds","y"))

#prep holidays
pcchol <- copy(cct)
pcchol <- pcchol[, c("caldate","holiday")]
setnames(pcchol,c("caldate"),c("ds"))
pcchol[, holiday := as.character(holiday)]

#prophet forecast model
pr <- prophet(daily.seasonality=FALSE, yearly.seasonality=TRUE, holidays=pcchol)
#pr <- prophet(pcc, growth="linear", daily.seasonality=FALSE, weekly.seasonality=TRUE, holidays=pcchol)
#add monthly seasonality
pr <- add_seasonality(pr, name='monthly', period=30.5, fourier.order=5)
#fit model
pr <- fit.prophet(pr,pcc)
#dataframe returned from mode
fcst <- predict(pr,pcc)
#plot model
prophet_plot_components(pr, fcst, uncertainty = TRUE, plot_cap = TRUE,
                        weekly_start = 0, yearly_start = 0)
#predict forward
future <- make_future_dataframe(pr, periods = 25) 
forecast <- predict(pr, future)
plot(pr, forecast, xlab="Calendar Date", ylab="Customer Connection")
#sample from posterior preditive distribution
predictive_samples(pr, pcc)

#merge together predicated and actual
forecastforcheck <- setDT(forecast)[, .(ds,trend)]
forecastforcheck <- forecastforcheck[ds>'2018-01-13']
setnames(forecastforcheck,c("ds","trend"),c("caldate","ccpred"))
forecastforcheck[, caldate := as.Date(caldate, format = "%Y-%m-%d")]
#merge together
predvactual <- merge(cctforcheck,forecastforcheck,by="caldate")
predvactual[, ccdelta := round(ccpred-ccscore,4)]

#average across the week
predvactual[caldate>='2018-01-14'&caldate<='2018-01-20', week := 1]
predvactual[caldate>='2018-01-21'&caldate<='2018-01-27', week := 2]
predvactual[caldate>='2018-01-28'&caldate<='2018-02-03', week := 3]
predvactual[caldate>='2018-02-04'&caldate<='2018-02-07', week := 4]
predvactual[, list(ccscore = round(mean(ccscore),4)*100,
                     ccpred = round(mean(ccpred),4)*100,
                     ccdelta = round(mean(ccpred)-mean(ccscore),4)*100), by="week"]

#library(mFilter)
#Hodrick-Prescott filter
temp <- pcc[,.(ds,y)]
temp <- setorder(temp,ds)
temp.hp1 <- hpfilter(temp[,y], freq=12,type="frequency",drift=TRUE)
temp.hp2 <- hpfilter(temp[,y], freq=52,type="frequency",drift=TRUE)
temp.hp3 <- hpfilter(temp[,y], freq=129600,type="lambda",drift=TRUE)
par(mfrow=c(2,1),mar=c(3,3,2,1),cex=.8)
plot(temp.hp1$x, ylim=c(0.2,0.4),
     main="Hodrick-Prescott filter of CC: Trend, drift=TRUE",
     col=1, ylab="")
lines(temp.hp1$trend,col=2)
lines(temp.hp2$trend,col=3)
lines(temp.hp3$trend,col=3)
legend("topleft",legend=c("series", 
                          "freq=12", "freq=52", "lambda=129,600"), col=1:4, lty=rep(1,4), ncol=1)
plot(temp.hp1$cycle,
     main="Hodrick-Prescott filter of CC: Cycle, drift=TRUE",
     col=2, ylab="", ylim=range(temp.hp1$cycle,na.rm=TRUE))
lines(temp.hp2$cycle,col=4)

# ARIMA(p, d, q) × (P, D, Q)S,
# with p = non-seasonal AR order, d = non-seasonal differencing, 
# q = non-seasonal MA order, 
# P = seasonal AR order, 
# D = seasonal differencing, 
# Q = seasonal MA order, and 
# S = time span of repeating seasonal pattern.

#library(tseries)
#test for unit root
temp <- pcc[,.(ds,y)]
temp <- setorder(temp,ds)
temp <- as.matrix(temp[,.(y)])
tseries::adf.test(temp[,"y"],k=1)
Acf(temp[,"y"])
#library(forecast)
fit <- auto.arima(temp[,"y"], start.p = 1, start.q = 1)
res <- residuals(fit)
Box.test(res) #box-pierce test

#write function
plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  set.seed(98115)
  mybinsize <- IQR(forecasterrors)/4
  mysd   <- sd(forecasterrors)
  mymin  <- min(forecasterrors) - mysd*5
  mymax  <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a grey histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="grey", freq=FALSE, breaks=mybins,main="distribution of residuals")
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

#TBATS allows for dynamic seasonality (versus ARIMA where seasonality is periodic)
# TBATS(omega, p,q, phi, <m1,k1>,...,<mJ,kJ>) where 
# omega is the Box-Cox parameter and phi is the damping parameter; 
# the error is modelled as an ARMA(p,q) process 
# and m1,...,mJ list the seasonal periods used in the model 
# and k1,...,kJ are the corresponding number of Fourier terms used for each seasonality.
#TBATS(1,{3,3},-,{<7,3>,<364,7>})
#https://robjhyndman.com/hyndsight/tbats-with-regressors/
#https://robjhyndman.com/hyndsight/dailydata/
##TO DO: ADD HOLIDAY EFFECTS
#library(forecast)
#TBATS
#day of week trends
x_new  <- msts(temp[,"y"], seasonal.periods=c(7,7*52))
fit <- tbats(x_new)
fc <- forecast(fit, h=7*52)
predvalues <- fc$mean[1:7]
plot(forecast(fit,h=7))

#plot distribution of residuals
plotForecastErrors(fc$residuals)
#ensure forecast errors are normally distributed with mean zero and constant variance
plot.ts(fc$residuals,main="LM Residuals by Day")

#double-seasonal HW with exponential smoothing
HWx_new_sea <- dshw(temp[,"y"], period1=7, period2=7*52, h=25)
HWx_new_exps <- dshw(temp[,"y"], period1=7, period2=7*52, h=25, beta=FALSE, gamma=FALSE)
plot(HWx_new_sea,main="Double-Seasonal Holt-Winters w/ exponential smoothing of MSTS time series")
plot(HWx_new_exps,main="Double-Seasonal Holt-Winters w/ exponential smoothing of MSTS time series")

#MSE
HWx_new_sea$model$mse
HWx_new_exps$model$mse

#plot distribution of residuals
plotForecastErrors(HWx_new_sea$residuals)
plotForecastErrors(HWx_new_exps$residuals)
#ensure forecast errors are normally distributed with mean zero and constant variance
plot.ts(HWx_new_sea$residuals,main="DS HW Seasonal Residuals by Day")
plot.ts(HWx_new_exps$residuals,main="DS HW Exp Smoothing Residuals by Day")




#qq plot
temp <- pcc[,.(ds,y)]
temp <- setorder(temp,ds)
temp <- as.matrix(temp[,.(y)])
qqnorm(temp[,"y"],main = "Normal Q-Q Plot",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
       plot.it = TRUE, datax = FALSE)
qqline(temp[,"y"], datax = FALSE)

#library(forecast)
# temp[,"y"] %>% forecast %>% plot
# fit <- ets(window(temp[,"y"], end=60))
# fc <- forecast(temp[,"y"], model=fit)

##library(forecast)
temp <- pcc[,.(ds,y)]
temp <- setorder(temp,ds)
temp <- as.matrix(temp[,.(y)])
fit <- auto.arima(temp,allowdrift=T)
fc <- forecast(fit,h=25)
predvalues <- fc$mean[1:25]
plot(forecast(fit,h=25))
#residuals and box test
Acf(residuals(fit))
Pacf(residuals(fit))
Box.test(residuals(fit), type="Ljung")

#merge together
cctforcheck <- setorder(cctforcheck,caldate)
predvactual <- cbind(cctforcheck,predvalues)
predvactual[, ccdelta := round(predvalues-ccscore,4)]

#average across the week
predvactual[caldate>='2018-01-14'&caldate<='2018-01-20', week := 1]
predvactual[caldate>='2018-01-21'&caldate<='2018-01-27', week := 2]
predvactual[caldate>='2018-01-28'&caldate<='2018-02-03', week := 3]
predvactual[caldate>='2018-02-04'&caldate<='2018-02-07', week := 4]
predvactual[, list(ccscore = round(mean(ccscore),4)*100,
                   predvalues = round(mean(predvalues),4)*100,
                   ccdelta = round(mean(predvalues)-mean(ccscore),4)*100), by="week"]



# #plot trends
# #set labels
# xlabel <- "Day in Year"
# ylabel <- "CC Top Box Score"
# tlabel <- "CC Top Box Score Trend"
# slabel <- "*Note: Uses calendar dates"
# #set data and variables
# pdata <- cct
# px <- cct[, caldayyear]
# py <- cct[, ccscore]
# groupvar <- cct[, calyear]
# #manual legend labels
# lname <- "Calendar Year"
# llabels <- c("2015", "2016","2017") 
# ybreaks <- 8
# #line chart, factored by one variable
# plot1 <- ggplot() +
#   geom_line(data=pdata, aes(x=px, y=py, group=factor(groupvar), colour=factor(groupvar))) + 
#   xlab(xlabel) + ylab(ylabel) + theme_bw() +
#   scale_colour_discrete(name=lname, labels=llabels, guide=guide_legend(order=1)) +
#   guides(colour = guide_legend(override.aes = list(size = 7))) + 
#   scale_x_continuous(limits=c(pdata[,min(px)],pdata[,max(px)]), breaks = scales::pretty_breaks(n = ybreaks)) +
#   scale_y_continuous(limits=c(pdata[,min(py)],pdata[,max(py)]),labels=scales::percent) +
#   labs(title = tlabel, subtitle = slabel)
# print(plot1)
# 
# 
# #basic time series forecast
# 
# ## Create a daily time series object
# zts <- zoo(cct[,ccscore], seq(from = as.Date("2015-01-01"), to = as.Date("2017-11-26"), by = 1))
# ## use auto.arima to choose ARIMA terms
# fit <- forecast::auto.arima(zts)
# ## forecast for next 60 time points
# fore <- forecast(fit, h = 60)
# ## plot it
# plot(fore)

# #compare to Tableau dashes
# #average across months
# cctm <- cct[, list(ccscore = mean(ccscore,na.rm=T)), by=c("calmonth","calyear")]
# plot1 <- ggplot() +
#   geom_line(data=cctm, aes(x=calmonth, y=ccscore, group=factor(calyear), colour=factor(calyear))) +
#   scale_y_continuous(limits=c(0,.35))
# print(plot1)



#predictive modeling
ccwk <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc_predmodel_ccscores.csv") #cc scores
hswk <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc_predmodel_homestore.csv") #home store customers
ptwk <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc_predmodel_partnertenure.csv") #partner tenure
tsdwk <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc_predmodel_tsd.csv") #TSDs
ccwkdt <- Reduce(function(x, y) {merge(x, y, by=c("STORE_NUM", "FSCL_WK_IN_YR_NUM", "FSCL_YR_NUM"), all = TRUE)}, list(hswk,ptwk,tsdwk,ccwk))
ccwkdt <- ccwkdt[FSCL_YR_NUM<=2017|(FSCL_YR_NUM==2018&FSCL_WK_IN_YR_NUM<=19)]
# #agg at week level
# ccwkdt <- ccwkdt[, list(cc = sum(Q2_2_TB_CNT,na.rm=T)/sum(Q2_2_RESPONSE_TOTAL,na.rm=T),
#                         hs = sum(HS_CUST_COUNT,na.rm=T)/sum(ALL_CUST_COUNT,na.rm=T),
#                         pt = mean(AVG_COMP_TENURE,na.rm=T),
#                         tsd = sum(CustTrans,na.rm=T)/sum(day_count,na.rm=T)),
#                         by=c("FSCL_WK_IN_YR_NUM","FSCL_YR_NUM")]
# setorder(ccwkdt,FSCL_YR_NUM,FSCL_WK_IN_YR_NUM)
#agg at the store-week level
ccwkdt <- ccwkdt[, list(cc = sum(Q2_2_TB_CNT,na.rm=T)/sum(Q2_2_RESPONSE_TOTAL,na.rm=T),
                        hs = sum(HS_CUST_COUNT,na.rm=T)/sum(ALL_CUST_COUNT,na.rm=T),
                        pt = mean(AVG_COMP_TENURE,na.rm=T),
                        tsd = sum(CustTrans,na.rm=T)/sum(day_count,na.rm=T)),
                 by=c("FSCL_WK_IN_YR_NUM","FSCL_YR_NUM","STORE_NUM")]
setorder(ccwkdt,FSCL_YR_NUM,FSCL_WK_IN_YR_NUM)
#remove week 53
ccwkdt <- ccwkdt[FSCL_WK_IN_YR_NUM<53]
#create a lag for cc
ccwkdt[, lag_cc_lw := lapply(.SD, function(x) c(NA, x[-.N])), by="STORE_NUM", .SDcols="cc"]
ccwkdt[, lag_cc_lw2 := lapply(.SD, function(x) c(NA, x[-.N])), by="STORE_NUM", .SDcols="lag_cc_lw"]
ccwkdt[, lag_cc_ly := lapply(.SD, function(x) c(NA, x[-.N])), by=c("STORE_NUM","FSCL_WK_IN_YR_NUM"), .SDcols="cc"]

# #make time-series indicator from years and weeks
# ccwkdt[, week := str_pad(round((FSCL_WK_IN_YR_NUM/53)*100,0), 2, pad = "0")]
# ccwkdt[, ts := as.numeric(paste0(FSCL_YR_NUM,".",week))]

#list-wise delete
ccwkdt <- na.omit(ccwkdt)

# #run a tobit regression censored between 0 and 1
# tobit1 <- censReg(cc ~ hs + pt + tsd, left = 0, right = 1, data = ccwkdt)
# summary.censReg(tobit1)

#run a linear regression since no observations censored
lm0 <- lm(cc ~ lag_cc_lw, data = ccwkdt[FSCL_YR_NUM==2018])
summary(lm0)
lm1 <- lm(cc ~ lag_cc_lw + lag_cc_ly, data = ccwkdt[FSCL_YR_NUM==2018])
summary(lm1)
lm2 <- lm(cc ~ hs + pt + tsd + lag_cc_lw, data = ccwkdt[FSCL_YR_NUM==2018])
summary(lm2)
lm3 <- lm(cc ~ hs + pt + tsd + lag_cc_lw + lag_cc_ly, data = ccwkdt[FSCL_YR_NUM==2018])
summary(lm3)
lm4 <- lm(cc ~ lag_cc_lw + lag_cc_lw2 + lag_cc_ly, data = ccwkdt[FSCL_YR_NUM==2018])
summary(lm4)
lm5 <- lm(cc ~ hs + pt + tsd + lag_cc_lw + lag_cc_lw2, data = ccwkdt[FSCL_YR_NUM==2018])
summary(lm5)
lm6 <- lm(cc ~ hs + pt + tsd + lag_cc_lw + lag_cc_lw2 + lag_cc_ly, data = ccwkdt[FSCL_YR_NUM==2018])
summary(lm6)
#compare the nested models
anova(lm0,lm1,lm2,lm3,lm4,lm5,lm6,test="Chisq")

# temp <- ccwkdt[,.(FSCL_WK_IN_YR_NUM,FSCL_YR_NUM,STORE_NUM,cc)]
# temp <- temp[, list(cc = mean(cc,na.rm=T)), by=c("FSCL_WK_IN_YR_NUM","FSCL_YR_NUM")]
# setorder(temp,FSCL_YR_NUM,FSCL_WK_IN_YR_NUM)
# decompose(ts(temp, frequency=52))
# acf(ts(temp, frequency=52))

#AIC value
AIC(lm3)
#log-likelihood
logLik(lm3)

#calculate a correlogram of the in-sample forecast errors (lags 1-30)
acf(lm3$residuals,lag.max=30)

#test whether there is significant evidence for non-zero correlations at lags 1-30
Box.test(lm3$residuals, lag=30, type="Ljung-Box")

#ensure forecast errors are normally distributed with mean zero and constant variance
plot.ts(lm3$residuals,main="LM Residuals by Day")

#check if the distribution of forecast errors is roughly centred on zero, and is more or less normally distributed
#write function
plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd   <- sd(forecasterrors)
  mymin  <- min(forecasterrors) - mysd*5
  mymax  <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a grey histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="grey", freq=FALSE, breaks=mybins,main="distribution of Forecast Errors")
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}
#plot
plotForecastErrors(lm3$residuals)

# library("TTR")
temp <- pcc[,.(ds,y)]
temp <- setorder(temp,ds)
temp <- as.matrix(temp[,.(y)])
temptimeseries <- ts(temp, frequency=365, start=c(2015,244))
plot.ts(temptimeseries)
# temptimeseriesSMA365 <- SMA(temptimeseries,n=365)
# plot.ts(temptimeseriesSMA365)
# temptimeseriescomponents <- decompose(temptimeseries)
# temptimeseriescomponents$seasonal
# temptimeseriesseasonallyadjusted <- tempti
# meseries - temptimeseriescomponents$seasonal
# plot(temptimeseriesseasonallyadjusted)

#HW, seasonal
hw1 <- HoltWinters(temptimeseries)
hw1fcast <- predict(m, n.ahead=21, prediction.interval=T)
plot(hw1,hw1fcast,main="HoltWinters Seasonal")
#HW, non-seasonal
hw2 <- HoltWinters(temptimeseries, gamma=FALSE)
hw2fcast <- predict(m, n.ahead=21, prediction.interval=T)
plot(hw2,hw2fcast,main="HoltWinters Non-Seasonal")
#HW with exponential smoothing
hw3 <- HoltWinters(temptimeseries, beta=FALSE, gamma=FALSE)
hw3fcast <- predict(m, n.ahead=21, prediction.interval=T)
plot(hw3,hw3fcast,main="HoltWinters w/ exponential smoothing")
