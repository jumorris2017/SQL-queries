## Topline CC Prediction ##
## forecasting basic time trend ##

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


#write functions 
#plot of forecast errors
# plotForecastErrors <- function(forecasterrors)
# {
#   # make a histogram of the forecast errors:
#   set.seed(98115)
#   mybinsize <- IQR(forecasterrors)/4
#   mysd   <- sd(forecasterrors)
#   mymin  <- min(forecasterrors) - mysd*5
#   mymax  <- max(forecasterrors) + mysd*3
#   # generate normally distributed data with mean 0 and standard deviation mysd
#   mynorm <- rnorm(10000, mean=0, sd=mysd)
#   mymin2 <- min(mynorm)
#   mymax2 <- max(mynorm)
#   if (mymin2 < mymin) { mymin <- mymin2 }
#   if (mymax2 > mymax) { mymax <- mymax2 }
#   # make a grey histogram of the forecast errors, with the normally distributed data overlaid:
#   mybins <- seq(mymin, mymax, mybinsize)
#   hist(forecasterrors, col="grey", freq=FALSE, breaks=mybins,main="distribution of residuals")
#   # freq=FALSE ensures the area under the histogram = 1
#   # generate normally distributed data with mean 0 and standard deviation mysd
#   myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
#   # plot the normal curve as a blue line on top of the histogram of forecast errors:
#   points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
# }

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
# 
# #omit the latest 3 weeks for validating
# cct <- cct[caldate<='2018-01-13']

#recode holiday as binary
cct[holidayflag=='N', holiday := 0];cct[holidayflag=='Y', holiday := 1]
cct[, holidayflag := NULL]

#create dataset for prophet
pcc <- copy(cct)
setnames(pcc,c("caldate","ccscore"),c("ds","y"))

# #qq plot
# temp <- pcc[,.(ds,y)]
# temp <- setorder(temp,ds)
# temp <- as.matrix(temp[,.(y)])
# qqnorm(temp[,"y"],main = "Normal Q-Q Plot",
#        xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
#        plot.it = TRUE, datax = FALSE)
# qqline(temp[,"y"], datax = FALSE)

#using library(forecast)
# temp <- pcc[,.(ds,y)]
# temp <- setorder(temp,ds)
# temp <- as.matrix(temp[,.(y)])
# #test for unit root
# tseries::adf.test(temp,k=1)
# fit <- auto.arima(temp,allowdrift=T)
# fc <- forecast(fit,h=25)
# predvalues <- fc$mean[1:25]
# plot(forecast(fit,h=25))
# #residuals and box test
# Acf(residuals(fit))
# Pacf(residuals(fit))
# #box-pierce test for null hypothesis of independence in ts
# Box.test(residuals(fit), type="Ljung")

#TBATS allows for dynamic seasonality (versus ARIMA where seasonality is periodic)
# TBATS(omega, p,q, phi, <m1,k1>,...,<mJ,kJ>) where 
# omega is the Box-Cox parameter and phi is the damping parameter; 
# the error is modelled as an ARMA(p,q) process 
# and m1,...,mJ list the seasonal periods used in the model 
# and k1,...,kJ are the corresponding number of Fourier terms used for each seasonality.

#using library(forecast)
#TBATS
#day of week trends
temp <- pcc[,.(ds,y)]
temp <- setorder(temp,ds)
x_new  <- msts(temp[,"y"], seasonal.periods=c(7,7*52))
fit <- tbats(x_new,set.seed(98115))
fc <- forecast(fit, h=25)
predvalues <- fc$mean[1:25]
plot(forecast(fit,h=25))
# #diagnosis...
# fit$likelihood #fc$model$likelhood
# fit$AIC
# length(fit$parameters$vect)

#make a vector of past/future dates
datevec_hist <- cct[, caldate]
datevec_pred <- as.Date(x = integer(25), origin = "1970-01-01")
for (i in 1:length(datevec_pred)) {
  datevec_pred[i] <- max(cct[,caldate]) + i
}
dates <- c(datevec_hist,datevec_pred)
dates <- dates[order(as.Date(dates, format = "%Y-%m-%d"))]

#make dt of historical (fc$x) and predicted values (fc$mean)
fc_full <- c(fc$x, fc$mean)
fc_full <- as.data.table(fc_full)
setnames(fc_full,"fc_full","ccscore")
fc_full[, predvalue := 1]
fc_full[1:length(fc$x), predvalue := 0]
fc_full[, caldate := dates]


# #plot distribution of residuals
# plotForecastErrors(fc$residuals)
# #ensure forecast errors are normally distributed with mean zero and constant variance
# plot.ts(fc$residuals,main="MSTS/TBATS Residuals by Day")
