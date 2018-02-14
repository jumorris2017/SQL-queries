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

#library(tseries)
#test for unit root
temp <- pcc[,.(ds,y)]
temp <- setorder(temp,ds)
temp <- as.matrix(temp[,.(y)])
adf.test(temp[,"y"],k=1)
#library(forecast)
fit <- auto.arima(temp[,"y"], start.p = 1, start.q = 1)
res <- residuals(fit)
Box.test(res) #box-pierce test

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
fit <- auto.arima(temp)
fc <- forecast(fit,h=25)
predvalues <- fc$mean[1:25]
plot(forecast(fit,h=25))
#residuals and box test
Acf(residuals(fit))
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



