## Topline CC Prediction ##
## forecasting basic time trend ##

#load libraries
library(data.table)
library(ggplot2)
library(forecast)
library(zoo)
library(prophet)
set.seed(98115)

#load data
cct <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc_topline_by_day.csv")

#set names as lower case for ease
setnames(cct, tolower(names(cct)))

#update variable classes
cct[, caldate := as.Date(caldate, format = "%d-%b-%y")]
cct[, ccscore := as.numeric(ccscore)]

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
pr <- prophet(pcc, growth="linear", daily.seasonality=TRUE, weekly.seasonality=TRUE, holidays=pcchol)
#dataframe returned from mode
fcst <- predict(pr,pcc)
#plot model
prophet_plot_components(pr, fcst, uncertainty = TRUE, plot_cap = TRUE,
                        weekly_start = 0, yearly_start = 0)
#predict forward
future <- make_future_dataframe(pr, periods = 185)
forecast <- predict(pr, future)
plot(pr, forecast, xlab="Calendar Date", ylab="Customer Connection")
#sample from posterior preditive distribution
predictive_samples(pr, pcc)



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


#compare to Tableau dashes
#average across months
cctm <- cct[, list(ccscore = mean(ccscore,na.rm=T)), by=c("calmonth","calyear")]
plot1 <- ggplot() +
  geom_line(data=cctm, aes(x=calmonth, y=ccscore, group=factor(calyear), colour=factor(calyear))) +
  scale_y_continuous(limits=c(0,.35))
print(plot1)




