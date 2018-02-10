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
cct <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc_topline_by_week.csv")

#set names as lower case for ease
setnames(cct, tolower(names(cct)))

#update variable classes
cct[, ccscore := as.numeric(ccscore)]

#only keep data post august 2015
cct <- cct[calyear>=2017|(calyear==2016&calweek>=45)]

#create dataset for prophet
pcc <- copy(cct)
setnames(pcc,c("calweek","ccscore"),c("ds","y"))

#prophet forecast model
pr <- prophet(weekly.seasonality=TRUE)
#pr <- prophet(pcc, growth="linear", daily.seasonality=FALSE, weekly.seasonality=TRUE, holidays=pcchol)
#add monthly seasonality
pr <- add_seasonality(pr, name='monthly', period=3.5, fourier.order=5)
#fit model
pr <- fit.prophet(pr,pcc)
#dataframe returned from mode
fcst <- predict(pr,pcc)
#plot model
prophet_plot_components(pr, fcst, uncertainty = TRUE, plot_cap = TRUE,
                        weekly_start = 0, yearly_start = 0)
#predict forward
future <- make_future_dataframe(pr, periods = 3)
forecast <- predict(pr, future)
plot(pr, forecast, xlab="Calendar Week", ylab="Customer Connection")
#sample from posterior preditive distribution
predictive_samples(pr, pcc)

