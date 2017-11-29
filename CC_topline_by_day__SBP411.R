## Topline CC Prediction ##
## forecasting basic time trend ##

#load libraries
library(data.table)
library(ggplot2)
library(forecast)
library(zoo)

#load data
cc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc_topline_by_day.csv")

#set names as lower case for ease
setnames(cc, tolower(names(cc)))

#update variable classes
cc[, caldate := as.Date(caldate, "%d-%b-%y")]
cc[, ccscore := as.numeric(ccscore)]

#recode holiday as binary
cc[holidayflag=='N', holiday := 0];cc[holidayflag=='Y', holiday := 1]
cc[, holidayflag := NULL]

#plot trends
#set labels
xlabel <- "Day in Year"
ylabel <- "CC Top Box Score"
tlabel <- "CC Top Box Score Trend"
slabel <- "*Note: Uses calendar dates"
#set data and variables
pdata <- cc
px <- cc[, caldayyear]
py <- cc[, ccscore]
groupvar <- cc[, calyear]
#manual legend labels
lname <- "Calendar Year"
llabels <- c("2015", "2016","2017") 
ybreaks <- 8
#line chart, factored by one variable
plot1 <- ggplot() +
  geom_line(data=pdata, aes(x=px, y=py, group=factor(groupvar), colour=factor(groupvar))) + 
  xlab(xlabel) + ylab(ylabel) + theme_bw() +
  scale_colour_discrete(name=lname, labels=llabels, guide=guide_legend(order=1)) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  scale_x_continuous(limits=c(pdata[,min(px)],pdata[,max(px)]), breaks = scales::pretty_breaks(n = ybreaks)) +
  scale_y_continuous(limits=c(pdata[,min(py)],pdata[,max(py)]),labels=scales::percent) +
  labs(title = tlabel, subtitle = slabel)
print(plot1)


#basic time series forecast

## Create a daily time series object
zts <- zoo(cc[,ccscore], seq(from = as.Date("2015-01-01"), to = as.Date("2017-11-26"), by = 1))
## use auto.arima to choose ARIMA terms
fit <- forecast::auto.arima(zts)
## forecast for next 60 time points
fore <- forecast(fit, h = 60)
## plot it
plot(fore)


