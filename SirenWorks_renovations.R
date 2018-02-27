##making plots for Siren Works renovations
##weekly trends and YoY comparison
##with YoY deltas called out
##CE measures: CC, SO, Speed, Cleanliness

#load libraries
library(data.table)
library(ggplot2)
library(tidyverse)
library(lubridate)

#group by stores that *are* or *are not* using the new plays

#load data
#ce data from survox
swce <- fread("Q:/Departments/WMO/Marketing Research/New Q drive/Foundational/Customer Voice/2.0/Ad Hoc Q/2018_2_20_Siren Works Renovations/ce101_2017.csv")
swce[, dateymd := ymd(date1)]
swce[, datemonth := month(dateymd)]
swce[adhoc==1, testcase := 1];swce[adhoc==2, testcase := 0]

#reduce number of variables
swce <- swce[, c("guid","dateymd","datemonth","testcase","q1",grep("q2",colnames(swce),value=T)),with=FALSE]

#pull in cust trans data
ctrans <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CE_SirenWorks_custtrans.csv")
setnames(ctrans,c("GUID_ID"),c("guid"))
ctrans[, datemonth := FSCL_PER_IN_YR_NUM-3]

#swing wide by month
ctrans <- dcast.data.table(ctrans, guid ~ datemonth, value.var="TRANS")
colnames(ctrans)[2:5] <- paste("transmnth", colnames(ctrans)[2:5], sep = "_")

#merge
temp <- merge(swce,ctrans,by="guid")

