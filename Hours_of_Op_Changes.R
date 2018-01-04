##Hours of Operations Project##
##From Brendan 12/27/17##

#load libraries
library(data.table)
library(ggplot2)
library(dplyr)
library(chron)

#load data - calendar year 2017
hrfu <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/Hours_of_Op_Changes_future.csv")
hrpa <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/Hours_of_Op_Changes_past.csv")
hrfu[, EFFDATE := as.Date(EFFDATE, "%d-%b-%y")]
hrpa[, EFFDATE := as.Date(EFFDATE, "%d-%b-%y")]
# hr[, HRS_TYPE_CD := NULL]; hr[, HOLIDAY_CD := NULL]; hr[, EXPDATE := NULL]
# hr[, STAFFOPEN := NULL]; hr[, STAFFCLOSE := NULL]; hr[, TIMEOPEN := NULL]

#get list of stores with future change
futurelist <- unique(hrfu[,STORE_NUMBER])
#keep only historical information for stores with future change
hrpa <- hrpa[STORE_NUMBER %in% futurelist]

#keep only subset of variable for past and rename them accordingly
hrpa <- hrpa[, c("STORE_NUMBER","DAYOFWEEK","TIMECLOSE","OPEN_24_HR_IND"), with=F]
setnames(hrpa,c("TIMECLOSE","OPEN_24_HR_IND"),c("TIMECLOSEPAST","OPEN_24_HR_INDPAST"))

#left merge by store and day
hr <- left_join(hrfu,hrpa,by=c("STORE_NUMBER","DAYOFWEEK"))
setDT(hr)

#convert timestamp to numeric
hr[, timec := as.numeric(chron(times = TIMECLOSE))*24]
hr[, timecpast := as.numeric(chron(times = TIMECLOSEPAST))*24]
#convert 00:00 to 24 (for midnight)
hr[timec<1, timec := timec+24]
hr[timecpast<1, timecpast := timecpast+24]
#calculate delta
#if new time is greater than old time, there is an increase in hours (delta is pos)
hr[timec>timecpast, timedelta := abs(timec-timecpast)]
#if new time is less than old time, there is a reduction in hours (delta is neg)
hr[timec<timecpast, timedelta := -(abs(timec-timecpast))]
#for no change
hr[timec==timecpast, timedelta := 0]
#edge cases
hr[timec>20&timecpast<5, timedelta := -(24-(abs(timec-timecpast)))]
hr[timec<5&timecpast>20, timedelta := 24-(abs(timec-timecpast))]

#drop cases where there is no change
hr <- hr[timedelta!=0]
#create negative or positive indicator
hr[timedelta>0, negchange := 0]
hr[timedelta<0, negchange := 1]

#descriptives
temp <- hr %>%
  group_by(negchange) %>%
  summarise(total = n(),
            meandelta = mean(timedelta,na.rm=T),
            mindelta = min(timedelta,na.rm=T),
            maxdelta = max(timedelta,na.rm=T),
            meanpasttime = mean(timecpast,na.rm=T),
            meannewtime = mean(timec,na.rm=T))

#for merging in sales information, round down half-hour increments (sales_hour at hour-start level)
hr[, timecpastend := floor(timec)]
hr[, timecpaststart := floor(timecpast)]
hr[, hourvec := paste0("c(",timecpaststart,":",timecpastend,")")]


# #create empty matrix of stores, days, and hour changes
# storeids <- unique(hr[,STORE_NUMBER])
#restrict to those who DROP hours
storeids <- unique(hr[negchange==1,STORE_NUMBER])
hours <- c(1:24)
days <- c(1:7)
newData <- expand.grid(storeids, days, hours)
names(newData) <- c("STORE_NUMBER","DAYOFWEEK","hours")
newData <- setorder(newData,STORE_NUMBER,DAYOFWEEK,hours)
#create matrix of ACTUAL stores, days, and hour changes
hrsub <- hr[, c("STORE_NUMBER","DAYOFWEEK","timecpaststart","timecpastend","timedelta"), with=F]
hrsub[timedelta<0, timecpaststart := timecpaststart-1]
hrsub[timedelta>0, timecpastend := timecpastend-1]
hrsub[, timedelta := NULL]
#hrsub[, hourlength := abs(timedelta)]
hrsub <- melt(hrsub, id=c("STORE_NUMBER","DAYOFWEEK"), value.name="hours")
hrsub[, variable := NULL]
hrsub[, flag := 1]
#get min and max for each store and day
hrminmax <- hrsub %>%
  group_by(STORE_NUMBER,DAYOFWEEK) %>%
  summarise(minhr = min(hours,na.rm=T),
            maxhr = max(hours,na.rm=T))
setDT(hrminmax)

#merge together
hrsub <- left_join(newData,hrsub,by=c("STORE_NUMBER","DAYOFWEEK","hours"))
setDT(hrsub)
#remove duplicates
hrsub <- hrsub %>% distinct
#merge together
hrsub <- left_join(hrsub,hrminmax,by=c("STORE_NUMBER","DAYOFWEEK"))
setDT(hrsub)
#set flag
hrsub[minhr<=hours&hours<=maxhr, flag := 1]
hrsub[, minhr := NULL]; hrsub[, maxhr := NULL]
#drop rows where flag is NA
hrsub <- na.omit(hrsub, cols="flag")


# #bring in comp - FY 17 Q4 COMP by hour
# hrcomp <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/Hours_of_Op_Changes_FY17Q4comp.csv")
# #convert sale hour to useable number
# hrcomp[, SALE_HOUR := SALE_HOUR/10000]
# #swing wide by hour
# hrcomp <- dcast.data.table(hrcomp, SALE_HOUR + STORE_NUMBER + DAYOFWEEK ~ FSCL_YR_NUM, value.var="SALESAMT")
# setnames(hrcomp,c("2016","2017","SALE_HOUR"),c("lysalesamt","salesamt","hours"))
# #keep only stores in hrsub
# hrcomp <- hrcomp[STORE_NUMBER %in% storeids]
# #merge
# temp <- left_join(hrsub,hrcomp,by=c("STORE_NUMBER","DAYOFWEEK","hours"))
# setDT(temp)
# #aggregate by store
# temp2 <- temp[salesamt>0&lysalesamt>0, list(salesamt = sum(salesamt,na.rm=T),
#                     lysalesamt = sum(lysalesamt,na.rm=T),
#                     comp = (sum(salesamt,na.rm=T)-sum(lysalesamt,na.rm=T))/sum(lysalesamt,na.rm=T)),
#                     by="STORE_NUMBER"]
# temp3 <- temp[salesamt>0&lysalesamt>0, list(salesamt = sum(salesamt,na.rm=T),
#                                             lysalesamt = sum(lysalesamt,na.rm=T),
#                                             comp = (sum(salesamt,na.rm=T)-sum(lysalesamt,na.rm=T))/sum(lysalesamt,na.rm=T))]

#bring in transactions from prior year during the reduction period
hrtrans <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/Hours_of_Op_Changes_FW7-11_FY17.csv")
#convert sale hour to useable number
hrtrans[, hours := SALE_HOUR/10000]
# #swing wide by hear
# hrtrans <- dcast.data.table(hrtrans, SALE_HOUR + STORE_NUMBER + DAYOFWEEK ~ FSCL_YR_NUM, value.var="SALESAMT")
# setnames(hrtrans,c("2016","2017","SALE_HOUR"),c("lysalesamt","salesamt","hours"))
#keep only stores in hrsub
hrtrans <- hrtrans[STORE_NUMBER %in% storeids]
#merge
temp <- left_join(hrsub,hrtrans,by=c("STORE_NUMBER","DAYOFWEEK","hours"))
setDT(temp)
#aggregate by store
temp2 <- temp[TRANS>0, list(TRANS = sum(TRANS,na.rm=T)), by="STORE_NUMBER"]
temp3 <- temp[TRANS>0, list(TRANS = sum(TRANS,na.rm=T),
                             meanTRANS = mean(TRANS,na.rm=T))]



############################
############################
############################
############################


#load data - calendar year 2016
hr16fu <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/Hours_of_Op_Changes_future16.csv")
hr16pa <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/Hours_of_Op_Changes_past16.csv")
hr16fu[, EFFDATE := as.Date(EFFDATE, "%d-%b-%y")]
hr16pa[, EFFDATE := as.Date(EFFDATE, "%d-%b-%y")]
# hr16[, HRS_TYPE_CD := NULL]; hr16[, HOLIDAY_CD := NULL]; hr16[, EXPDATE := NULL]
# hr16[, STAFFOPEN := NULL]; hr16[, STAFFCLOSE := NULL]; hr16[, TIMEOPEN := NULL]

#get list of stores with future change
futurelist <- unique(hr16fu[,STORE_NUMBER])
#keep only historical information for stores with future change
hr16pa <- hr16pa[STORE_NUMBER %in% futurelist]

#keep only subset of variable for past and rename them accordingly
hr16pa <- hr16pa[, c("STORE_NUMBER","DAYOFWEEK","TIMECLOSE","OPEN_24_HR_IND"), with=F]
setnames(hr16pa,c("TIMECLOSE","OPEN_24_HR_IND"),c("TIMECLOSEPAST","OPEN_24_HR_INDPAST"))

#left merge by store and day
hr16 <- left_join(hr16fu,hr16pa,by=c("STORE_NUMBER","DAYOFWEEK"))
setDT(hr16)

#convert timestamp to numeric
hr16[, timec := as.numeric(chron(times = TIMECLOSE))*24]
hr16[, timecpast := as.numeric(chron(times = TIMECLOSEPAST))*24]
#convert 00:00 to 24 (for midnight)
hr16[timec<1, timec := timec+24]
hr16[timecpast<1, timecpast := timecpast+24]
#calculate delta
#if new time is greater than old time, there is an increase in hours (delta is pos)
hr16[timec>timecpast, timedelta := abs(timec-timecpast)]
#if new time is less than old time, there is a reduction in hours (delta is neg)
hr16[timec<timecpast, timedelta := -(abs(timec-timecpast))]
#for no change
hr16[timec==timecpast, timedelta := 0]
#edge cases
hr16[timec>20&timecpast<5, timedelta := -(24-(abs(timec-timecpast)))]
hr16[timec<5&timecpast>20, timedelta := 24-(abs(timec-timecpast))]

#drop cases where there is no change
hr16 <- hr16[timedelta!=0]
#create negative or positive indicator
hr16[timedelta>0, negchange := 0]
hr16[timedelta<0, negchange := 1]

#descriptives
temp16 <- hr16 %>%
  group_by(negchange) %>%
  summarise(total = n(),
            meandelta = mean(timedelta,na.rm=T),
            mindelta = min(timedelta,na.rm=T),
            maxdelta = max(timedelta,na.rm=T),
            meanpasttime = mean(timecpast,na.rm=T),
            meannewtime = mean(timec,na.rm=T))

#from here, can pull in comp, volume, etc.
#also able to look at reductions in labor
#can pull in average comp, as well as comp by daypart



