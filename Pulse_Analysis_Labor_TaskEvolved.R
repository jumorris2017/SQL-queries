#June FY18

#load libaries
library(data.table)
library(lubridate)

#set path (new Q)
data_dir <- "O:/CoOp/CoOp194_PROReportng&OM/Julie"
dt <- fread(paste0(data_dir,"/Pulse_Analysis_Labor_TaskEvolved2.csv"))
dt <- setorder(dt,-JOB_ID,DAYPART,TPERIOD,-REGION)
dt <- dt[!is.na(REGION)]
#write t.test function
tfunc <- function(x1,x2,sd,df){(x1-x2)/sqrt((sd^2)/df)}
#form table to make excel table easier
temp <- rbind(dt[JOB_ID==50000362&DAYPART=='PM'&TPERIOD=='0_PRE',],
              dt[JOB_ID==50000362&DAYPART=='PM'&TPERIOD=='1_POST',],
              dt[JOB_ID==50000362&DAYPART=='MID'&TPERIOD=='0_PRE',],
              dt[JOB_ID==50000362&DAYPART=='MID'&TPERIOD=='1_POST',],
              dt[JOB_ID==50000362&DAYPART=='RESTOFDAY'&TPERIOD=='0_PRE',],
              dt[JOB_ID==50000362&DAYPART=='RESTOFDAY'&TPERIOD=='1_POST',],
              dt[JOB_ID==50000358&DAYPART=='PM'&TPERIOD=='0_PRE',],
              dt[JOB_ID==50000358&DAYPART=='PM'&TPERIOD=='1_POST',],
              dt[JOB_ID==50000358&DAYPART=='MID'&TPERIOD=='0_PRE',],
              dt[JOB_ID==50000358&DAYPART=='MID'&TPERIOD=='1_POST',],
              dt[JOB_ID==50000358&DAYPART=='RESTOFDAY'&TPERIOD=='0_PRE',],
              dt[JOB_ID==50000358&DAYPART=='RESTOFDAY'&TPERIOD=='1_POST',],
              dt[JOB_ID==50000117&DAYPART=='PM'&TPERIOD=='0_PRE',],
              dt[JOB_ID==50000117&DAYPART=='PM'&TPERIOD=='1_POST',],
              dt[JOB_ID==50000117&DAYPART=='MID'&TPERIOD=='0_PRE',],
              dt[JOB_ID==50000117&DAYPART=='MID'&TPERIOD=='1_POST',],
              dt[JOB_ID==50000117&DAYPART=='RESTOFDAY'&TPERIOD=='0_PRE',],
              dt[JOB_ID==50000117&DAYPART=='RESTOFDAY'&TPERIOD=='1_POST',])
temp <- setorder(temp,-JOB_ID,DAYPART,TPERIOD,-REGION)
write.csv(temp,paste0(data_dir,"/temp2.csv"))

tfunc(dt[JOB_ID==50000362&DAYPART=='MID'&TPERIOD=='0_PRE',AVG_SCORE],
      dt[JOB_ID==50000362&DAYPART=='MID'&TPERIOD=='1_POST',AVG_SCORE],
      dt[JOB_ID==50000362&DAYPART=='MID'&TPERIOD=='1_POST',SD_SCORE],
      dt[JOB_ID==50000362&DAYPART=='MID'&TPERIOD=='0_PRE',RESP_TOTAL])

tfunc(dt[JOB_ID==50000362&DAYPART=='PM'&TPERIOD=='0_PRE',AVG_SCORE],
      dt[JOB_ID==50000362&DAYPART=='PM'&TPERIOD=='1_POST',AVG_SCORE],
      dt[JOB_ID==50000362&DAYPART=='PM'&TPERIOD=='1_POST',SD_SCORE],
      dt[JOB_ID==50000362&DAYPART=='PM'&TPERIOD=='0_PRE',RESP_TOTAL])

tfunc(dt[JOB_ID==50000362&DAYPART=='RESTOFDAY'&TPERIOD=='0_PRE',AVG_SCORE],
      dt[JOB_ID==50000362&DAYPART=='RESTOFDAY'&TPERIOD=='1_POST',AVG_SCORE],
      dt[JOB_ID==50000362&DAYPART=='RESTOFDAY'&TPERIOD=='1_POST',SD_SCORE],
      dt[JOB_ID==50000362&DAYPART=='RESTOFDAY'&TPERIOD=='0_PRE',RESP_TOTAL])

tfunc(dt[JOB_ID==50000358&DAYPART=='MID'&TPERIOD=='0_PRE',AVG_SCORE],
      dt[JOB_ID==50000358&DAYPART=='MID'&TPERIOD=='1_POST',AVG_SCORE],
      dt[JOB_ID==50000358&DAYPART=='MID'&TPERIOD=='1_POST',SD_SCORE],
      dt[JOB_ID==50000358&DAYPART=='MID'&TPERIOD=='0_PRE',RESP_TOTAL])

tfunc(dt[JOB_ID==50000358&DAYPART=='PM'&TPERIOD=='0_PRE',AVG_SCORE],
      dt[JOB_ID==50000358&DAYPART=='PM'&TPERIOD=='1_POST',AVG_SCORE],
      dt[JOB_ID==50000358&DAYPART=='PM'&TPERIOD=='1_POST',SD_SCORE],
      dt[JOB_ID==50000358&DAYPART=='PM'&TPERIOD=='0_PRE',RESP_TOTAL])

tfunc(dt[JOB_ID==50000358&DAYPART=='RESTOFDAY'&TPERIOD=='0_PRE',AVG_SCORE],
      dt[JOB_ID==50000358&DAYPART=='RESTOFDAY'&TPERIOD=='1_POST',AVG_SCORE],
      dt[JOB_ID==50000358&DAYPART=='RESTOFDAY'&TPERIOD=='1_POST',SD_SCORE],
      dt[JOB_ID==50000358&DAYPART=='RESTOFDAY'&TPERIOD=='0_PRE',RESP_TOTAL])

tfunc(dt[JOB_ID==50000117&DAYPART=='MID'&TPERIOD=='0_PRE',AVG_SCORE],
      dt[JOB_ID==50000117&DAYPART=='MID'&TPERIOD=='1_POST',AVG_SCORE],
      dt[JOB_ID==50000117&DAYPART=='MID'&TPERIOD=='1_POST',SD_SCORE],
      dt[JOB_ID==50000117&DAYPART=='MID'&TPERIOD=='0_PRE',RESP_TOTAL])

tfunc(dt[JOB_ID==50000117&DAYPART=='PM'&TPERIOD=='0_PRE',AVG_SCORE],
      dt[JOB_ID==50000117&DAYPART=='PM'&TPERIOD=='1_POST',AVG_SCORE],
      dt[JOB_ID==50000117&DAYPART=='PM'&TPERIOD=='1_POST',SD_SCORE],
      dt[JOB_ID==50000117&DAYPART=='PM'&TPERIOD=='0_PRE',RESP_TOTAL])

tfunc(dt[JOB_ID==50000117&DAYPART=='RESTOFDAY'&TPERIOD=='0_PRE',AVG_SCORE],
      dt[JOB_ID==50000117&DAYPART=='RESTOFDAY'&TPERIOD=='1_POST',AVG_SCORE],
      dt[JOB_ID==50000117&DAYPART=='RESTOFDAY'&TPERIOD=='1_POST',SD_SCORE],
      dt[JOB_ID==50000117&DAYPART=='RESTOFDAY'&TPERIOD=='0_PRE',RESP_TOTAL])



#get pulse values from partners who stay after close
cl1 <- fread(paste0(data_dir,"/labortest_closetimes.csv"))
cl2 <- fread(paste0(data_dir,"/labortest_pulsescores.csv"))
setnames(cl1,"STORE_NUMBER","STORE_NUM")

#get dates and times
cl1[, CAL_DT := gsub("/","-",CAL_DT)]
cl1[, busdt := parse_date_time(CAL_DT, orders="mdy HM")]
cl1[, busdt := date(busdt)]
cl1[, closetime := parse_date_time(PERM_BUSINESS_CLOSE_TIME, orders="HMS")]
cl1[, closetime := hour(closetime)]

cl2[, shiftdate := gsub("/","-",SHIFT_END_DTM_LCL)]
cl2[, busdt := parse_date_time(shiftdate, orders="mdy HM")]
cl2[, busdt := date(busdt)]
cl2[, shiftdate := NULL]
cl2[, endtime := gsub("/","-",SHIFT_END_DTM_LCL)]
cl2[, endtime := parse_date_time(endtime, orders="mdy HM")]
cl2[, endtime := hour(endtime)]

#merge
cl <- Reduce(function(x, y) {merge(x, y, by=c("STORE_NUM", "busdt"), all.y = TRUE)}, list(cl1, cl2))

#assess if end time is later than close time
cl[closetime>=endtime, postcloser := 0]
cl[closetime<endtime, postcloser := 1]

#look at closers
cl0 <- cl[postcloser==1]
#get average scores
temp <- cl0[, list(N = .N, AVG_SCORE = round(mean(RESP_ID,na.rm=T),2)),
    by=c("JOB_ID","TPERIOD")]
temp <- setorder(temp,-JOB_ID,TPERIOD)