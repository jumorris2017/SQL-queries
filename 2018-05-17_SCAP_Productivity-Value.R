#managing data for SCAP

#load libraries
library(data.table)
library(tidyr)
library(dplyr)
library(lubridate)
library(nlme)

#scap status 4/1/18 (first day of Q3)
#CC data rolling 3 month (Q2)

#set folder directory
data_dir <- "O:/CoOp/CoOp194_PROReportng&OM/Julie"
scap <- fread(paste0(data_dir,"/SCAP_PTF_20180524.csv"))
store <- fread(paste0(data_dir,"/scap_partner_stores20180401.csv"))
store[, MOSTREC := NULL]
cc <- fread(paste0(data_dir,"/scap_cc_q2fy18.csv"))
cc[, c("FSCL_YR_NUM","FSCL_QTR_IN_YR_NUM") := NULL]
tsd <- fread(paste0(data_dir,"/scap_tsd_q2fy18.csv"))
tsd[, c("FSCL_YR_NUM","FSCL_QTR_IN_YR_NUM") := NULL]
setnames(tsd,"STORE_NUMBER","STORE_NUM")
uplh <- fread(paste0(data_dir,"/scap_uplh_q2fy18.csv"))
urbanity <- fread(paste0(data_dir,"/urbanity.csv"))

#restrict
scap <- scap[SCAPSTATUS=="Active-Participant"|SCAPSTATUS=="Graduated"]
#scap <- scap[SCAPSTATUS=="Active-Participant"|SCAPSTATUS=="Graduated"|SCAPSTATUS=="GFA-Participant"]

#reduce to only partner id and date
scap[, PRTNR_NUM := as.numeric(as.character(PARTNERID))]
scap <- scap[, .(PRTNR_NUM)]
scap <- na.omit(scap,cols="PRTNR_NUM")

#create dataset of binary indicators where partners were active participants
scap[, scap := 1]

#merge partner-level
##inner join on SCAP and recent shift
dt1 <- Reduce(function(x, y) {merge(x, y, by=c("PRTNR_NUM"), 
                                     all = FALSE)}, list(scap,store))
# storelist <- unique(dt1[scap==1,STORE_NUM])
#RECODE JOBS: hourly vs sm-level
dt1[JOB_ID %in% c(50000117,50000118), hourly := 0]
dt1[JOB_ID %in% c(50000362,50000358,50018175), hourly := 1]
dt1[hourly==1&scap==1, scap_hourly := 1];dt1[hourly==0, scap_hourly := 0]
dt1[hourly==0&scap==1, scap_sm := 1];dt1[hourly==1, scap_sm := 0]
#merge store-level
dt <- Reduce(function(x, y) {merge(x, y, by=c("STORE_NUM"), 
                                   all = TRUE)}, list(cc,tsd,uplh,urbanity))
dt[STORE_NUM %in% dt1[scap==1,STORE_NUM], scap := 1]
dt[STORE_NUM %in% dt1[scap_hourly==1,STORE_NUM], scap_hourly := 1]
dt[STORE_NUM %in% dt1[scap_sm==1,STORE_NUM], scap_sm := 1]

#make DT variable binary
dt[DRIVE_THRU=='N', DT := 0];dt[DRIVE_THRU=='Y', DT := 1]
dt[, DRIVE_THRU := NULL]

#make 0's for non-scap stores
dt[is.na(dt[,scap]), scap := 0];
dt[is.na(dt[,scap_hourly]), scap_hourly := 0];
dt[is.na(dt[,scap_sm]), scap_sm := 0]
#make vars
dt[, cc_score := round(Q2_2_TB_CNT/Q2_2_RESPONSE_TOTAL,3)]
dt[, tsd := round(CustTrans/day_count,1)]
dt[, uplh := round(TOT_UNITS/TOT_HOURS,1)]
dt <- na.omit(dt)

#KEEP
t.test(dt[URBANITY=="U4"&DT==0&scap_hourly==0,cc_score],dt[URBANITY=="U4"&DT==0&scap_hourly==1,cc_score])
t.test(dt[URBANITY=="U1"&DT==0&scap_hourly==0,cc_score],dt[URBANITY=="U1"&DT==0&scap_hourly==1,cc_score])

# summary(aov(Q2_2_TB_SCORE ~ scap_hourly + scap_sm + DT + URBANITY, data=dt))
t.test(dt[URBANITY=="U1"&scap_hourly==0,UPLH],dt[URBANITY=="U1"&scap_hourly==1,UPLH])
t.test(dt[URBANITY=="U2"&scap_hourly==0,UPLH],dt[URBANITY=="U2"&scap_hourly==1,UPLH])
t.test(dt[URBANITY=="U3"&scap_hourly==0,UPLH],dt[URBANITY=="U3"&scap_hourly==1,UPLH])
t.test(dt[URBANITY=="U4"&scap_hourly==0,UPLH],dt[URBANITY=="U4"&scap_hourly==1,UPLH])
t.test(dt[URBANITY=="U5"&scap_hourly==0,UPLH],dt[URBANITY=="U5"&scap_hourly==1,UPLH])
t.test(dt[URBANITY=="U6"&scap_hourly==0,UPLH],dt[URBANITY=="U6"&scap_hourly==1,UPLH])
t.test(dt[URBANITY=="U7"&scap_hourly==0,UPLH],dt[URBANITY=="U7"&scap_hourly==1,UPLH])

#cc
#all stores
summary(lmList(Q2_2_TB_SCORE ~ scap_hourly + scap_sm | URBANITY, data=dt))
#cafe-only
summary(lmList(Q2_2_TB_SCORE ~ scap_hourly + scap_sm | URBANITY, data=dt[DT==0]))
#DT
summary(lmList(Q2_2_TB_SCORE ~ scap_hourly + scap_sm | URBANITY, data=dt[DT==1]))

#UPLH
#all
summary(lmList(UPLH ~ scap_hourly + scap_sm | URBANITY, data=dt))
#cafe-only
summary(lmList(UPLH ~ scap_hourly + scap_sm | URBANITY, data=dt[DT==0]))
#DT
summary(lmList(UPLH ~ scap_hourly + scap_sm | URBANITY, data=dt[DT==1]))


#model by urbanity
newData <- dt[, .(STORE_NUM,URBANITY,scap_hourly,scap_sm,DT)]
ll = lmList(UPLH ~ scap_hourly + scap_sm + DT | URBANITY, data=dt)
predict(ll, newData)
newData[["value"]] <- predict(ll, newData)
setnames(newData,"value","predUPLH")

#t.tests
#summary: hourly scap participation impacts CC in U1 and U4 cafe-only stores
t.test(newData[scap_hourly==0,predUPLH],newData[scap_hourly==1,predUPLH])
t.test(newData[scap_sm==0,predUPLH],newData[scap_sm==1,predUPLH])

##OLD
#t.tests
t.test(dt[scap==0,cc_score],dt[scap==1,cc_score])
t.test(dt[scap==0,uplh],dt[scap==1,uplh])

#aggregate by scap vs. non-scap stores
dtagg <- dt[, list(Nstores = .N,
                Q2_2_RESPONSE_TOTAL=sum(Q2_2_RESPONSE_TOTAL,na.rm=T),
                Q2_2_TB_CNT=sum(Q2_2_TB_CNT,na.rm=T),
                CustTrans=sum(CustTrans,na.rm=T),
                day_count=sum(day_count,na.rm=T),
                TOT_UNITS=sum(TOT_UNITS,na.rm=T),
                TOT_HOURS=sum(TOT_HOURS,na.rm=T)),
         by=c("scap")]
dtagg[, cc_score := round(Q2_2_TB_CNT/Q2_2_RESPONSE_TOTAL,3)]
dtagg[, tsd := round(CustTrans/day_count,1)]
dtagg[, uplh := round(TOT_UNITS/TOT_HOURS,1)]

