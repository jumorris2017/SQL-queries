#tracking SR CE response repeats by GUID
#P12 FY16 & P6 FY17 & P12 FY18 & P6 FY18

#load libraries
library(data.table)
library(lubridate)

#load data
data_dir <- "O:/CoOp/CoOp194_PROReportng&OM/Julie"
sr <- fread(paste0(data_dir,"/ce_survey_guids_p6fy16and17_p12fy17and18.csv"))
ex <- fread(paste0(data_dir,"/ce_survey_guids_experian.csv"))
ten <- fread(paste0(data_dir,"/ce_survey_guids_sr_tenure.csv"))

#merge together
srfull <- Reduce(function(x, y) {merge(x, y, 
                                       by=c("GUID_USER_ID","FSCL_YR_NUM","FSCL_PER_IN_YR_NUM"), 
                                       all = TRUE)}, list(sr,ex))
srfull <- Reduce(function(x, y) {merge(x, y, by=c("GUID_USER_ID"), all = TRUE)}, list(srfull,ten))

#vectors of guids by period
g_p12fy16 <- srfull[FSCL_YR_NUM==2016&FSCL_PER_IN_YR_NUM==12,GUID_USER_ID]
g_p6fy17 <- srfull[FSCL_YR_NUM==2017&FSCL_PER_IN_YR_NUM==6,GUID_USER_ID]
g_p12fy17 <- srfull[FSCL_YR_NUM==2017&FSCL_PER_IN_YR_NUM==12,GUID_USER_ID]
g_p6fy18 <- srfull[FSCL_YR_NUM==2018&FSCL_PER_IN_YR_NUM==6,GUID_USER_ID]

#percent repeat
#first period to next 3 periods
length(intersect(g_p12fy16,g_p6fy17))/length(g_p12fy16) #1 period later
length(intersect(g_p12fy16,g_p12fy17))/length(g_p12fy16)
length(intersect(g_p12fy16,g_p6fy18))/length(g_p12fy16)
#second period to next 2 periods
length(intersect(g_p6fy17,g_p12fy17))/length(g_p6fy17) #1 period later
length(intersect(g_p6fy17,g_p6fy18))/length(g_p6fy17)
#third period to next 1 period
length(intersect(g_p12fy17,g_p6fy18))/length(g_p12fy17) #1 period later

#calculate tenure
srfull[, joindt := as_date(ymd_hms(MBR_JOIN_DT))]
srfull[FSCL_YR_NUM==2016&FSCL_PER_IN_YR_NUM==12, last_per_date := as_date("2016-10-02")] #update for each period
srfull[FSCL_YR_NUM==2017&FSCL_PER_IN_YR_NUM==6, last_per_date := as_date("2017-04-02")] #update for each period
srfull[FSCL_YR_NUM==2017&FSCL_PER_IN_YR_NUM==12, last_per_date := as_date("2017-10-01")] #update for each period
srfull[FSCL_YR_NUM==2018&FSCL_PER_IN_YR_NUM==6, last_per_date := as_date("2018-04-01")] #update for each period
srfull[, srtenure := today-joindt]
srfull[, srtenure_yrs := as.numeric(srtenure, units="days")]
srfull[, srtenure_yrs := round(srtenure_yrs/365.25,2)]