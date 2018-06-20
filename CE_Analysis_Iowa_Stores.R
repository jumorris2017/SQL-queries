#Pam Iowa store visits
#June FY18

#load libaries
library(data.table)
library(lubridate)

#set path (new Q)
data_dir <- "O:/CoOp/CoOp194_PROReportng&OM/Julie"

#load data
ce <- fread(paste0(data_dir,"/Iowa_Stores_Q2FY18_CEtopline.csv"))
cepk <- fread(paste0(data_dir,"/Iowa_Stores_Q2FY18_CEpeak.csv"))
cech <- fread(paste0(data_dir,"/Iowa_Stores_Q2FY18_CEbychannel.csv"))
chmix <- fread(paste0(data_dir,"/Iowa_Stores_Q2FY18_Channel-Mix.csv"))
pt <- fread(paste0(data_dir,"/Iowa_Stores_Q2FY18_PartnerTenure.csv"))
sm <- fread(paste0(data_dir,"/Iowa_Stores_Q2FY18_SM-Stability.csv"))

#swing channel wide
cech[QSTN_ID=="Q2_2", qstn := "CC"]
cech[QSTN_ID=="Q2_1", qstn := "SPEED"]
cech[QSTN_ID=="Q2_3", qstn := "ABVBEYND"]
cech[QSTN_ID=="Q2_4", qstn := "ACCURACY"]
cech[QSTN_ID=="Q2_5", qstn := "BEVTASTE"]
cech[QSTN_ID=="Q2_6", qstn := "FOODTASTE"]
cech[QSTN_ID=="Q2_7", qstn := "CLEAN"]
cech <- dcast.data.table(cech, STORE_NUM + qstn ~ ORD_MTHD_CD, value.var="TB_SCORE")
cech <- dcast.data.table(cech, STORE_NUM ~ qstn, value.var=c("CAFE","MOP","OTW"))
#make store ops vars
cech[, CAFE_STOREOPS := round(rowMeans(subset(cech, select = c(CAFE_ABVBEYND,CAFE_ACCURACY,CAFE_SPEED,CAFE_BEVTASTE,CAFE_FOODTASTE,CAFE_CLEAN)), na.rm = TRUE),4)]
cech[, MOP_STOREOPS := round(rowMeans(subset(cech, select = c(MOP_ABVBEYND,MOP_ACCURACY,MOP_SPEED,MOP_BEVTASTE,MOP_FOODTASTE,MOP_CLEAN)), na.rm = TRUE),4)]
cech[, OTW_STOREOPS := round(rowMeans(subset(cech, select = c(OTW_ABVBEYND,OTW_ACCURACY,OTW_SPEED,OTW_BEVTASTE,OTW_FOODTASTE,OTW_CLEAN)), na.rm = TRUE),4)]

#calculate tenure; library(lubridate)
pt[, hiredt := as_date(mdy_hm(MOST_RECENT_HIRE_DT))]
pt[, today := as_date(Sys.Date())]
pt[, tenure := today-hiredt]
pt[, tenure_yrs := as.numeric(tenure, units="days")]
pt[, tenure_yrs := round(tenure_yrs/365.25,2)]
#average by store for hourly partners
pthr <- pt[JOB_ID==50000362|JOB_ID==50000358, list(hrly_sbux_tenure_yrs = round(mean(tenure_yrs,na.rm=T),2)),
                                           by="STORE_NUM"]
ptsm <- pt[JOB_ID==50000117, list(sm_sbux_tenure_yrs = round(mean(tenure_yrs,na.rm=T),2)),
         by="STORE_NUM"]

#restrict vars
chmix <- chmix[, .(STORE_NUM,DRIVE_THRU_IND,MOP_PCT,PEAK_MOP_PCT,OTW_PCT,PEAK_OTW_PCT)]
sm <- sm[, .(STORE_NUM,SM1YRSTABLE,SM2YRSTABLE)]

#full data
iowa <- Reduce(function(x, y) {merge(x, y, by=c("STORE_NUM"), all = TRUE)}, list(ce,cepk,cech,chmix,pthr,ptsm,sm))
iowa <- setorder(iowa,STORE_NUM)
write.csv(iowa, paste0(data_dir,"/Iowa_Stores_CE.csv"))






##UPDATE FOR TOPLINE
pt <- fread(paste0(data_dir,"/Iowa_Stores_Q2FY18_PartnerTenure.csv"))
ptmam <- fread(paste0(data_dir,"/MAM_Q2FY18_PartnerTenure.csv"))
ptus <- fread(paste0(data_dir,"/US_Q2FY18_PartnerTenure.csv"))

#calculate tenure; library(lubridate)
pt[, hiredt := as_date(mdy_hm(MOST_RECENT_HIRE_DT))]
pt[, today := as_date(Sys.Date())]
pt[, tenure := today-hiredt]
pt[, tenure_yrs := as.numeric(tenure, units="days")]
pt[, tenure_yrs := round(tenure_yrs/365.25,2)]
#average by store for hourly partners
pt[JOB_ID==50000362|JOB_ID==50000358, list(hrly_sbux_tenure_yrs = round(mean(tenure_yrs,na.rm=T),2))]
pt[JOB_ID==50000117, list(sm_sbux_tenure_yrs = round(mean(tenure_yrs,na.rm=T),2))]

#calculate tenure; library(lubridate)
ptmam[, hiredt := as_date(mdy_hm(MOST_RECENT_HIRE_DT))]
ptmam[, today := as_date(Sys.Date())]
ptmam[, tenure := today-hiredt]
ptmam[, tenure_yrs := as.numeric(tenure, units="days")]
ptmam[, tenure_yrs := round(tenure_yrs/365.25,2)]
#average by store for hourly partners
ptmam[JOB_ID==50000362|JOB_ID==50000358, list(hrly_sbux_tenure_yrs = round(mean(tenure_yrs,na.rm=T),2))]
ptmam[JOB_ID==50000117, list(sm_sbux_tenure_yrs = round(mean(tenure_yrs,na.rm=T),2))]

#calculate tenure; library(lubridate)
ptus[, hiredt := as_date(mdy_hm(MOST_RECENT_HIRE_DT))]
ptus[, today := as_date(Sys.Date())]
ptus[, tenure := today-hiredt]
ptus[, tenure_yrs := as.numeric(tenure, units="days")]
ptus[, tenure_yrs := round(tenure_yrs/365.25,2)]
#average by store for hourly partners
ptus[JOB_ID==50000362|JOB_ID==50000358, list(hrly_sbux_tenure_yrs = round(mean(tenure_yrs,na.rm=T),2))]
ptus[JOB_ID==50000117, list(sm_sbux_tenure_yrs = round(mean(tenure_yrs,na.rm=T),2))]


