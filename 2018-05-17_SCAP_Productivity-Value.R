#managing data for SCAP

#load libraries
library(data.table)
library(tidyr)
library(dplyr)
library(lubridate)

#scap status 4/1/18 (first day of Q3)
#CC data rolling 3 month (Q2)

#set folder directory
data_dir <- "O:/CoOp/CoOp194_PROReportng&OM/Julie"
scap <- fread(paste0(data_dir,"/PartnerFile20180401.csv"))
setnames(scap,"PARTNERID","PRTNR_NUM")
store <- fread(paste0(data_dir,"/scap_partner_stores20180401.csv"))
store[, MOSTREC := NULL]
cc <- fread(paste0(data_dir,"/scap_cc_q2fy18.csv"))
cc[, c("FSCL_YR_NUM","FSCL_QTR_IN_YR_NUM") := NULL]
tsd <- fread(paste0(data_dir,"/scap_tsd_q2fy18.csv"))
tsd[, c("FSCL_YR_NUM","FSCL_QTR_IN_YR_NUM") := NULL]
setnames(tsd,"STORE_NUMBER","STORE_NUM")

#restrict
scap <- scap[SCAPSTATUS=="Active-Participant"|SCAPSTATUS=="Graduated"]

#reduce to only partner id and date
scap <- scap[, .(PRTNR_NUM)]
#order by ptf
scap <- setorder(scap)

#create dataset of binary indicators where partners were active participants
scap[, scap := 1]

#merge
dt1 <- Reduce(function(x, y) {merge(x, y, by=c("PRTNR_NUM"), 
                                     all = TRUE)}, list(scap,store))
storelist <- unique(dt1[,STORE_NUM])
dt <- Reduce(function(x, y) {merge(x, y, by=c("STORE_NUM"), 
                                   all = TRUE)}, list(cc,tsd))
dt[STORE_NUM %in% storelist, scap := 1]

#make 0's for non-scap stores
dt[is.na(dt[,scap]), scap := 0]

#cc
dt <- dt[, list(Nstores = .N,
                Q2_2_RESPONSE_TOTAL=sum(Q2_2_RESPONSE_TOTAL,na.rm=T),
                Q2_2_TB_CNT=sum(Q2_2_TB_CNT,na.rm=T),
                CustTrans=sum(CustTrans,na.rm=T),
                day_count=sum(day_count,na.rm=T)),
         by=c("scap")]
dt[, cc_score := round(Q2_2_TB_CNT/Q2_2_RESPONSE_TOTAL,3)]
dt[, tsd := round(CustTrans/day_count,1)]


