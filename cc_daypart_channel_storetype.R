##R code for CC scores by daypart (5 categories), channel (MOP, OTW, CAFE) and store type (CAFE, DT).
##For Kelly Minnaar request 1/18/2018 (Monthly Finance Deck).

#load libraries
library(data.table)

#emailed bullet #1
##by storetype for PM (daypart==3), late PM (daypart==4), and Evening (daypart==5)
#load data
fincc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc_daypart_channel_storetype.csv")

#aggregate
temp <- fincc[, list(
  cc_score = sum(TOTAL_TB,na.rm=T)/sum(TOTAL_RSPNS,na.rm=T)),
  by=c("FSCL_YR_NUM","FSCL_QTR_IN_YR_NUM","DAY_PART","DRIVE_THRU_IND")]
temp <- setorder(temp,DRIVE_THRU_IND,DAY_PART,FSCL_YR_NUM,FSCL_QTR_IN_YR_NUM)
setcolorder(temp,c("DRIVE_THRU_IND","DAY_PART","FSCL_QTR_IN_YR_NUM","FSCL_YR_NUM","cc_score"))
temp[, FSCL_QTR_IN_YR_NUM := NULL]
write.xlsx(temp,"O:/CoOp/CoOp194_PROReportng&OM/Julie/cc_daypart_storetype.xlsx")


#emailed bullet #2
##by storetype for PM (daypart==3), late PM (daypart==4), and Evening (daypart==5)
#load data
fincc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc_daypart_channel_storetype.csv")

#keep only DT stores
fincc <- fincc[DRIVE_THRU_IND==1]
#keep only cafe and OTW purchases
fincc <- fincc[ORD_MTHD_CD=='CAFE'|ORD_MTHD_CD=='OTW']

#aggregate
temp <- fincc[, list(
  TOTAL_TB = sum(TOTAL_TB, na.rm=T),
  TOTAL_RSPNS = sum(TOTAL_RSPNS, na.rm=T),
  cc_score = sum(TOTAL_TB,na.rm=T)/sum(TOTAL_RSPNS,na.rm=T)),
  by=c("FSCL_YR_NUM","FSCL_QTR_IN_YR_NUM","DAY_PART","ORD_MTHD_CD")]
temp <- setorder(temp,ORD_MTHD_CD,DAY_PART,FSCL_YR_NUM,FSCL_QTR_IN_YR_NUM)
setcolorder(temp,c("ORD_MTHD_CD","DAY_PART","FSCL_QTR_IN_YR_NUM","FSCL_YR_NUM","cc_score"))
temp[, FSCL_QTR_IN_YR_NUM := NULL]
write.xlsx(temp,"O:/CoOp/CoOp194_PROReportng&OM/Julie/cc_daypart_channel_DTstores.xlsx")



#emailed bullet #1
##by storetype for LATE PM AND EVENING COMBINED
#load data
fincc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc_daypart_channel_storetype.csv")
#create new indicator combining late pm and evening
fincc[DAY_PART==4|DAY_PART==5, pmeve := 1]
fincc <- fincc[pmeve==1]

#aggregate
temp <- fincc[, list(
  cc_score = sum(TOTAL_TB,na.rm=T)/sum(TOTAL_RSPNS,na.rm=T)),
  by=c("FSCL_YR_NUM","FSCL_QTR_IN_YR_NUM","pmeve","DRIVE_THRU_IND")]
temp <- setorder(temp,DRIVE_THRU_IND,pmeve,FSCL_YR_NUM,FSCL_QTR_IN_YR_NUM)
setcolorder(temp,c("DRIVE_THRU_IND","pmeve","FSCL_QTR_IN_YR_NUM","FSCL_YR_NUM","cc_score"))
temp[, FSCL_QTR_IN_YR_NUM := NULL]
write.xlsx(temp,"O:/CoOp/CoOp194_PROReportng&OM/Julie/cc_daypart_storetype_pmeve.xlsx")


#emailed bullet #2
##by storetype for PM (daypart==3), late PM (daypart==4), and Evening (daypart==5)
#load data
fincc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc_daypart_channel_storetype.csv")
#create new indicator combining late pm and evening
fincc[DAY_PART==4|DAY_PART==5, pmeve := 1]
fincc <- fincc[pmeve==1]
#keep only DT stores
fincc <- fincc[DRIVE_THRU_IND==1]
#keep only cafe and OTW purchases
fincc <- fincc[ORD_MTHD_CD=='CAFE'|ORD_MTHD_CD=='OTW']

#aggregate
temp <- fincc[, list(
  cc_score = sum(TOTAL_TB,na.rm=T)/sum(TOTAL_RSPNS,na.rm=T)),
  by=c("FSCL_YR_NUM","FSCL_QTR_IN_YR_NUM","pmeve","ORD_MTHD_CD")]
temp <- setorder(temp,ORD_MTHD_CD,pmeve,FSCL_YR_NUM,FSCL_QTR_IN_YR_NUM)
setcolorder(temp,c("ORD_MTHD_CD","pmeve","FSCL_QTR_IN_YR_NUM","FSCL_YR_NUM","cc_score"))
temp[, FSCL_QTR_IN_YR_NUM := NULL]
write.xlsx(temp,"O:/CoOp/CoOp194_PROReportng&OM/Julie/cc_daypart_channel_DTstores_pmeve.xlsx")



#final look: all stores, all transactions, by dayparts
#load data
fincc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc_daypart_channel_storetype.csv")

#aggregate
temp <- fincc[, list(
  cc_score = sum(TOTAL_TB,na.rm=T)/sum(TOTAL_RSPNS,na.rm=T)),
  by=c("FSCL_YR_NUM","FSCL_QTR_IN_YR_NUM","DAY_PART")]
temp <- setorder(temp,FSCL_YR_NUM,FSCL_QTR_IN_YR_NUM,DAY_PART)
setcolorder(temp,c("DAY_PART","FSCL_QTR_IN_YR_NUM","FSCL_YR_NUM","cc_score"))
temp[, FSCL_QTR_IN_YR_NUM := NULL]
write.xlsx(temp,"O:/CoOp/CoOp194_PROReportng&OM/Julie/cc_daypart_all.xlsx")


#final look: all stores, all transactions, for PM-Evening
#load data
fincc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc_daypart_channel_storetype.csv")
#create new indicator combining late pm and evening
fincc[DAY_PART==4|DAY_PART==5, pmeve := 1]
fincc <- fincc[pmeve==1]
#aggregate
temp <- fincc[, list(
  cc_score = sum(TOTAL_TB,na.rm=T)/sum(TOTAL_RSPNS,na.rm=T)),
  by=c("FSCL_YR_NUM","FSCL_QTR_IN_YR_NUM","pmeve")]
temp <- setorder(temp,FSCL_YR_NUM,FSCL_QTR_IN_YR_NUM,pmeve)
setcolorder(temp,c("pmeve","FSCL_QTR_IN_YR_NUM","FSCL_YR_NUM","cc_score"))
temp[, FSCL_QTR_IN_YR_NUM := NULL]
write.xlsx(temp,"O:/CoOp/CoOp194_PROReportng&OM/Julie/cc_daypart_all_pmeve.xlsx")





