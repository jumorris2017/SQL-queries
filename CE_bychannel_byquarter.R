##CE by Channel
##1/4/17

#load libraries
library(data.table)
library(ggplot2)
library(xlsx)

#slide #1

#load data
ch <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CE_bychannel_byquarter.csv")

#calculate TB score
ch[, tbscore := round((TOTAL_TB/TOTAL_RSPNS)*100,0)]

#reshape from long to wide to get year delta
ch <- dcast.data.table(ch, QSTN_ID + ORD_MTHD_CD ~ FSCL_YR_NUM, value.var="tbscore")
#setnames
setnames(ch,c("2017","2018"),c("tbFY17Q4","tbFY18Q1"))

#calculate deltas
ch[, tbdelta := round((tbFY18Q1-tbFY17Q4),0)]

#write to .xlsx
write.xlsx(ch,"O:/CoOp/CoOp194_PROReportng&OM/Julie/MOP_Slide1.xlsx")

###

# #Speed perceptions for Scott Peppel 1/8/17
# #load data
# sp <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/Speed_bydaypart_FY16Q4-FY18Q1.csv")
# #aggregate by daypart
# sp <- sp[, list(TOTAL_TB = sum(TOTAL_TB,na.rm=T),
#                 TOTAL_RSPNS = sum(TOTAL_RSPNS,na.rm=T)), 
#          by=c("DAY_PART","FSCL_YR_NUM","FSCL_QTR_IN_YR_NUM")]
# #get total day
# #aggregate by daypart
# sp2 <- sp[, list(TOTAL_TB = sum(TOTAL_TB,na.rm=T),
#                 TOTAL_RSPNS = sum(TOTAL_RSPNS,na.rm=T)), 
#          by=c("FSCL_YR_NUM","FSCL_QTR_IN_YR_NUM")]
# sp2[, DAY_PART := 5]
# #rbind together
# l = list(sp,sp2)
# sp <- rbindlist(l,use.names=T,fill=T)
# 
# #calculate TB score
# sp[, tbscore := round((TOTAL_TB/TOTAL_RSPNS)*100,0)]
# 
# #write to .xlsx
# setorder(sp,FSCL_YR_NUM,FSCL_QTR_IN_YR_NUM,DAY_PART)
# write.xlsx(sp,"O:/CoOp/CoOp194_PROReportng&OM/Julie/Speed_bydaypart_FY16Q4-FY18Q1.xlsx")
