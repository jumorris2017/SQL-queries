##high performing stores by day part

#load libraries
library(data.table)

#load data.
hp <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/store_daypart_comp_q1fy18.csv") 
cc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc_score_bydaypart_by_store_q1fy18.csv") 
setnames(cc,"STORE_NUM","STORE_NUMBER")
pu1 <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/pulse_q1_byhour_bypartner_q1fy18.csv")
setnames(pu1,"STORE_NUM","STORE_NUMBER")
pu2d <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/pulse_q2d_byhour_bypartner_q1fy18.csv")
setnames(pu2d,"STORE_NUM","STORE_NUMBER")
##cc
#keep AM/PM
cc <- cc[DAY_PART==2|DAY_PART==4]
#keep only necessary variables
cc <- cc[,.(STORE_NUMBER,DAY_PART,TOTAL_TB,TOTAL_RSPNS,CC_SCORE)]
#swing wide
cc <- dcast.data.table(cc, STORE_NUMBER ~ DAY_PART, value.var=c("TOTAL_TB","TOTAL_RSPNS","CC_SCORE"))
#get delta
cc[, ccdelta := CC_SCORE_4 - CC_SCORE_2]
cc <- na.omit(cc)

#pulse
#aggregate by day part and store (just need AM and PM)
pu1[shift_start_hour>=6&shift_start_hour<=10, DAY_PART_NAME := 'AM']
pu1[shift_start_hour>=13&shift_start_hour<=16, DAY_PART_NAME := 'PM']
#keep only am and pm
pu1 <- na.omit(pu1,cols="DAY_PART_NAME")
#aggregate
pu1 <- pu1[, lapply(.SD,sum,na.rm=T), by=c("STORE_NUMBER","DAY_PART_NAME"), .SDcols=c("Q1_RESP","Q1_TB")]
#swing wide
pu1 <- dcast.data.table(pu1, STORE_NUMBER ~ DAY_PART_NAME, value.var=c("Q1_RESP","Q1_TB"))
#create TB scores
pu1[, pulse_q1_am := Q1_TB_AM/Q1_RESP_AM]
pu1[, pulse_q1_pm := Q1_TB_PM/Q1_RESP_PM]
pu1 <- na.omit(pu1,cols="pulse_q1_pm")

#pulse
#aggregate by day part and store (just need AM and PM)
pu2d[shift_start_hour>=6&shift_start_hour<=10, DAY_PART_NAME := 'AM']
pu2d[shift_start_hour>=13&shift_start_hour<=16, DAY_PART_NAME := 'PM']
#keep only am and pm
pu2d <- na.omit(pu2d,cols="DAY_PART_NAME")
#aggregate
pu2d <- pu2d[, lapply(.SD,sum,na.rm=T), by=c("STORE_NUMBER","DAY_PART_NAME"), .SDcols=c("Q2D_RESP","Q2D_TB")]
#swing wide
pu2d <- dcast.data.table(pu2d, STORE_NUMBER ~ DAY_PART_NAME, value.var=c("Q2D_RESP","Q2D_TB"))
#create TB scores
pu2d[, pulse_q2d_am := Q2D_TB_AM/Q2D_RESP_AM]
pu2d[, pulse_q2d_pm := Q2D_TB_PM/Q2D_RESP_PM]
pu2d <- na.omit(pu2d,cols="pulse_q2d_pm")

#keep only high performing stores for whom we have CC results
hp <- hp[STORE_NUMBER %in% unique(cc[,STORE_NUMBER])]
#merge
hp <- merge(hp,cc,by="STORE_NUMBER")
hp <- merge(hp,pu1,by="STORE_NUMBER")
hp <- merge(hp,pu2d,all.x=T,by="STORE_NUMBER")

#keep afternoons
hp <- hp[DAY_PART_NAME=='PM']

#omit stores without LY info
hp <- na.omit(hp,cols=c("SALES_AMT","SALES_LY_AMT",
                        "CC_SCORE_2","CC_SCORE_4",
                        "pulse_q1_am","pulse_q1_pm"))

#calculate comp
hp[, comp := (SALES_AMT-SALES_LY_AMT)/SALES_LY_AMT]

#split by comp into 10 groups
hp <- hp %>% mutate(ntile = ntile(comp, 10))
setDT(hp)
hp <- na.omit(hp,cols="ntile")

#recode ntiles
hp[ntile==1, tiers3 := "3-bottom10"] #bottom 10%
hp[ntile>=2&ntile<=9, tiers3 := "2-mid80"] #middle 80%
hp[ntile==10, tiers3 := "1-top10"] #top 10%

#aggregate by tier
#give fake store number for agging
hp[, storeN := 1]
hp <- hp[, lapply(.SD,sum,na.rm=T), by="tiers3", .SDcols=c("storeN","SALES_AMT","SALES_LY_AMT","TOTAL_TB_2","TOTAL_TB_4","TOTAL_RSPNS_2","TOTAL_RSPNS_4",
                                                           "Q1_RESP_AM","Q1_RESP_PM","Q1_TB_AM","Q1_TB_PM",
                                                           "Q2D_RESP_AM","Q2D_RESP_PM","Q2D_TB_AM","Q2D_TB_PM")]
hp[, comp := round((SALES_AMT-SALES_LY_AMT)/SALES_LY_AMT,3)]
hp[, cc_am := round(TOTAL_TB_2/TOTAL_RSPNS_2,3)]
hp[, cc_pm := round(TOTAL_TB_4/TOTAL_RSPNS_4,3)]
hp[, ccdelta := cc_pm - cc_am]
hp[, pulse_q1_am := round(Q1_TB_AM/Q1_RESP_AM,3)]
hp[, pulse_q1_pm := round(Q1_TB_PM/Q1_RESP_PM,3)]
hp[, pulseq1delta := pulse_q1_pm - pulse_q1_am]
hp[, pulse_q2d_am := round(Q2D_TB_AM/Q2D_RESP_AM,3)]
hp[, pulse_q2d_pm := round(Q2D_TB_PM/Q2D_RESP_PM,3)]
hp[, pulseq2ddelta := pulse_q2d_pm - pulse_q2d_am]

#set order
hp <- setorder(hp,tiers3)
write.xlsx(hp,"O:/CoOp/CoOp194_PROReportng&OM/Julie/high_PM_comping_stores_q1fy18.xlsx")









