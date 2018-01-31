##high performing stores by day part

#load libraries
library(data.table)

#load data.
hp <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/store_daypart_comp_q1fy18.csv") 
cc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc_score_bydaypart_by_store_q1fy18.csv") 
setnames(cc,"STORE_NUM","STORE_NUMBER")
pu <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/pulse_q1_byhour_bypartner_q1fy18.csv")
setnames(pu,"STORE_NUM","STORE_NUMBER")
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
pu[shift_start_hour>=6&shift_start_hour<=10, DAY_PART_NAME := 'AM']
pu[shift_start_hour>=13&shift_start_hour<=16, DAY_PART_NAME := 'PM']
#keep only am and pm
pu <- na.omit(pu,cols="DAY_PART_NAME")
#aggregate
pu <- pu[, lapply(.SD,sum,na.rm=T), by=c("STORE_NUMBER","DAY_PART_NAME"), .SDcols=c("Q1_RESP","Q1_TB")]
#swing wide
pu <- dcast.data.table(pu, STORE_NUMBER ~ DAY_PART_NAME, value.var=c("Q1_RESP","Q1_TB"))
#create TB scores
pu[, pulse_am := Q1_TB_AM/Q1_RESP_AM]
pu[, pulse_pm := Q1_TB_PM/Q1_RESP_PM]
pu <- na.omit(pu,cols="pulse_pm")

#keep only high performing stores for whom we have CC results
hp <- hp[STORE_NUMBER %in% unique(cc[,STORE_NUMBER])]
#merge
hp <- merge(hp,cc,by="STORE_NUMBER")
hp <- merge(hp,pu,by="STORE_NUMBER")

#keep afternoons
hp <- hp[DAY_PART_NAME=='PM']

#omit stores without LY info
hp <- na.omit(hp)

#calculate comp
hp[, comp := (SALES_AMT-SALES_LY_AMT)/SALES_LY_AMT]

#split by comp into 10 groups
hp <- hp %>% mutate(ntile = ntile(comp, 10))
setDT(hp)
hp <- na.omit(hp,cols="ntile")

#recode ntiles
hp[ntile==1, tiers3 := "1-top10"] #bottom 10%
hp[ntile>=2&ntile<=9, tiers3 := "2-mid80"] #middle 80%
hp[ntile==10, tiers3 := "3-bottom10"] #top 10%

#aggregate by tier
hp <- hp[, lapply(.SD,sum,na.rm=T), by="tiers3", .SDcols=c("TOTAL_TB_2","TOTAL_TB_4","TOTAL_RSPNS_2","TOTAL_RSPNS_4",
                                                           "Q1_RESP_AM","Q1_RESP_PM","Q1_TB_AM","Q1_TB_PM")]
hp[, cc_am := round(TOTAL_TB_2/TOTAL_RSPNS_2,3)]
hp[, cc_pm := round(TOTAL_TB_4/TOTAL_RSPNS_4,3)]
hp[, ccdelta := cc_pm - cc_am]
hp[, pulse_am := round(Q1_TB_AM/Q1_RESP_AM,3)]
hp[, pulse_pm := round(Q1_TB_PM/Q1_RESP_PM,3)]
#set order
hp <- setorder(hp,tiers3)
write.xlsx(hp,"O:/CoOp/CoOp194_PROReportng&OM/Julie/high_PM_comping_stores_q1fy18.xlsx")









