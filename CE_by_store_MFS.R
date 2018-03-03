##Links CE and Pulse data to MFS stores based on converstion dates
##Some stores converted before our CE survey began
##CE data should be restricted to post-August 2015 for accuracy
##Want 4 months pre- and 4 months post-
##So, earliest conversion dates we can take are Dec 2015

#load libraries
library(data.table)
library(ggplot2)
library(lubridate)
library(xlsx)

#load data
#stores and converstion dates
mfsst <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/MFS_Roster_2_12_2018.csv")
#ce scores
mfsce <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CE_by_store_MFS_2014-2018.csv")
usce <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CE_by_store_US_2014-2018.csv")
#pulse scores
mfspulse <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/Pulse_by_store_MFS_2017-2018.csv")
uspulse <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/Pulse_by_store_US_2017-2018.csv")

#create 4-month pre- and post- bands
#stores
mfsst[, CONVDATE := mdy(CONVDATE)]
mfsst[, convpre := CONVDATE %m-% months(4)]
mfsst[, convpost := CONVDATE %m+% months(4)]
mfsst[, convprefull := paste0(year(convpre),".",str_pad(month(convpre), 2, pad = "0"))]
mfsst[, CONVDATEfull := paste0(year(CONVDATE),".",str_pad(month(CONVDATE), 2, pad = "0"))]
mfsst[, convpostfull := paste0(year(convpost),".",str_pad(month(convpost), 2, pad = "0"))]
mfsst[, c("CONVDATE","convpre","convpost") := NULL]

#paste months and years together to get get CE survey dates
#CE scores
mfsce[, surveydatefull := paste0(CAL_YR_NUM,".",str_pad(CAL_MNTH_IN_YR_NUM, 2, pad = "0"))]
# usce[, surveydatefull := paste0(CAL_YR_NUM,".",str_pad(CAL_MNTH_IN_YR_NUM, 2, pad = "0"))]
#Pulse scores
mfspulse[, surveydatefull := paste0(CAL_YR_NUM,".",str_pad(CAL_MNTH_IN_YR_NUM, 2, pad = "0"))]
# uspulse[, surveydatefull := paste0(CAL_YR_NUM,".",str_pad(CAL_MNTH_IN_YR_NUM, 2, pad = "0"))]
#rename us scores
colnames(usce)[3:ncol(usce)] <- paste0("US",colnames(usce)[3:ncol(usce)])
colnames(uspulse)[1:3] <- paste0("US",colnames(uspulse)[1:3])
#left join to pull in US ce scores, and tie to each msf store
mfsce <- left_join(mfsce,usce,by=c("CAL_YR_NUM","CAL_MNTH_IN_YR_NUM"))
setDT(mfsce)
mfspulse <- left_join(mfspulse,uspulse,by=c("CAL_YR_NUM","CAL_MNTH_IN_YR_NUM"))
setDT(mfspulse)

##CUSTOMER EXPERIENCE

#merge store numbers into ce data
temp <- left_join(mfsce,mfsst,by="STORE_NUM")
setDT(temp)

#agg CE scores for pre- and post- periods
temp[(convprefull<=surveydatefull)&(surveydatefull<=convpostfull), withinrange_full := 1]
temp[(convprefull<=surveydatefull)&(surveydatefull<CONVDATEfull), withinrange_pre := 1]
temp[(CONVDATEfull<=surveydatefull)&(surveydatefull<=convpostfull), withinrange_post := 1]

#drop rows not in full range
temp <- temp[withinrange_full==1]
#agg responses by pre-and post-period
temp <- temp[, lapply(.SD,sum,na.rm=T), .SDcols=c(grep("Q",colnames(temp),value=T)),
     by=c("withinrange_pre","withinrange_post")]
#calculate TB scores - MFS stores
temp[, cc_MFS_score := Q2_2_TB_CNT/Q2_2_RESPONSE_TOTAL]
temp[, q2_1_MFS_scr := Q2_1_TB_CNT/Q2_1_RESPONSE_TOTAL]
temp[, q2_3_MFS_scr := Q2_3_TB_CNT/Q2_3_RESPONSE_TOTAL]
temp[, q2_4_MFS_scr := Q2_4_TB_CNT/Q2_4_RESPONSE_TOTAL]
temp[, q2_5_MFS_scr := Q2_5_TB_CNT/Q2_5_RESPONSE_TOTAL]
temp[, q2_6_MFS_scr := Q2_6_TB_CNT/Q2_6_RESPONSE_TOTAL]
temp[, q2_7_MFS_scr := Q2_7_TB_CNT/Q2_7_RESPONSE_TOTAL]
temp[, so_MFS_score := rowMeans(subset(temp, select = c(grep("_MFS_scr",colnames(temp),value=T))), na.rm = TRUE)]
#calculate TB scores - US scores
temp[, cc_US_score := USQ2_2_TB_CNT/USQ2_2_RESPONSE_TOTAL]
temp[, q2_1_US_scr := USQ2_1_TB_CNT/USQ2_1_RESPONSE_TOTAL]
temp[, q2_3_US_scr := USQ2_3_TB_CNT/USQ2_3_RESPONSE_TOTAL]
temp[, q2_4_US_scr := USQ2_4_TB_CNT/USQ2_4_RESPONSE_TOTAL]
temp[, q2_5_US_scr := USQ2_5_TB_CNT/USQ2_5_RESPONSE_TOTAL]
temp[, q2_6_US_scr := USQ2_6_TB_CNT/USQ2_6_RESPONSE_TOTAL]
temp[, q2_7_US_scr := USQ2_7_TB_CNT/USQ2_7_RESPONSE_TOTAL]
temp[, so_US_score := rowMeans(subset(temp, select = c(grep("_US_scr",colnames(temp),value=T))), na.rm = TRUE)]

#rename pre- and post- variable
temp[withinrange_pre==1, post_period := 0];temp[withinrange_post==1, post_period := 1]

# #t.tests (re-run and omit agg code)
# t.test(temp[post_period==0,cc_MFS_score],temp[post_period==1,cc_MFS_score])
# t.test(temp[post_period==0,so_MFS_score],temp[post_period==1,so_MFS_score])

#subset variables
temp <- temp[, .(post_period,cc_MFS_score,so_MFS_score,cc_US_score,so_US_score)]
temp <- temp[, lapply(.SD,function(x) round(x,4)*100), by="post_period",
     .SDcols=grep("score",colnames(temp),value=T)]
temp[, cc_MFStoUS_delta := cc_MFS_score-cc_US_score]
temp[, so_MFStoUS_delta := so_MFS_score-so_US_score]
tempdelta <- temp[post_period==1] - temp[post_period==0]
tempdelta[, post_period := "delta"]
temp <- rbind(temp,tempdelta)
#write to .csv
write.xlsx(temp,file="O:/CoOp/CoOp194_PROReportng&OM/Julie/MFS_CE_pre-post.xlsx")


###PULSE
##TB & AVG

#merge store numbers into pulse data
temp <- left_join(mfspulse,mfsst,by="STORE_NUM")
setDT(temp)

#agg CE scores for pre- and post- periods
temp[(convprefull<=surveydatefull)&(surveydatefull<=convpostfull), withinrange_full := 1]
temp[(convprefull<=surveydatefull)&(surveydatefull<CONVDATEfull), withinrange_pre := 1]
temp[(CONVDATEfull<=surveydatefull)&(surveydatefull<=convpostfull), withinrange_post := 1]

#drop rows not in full range
temp <- temp[withinrange_full==1]

#back out of averages to get sum, so can re-agg average
temp[, Q1_SUM := Q1_RESP*Q1_AVG]
temp[, USQ1_SUM := USQ1_RESP*USQ1_AVG]

#agg responses by pre-and post-period
temp <- temp[, lapply(.SD,sum,na.rm=T), .SDcols=c(grep("Q",colnames(temp),value=T)),
             by=c("withinrange_pre","withinrange_post")]
#calculate TB scores - MFS stores
temp[, q1_MFS_score := Q1_TB/Q1_RESP]
#calculate TB scores - US scores
temp[, q1_US_score := USQ1_TB/USQ1_RESP]
#calculate average scores - MFS stores
temp[, q1_MFS_avg := Q1_SUM/Q1_RESP]
#calculate average scores - US scores
temp[, q1_US_avg := USQ1_SUM/USQ1_RESP]

#rename pre- and post- variable
temp[withinrange_pre==1, post_period := 0];temp[withinrange_post==1, post_period := 1]

# #t.tests (re-run and omit agg code)
# t.test(temp[post_period==0,q1_MFS_avg],temp[post_period==1,q1_MFS_avg])

#subset variables
temp <- temp[, .(post_period,q1_MFS_score,q1_US_score,q1_MFS_avg,q1_US_avg)]
temp[, c("q1_MFS_score","q1_US_score") := lapply(.SD,function(x) round(x,4)*100), .SDcols=c("q1_MFS_score","q1_US_score")]
temp[, c("q1_MFS_avg","q1_US_avg") := lapply(.SD,function(x) round(x,2)), .SDcols=c("q1_MFS_avg","q1_US_avg")]
temp[, q1_MFStoUS_delta_TB := q1_MFS_score-q1_US_score]
temp[, q1_MFStoUS_delta_avg := q1_MFS_avg-q1_US_avg]
tempdelta <- temp[post_period==1] - temp[post_period==0]
tempdelta[, post_period := "delta"]
temp <- rbind(temp,tempdelta)
#write to .csv
write.xlsx(temp,file="O:/CoOp/CoOp194_PROReportng&OM/Julie/MFS_Pulse_pre-post.xlsx")



#turnover

#load data (from Megan)
hc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Megan/StoreRisk/EverythingEverJan.csv")
setnames(hc,"StoreNum","STORE_NUM")
hc[, terms := rowSums(.SD,na.rm=T), .SDcols=c("barista.term","shift.term","sm.term")]

#reduce variables
hc <- hc[, .(STORE_NUM,FP,FY,TotHC,terms)]

#paste months and years together to get get CE survey dates
#headcount
hc[, surveydatefull := paste0(FY,".",str_pad(FP, 2, pad = "0"))]

#restrict to MFS stores
mfshc <- hc[STORE_NUM %in% mfsst[,STORE_NUM]]

##total us term rate
ushc <- hc[, .(TotHC,terms,surveydatefull)]
ushc <- ushc[, lapply(.SD,sum,na.rm=T), .SDcols=c("TotHC","terms"),
             by=c("surveydatefull")]
colnames(ushc)[2:ncol(ushc)] <- paste0("US",colnames(ushc)[2:ncol(ushc)])

#left join to pull in US ce scores, and tie to each msf store
mfshc <- left_join(mfshc,ushc,by=c("surveydatefull"))
setDT(mfshc)

#merge store numbers into hc data
temp <- left_join(mfshc,mfsst,by="STORE_NUM")
setDT(temp)

#agg CE scores for pre- and post- periods
temp[(convprefull<=surveydatefull)&(surveydatefull<=convpostfull), withinrange_full := 1]
temp[(convprefull<=surveydatefull)&(surveydatefull<CONVDATEfull), withinrange_pre := 1]
temp[(CONVDATEfull<=surveydatefull)&(surveydatefull<=convpostfull), withinrange_post := 1]

#drop rows not in full range
temp <- temp[withinrange_full==1]

#agg responses by pre-and post-period
temp <- temp[, lapply(.SD,sum,na.rm=T), .SDcols=c("TotHC","terms","USTotHC","USterms"),
             by=c("withinrange_pre","withinrange_post")]
#calculate term rate - MFS stores
temp[, MFStermrate := terms/TotHC]
#calculate term rate - US stores
temp[, UStermrate := USterms/USTotHC]

#rename pre- and post- variable
temp[withinrange_pre==1, post_period := 0];temp[withinrange_post==1, post_period := 1]

# #t.tests (re-run and omit agg code)
# t.test(temp[post_period==0,MFStermrate],temp[post_period==1,MFStermrate])

#subset variables
temp <- temp[, .(post_period,MFStermrate,UStermrate)]
temp[, c("MFStermrate","UStermrate") := lapply(.SD,function(x) round(x,4)*100), .SDcols=c("MFStermrate","UStermrate")]
temp[, MFStoUS_termrate_delta := MFStermrate-UStermrate]
tempdelta <- temp[post_period==1] - temp[post_period==0]
tempdelta[, post_period := "delta"]
temp <- rbind(temp,tempdelta)


#Q1FY18 snapshot of turnover

#restrict to MFS stores for Q1FY18
mfshc <- hc[STORE_NUM %in% mfsst[,STORE_NUM]]
mfshc <- mfshc[FY==2018&(FP>=1&FP<=3)]

##total us term rate
ushc <- hc[, .(TotHC,terms,FP,FY)]
ushc <- ushc[FY==2018&(FP>=1&FP<=3)]
ushc <- ushc[, lapply(.SD,sum,na.rm=T), .SDcols=c("TotHC","terms"),
             by=c("FP","FY")]
colnames(ushc)[3:ncol(ushc)] <- paste0("US",colnames(ushc)[3:ncol(ushc)])

#left join to pull in US ce scores, and tie to each msf store
mfshc <- left_join(mfshc,ushc,by=c("FP","FY"))
setDT(mfshc)

#merge store numbers into hc data
temp <- left_join(mfshc,mfsst,by="STORE_NUM")
setDT(temp)

#agg responses for the quarter
temp <- temp[, lapply(.SD,sum,na.rm=T), .SDcols=c("TotHC","terms","USTotHC","USterms"),
             by=c("FY")]
#calculate term rate - MFS stores
temp[, MFStermrate := terms/TotHC]
#calculate term rate - US stores
temp[, UStermrate := USterms/USTotHC]

#subset variables
temp <- temp[, .( FY,MFStermrate,UStermrate)]
temp[, c("MFStermrate","UStermrate") := lapply(.SD,function(x) round(x,4)*100), .SDcols=c("MFStermrate","UStermrate")]
temp[, MFStoUS_termrate_delta := MFStermrate-UStermrate]

