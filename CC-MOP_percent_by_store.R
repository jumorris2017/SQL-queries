##CC by MOP percentage##

#load libraries
library(data.table)
library(xlsx)
library(ggplot2)
library(ggthemes)

#CC score for NON-peak MOP transactions, locking stores into FY18Q1 mop adoptions rates
#load data
mop <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/MOP_percent_by_store.csv")
mopcc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC_by_store_ALL_FY18Q1-FY17Q1NONPEAK.csv")
mopcc[, STORE_NUM := as.numeric(STORE_NUM)]

#calculate MOP percentage
mop[, moppct := PEAK_MOP_TRANS_CNT/PEAK_TTL_TRANS_CNT]
mop <- na.omit(mop,cols="moppct")

#organize stores into core, high, and super high
#group by FY 18 Q1
mop17 <- mop[FSCL_YR_NUM==2018&FSCL_QTR_IN_YR_NUM==1]
mop17[moppct>=0&moppct<.10, mopgroup := "1 - core"] #core
mop17[moppct>=.10&moppct<.20, mopgroup := "2 - high"] #high
mop17[moppct>=.20, mopgroup := "3 - super high"] #super high
mop17 <- mop17[, c("STORE_NUM","mopgroup")]

#merge
mop <- left_join(mop,mop17,by="STORE_NUM")
setDT(mop)
mop <- na.omit(mop,cols="mopgroup")

#CC - MOP transactions only
mopcc <- mopcc[ORD_MTHD_CD=="MOP"]

#reshape from long to wide to get year delta
mopcc <- dcast.data.table(mopcc, DRIVE_THRU_IND + STORE_NUM + FSCL_YR_NUM + FSCL_QTR_IN_YR_NUM ~ QSTN_ID, value.var=c("TB_COUNT","RSPNS_COUNT"))
mopcc <- na.omit(mopcc,cols=c("TB_COUNT_Q2_2","RSPNS_COUNT_Q2_2","DRIVE_THRU_IND"))

#calculate TB score
mopcc[, tbcc := (TB_COUNT_Q2_2/RSPNS_COUNT_Q2_2)]

#merge
mop <- left_join(mop,mopcc,by=c("STORE_NUM","FSCL_YR_NUM","FSCL_QTR_IN_YR_NUM"))
setDT(mop)

#drop FY18Q2
mop <- mop[(FSCL_YR_NUM==2018&FSCL_QTR_IN_YR_NUM==1)|FSCL_YR_NUM==2017]
mop <- mop[DRIVE_THRU_IND==1|DRIVE_THRU_IND==0]

#aggregate
temp <- mop[, list(
  TB_COUNT_Q2_2 = sum(TB_COUNT_Q2_2,na.rm=T),
  RSPNS_COUNT_Q2_2 = sum(RSPNS_COUNT_Q2_2,na.rm=T),
  n = .N,
  tbcc = sum(TB_COUNT_Q2_2,na.rm=T)/sum(RSPNS_COUNT_Q2_2,na.rm=T)),
  by=c("mopgroup","FSCL_YR_NUM","FSCL_QTR_IN_YR_NUM","DRIVE_THRU_IND")]
temp <- setorder(temp,DRIVE_THRU_IND,mopgroup,FSCL_YR_NUM,FSCL_QTR_IN_YR_NUM)
write.xlsx(temp,"O:/CoOp/CoOp194_PROReportng&OM/Julie/CCscore_byFY18Q1_MOPgroup-NONpeak.xlsx")


#CC score and average MOP trans for NON-peak MOP transactions, ignoring mop adoptions groups
#load data
mop <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/MOP_percent_by_store.csv")
mopcc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC_by_store_ALL_FY18Q1-FY17Q1NONPEAK.csv")
mopcc[, STORE_NUM := as.numeric(STORE_NUM)]

#calculate MOP percentage
mop[, moppct := PEAK_MOP_TRANS_CNT/PEAK_TTL_TRANS_CNT]
mop <- na.omit(mop,cols="moppct")

#keep only FY 18 Q1
mop <- left_join(mop,mop17,by="STORE_NUM")
setDT(mop)
mop <- na.omit(mop,cols="moppct")

#CC - MOP transactions only
mopcc <- mopcc[ORD_MTHD_CD=="MOP"]

#reshape from long to wide to get year delta
mopcc <- dcast.data.table(mopcc, DRIVE_THRU_IND + STORE_NUM + FSCL_YR_NUM + FSCL_QTR_IN_YR_NUM ~ QSTN_ID, value.var=c("TB_COUNT","RSPNS_COUNT","PEAK_TB_COUNT","PEAK_RSPNS_COUNT"))
mopcc <- na.omit(mopcc,cols=c("TB_COUNT_Q2_2","RSPNS_COUNT_Q2_2","DRIVE_THRU_IND"))

#calculate TB score
mopcc[, peaktbcc := (PEAK_TB_COUNT_Q2_2/PEAK_RSPNS_COUNT_Q2_2)]
mopcc[, tbcc := (TB_COUNT_Q2_2/RSPNS_COUNT_Q2_2)]

#merge
mop <- left_join(mop,mopcc,by=c("STORE_NUM","FSCL_YR_NUM","FSCL_QTR_IN_YR_NUM"))
setDT(mop)

#drop FY18Q2
mop <- mop[(FSCL_YR_NUM==2018&FSCL_QTR_IN_YR_NUM==1)|FSCL_YR_NUM==2017]
mop <- mop[DRIVE_THRU_IND==1|DRIVE_THRU_IND==0]

#aggregate
temp <- mop[, list(
  PEAK_TB_COUNT_Q2_2 = sum(PEAK_TB_COUNT_Q2_2,na.rm=T),
  PEAK_RSPNS_COUNT_Q2_2 = sum(PEAK_RSPNS_COUNT_Q2_2,na.rm=T),
  TB_COUNT_Q2_2 = sum(TB_COUNT_Q2_2,na.rm=T),
  RSPNS_COUNT_Q2_2 = sum(RSPNS_COUNT_Q2_2,na.rm=T),
  n = .N,
  peaktbcc = sum(PEAK_TB_COUNT_Q2_2,na.rm=T)/sum(PEAK_RSPNS_COUNT_Q2_2,na.rm=T),
  tbcc = sum(TB_COUNT_Q2_2,na.rm=T)/sum(RSPNS_COUNT_Q2_2,na.rm=T),
  avgmoppct = mean(moppct,na.rm=T)),
  by=c("FSCL_YR_NUM","FSCL_QTR_IN_YR_NUM","DRIVE_THRU_IND")]
temp <- setorder(temp,DRIVE_THRU_IND,FSCL_YR_NUM,FSCL_QTR_IN_YR_NUM)
write.xlsx(temp,"O:/CoOp/CoOp194_PROReportng&OM/Julie/CCscore-MOP-NONpeak.xlsx")


##by channel: CAFE, MOP, OTW
#load data
mopcc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC_by_channel_peak-nonpeak.csv")
mopcc[, STORE_NUM := as.numeric(STORE_NUM)]

#reshape from long to wide to get year delta
mopcc <- dcast.data.table(mopcc, ORD_MTHD_CD + STORE_NUM + FSCL_YR_NUM + FSCL_QTR_IN_YR_NUM ~ QSTN_ID, value.var=c("TB_COUNT","RSPNS_COUNT","PEAK_TB_COUNT","PEAK_RSPNS_COUNT"))
mopcc <- na.omit(mopcc,cols=c("TB_COUNT_Q2_2","RSPNS_COUNT_Q2_2","ORD_MTHD_CD"))

#calculate TB score
mopcc[, peaktbcc := (PEAK_TB_COUNT_Q2_2/PEAK_RSPNS_COUNT_Q2_2)]
mopcc[, tbcc := (TB_COUNT_Q2_2/RSPNS_COUNT_Q2_2)]

#aggregate
temp <- mopcc[, list(
  PEAK_TB_COUNT_Q2_2 = sum(PEAK_TB_COUNT_Q2_2,na.rm=T),
  PEAK_RSPNS_COUNT_Q2_2 = sum(PEAK_RSPNS_COUNT_Q2_2,na.rm=T),
  TB_COUNT_Q2_2 = sum(TB_COUNT_Q2_2,na.rm=T),
  RSPNS_COUNT_Q2_2 = sum(RSPNS_COUNT_Q2_2,na.rm=T),
  n = .N,
  peaktbcc = sum(PEAK_TB_COUNT_Q2_2,na.rm=T)/sum(PEAK_RSPNS_COUNT_Q2_2,na.rm=T),
  tbcc = sum(TB_COUNT_Q2_2,na.rm=T)/sum(RSPNS_COUNT_Q2_2,na.rm=T)),
  by=c("FSCL_YR_NUM","FSCL_QTR_IN_YR_NUM","ORD_MTHD_CD")]
temp <- setorder(temp,ORD_MTHD_CD,FSCL_YR_NUM,FSCL_QTR_IN_YR_NUM)
write.xlsx(temp,"O:/CoOp/CoOp194_PROReportng&OM/Julie/CCscore-bychannel_peak-nonpeak.xlsx")


##by store type: DT VS CAFE
#load data
mopcc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC_by_channel_peak-nonpeak.csv")
mopcc[, STORE_NUM := as.numeric(STORE_NUM)]
#MOP TRANS ONLY
mopcc <- mopcc[ORD_MTHD_CD=='MOP']
#aggregate
temp <- mopcc[, list(
  PEAK_TB_COUNT = sum(PEAK_TB_COUNT,na.rm=T),
  PEAK_RSPNS_COUNT = sum(PEAK_RSPNS_COUNT,na.rm=T),
  TB_COUNT = sum(TB_COUNT,na.rm=T),
  RSPNS_COUNT = sum(RSPNS_COUNT,na.rm=T),
  n = .N,
  peaktbcc = sum(PEAK_TB_COUNT,na.rm=T)/sum(PEAK_RSPNS_COUNT,na.rm=T),
  tbcc = sum(TB_COUNT,na.rm=T)/sum(RSPNS_COUNT,na.rm=T)),
  by=c("FSCL_YR_NUM","FSCL_QTR_IN_YR_NUM","DRIVE_THRU_IND")]
temp <- setorder(temp,DRIVE_THRU_IND,FSCL_YR_NUM,FSCL_QTR_IN_YR_NUM)
write.xlsx(temp,"O:/CoOp/CoOp194_PROReportng&OM/Julie/CCscore-bychannel_cafe-dt.xlsx")


#by peak v non-peak
#load data
mopcc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC_by_channel_peak-nonpeak_dt-cafe.csv")
mopcc[, STORE_NUM := as.numeric(STORE_NUM)]
#MOP TRANS ONLY
mopcc <- mopcc[ORD_MTHD_CD=='MOP']
#confirm non-peak is accurate
mopcc[, nonpeaktb := TB_COUNT-PEAK_TB_COUNT]
mopcc[, nonpeaktotal := RSPNS_COUNT-PEAK_RSPNS_COUNT]
#aggregate
temp <- mopcc[, list(
  PEAK_TB_COUNT = sum(PEAK_TB_COUNT,na.rm=T),
  PEAK_RSPNS_COUNT = sum(PEAK_RSPNS_COUNT,na.rm=T),
  NON_PEAK_TB_COUNT = sum(nonpeaktb,na.rm=T),
  NON_PEAK_RSPNS_COUNT = sum(nonpeaktotal,na.rm=T),
  n = .N,
  peaktbcc = sum(PEAK_TB_COUNT,na.rm=T)/sum(PEAK_RSPNS_COUNT,na.rm=T),
  nonpeaktbcc = sum(nonpeaktb,na.rm=T)/sum(nonpeaktotal,na.rm=T)),
  by=c("FSCL_YR_NUM","FSCL_QTR_IN_YR_NUM","ORD_MTHD_CD")]
temp <- setorder(temp,ORD_MTHD_CD,FSCL_YR_NUM,FSCL_QTR_IN_YR_NUM)
write.xlsx(temp,"O:/CoOp/CoOp194_PROReportng&OM/Julie/CCscore-peak-nonpeak.xlsx")


#slide 1 - cc/so scores
#load data
mopcc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc-so_formopstuff.csv")
#aggregate
temp <- mopcc[, list(
  TB_COUNT = sum(TB_COUNT,na.rm=T),
  RSPNS_COUNT = sum(RSPNS_COUNT,na.rm=T),
  n = .N,
  tbcc = sum(TB_COUNT,na.rm=T)/sum(RSPNS_COUNT,na.rm=T)),
  by=c("FSCL_YR_NUM","FSCL_QTR_IN_YR_NUM","QSTN_ID","ORD_MTHD_CD")]
temp <- setorder(temp,ORD_MTHD_CD,QSTN_ID,FSCL_YR_NUM,FSCL_QTR_IN_YR_NUM)
write.xlsx(temp,"O:/CoOp/CoOp194_PROReportng&OM/Julie/cc-so_scores_formopstuff.xlsx")






#SLIDES 5-6

#load data
mop <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/MOP_percent_by_store.csv")
mopcc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC_by_store_cafe_only_FY18Q1-FY17Q1.csv")
mopcc[, STORE_NUM := as.numeric(STORE_NUM)]

#calculate MOP percentage
mop[, moppct := PEAK_MOP_TRANS_CNT/PEAK_TTL_TRANS_CNT]
mop <- na.omit(mop,cols="moppct")

#organize stores into core, high, and super high
#group by FY 17 Q1
mop17 <- mop[FSCL_YR_NUM==2017&FSCL_QTR_IN_YR_NUM==1]
mop17[moppct>=0&moppct<.10, mopgroup := "1 - core"] #core
mop17[moppct>=.10&moppct<.20, mopgroup := "2 - high"] #high
mop17[moppct>=.20, mopgroup := "3 - super high"] #super high
mop17 <- mop17[, c("STORE_NUM","mopgroup")]

#keep only FY 18 Q1
#mop <- mop[FSCL_YR_NUM==2018&FSCL_QTR_IN_YR_NUM==1]
mop <- left_join(mop,mop17,by="STORE_NUM")
setDT(mop)
mop <- na.omit(mop,cols="mopgroup")

#CC - MOP transactions only
mopcc <- mopcc[ORD_MTHD_CD=="MOP"]

#reshape from long to wide to get year delta
mopcc <- dcast.data.table(mopcc, ORD_MTHD_CD + STORE_NUM + FSCL_YR_NUM + FSCL_QTR_IN_YR_NUM ~ QSTN_ID, value.var=c("PEAK_TB_COUNT","PEAK_RSPNS_COUNT"))

#calculate TB score
mopcc[, tbcc := (PEAK_TB_COUNT_Q2_2/PEAK_RSPNS_COUNT_Q2_2)]

#merge
mop <- left_join(mop,mopcc,by=c("STORE_NUM","FSCL_YR_NUM","FSCL_QTR_IN_YR_NUM"))
setDT(mop)

#aggregate
temp <- mop[, list(
                   PEAK_TB_COUNT = sum(PEAK_TB_COUNT,na.rm=T),
                   PEAK_RSPNS_COUNT = sum(PEAK_RSPNS_COUNT,na.rm=T),
                   n = .N,
                   tbcc = sum(PEAK_TB_COUNT_Q2_2,na.rm=T)/sum(PEAK_RSPNS_COUNT_Q2_2,na.rm=T)),
            by=c("mopgroup","FSCL_YR_NUM","FSCL_QTR_IN_YR_NUM")]
temp <- setorder(temp,FSCL_YR_NUM,FSCL_QTR_IN_YR_NUM,mopgroup)
#write file
write.xlsx(temp,"O:/CoOp/CoOp194_PROReportng&OM/Julie/MOP_Slide6_CCforMOPcustomers.xlsx")


#SLIDE 4

#load data
mop <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/MOP_percent_by_store.csv")
mopcc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC-SP_by_store_cafe_only_FY18Q1-FY17Q1.csv")
mopcc[, STORE_NUM := as.numeric(STORE_NUM)]

#remove PNW stores
pnw <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/pnw_storelist.csv")
pnwvec <- unique(pnw[,STORE_NUM])
setkey(mop, STORE_NUM)
mop <- mop[!pnw]
setkey(mopcc, STORE_NUM)
mopcc <- mopcc[!pnw]

#calculate MOP percentage
mop[, moppct := PEAK_MOP_TRANS_CNT/PEAK_TTL_TRANS_CNT]
mop <- na.omit(mop,cols="moppct")

#organize stores into core, high, and super high
#keep only FY 18 Q1
mop <- mop[FSCL_YR_NUM==2018&FSCL_QTR_IN_YR_NUM==1]
mop <- na.omit(mop,cols="moppct")

#MOP transactions only & speed only
mopcc <- mopcc[ORD_MTHD_CD=="MOP"&QSTN_ID=="Q2_1"]

#reshape from long to wide to get year delta
mopcc <- dcast.data.table(mopcc, STORE_NUM ~ FSCL_YR_NUM, value.var=c("PEAK_TB_COUNT","PEAK_RSPNS_COUNT"))

#split by mop into  20 groups
mop <- mop %>% mutate(ntile = ntile(moppct, 20))
mopcc <- left_join(mopcc, mop, by=c("STORE_NUM"))
setDT(mopcc)
mopcc <- na.omit(mopcc,cols="ntile")

#aggregate
temp <- mopcc[, list(n = .N,
                     mean_daily_peak_MOP_trans = mean(PEAK_MOP_TRANS_CNT,na.rm=T)/91,
                     tbsp_FY17Q1 = sum(PEAK_TB_COUNT_2017,na.rm=T)/sum(PEAK_RSPNS_COUNT_2017,na.rm=T),
                     tbsp_FY18Q1 = sum(PEAK_TB_COUNT_2018,na.rm=T)/sum(PEAK_RSPNS_COUNT_2018,na.rm=T)),
              by="ntile"]
temp <- setorder(temp,ntile)
#write file
write.xlsx(temp,"O:/CoOp/CoOp194_PROReportng&OM/Julie/MOP_Slide4.xlsx")


#SLIDE 2

#load data
mop <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/MOP_percent_by_store.csv")
mopcc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC-SP_by_store_cafe_only_FY18Q1-FY17Q4.csv")
mopcc[, STORE_NUM := as.numeric(STORE_NUM)]
mopcc15 <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC_by_store_cafe_only_FY15Q4.csv")
mopcc15[, STORE_NUM := as.numeric(STORE_NUM)]

#remove PNW stores
pnw <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/pnw_storelist.csv")
pnwvec <- unique(pnw[,STORE_NUM])
setkey(mop, STORE_NUM)
mop <- mop[!pnw]
setkey(mopcc, STORE_NUM)
mopcc <- mopcc[!pnw]
setkey(mopcc15, STORE_NUM)
mopcc15 <- mopcc15[!pnw]

#calculate MOP percentage
mop[, moppct := PEAK_MOP_TRANS_CNT/PEAK_TTL_TRANS_CNT]
mop <- na.omit(mop,cols="moppct")

#organize stores into core, high, and super high
#keep only FY 18 Q1
mop <- mop[FSCL_YR_NUM==2018&FSCL_QTR_IN_YR_NUM==1]
mop <- na.omit(mop,cols="moppct")

#MOP transactions only & CC only
mopcc <- mopcc[ORD_MTHD_CD=="CAFE"&QSTN_ID=="Q2_2"]

#reshape from long to wide to get year delta
mopcc <- dcast.data.table(mopcc, STORE_NUM ~ FSCL_YR_NUM, value.var=c("PEAK_TB_COUNT","PEAK_RSPNS_COUNT"))
mopcc15 <- dcast.data.table(mopcc15, STORE_NUM ~ FSCL_YR_NUM, value.var=c("PEAK_TB_COUNT","PEAK_RSPNS_COUNT"))

#split by mop into  20 groups
mop <- mop %>% mutate(ntile = ntile(moppct, 20))
mopcc <- left_join(mopcc, mop, by=c("STORE_NUM"))
setDT(mopcc)
mopcc <- na.omit(mopcc,cols="ntile")

#MERGE IN 2015 DATA
temp <- left_join(mopcc,mopcc15,by=c("STORE_NUM"))
setDT(temp)

#correlations
temp[, tbcc_FY15Q4 := PEAK_TB_COUNT_2015/PEAK_RSPNS_COUNT_2015]
temp[, tbcc_FY17Q4 := PEAK_TB_COUNT_2017/PEAK_RSPNS_COUNT_2017]
temp[, tbcc_FY18Q1 := PEAK_TB_COUNT_2018/PEAK_RSPNS_COUNT_2018]
temp3 <- na.omit(temp,cols=c("tbcc_FY15Q4","moppct"))
cor(temp3[,tbcc_FY15Q4],temp3[,moppct])
cor(temp3[,tbcc_FY17Q4],temp3[,moppct])
cor(temp3[,tbcc_FY18Q1],temp3[,moppct])

#aggregate
temp <- temp[, list(n = .N,
                     mean_daily_peak_MOP_trans = mean(PEAK_MOP_TRANS_CNT,na.rm=T)/91,
                     tbsp_FY15Q4 = sum(PEAK_TB_COUNT_2015,na.rm=T)/sum(PEAK_RSPNS_COUNT_2015,na.rm=T),
                     tbsp_FY17Q4 = sum(PEAK_TB_COUNT_2017,na.rm=T)/sum(PEAK_RSPNS_COUNT_2017,na.rm=T),
                     tbsp_FY18Q1 = sum(PEAK_TB_COUNT_2018,na.rm=T)/sum(PEAK_RSPNS_COUNT_2018,na.rm=T)),
             by="ntile"]
temp <- setorder(temp,ntile)
#write file
write.xlsx(temp,"O:/CoOp/CoOp194_PROReportng&OM/Julie/MOP_Slide2.xlsx")



##FY15 Q4 UPDATE
mop15 <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC-SP_by_store_cafe_onlyFY15Q4.csv")
mop15[, STORE_NUM := as.numeric(STORE_NUM)]
mop15 <- mop15[STORE_NUM %in% unique(mop17[,STORE_NUM])]
mop15 <- left_join(mop15,mop17,by="STORE_NUM")
setDT(mop15)
mop15 <- na.omit(mop15,cols="mopgroup")

#remove PNW stores
pnw <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/pnw_storelist.csv")
pnwvec <- unique(pnw[,STORE_NUM])
setkey(mop15, STORE_NUM)
mop15 <- mop15[!pnw]

#CC - NON-MOP transactions only
mop15 <- mop15[ORD_MTHD_CD=="CAFE"]

#calculate CC TB
mop15[, tb_score := PEAK_TB_COUNT/PEAK_RSPNS_COUNT]
mop15 <- na.omit(mop15,cols="tb_score")

#aggregate
temp <- mop15[, list(
  N = .N,
  tb_score = sum(PEAK_TB_COUNT,na.rm=T)/sum(PEAK_RSPNS_COUNT,na.rm=T)),
  by=c("mopgroup","QSTN_ID")]
temp <- setorder(temp,QSTN_ID,mopgroup)
#write file
write.xlsx(temp,file="O:/CoOp/CoOp194_PROReportng&OM/Julie/CC-SP-MOP_percent_PRE-LAUNCH.xlsx")


#SLIDE 6

#load data
mop <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/MOP_percent_by_store.csv")
mopcc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC_by_store_cafe_only_FY18Q1-FY17Q1.csv")
mopcc[, STORE_NUM := as.numeric(STORE_NUM)]

#calculate MOP percentage
mop[, moppct := PEAK_MOP_TRANS_CNT/PEAK_TTL_TRANS_CNT]
mop <- na.omit(mop,cols="moppct")

#organize stores into core, high, and super high
#group by FY 17 Q1
mop17 <- mop[FSCL_YR_NUM==2017|(FSCL_YR_NUM==2018&FSCL_QTR_IN_YR_NUM==1)]
mop17[moppct>=0&moppct<.10, mopgroup := "1 - core"] #core
mop17[moppct>=.10&moppct<.20, mopgroup := "2 - high"] #high
mop17[moppct>=.20, mopgroup := "3 - super high"] #super high
mop17 <- mop17[, c("STORE_NUM","FSCL_YR_NUM","FSCL_QTR_IN_YR_NUM","mopgroup")]

#merge
mop <- left_join(mop,mop17,by=c("STORE_NUM","FSCL_YR_NUM","FSCL_QTR_IN_YR_NUM"))
setDT(mop)
mop <- na.omit(mop,cols="mopgroup")

#CC - MOP transactions only
mopcc <- mopcc[ORD_MTHD_CD=="MOP"]

#reshape from long to wide to get year delta
mopcc <- dcast.data.table(mopcc, ORD_MTHD_CD + STORE_NUM + FSCL_YR_NUM + FSCL_QTR_IN_YR_NUM ~ QSTN_ID, value.var=c("PEAK_TB_COUNT","PEAK_RSPNS_COUNT"))

#calculate TB score
mopcc[, tbcc := (PEAK_TB_COUNT_Q2_2/PEAK_RSPNS_COUNT_Q2_2)]

#merge
mop <- merge(mopcc,mop,all=FALSE,by=c("STORE_NUM","FSCL_YR_NUM","FSCL_QTR_IN_YR_NUM"))
setDT(mop)

#change to percent for interpretation
mop[, cctbpercent := tbcc*100]
mop[, moppercent := moppct*100]

#regressions
summary(lm(data=mop, cctbpercent ~ moppercent))
mopprpcafe <- mop[,list(moppct = round(mean(moppct,na.rm=T),4)),by=c("FSCL_YR_NUM","FSCL_QTR_IN_YR_NUM")]
mopprpdt <- mop[,list(moppct = round(mean(moppct,na.rm=T),4)),by=c("FSCL_YR_NUM","FSCL_QTR_IN_YR_NUM")]

#get proportion in each group
props <- mop %>%
  group_by(FSCL_YR_NUM,FSCL_QTR_IN_YR_NUM,mopgroup) %>%
  summarise (n = n()) %>%
  mutate(pct = round(n / sum(n),4))
setDT(props)
#write file
write.xlsx(props,"O:/CoOp/CoOp194_PROReportng&OM/Julie/MOP_Slide6_pctinmopgroups.xlsx")


#aggregate
temp <- mop[, list(
  tbcc = sum(PEAK_TB_COUNT_Q2_2,na.rm=T)/sum(PEAK_RSPNS_COUNT_Q2_2,na.rm=T)),
  by=c("FSCL_YR_NUM","FSCL_QTR_IN_YR_NUM")]
temp <- setorder(temp,FSCL_YR_NUM,FSCL_QTR_IN_YR_NUM)
#merge
temp <- left_join(temp,props,by=c("mopgroup","FSCL_YR_NUM","FSCL_QTR_IN_YR_NUM"))
setDT(temp)
#write file
write.xlsx(temp,"O:/CoOp/CoOp194_PROReportng&OM/Julie/MOP_Slide6_CCforMOPcust_ALLstores.xlsx")

# ##to calculate percent of transactions occuring at cafe (vs. DT) stores
# tempall <- temp
# setnames(tempall,c("mopgroup","fsclyr","fsclqtr","alltb","alln","allpct"))
# tempcafe <- temp
# setnames(tempcafe,c("mopgroup","fsclyr","fsclqtr","cafetb","cafen","cafepct"))
# tempdt <- temp
# setnames(tempdt,c("mopgroup","fsclyr","fsclqtr","dttb","dtn","dtpct"))
# full <- merge(tempall,tempcafe,all=T,by=c("mopgroup","fsclyr","fsclqtr"))
# full <- merge(full,tempdt,all=T,by=c("mopgroup","fsclyr","fsclqtr"))
# write.xlsx(full,"O:/CoOp/CoOp194_PROReportng&OM/Julie/MOP_all_cafe_dt_CC.xlsx")
# full[, pctcafestores := cafen/alln]



#load data
mop <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/MOP_percent_by_store.csv")
mopcccafe <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC_by_store_cafe_only_FY18Q1-FY17Q1.csv")
mopccdt <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC_by_store_OTW_only_FY18Q1-FY17Q1.csv")
mop[STORE_NUM %in% unique(mopcccafe[,STORE_NUM]), cafeonlyflag := 1]
mop[STORE_NUM %in% unique(mopccdt[,STORE_NUM]), cafeonlyflag := 0]
temp <- mop[cafeonlyflag==1|cafeonlyflag==0, list(PEAK_MOP_TRANS_CNT = sum(PEAK_MOP_TRANS_CNT,na.rm=T),
           PEAK_TTL_TRANS_CNT = sum(PEAK_TTL_TRANS_CNT,na.rm=T),
           meanMOPprp = sum(PEAK_MOP_TRANS_CNT,na.rm=T)/sum(PEAK_TTL_TRANS_CNT,na.rm=T)),
    by=c("FSCL_YR_NUM","FSCL_QTR_IN_YR_NUM","cafeonlyflag")]
temp <- temp[(FSCL_YR_NUM==2018&FSCL_QTR_IN_YR_NUM<2)|FSCL_YR_NUM==2017]
temp <- setorder(temp,cafeonlyflag,FSCL_YR_NUM,FSCL_QTR_IN_YR_NUM)
#swing wide
temp2 <- dcast.data.table(temp, FSCL_YR_NUM + FSCL_QTR_IN_YR_NUM ~ cafeonlyflag, value.var=c("PEAK_MOP_TRANS_CNT","PEAK_TTL_TRANS_CNT","meanMOPprp"))
#percent of transactions
temp2[, pctPEAK_MOP_TRANScafe := PEAK_MOP_TRANS_CNT_1/(PEAK_MOP_TRANS_CNT_1+PEAK_MOP_TRANS_CNT_0)]
temp2[, pctTTL_TRANScafe := PEAK_TTL_TRANS_CNT_1/(PEAK_TTL_TRANS_CNT_1+PEAK_TTL_TRANS_CNT_0)]



##corrlations between mop percent and cc score
#load data
mop <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/MOP_percent_by_store.csv")
mopcc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC_by_store_ALL_FY18Q1-FY17Q1.csv")
mopcc[, STORE_NUM := as.numeric(STORE_NUM)]

#calculate MOP percentage
mop[, moppct := PEAK_MOP_TRANS_CNT/PEAK_TTL_TRANS_CNT]
mop <- na.omit(mop,cols="moppct")
#MOP transactions only & CC only
mopcc <- mopcc[QSTN_ID=="Q2_2"&ORD_MTHD_CD=="MOP"]

#merge
mop <- left_join(mop,mopcc,by=c("STORE_NUM","FSCL_YR_NUM","FSCL_QTR_IN_YR_NUM"))
setDT(mop)
mop[, tbcc := PEAK_TB_COUNT/PEAK_RSPNS_COUNT]
mop <- na.omit(mop,cols=c("tbcc","moppct","FSCL_YR_NUM","FSCL_QTR_IN_YR_NUM"))
cor(mop[FSCL_YR_NUM==2018&FSCL_QTR_IN_YR_NUM==1,tbcc],mop[FSCL_YR_NUM==2018&FSCL_QTR_IN_YR_NUM==1,moppct])
cor(mop[FSCL_YR_NUM==2017&FSCL_QTR_IN_YR_NUM==4,tbcc],mop[FSCL_YR_NUM==2017&FSCL_QTR_IN_YR_NUM==4,moppct])
cor(mop[FSCL_YR_NUM==2017&FSCL_QTR_IN_YR_NUM==3,tbcc],mop[FSCL_YR_NUM==2017&FSCL_QTR_IN_YR_NUM==3,moppct])
cor(mop[FSCL_YR_NUM==2017&FSCL_QTR_IN_YR_NUM==2,tbcc],mop[FSCL_YR_NUM==2017&FSCL_QTR_IN_YR_NUM==2,moppct])
cor(mop[FSCL_YR_NUM==2017&FSCL_QTR_IN_YR_NUM==1,tbcc],mop[FSCL_YR_NUM==2017&FSCL_QTR_IN_YR_NUM==1,moppct])



##corrlations between mop percent DELTA and cc score DELTA
#load data
mop <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/MOP_percent_by_store.csv")
mopcc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC_by_store_cafe_only_FY18Q1-FY17Q1.csv")
mopcc[, STORE_NUM := as.numeric(STORE_NUM)]

#calculate MOP percentage
mop[, moppct := PEAK_MOP_TRANS_CNT/PEAK_TTL_TRANS_CNT]
mop <- na.omit(mop,cols="moppct")
#MOP transactions only & cc only
mopcc <- mopcc[QSTN_ID=="Q2_2"&ORD_MTHD_CD=="MOP"]

#merge
mop <- left_join(mop,mopcc,by=c("STORE_NUM","FSCL_YR_NUM","FSCL_QTR_IN_YR_NUM"))
setDT(mop)
mop <- mop[(FSCL_YR_NUM==2018&FSCL_QTR_IN_YR_NUM==1)|(FSCL_YR_NUM==2017&FSCL_QTR_IN_YR_NUM==1)]
mop[, tbcc := PEAK_TB_COUNT/PEAK_RSPNS_COUNT]
mop <- na.omit(mop,cols=c("tbcc","moppct","FSCL_YR_NUM","FSCL_QTR_IN_YR_NUM"))

#model change
mop <- dcast.data.table(mop, STORE_NUM ~ FSCL_YR_NUM, value.var=c("moppct","tbcc"))
mop <- na.omit(mop,cols=c("moppct_2018","moppct_2017","tbcc_2018","tbcc_2017"))
mop[, tbccdelta := tbcc_2018-tbcc_2017]
mop[, moppctdelta := moppct_2018-moppct_2017]
cor(mop[,tbccdelta],mop[,moppctdelta])


##plotting CC score (y-axis) by mop adoption rates (x-axis), with mop split into 50 categories
#load data
mop <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/MOP_percent_by_store.csv")
mopcc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC_by_store_FY18Q1-FY16Q4.csv")
mopcc[, STORE_NUM := as.numeric(STORE_NUM)]
#MOP transactions only & cc only
mopcc <- mopcc[QSTN_ID=="Q2_2"&ORD_MTHD_CD=="MOP"]

#merge
mop <- left_join(mop,mopcc,by=c("STORE_NUM","FSCL_YR_NUM","FSCL_QTR_IN_YR_NUM","DRIVE_THRU_IND"))
setDT(mop)

#create 6-month averages
mop[(FSCL_YR_NUM==2018&FSCL_QTR_IN_YR_NUM==1)|(FSCL_YR_NUM==2017&FSCL_QTR_IN_YR_NUM==4), sixmon := 3]
mop[(FSCL_YR_NUM==2017&FSCL_QTR_IN_YR_NUM==3)|(FSCL_YR_NUM==2017&FSCL_QTR_IN_YR_NUM==2), sixmon := 2]
mop[(FSCL_YR_NUM==2017&FSCL_QTR_IN_YR_NUM==1)|(FSCL_YR_NUM==2016&FSCL_QTR_IN_YR_NUM==4), sixmon := 1]
#lapply
mop <- mop[, lapply(.SD,sum,na.rm=T), by=c("STORE_NUM","sixmon"),
           .SDcols=c("PEAK_MOP_TRANS_CNT","PEAK_TTL_TRANS_CNT","PEAK_TB_COUNT","PEAK_RSPNS_COUNT",
                     "TB_COUNT","RSPNS_COUNT")]

#calculate MOP percentage
mop[, peakmoppct := PEAK_MOP_TRANS_CNT/PEAK_TTL_TRANS_CNT]
mop[, tbcc := TB_COUNT/RSPNS_COUNT]
mop[, peaktbcc := PEAK_TB_COUNT/PEAK_RSPNS_COUNT]
mop <- na.omit(mop,cols=c("tbcc","peakmoppct","peaktbcc"))

#model change
mop <- dcast.data.table(mop, STORE_NUM ~ sixmon, value.var=c("PEAK_MOP_TRANS_CNT","PEAK_TTL_TRANS_CNT",
                                                             "PEAK_TB_COUNT","PEAK_RSPNS_COUNT",
                                                             "TB_COUNT","RSPNS_COUNT",
                                                             "tbcc","peakmoppct","peaktbcc"))
mop <- na.omit(mop,cols=c("peakmoppct_3","peakmoppct_2","peakmoppct_1"))

#split mop into 50 categories
moptemp <- mop %>% mutate(fiftytile_3 = ntile(peakmoppct_3, 50),
                          fiftytile_2 = ntile(peakmoppct_2, 50),
                          fiftytile_1 = ntile(peakmoppct_1, 50))
setDT(moptemp)
#take average CC by those categories
moptemp_3 <- moptemp[, list(tbcc_3 = sum(TB_COUNT_3,na.rm=T)/sum(RSPNS_COUNT_3,na.rm=T),
                            tbcc_2 = sum(TB_COUNT_2,na.rm=T)/sum(RSPNS_COUNT_2,na.rm=T),
                            tbcc_1 = sum(TB_COUNT_1,na.rm=T)/sum(RSPNS_COUNT_1,na.rm=T)),
                     by=c("fiftytile_3")]

#melt for plotting
moptemp_3 <- melt(moptemp_3, id="fiftytile_3")


#set labels
xlabel <- "MOP Adoption"
ylabel <- "MOP CC Score"
tlabel <- "MOP CC Score by MOP Adoption"
caption <- "Note: Stores grouped into MOP adoption categories based on Q4FY17 - Q1FY18"
sublabel <- "All Stores (Cafe & DT), All Transactions (Peak & Non-Peak)"
#manual legend labels
lname <- "6-Month Averages"
llabels <- c("Q4FY17 - Q1FY18","Q2FY17 - Q3FY17","Q4FY16 - Q1FY17") 
#set data and variables
pdata <- moptemp_3
px <- moptemp_3[, fiftytile_3]
py <- moptemp_3[, value]
groupvar <- moptemp_3[, variable]

#plot
p1 <- ggplot(data=pdata) +
  geom_line(aes(x=px, y=py, group=factor(groupvar), colour=factor(groupvar))) + 
  xlab(xlabel) + ylab(ylabel) + theme_economist() + 
  scale_colour_economist(name=lname, labels=llabels, guide=guide_legend(order=1)) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption)
print(p1)