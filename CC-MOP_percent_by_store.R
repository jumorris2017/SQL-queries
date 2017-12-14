##CC by MOP percentage##

#load libraries
library(data.table)
library(xlsx)
library(ggplot2)

#load data
mop <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/MOP_percent_by_store.csv")
mopcc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC_by_store_cafe_only.csv")
mopcc[, STORE_NUM := as.numeric(STORE_NUM)]

#calculate MOP percentage
mop[, moppct := PEAK_MOP_TRANS_CNT/PEAK_TTL_TRANS_CNT]
mop <- na.omit(mop,cols="moppct")

#organize stores into core, high, and super high
#group by FY 17 Q1
mop17 <- mop[FSCL_YR_NUM==2017]
mop17[moppct>=0&moppct<.10, mopgroup := "1 - core"] #core
mop17[moppct>=.10&moppct<.20, mopgroup := "2 - high"] #high
mop17[moppct>=.20, mopgroup := "3 - super high"] #super high
mop17 <- mop17[, c("STORE_NUM","mopgroup")]

#keep only FY 18
mop <- mop[FSCL_YR_NUM==2018]
mop <- left_join(mop,mop17,by="STORE_NUM")
setDT(mop)
mop <- na.omit(mop,cols="mopgroup")

#CC - NON-MOP transactions only
mopcc <- mopcc[ORD_MTHD_CD=="CAFE"]

#calculate CC TB
mopcc[, ccscore := PEAK_TB_COUNT/PEAK_RSPNS_COUNT]
mopcc <- na.omit(mopcc,cols="ccscore")

#merge
mop <- left_join(mop,mopcc,by=c("STORE_NUM","FSCL_YR_NUM","FSCL_QTR_IN_YR_NUM"))
setDT(mop)

#aggregate
temp <- mop[, list(
                   PEAK_TB_COUNT = sum(PEAK_TB_COUNT,na.rm=T),
                   PEAK_RSPNS_COUNT = sum(PEAK_RSPNS_COUNT,na.rm=T),
                   #N = .N,
                   ccscore = round(sum(PEAK_TB_COUNT,na.rm=T)/sum(PEAK_RSPNS_COUNT,na.rm=T),4)*100), 
            by="mopgroup"]
temp <- setorder(temp,mopgroup)
#write file
write.xlsx(temp,file="O:/CoOp/CoOp194_PROReportng&OM/Julie/CC-MOP_percent.xlsx")





##FY15 Q4 UPDATE
mop15 <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC-SP_by_store_cafe_onlyFY15Q4.csv")
mop15[, STORE_NUM := as.numeric(STORE_NUM)]
mop15 <- mop15[STORE_NUM %in% unique(mop17[,STORE_NUM])]
mop15 <- left_join(mop15,mop17,by="STORE_NUM")
setDT(mop15)
mop15 <- na.omit(mop15,cols="mopgroup")

#CC - NON-MOP transactions only
mop15 <- mop15[ORD_MTHD_CD=="CAFE"]

#calculate CC TB
mop15[, tb_score := PEAK_TB_COUNT/PEAK_RSPNS_COUNT]
mop15 <- na.omit(mop15,cols="tb_score")

#aggregate
temp <- mop15[, list(
  PEAK_TB_COUNT = sum(PEAK_TB_COUNT,na.rm=T),
  PEAK_RSPNS_COUNT = sum(PEAK_RSPNS_COUNT,na.rm=T),
  #N = .N,
  tb_score = round(sum(PEAK_TB_COUNT,na.rm=T)/sum(PEAK_RSPNS_COUNT,na.rm=T),4)*100), 
  by=c("mopgroup","QSTN_ID")]
temp <- setorder(temp,QSTN_ID,mopgroup)
#write file
write.xlsx(temp,file="O:/CoOp/CoOp194_PROReportng&OM/Julie/CC-SP-MOP_percent.xlsx")

