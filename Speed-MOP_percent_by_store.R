##Speed/CC by MOP percentage##

#load libraries
library(data.table)
library(tidyverse)
library(xlsx)
library(ggplot2)
library(ggthemes)


#SLIDE 2 - Speed

#load data
mop <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/MOP_percent_by_store_FY18Q2.csv")
mopsp <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/SP_by_store_cafe_only_FY18Q2-FY17Q1.csv")
mopsp[, STORE_NUM := as.numeric(STORE_NUM)]
mopsp15 <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC-SP_by_store_cafe_onlyFY15Q4.csv")
mopsp15 <- mopsp15[QSTN_ID=='Q2_1']
mopsp15[, STORE_NUM := as.numeric(STORE_NUM)]

#remove PNW stores
pnw <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/pnw_storelist.csv")
pnwvec <- unique(pnw[,STORE_NUM])
setkey(mop, STORE_NUM)
mop <- mop[!pnw]
setkey(mopsp, STORE_NUM)
mopsp <- mopsp[!pnw]
setkey(mopsp15, STORE_NUM)
mopsp15 <- mopsp15[!pnw]
#restrict and rename
mopsp15 <- mopsp[, .(STORE_NUM,PEAK_TB_COUNT,PEAK_RSPNS_COUNT)]
setnames(mopsp15,c("PEAK_TB_COUNT","PEAK_RSPNS_COUNT"),c("PEAK_TB_COUNT_2015","PEAK_RSPNS_COUNT_2015"))

#calculate MOP percentage
mop[, moppct := PEAK_MOP_TRANS_CNT/PEAK_TTL_TRANS_CNT]
mop <- na.omit(mop,cols="moppct")

#organize stores into core, high, and super high
#keep only FY 18 Q2
mop <- mop[FSCL_YR_NUM==2018&FSCL_QTR_IN_YR_NUM==2]
mop <- na.omit(mop,cols="moppct")

#MOP transactions only & CC only
mopsp <- mopsp[ORD_MTHD_CD=="CAFE"]

#reshape from long to wide to get delta
# mopsp <- mopsp[FSCL_YR_NUM==2018]
mopsp <- dcast.data.table(mopsp, STORE_NUM ~ FSCL_QTR_IN_YR_NUM + FSCL_YR_NUM, value.var=c("PEAK_TB_COUNT","PEAK_RSPNS_COUNT"))

#split by mop into  20 groups
mop <- mop %>% mutate(ntile = ntile(PEAK_MOP_PCT, 20))
mopsp <- left_join(mopsp, mop, by=c("STORE_NUM"))
setDT(mopsp)
mopsp <- na.omit(mopsp,cols="ntile")

#MERGE IN 2015 DATA
temp <- left_join(mopsp,mopsp15,by=c("STORE_NUM"))
setDT(temp)

#aggregate
temp <- temp[, list(n = .N,
                    meanPEAK_MOP_PCT = mean(PEAK_MOP_PCT,na.rm=T),
                    mean_daily_peak_MOP_trans = round(mean(PEAK_MOP_TRANS_CNT,na.rm=T)/91,0),
                    tbsp_FY15Q4 = sum(PEAK_TB_COUNT_2015,na.rm=T)/sum(PEAK_RSPNS_COUNT_2015,na.rm=T),
                    tbsp_FY17Q1 = sum(PEAK_TB_COUNT_1_2017,na.rm=T)/sum(PEAK_RSPNS_COUNT_1_2017,na.rm=T),
                    tbsp_FY17Q2 = sum(PEAK_TB_COUNT_2_2017,na.rm=T)/sum(PEAK_RSPNS_COUNT_2_2017,na.rm=T),
                    tbsp_FY17Q3 = sum(PEAK_TB_COUNT_3_2017,na.rm=T)/sum(PEAK_RSPNS_COUNT_3_2017,na.rm=T),
                    tbsp_FY17Q4 = sum(PEAK_TB_COUNT_4_2017,na.rm=T)/sum(PEAK_RSPNS_COUNT_4_2017,na.rm=T),
                    tbsp_FY18Q1 = sum(PEAK_TB_COUNT_1_2018,na.rm=T)/sum(PEAK_RSPNS_COUNT_1_2018,na.rm=T),
                    tbsp_FY18Q2 = sum(PEAK_TB_COUNT_2_2018,na.rm=T)/sum(PEAK_RSPNS_COUNT_2_2018,na.rm=T)),
             by="ntile"]
temp <- setorder(temp,ntile)
#write file
write.csv(temp,"O:/CoOp/CoOp194_PROReportng&OM/Julie/2018-04-05_Speed_peakcafe_by_MOPtile.csv")








#SLIDE 2 - CC

#load data
mop <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/MOP_percent_by_store_FY18Q2.csv")
mopcc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC_by_store_cafe_only_FY18Q2-FY17Q1.csv")
mopcc[, STORE_NUM := as.numeric(STORE_NUM)]
mopcc15 <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC-SP_by_store_cafe_onlyFY15Q4.csv")
mopcc15 <- mopcc15[QSTN_ID=='Q2_2']
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
#restrict and rename
mopcc15 <- mopcc[, .(STORE_NUM,PEAK_TB_COUNT,PEAK_RSPNS_COUNT)]
setnames(mopcc15,c("PEAK_TB_COUNT","PEAK_RSPNS_COUNT"),c("PEAK_TB_COUNT_2015","PEAK_RSPNS_COUNT_2015"))

#calculate MOP percentage
mop[, moppct := PEAK_MOP_TRANS_CNT/PEAK_TTL_TRANS_CNT]
mop <- na.omit(mop,cols="moppct")

#organize stores into core, high, and super high
#keep only FY 18 Q2
mop <- mop[FSCL_YR_NUM==2018&FSCL_QTR_IN_YR_NUM==2]
mop <- na.omit(mop,cols="moppct")

#MOP transactions only & CC only
mopcc <- mopcc[ORD_MTHD_CD=="CAFE"]

#reshape from long to wide to get delta
# mopcc <- mopcc[FSCL_YR_NUM==2018]
mopcc <- dcast.data.table(mopcc, STORE_NUM ~ FSCL_QTR_IN_YR_NUM + FSCL_YR_NUM, value.var=c("PEAK_TB_COUNT","PEAK_RSPNS_COUNT"))

#split by mop into  20 groups
mop <- mop %>% mutate(ntile = ntile(PEAK_MOP_PCT, 20))
mopcc <- left_join(mopcc, mop, by=c("STORE_NUM"))
setDT(mopcc)
mopcc <- na.omit(mopcc,cols="ntile")

#MERGE IN 2015 DATA
temp <- left_join(mopcc,mopcc15,by=c("STORE_NUM"))
setDT(temp)

#aggregate
temp <- temp[, list(n = .N,
                    meanPEAK_MOP_PCT = mean(PEAK_MOP_PCT,na.rm=T),
                    mean_daily_peak_MOP_trans = round(mean(PEAK_MOP_TRANS_CNT,na.rm=T)/91,0),
                    tbcc_FY15Q4 = sum(PEAK_TB_COUNT_2015,na.rm=T)/sum(PEAK_RSPNS_COUNT_2015,na.rm=T),
                    tbcc_FY17Q1 = sum(PEAK_TB_COUNT_1_2017,na.rm=T)/sum(PEAK_RSPNS_COUNT_1_2017,na.rm=T),
                    tbcc_FY17Q2 = sum(PEAK_TB_COUNT_2_2017,na.rm=T)/sum(PEAK_RSPNS_COUNT_2_2017,na.rm=T),
                    tbcc_FY17Q3 = sum(PEAK_TB_COUNT_3_2017,na.rm=T)/sum(PEAK_RSPNS_COUNT_3_2017,na.rm=T),
                    tbcc_FY17Q4 = sum(PEAK_TB_COUNT_4_2017,na.rm=T)/sum(PEAK_RSPNS_COUNT_4_2017,na.rm=T),
                    tbcc_FY18Q1 = sum(PEAK_TB_COUNT_1_2018,na.rm=T)/sum(PEAK_RSPNS_COUNT_1_2018,na.rm=T),
                    tbcc_FY18Q2 = sum(PEAK_TB_COUNT_2_2018,na.rm=T)/sum(PEAK_RSPNS_COUNT_2_2018,na.rm=T)),
             by="ntile"]
temp <- setorder(temp,ntile)
#write file
write.csv(temp,"O:/CoOp/CoOp194_PROReportng&OM/Julie/2018-04-05_CC_peakcafe_by_MOPtile.csv")