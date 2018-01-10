##CC by MOP percentage##

#load libraries
library(data.table)
library(xlsx)
library(ggplot2)

#SLIDES 5-6

#load data
mop <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/MOP_percent_by_store.csv")
#mopcc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC_by_store_cafe_only.csv")
mopcc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC-SP_by_store_cafe_only_FY18Q1.csv")
mopcc[, STORE_NUM := as.numeric(STORE_NUM)]

#calculate MOP percentage
mop[, moppct := PEAK_MOP_TRANS_CNT/PEAK_TTL_TRANS_CNT]
mop <- na.omit(mop,cols="moppct")

#remove PNW stores
pnw <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/pnw_storelist.csv")
pnwvec <- unique(pnw[,STORE_NUM])
setkey(mop, STORE_NUM)
mop <- mop[!pnw]
setkey(mopcc, STORE_NUM)
mopcc <- mopcc[!pnw]

#organize stores into core, high, and super high
#group by FY 17 Q1
mop17 <- mop[FSCL_YR_NUM==2017&FSCL_QTR_IN_YR_NUM==1]
mop17[moppct>=0&moppct<.10, mopgroup := "1 - core"] #core
mop17[moppct>=.10&moppct<.20, mopgroup := "2 - high"] #high
mop17[moppct>=.20, mopgroup := "3 - super high"] #super high
mop17 <- mop17[, c("STORE_NUM","mopgroup")]

#keep only FY 18 Q1
mop <- mop[FSCL_YR_NUM==2018&FSCL_QTR_IN_YR_NUM==1]
mop <- left_join(mop,mop17,by="STORE_NUM")
setDT(mop)
mop <- na.omit(mop,cols="mopgroup")

##UPDATE FOR SPEED##

#CC - NON-MOP transactions only
mopcc <- mopcc[ORD_MTHD_CD=="CAFE"]

# #calculate TB score
# mopcc[, tbscore := round((PEAK_TB_COUNT/PEAK_RSPNS_COUNT)*100,0)]

#reshape from long to wide to get year delta
mopcc <- dcast.data.table(mopcc, ORD_MTHD_CD + STORE_NUM ~ QSTN_ID, value.var=c("PEAK_TB_COUNT","PEAK_RSPNS_COUNT"))
# #setnames
# setnames(mopcc,c("Q2_1","Q2_2"),c("tbspeed","tbcc"))

#calculate TB score
mopcc[, tbspeed := round((PEAK_TB_COUNT_Q2_1/PEAK_RSPNS_COUNT_Q2_1)*100,0)]
mopcc[, tbcc := round((PEAK_TB_COUNT_Q2_2/PEAK_RSPNS_COUNT_Q2_2)*100,0)]

#merge
mop <- left_join(mop,mopcc,by=c("STORE_NUM"))
setDT(mop)

#aggregate
temp <- mop[, list(
                   #PEAK_TB_COUNT = sum(PEAK_TB_COUNT,na.rm=T),
                   #PEAK_RSPNS_COUNT = sum(PEAK_RSPNS_COUNT,na.rm=T),
                   n = .N,
                   tbspeed = sum(PEAK_TB_COUNT_Q2_1,na.rm=T)/sum(PEAK_RSPNS_COUNT_Q2_1,na.rm=T),
                   tbcc = sum(PEAK_TB_COUNT_Q2_2,na.rm=T)/sum(PEAK_RSPNS_COUNT_Q2_2,na.rm=T)),
            by="mopgroup"]
temp <- setorder(temp,mopgroup)
#write file
#write.xlsx(temp,file="O:/CoOp/CoOp194_PROReportng&OM/Julie/CC-SP-MOP_percent_FY18Q1.xlsx")
write.xlsx(temp,"O:/CoOp/CoOp194_PROReportng&OM/Julie/MOP_Slides5-6.xlsx")



#SLIDE 4

#load data
mop <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/MOP_percent_by_store.csv")
#mopcc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC_by_store_cafe_only.csv")
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
##UPDATE FOR SPEED##

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
#mopcc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC_by_store_cafe_only.csv")
mopcc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC-SP_by_store_cafe_only_FY18Q1-FY17Q4.csv")
mopcc[, STORE_NUM := as.numeric(STORE_NUM)]
mopcc15 <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC-SP_by_store_cafe_only_FY15Q4.csv")
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
##UPDATE FOR SPEED##

#MOP transactions only & speed only
mopcc <- mopcc[ORD_MTHD_CD=="CAFE"&QSTN_ID=="Q2_1"]

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
  #PEAK_TB_COUNT = sum(PEAK_TB_COUNT,na.rm=T),
  #PEAK_RSPNS_COUNT = sum(PEAK_RSPNS_COUNT,na.rm=T),
  N = .N,
  tb_score = sum(PEAK_TB_COUNT,na.rm=T)/sum(PEAK_RSPNS_COUNT,na.rm=T)),
  by=c("mopgroup","QSTN_ID")]
temp <- setorder(temp,QSTN_ID,mopgroup)
#write file
write.xlsx(temp,file="O:/CoOp/CoOp194_PROReportng&OM/Julie/CC-SP-MOP_percent_PRE-LAUNCH.xlsx")

