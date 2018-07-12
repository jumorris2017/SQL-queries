##Speed/CC by MOP percentage##

#load libraries
library(data.table)
library(tidyverse)
library(ggplot2)
library(ggthemes)


#SLIDE 2 - Speed

#load data
mop <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/MOP_percent_by_store_FY18Q3.csv")
mopsp <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/SP_cafeonly_peak_FY18Q3-FY17Q1_FY15Q4.csv")

#organize stores into core, high, and super high
#keep only FY 18 Q3
mop <- mop[FSCL_YR_NUM==2018&FSCL_QTR_IN_YR_NUM==3]
mop <- na.omit(mop,cols="PEAK_MOP_PCT")

#reshape from long to wide to get delta
mopsp <- dcast.data.table(mopsp, STORE_NUM ~ FSCL_QTR_IN_YR_NUM + FSCL_YR_NUM, value.var=c("TOTAL_TB","TOTAL_RSPNS"))

#split by mop into  20 groups
mop <- mop %>% mutate(ntile = ntile(PEAK_MOP_PCT, 20))
mopsp <- left_join(mopsp, mop, by=c("STORE_NUM"))
setDT(mopsp)
mopsp <- na.omit(mopsp,cols="ntile")

#aggregate
temp <- mopsp[, list(n = .N,
                    meanPEAK_MOP_PCT = round(mean(PEAK_MOP_PCT,na.rm=T),2),
                    mean_daily_peak_MOP_trans = round(mean(PEAK_MOP_TRANS_CNT,na.rm=T)/91,0),
                    #tbsp_FY15Q4 = sum(TOTAL_TB_4_2015,na.rm=T)/sum(TOTAL_RSPNS_4_2015,na.rm=T),
                    tbsp_FY17Q1 = round(sum(TOTAL_TB_1_2017,na.rm=T)/sum(TOTAL_RSPNS_1_2017,na.rm=T),4),
                    tbsp_FY17Q2 = round(sum(TOTAL_TB_2_2017,na.rm=T)/sum(TOTAL_RSPNS_2_2017,na.rm=T),4),
                    tbsp_FY17Q3 = round(sum(TOTAL_TB_3_2017,na.rm=T)/sum(TOTAL_RSPNS_3_2017,na.rm=T),4),
                    tbsp_FY17Q4 = round(sum(TOTAL_TB_4_2017,na.rm=T)/sum(TOTAL_RSPNS_4_2017,na.rm=T),4),
                    tbsp_FY18Q1 = round(sum(TOTAL_TB_1_2018,na.rm=T)/sum(TOTAL_RSPNS_1_2018,na.rm=T),4),
                    tbsp_FY18Q2 = round(sum(TOTAL_TB_2_2018,na.rm=T)/sum(TOTAL_RSPNS_2_2018,na.rm=T),4),
                    tbsp_FY18Q3 = round(sum(TOTAL_TB_3_2018,na.rm=T)/sum(TOTAL_RSPNS_3_2018,na.rm=T),4)),
             by="ntile"]
temp <- setorder(temp,ntile)
#write file
write.csv(temp,"O:/CoOp/CoOp194_PROReportng&OM/Julie/FY18Q3_MOP_QBR_Speed_peak_cafe_by_MOPtile.csv")
