#drivers by channel

library(data.table)
library(flipRegression)

#load data
pce <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CE_bychannel_q2fy18_CANADA.csv")
#swing wide
pce2 <- dcast.data.table(pce, STORE_NUM + ORD_MTHD_CD ~ QSTN_ID, value.var="TB_SCORE")

#ALL STORES: 
#CAFE
Regression(Q1 ~ Q2_2 + Q2_1 + Q2_3 + 
             Q2_4 + Q2_5 + Q2_6 + Q2_7,
           data=pce2[ORD_MTHD_CD=='CAFE'],
           output = "Relative Importance Analysis")

#MOP
Regression(Q1 ~ Q2_2 + Q2_1 + Q2_3 + 
             Q2_4 + Q2_5 + Q2_6 + Q2_7,
           data=pce2[ORD_MTHD_CD=='MOP'],
           output = "Relative Importance Analysis")

#OTW
Regression(Q1 ~ Q2_2 + Q2_1 + Q2_3 + 
             Q2_4 + Q2_5 + Q2_6 + Q2_7,
           data=pce2[ORD_MTHD_CD=='OTW'],
           output = "Relative Importance Analysis")