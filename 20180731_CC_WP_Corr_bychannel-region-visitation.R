
#load libraries
library(data.table)
library(Hmisc)
library(ppcor)
library(dplyr)
library(PerformanceAnalytics)

#load data
data_dir <- "O:/CoOp/CoOp194_PROReportng&OM/Julie"
st <- fread(paste0(data_dir,"/20180731_cc_wp_corr_storelevel.csv"))
cu <- fread(paste0(data_dir,"/20180731_cc_wp_corr_custlevel.csv"))

#correlation tests

#customer level
cor.test(cu[,TOTAL_TB_CC],cu[,TOTAL_TB_WP])

#partial correlation controlling for P30D frequency
# pcor.test(cu[,TOTAL_TB_CC],cu[,TOTAL_TB_WP],cu[,TRANS],method="pearson")
# pcor.test(cu[,TOTAL_TB_CC],cu[,TOTAL_TB_WP],cu[,TRANS_GROUP],method="pearson")

#correlations within trans groups
cu %>% group_by(TRANS_GROUP) %>% summarize(cor=cor(TOTAL_TB_CC,TOTAL_TB_WP))

#swing store level dt wide by question
st <- dcast.data.table(st, STORE_NUM + RGN_DESCR + ORD_MTHD_CD ~ QSTN_ID, value.var="TB_SCORE")
setnames(st,c("Q2_2","Q2_8"),c("TOTAL_TB_CC","TOTAL_TB_WP"))

#store level
cor.test(st[,TOTAL_TB_CC],st[,TOTAL_TB_WP])

#correlations within trans groups
st %>% group_by(RGN_DESCR) %>% summarize(cor=cor(TOTAL_TB_CC,TOTAL_TB_WP))
st %>% group_by(ORD_MTHD_CD) %>% summarize(cor=cor(TOTAL_TB_CC,TOTAL_TB_WP))