#expected CC and speed values for Neha deployment
#using TSDs, region, and home store % as indicators

#load libaries
library(data.table)
library(nlme)
library(writexl)

#set path (new Q)
data_dir <- "Q:/Departments/WMO/Marketing Research/New Q drive/Foundational/Customer Voice/2.0/Foundational/2018_03_12_Deployment_Expected_CC_and_Speed"
data_dir_J <- "O:/CoOp/CoOp194_PROReportng&OM/Julie"

#load data
ce <- fread(paste0(data_dir_J,"/FP9FY18_CC_StoreLevel.csv"))
hs <- fread(paste0(data_dir_J,"/FP9FY18_homestore.csv"))
tsd <- fread(paste0(data_dir_J,"/FP9FY18_TSD.csv"))

#aggregate
hs <- hs[, lapply(.SD,sum(x,na.rm=T)), by=c("STORE_NUM","FSCL_YR_NUM"),
         .SDcols=c("HS_CUST_COUNT","ALL_CUST_COUNT")]
tsd <- tsd[, lapply(.SD,sum(x,na.rm=T)), by=c("STORE_NUM","FSCL_YR_NUM","RGN_ORG_LVL_DESCR"),
         .SDcols=c("CustTrans","day_count")]

# #keep only latest month
ce <- ce[FSCL_PER_IN_YR_NUM==9]

#calculate CC score
ce[, CC_TB_SCORE := CC_TB_CNT/CC_RESPONSE_TOTAL]

#merge
cedt <- Reduce(function(x, y) {merge(x, y, 
                                     by=c("STORE_NUM","FSCL_YR_NUM"), 
                                     all = TRUE)}, list(ce,hs,tsd))

#calculate CC score
cedt[, CC_TB_SCORE := CC_TB_CNT/CC_RESPONSE_TOTAL]

#calculate home store percent
cedt[, hspct := round(HS_CUST_COUNT/ALL_CUST_COUNT,4)]

#calculate TSDs
cedt[, tsd := round(CustTrans/day_count,1)]

#na omit region
cedt <- na.omit(cedt,cols=c("CC_TB_SCORE","hspct","tsd","RGN_ORG_LVL_DESCR"))

#run models split by region - CC
newData <- cedt[, .(STORE_NUM,RGN_ORG_LVL_DESCR,hspct,tsd)]
ll = lmList(CC_TB_SCORE ~ hspct + tsd | RGN_ORG_LVL_DESCR, data=cedt)
predict(ll, newData)
newData[["value"]] <- predict(ll, newData)
setnames(newData,"value","predCCscore")

#write.csv
newData <- newData[, .(STORE_NUM,predCCscore)]
ceobs <- cedt[, .(STORE_NUM,CC_TB_SCORE)]
newData <- Reduce(function(x, y) {merge(x, y, by=c("STORE_NUM"), 
                                     all = TRUE)}, list(newData,ceobs))
newData <- na.omit(newData)
newData <- newData[, lapply(.SD, function(x) round(x,4)), .SDcols=c("predCCscore","CC_TB_SCORE"),
        by="STORE_NUM"]
#write.csv
write.csv(newData,file=paste0(data_dir,"/P9_FY18_CC_observed-expected_StoreLevel.csv"))





# #for PAM
# 
# #rolling 2m
# 
# #load data
# ce <- fread(paste0(data_dir_J,"/FP9FY18_CC_StoreLevel.csv"))
# hs <- fread(paste0(data_dir_J,"/FP9FY18_homestore.csv"))
# tsd <- fread(paste0(data_dir_J,"/FP9FY18_tsd.csv"))
# 
# #aggregate
# ce <- ce[, lapply(.SD,sum(x,na.rm=T)), by=c("STORE_NUM","FSCL_YR_NUM"),
#          .SDcols=c("CC_RESPONSE_TOTAL","CC_TB_CNT")]
# hs <- hs[, lapply(.SD,sum(x,na.rm=T)), by=c("STORE_NUM","FSCL_YR_NUM"),
#          .SDcols=c("HS_CUST_COUNT","ALL_CUST_COUNT")]
# tsd <- tsd[, lapply(.SD,sum(x,na.rm=T)), by=c("STORE_NUM","FSCL_YR_NUM","RGN_ORG_LVL_DESCR"),
#            .SDcols=c("CustTrans","day_count")]
# 
# #calculate CC score
# ce[, CC_TB_SCORE := CC_TB_CNT/CC_RESPONSE_TOTAL]
# 
# #merge
# cedt <- Reduce(function(x, y) {merge(x, y, 
#                                      by=c("STORE_NUM","FSCL_YR_NUM"), 
#                                      all = TRUE)}, list(ce,hs,tsd))
# 
# #calculate CC score
# cedt[, cc_score_observed := CC_TB_CNT/CC_RESPONSE_TOTAL]
# 
# #calculate home store percent
# cedt[, homestore_prp := round(HS_CUST_COUNT/ALL_CUST_COUNT,4)]
# 
# #calculate tsds
# cedt[, tsd := round(CustTrans/day_count,1)]
# 
# #na omit region
# cedt <- na.omit(cedt,cols=c("cc_score_observed","homestore_prp","tsd","RGN_ORG_LVL_DESCR"))
# 
# #run models split by region - CC
# newData <- cedt[, .(STORE_NUM,RGN_ORG_LVL_DESCR,homestore_prp,tsd)]
# ll = lmList(cc_score_observed ~ homestore_prp + tsd | RGN_ORG_LVL_DESCR, data=cedt)
# predict(ll, newData)
# newData[["value"]] <- predict(ll, newData)
# setnames(newData,"value","cc_score_pred")
# 
# #write.csv
# newData <- newData[, .(STORE_NUM,cc_score_pred)]
# ceobs <- cedt[, .(STORE_NUM,cc_score_observed)]
# newData <- Reduce(function(x, y) {merge(x, y, by=c("STORE_NUM"), 
#                                         all = TRUE)}, list(newData,ceobs))
# newData <- na.omit(newData)
# newData <- newData[, lapply(.SD, function(x) round(x,4)), .SDcols=c("cc_score_pred","cc_score_observed"),
#                    by="STORE_NUM"]
# newData <- setcolorder(newData,c("STORE_NUM","cc_score_observed","cc_score_pred"))
# newData[, cc_score_op_delta := cc_score_observed - cc_score_pred]
# #bring in home store, tsd, and region
# stdt <- cedt[, .(STORE_NUM,homestore_prp,tsd,RGN_ORG_LVL_DESCR)]
# newData <- Reduce(function(x,y){merge(x,y,by="STORE_NUM",all.x=T)}, list(newData,stdt))
# setnames(newData,"RGN_ORG_LVL_DESCR","region")
# #write
# write_xlsx(newData,paste0(data_dir_J,"/FY18P9_R2M_CC_observed-predicted_storelevel.xlsx"))
# 
# 
# 
# #rolling 2m
# 
# #load data
# ce <- fread(paste0(data_dir_J,"/FP9FY18_CC_StoreLevel.csv"))
# hs <- fread(paste0(data_dir_J,"/FP9FY18_homestore.csv"))
# tsd <- fread(paste0(data_dir_J,"/FP9FY18_tsd.csv"))
# 
# #aggregate
# ce <- ce[FSCL_PER_IN_YR_NUM==9]
# hs <- hs[, lapply(.SD,sum(x,na.rm=T)), by=c("STORE_NUM","FSCL_YR_NUM"),
#          .SDcols=c("HS_CUST_COUNT","ALL_CUST_COUNT")]
# tsd <- tsd[, lapply(.SD,sum(x,na.rm=T)), by=c("STORE_NUM","FSCL_YR_NUM","RGN_ORG_LVL_DESCR"),
#            .SDcols=c("CustTrans","day_count")]
# 
# #calculate CC score
# ce[, CC_TB_SCORE := CC_TB_CNT/CC_RESPONSE_TOTAL]
# 
# #merge
# cedt <- Reduce(function(x, y) {merge(x, y, 
#                                      by=c("STORE_NUM","FSCL_YR_NUM"), 
#                                      all = TRUE)}, list(ce,hs,tsd))
# 
# #calculate CC score
# cedt[, cc_score_observed := CC_TB_CNT/CC_RESPONSE_TOTAL]
# 
# #calculate home store percent
# cedt[, homestore_prp := round(HS_CUST_COUNT/ALL_CUST_COUNT,4)]
# 
# #calculate tsds
# cedt[, tsd := round(CustTrans/day_count,1)]
# 
# #na omit region
# cedt <- na.omit(cedt,cols=c("cc_score_observed","homestore_prp","tsd","RGN_ORG_LVL_DESCR"))
# 
# #run models split by region - CC
# newData <- cedt[, .(STORE_NUM,RGN_ORG_LVL_DESCR,homestore_prp,tsd)]
# ll = lmList(cc_score_observed ~ homestore_prp + tsd | RGN_ORG_LVL_DESCR, data=cedt)
# predict(ll, newData)
# newData[["value"]] <- predict(ll, newData)
# setnames(newData,"value","cc_score_pred")
# 
# #write.csv
# newData <- newData[, .(STORE_NUM,cc_score_pred)]
# ceobs <- cedt[, .(STORE_NUM,cc_score_observed)]
# newData <- Reduce(function(x, y) {merge(x, y, by=c("STORE_NUM"), 
#                                         all = TRUE)}, list(newData,ceobs))
# newData <- na.omit(newData)
# newData <- newData[, lapply(.SD, function(x) round(x,4)), .SDcols=c("cc_score_pred","cc_score_observed"),
#                    by="STORE_NUM"]
# newData <- setcolorder(newData,c("STORE_NUM","cc_score_observed","cc_score_pred"))
# newData[, cc_score_op_delta := cc_score_observed - cc_score_pred]
# #bring in home store, tsd, and region
# stdt <- cedt[, .(STORE_NUM,homestore_prp,tsd,RGN_ORG_LVL_DESCR)]
# newData <- Reduce(function(x,y){merge(x,y,by="STORE_NUM",all.x=T)}, list(newData,stdt))
# setnames(newData,"RGN_ORG_LVL_DESCR","region")
# #write
# write_xlsx(newData,paste0(data_dir_J,"/FY18P9_CC_observed-predicted_storelevel.xlsx"))