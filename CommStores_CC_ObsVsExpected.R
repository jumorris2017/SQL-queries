#expected CC and speed values for Neha deployment
#using TSDs, region, and home store % as indicators

#load libaries
library(data.table)
library(nlme)

##MARCH
data_dir_J <- "O:/CoOp/CoOp194_PROReportng&OM/Julie"

#load data
ce <- fread(paste0(data_dir_J,"/FP4-6FY18_CC-Speed_StoreLevel_CS.csv"))
hs <- fread(paste0(data_dir_J,"/FP4-6FY18_homestore_CS.csv"))
tsd <- fread(paste0(data_dir_J,"/FP4-6FY18_TSD_CS.csv"))

#aggregate for quarter
ce <- ce[, list(CC_RESPONSE_TOTAL = sum(CC_RESPONSE_TOTAL,na.rm=T),
                SP_RESPONSE_TOTAL = sum(SP_RESPONSE_TOTAL,na.rm=T),
                CC_TB_CNT = sum(CC_TB_CNT,na.rm=T),
                SP_TB_CNT = sum(SP_TB_CNT,na.rm=T),
                CC_TB_SCORE = round(sum(CC_TB_CNT,na.rm=T)/sum(CC_RESPONSE_TOTAL,na.rm=T),4),
                SP_TB_SCORE = round(sum(SP_TB_CNT,na.rm=T)/sum(SP_RESPONSE_TOTAL,na.rm=T),4)),
         by=c("STORE_NUM","FSCL_YR_NUM")]

hs <- hs[, list(HS_CUST_COUNT = sum(HS_CUST_COUNT,na.rm=T),
                ALL_CUST_COUNT = sum(ALL_CUST_COUNT,na.rm=T)),
         by=c("STORE_NUM","FSCL_YR_NUM")]

tsd <- tsd[, list(CustTrans = sum(CustTrans,na.rm=T),
                  day_count = sum(day_count,na.rm=T)),
           by=c("STORE_NUM","FSCL_YR_NUM","RGN_ORG_LVL_DESCR")]

#calculate home store percent
hs[, hspct := round(HS_CUST_COUNT/ALL_CUST_COUNT,4)]

#calculate TSDs
tsd[, tsd := round(CustTrans/day_count,1)]

#merge
cedt <- Reduce(function(x, y) {merge(x, y, 
                                     by=c("STORE_NUM","FSCL_YR_NUM"), 
                                     all = TRUE)}, list(ce,hs,tsd))
#na omit region
cedt <- na.omit(cedt,cols=c("CC_TB_SCORE","SP_TB_SCORE","tsd","RGN_ORG_LVL_DESCR"))

# #calculate home store percent
# cedt[, hspct := round(HS_CUST_COUNT/ALL_CUST_COUNT,4)]
# 
# #calculate TSDs
# cedt[, tsd := round(CustTrans/day_count,1)]

#run models split by region - CC
newData <- cedt[, .(STORE_NUM,RGN_ORG_LVL_DESCR,tsd)]
ll = lmList(CC_TB_SCORE ~ tsd | RGN_ORG_LVL_DESCR, data=cedt)
predict(ll, newData)
newData[["value"]] <- predict(ll, newData)
setnames(newData,"value","predCCscore")

#prep predictions
newData <- newData[, .(STORE_NUM,predCCscore)]
newData[, predCCscore := round(predCCscore,4)]
#prep observed
cedt <- cedt[, .(STORE_NUM,CC_TB_SCORE)]
cedt[, CC_TB_SCORE := round(as.numeric(CC_TB_SCORE),4)]
cedt <- Reduce(function(x, y) {merge(x, y, by=c("STORE_NUM"), all = TRUE)}, list(newData,cedt))
#restrict to comm stores
cedt <- cedt[STORE_NUM %in% c(47806,50622,27892,27777,25685,5721,49905,20314,49009,50234)]

#drop speed
# write.csv(cedt,file=paste0(data_dir,"/P6_FY18_CC_observed-expected_StoreLevel_v2.csv"))