#expected CC and speed values for Neha deployment
#using TSDs, region, and home store % as indicators

#load libaries
library(data.table)
library(nlme)

##MARCH
#set path (new Q)
data_dir <- "Q:/Departments/WMO/Marketing Research/New Q drive/Foundational/Customer Voice/2.0/Foundational/2018_03_12_Deployment_Expected_CC_and_Speed"
data_dir_J <- "O:/CoOp/CoOp194_PROReportng&OM/Julie"

#load data
ce <- fread(paste0(data_dir_J,"/FP5-6FY18_CC-Speed_StoreLevel_v2.csv"))
hs <- fread(paste0(data_dir_J,"/FP5-6FY18_homestore.csv"))
hs <- hs[STORE_NUM %in% c(3333,
                       5296,
                       6499,
                       7307,
                       7370,
                       8228,
                       11737,
                       13439,
                       26929,
                       8123,
                       8222,
                       8292,
                       8955,
                       10185)]
tsd <- fread(paste0(data_dir_J,"/FP5-6FY18_TSD_v2.csv"))

#restrict to just March
ce <- ce[FSCL_PER_IN_YR_NUM==6]
hs <- hs[FSCL_PER_IN_YR_NUM==6]
tsd <- tsd[FSCL_PER_IN_YR_NUM==6]

#aggregate for rolling-2
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
cedt <- cedt[!STORE_NUM %in% c(5296, 6499, 8292)]
#na omit region
cedt <- na.omit(cedt,cols=c("CC_TB_SCORE","SP_TB_SCORE","hspct","tsd","RGN_ORG_LVL_DESCR"))

#run models
newData <- cedt[, .(STORE_NUM,hspct,tsd)]
ll = lm(CC_TB_SCORE ~ hspct + tsd, data=cedt)
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
#drop speed
cedt <- cedt[, .(STORE_NUM,CC_TB_SCORE,predCCscore)]
write.csv(cedt,file=paste0(data_dir,"/P6_FY18_CC_observed-expected_StoreLevel_v2.csv"))