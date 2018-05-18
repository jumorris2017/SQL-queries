#expected CC and speed values for Neha deployment
#Apr 2018 scores
#using TSDs, region, and home store % as indicators

#load libaries
library(data.table)
library(nlme)

#set path (new Q)
data_dir <- "Q:/Departments/WMO/Marketing Research/New Q drive/Foundational/Customer Voice/2.0/Foundational/2018_03_12_Deployment_Expected_CC_and_Speed"
data_dir_J <- "O:/CoOp/CoOp194_PROReportng&OM/Julie"

#load data
ce <- fread(paste0(data_dir_J,"/FY18_CC-Speed_StoreLevel.csv"))
hs <- fread(paste0(data_dir_J,"/FP7FY18_homestore.csv"))
tsd <- fread(paste0(data_dir_J,"/FP7FY18_TSD.csv"))

#keep only april
ce <- ce[FSCL_PER_IN_YR_NUM==7]
hs <- hs[FSCL_PER_IN_YR_NUM==7]
tsd <- tsd[FSCL_PER_IN_YR_NUM==7]
#calculate CC score
ce[, CC_TB_SCORE := CC_TB_CNT/CC_RESPONSE_TOTAL]

#merge
cedt <- Reduce(function(x, y) {merge(x, y, 
                                     by=c("STORE_NUM","FSCL_YR_NUM","FSCL_PER_IN_YR_NUM"), 
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
ceobs <- ce[, .(STORE_NUM,CC_TB_SCORE)]
newData <- Reduce(function(x, y) {merge(x, y, by=c("STORE_NUM"), 
                                     all = TRUE)}, list(newData,ceobs))
newData <- na.omit(newData)
write.csv(newData,file=paste0(data_dir,"/P7_FY18_CC_observed-expected_StoreLevel.csv"))