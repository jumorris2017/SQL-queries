#expected CC and speed values for Neha deployment
#feb 2018 scores
#using TSDs, region, and home store % as indicators

#load libaries
library(data.table)
library(nlme)


##FEBRUARY
#set path (new Q)
data_dir <- "Q:/Departments/WMO/Marketing Research/New Q drive/Foundational/Customer Voice/2.0/Foundational/2018_03_12_Deployment_Expected_CC_and_Speed"

#load data
ce <- fread(paste0(data_dir,"/FebFY18_CC-Speed_StoreLevel.csv"))
hs <- fread(paste0(data_dir,"/FP5FY18_homestore.csv"))
tsd <- fread(paste0(data_dir,"/FP5FY18_TSD.csv"))

#calculate home store percent
hs[, hspct := round(HS_CUST_COUNT/ALL_CUST_COUNT,4)]

#calculate TSDs
tsd[, tsd := round(CustTrans/day_count,1)]

#merge
cedt <- Reduce(function(x, y) {merge(x, y, 
                                     by=c("STORE_NUM","FSCL_YR_NUM","FSCL_PER_IN_YR_NUM"), 
                                     all = TRUE)}, list(ce,hs,tsd))
#na omit region
cedt <- na.omit(cedt,cols=c("CC_TB_SCORE","SP_TB_SCORE","hspct","tsd","RGN_ORG_LVL_DESCR"))

#run models split by region - CC
newData <- cedt[, .(STORE_NUM,RGN_ORG_LVL_DESCR,hspct,tsd)]
ll = lmList(CC_TB_SCORE ~ hspct + tsd | RGN_ORG_LVL_DESCR, data=cedt)
predict(ll, newData)
newData[["value"]] <- predict(ll, newData)
setnames(newData,"value","predCCscore")

#run models split by region - Speed
ll = lmList(SP_TB_SCORE ~ hspct + tsd | RGN_ORG_LVL_DESCR, data=cedt)
predict(ll, newData)
newData[["value"]] <- predict(ll, newData)
setnames(newData,"value","predSPscore")

#write.csv
newData <- newData[, .(STORE_NUM,predCCscore,predSPscore)]
write.csv(newData,file=paste0(data_dir,"/FebFY18_EXP_CC-Speed_StoreLevel.csv"))



##JANUARY
#set path (new Q)
data_dir <- "Q:/Departments/WMO/Marketing Research/New Q drive/Foundational/Customer Voice/2.0/Foundational/2018_03_12_Deployment_Expected_CC_and_Speed"

#load data
ce <- fread(paste0(data_dir,"/JanFY18_CC-Speed_StoreLevel.csv"))
hs <- fread(paste0(data_dir,"/FP4FY18_homestore.csv"))
tsd <- fread(paste0(data_dir,"/FP4FY18_TSD.csv"))

#calculate CC/SP score
ce[, CC_TB_SCORE := CC_TB_CNT/CC_RESPONSE_TOTAL]
ce[, SP_TB_SCORE := SP_TB_CNT/SP_RESPONSE_TOTAL]

#calculate home store percent
hs[, hspct := round(HS_CUST_COUNT/ALL_CUST_COUNT,4)]

#calculate TSDs
tsd[, tsd := round(CustTrans/day_count,1)]

#merge
cedt <- Reduce(function(x, y) {merge(x, y, 
                                     by=c("STORE_NUM","FSCL_YR_NUM","FSCL_PER_IN_YR_NUM"), 
                                     all = TRUE)}, list(ce,hs,tsd))
#na omit region
cedt <- na.omit(cedt,cols=c("CC_TB_SCORE","SP_TB_SCORE","hspct","tsd","RGN_ORG_LVL_DESCR"))

#calculate home store percent
cedt[, hspct := round(HS_CUST_COUNT/ALL_CUST_COUNT,4)]

#calculate TSDs
cedt[, tsd := round(CustTrans/day_count,1)]

#run models split by region - CC
newData <- cedt[, .(STORE_NUM,RGN_ORG_LVL_DESCR,hspct,tsd)]
ll = lmList(CC_TB_SCORE ~ hspct + tsd | RGN_ORG_LVL_DESCR, data=cedt)
predict(ll, newData)
newData[["value"]] <- predict(ll, newData)
setnames(newData,"value","predCCscore")

#run models split by region - Speed
ll = lmList(SP_TB_SCORE ~ hspct + tsd | RGN_ORG_LVL_DESCR, data=cedt)
predict(ll, newData)
newData[["value"]] <- predict(ll, newData)
setnames(newData,"value","predSPscore")

#write.csv
newData <- newData[, .(STORE_NUM,predCCscore,predSPscore)]
write.csv(newData,file=paste0(data_dir,"/JanFY18_EXP_CC-Speed_StoreLevel.csv"))