#expected CC and speed values
#dec/jan/feb fy2018 scores
#using TSDs, region, and home store % as indicators

#load libaries
library(data.table)
library(nlme)

##FEBRUARY
#set path (new Q)
data_dir <- "O:/CoOp/CoOp194_PROReportng&OM/Julie"

#load data
ce <- fread(paste0(data_dir,"/FP3-5_FY18_CC-Speed_StoreLevel.csv"))
hs <- fread(paste0(data_dir,"/FP3-5_FY18_homestore.csv"))
tsd <- fread(paste0(data_dir,"/FP3-5_FY18_TSD.csv"))
rural <- fread(paste0(data_dir,"/storenum_urbanity.csv"))
prodmix <- fread(paste0(data_dir,"/product_mix.csv"))

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