##CC by comps for US stores

##load libraries
library(data.table)
library(xlsx)
library(dplyr)

#slide #11
#load data
#part 1
p1 <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/Comps_by_store_US_pt1.csv")
#part 2
p2 <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/Comps_by_store_US_pt2.csv")

#rename merge id columns to match
setnames(p1,c("STORE_NUM"),c("STORE_NUMBER"))
#change store number and date values to numeric from characters
p1[, STORE_NUMBER := lapply(.SD, as.numeric), .SDcols = "STORE_NUMBER"]
#merge by store number, month, and year
pfull <- Reduce(function(x,y) {merge(x,y,by=c("STORE_NUMBER","FSCL_QTR_IN_YR_NUM","FSCL_YR_NUM"),all.x=TRUE)}, list(p1,p2))

#aggregate for Q4
# pagg <- pfull[, lapply(.SD, sum, na.rm=T), .SDcols=c("Q2_2_RESPONSE_TOTAL","Q2_2_TB_CNT",
#                                                      "QuarterlySales","LYQuarterlySales","CustTrans","day_count"),
#               by=c("STORE_NUMBER","FSCL_YR_NUM")]

#aggregate for rolling 3
pagg <- pfull[, lapply(.SD, sum, na.rm=T), .SDcols=c("Q2_2_RESPONSE_TOTAL","Q2_2_TB_CNT",
                                                     "QuarterlySales","LYQuarterlySales","CustTrans","day_count"),
              by=c("STORE_NUMBER","FSCL_YR_NUM")]
# pagg[, FSCL_YR_NUM := "Rolling 3"]

#calculate CC  top box score
pagg[, Q2_2_TB_SCORE := round(Q2_2_TB_CNT/Q2_2_RESPONSE_TOTAL,4)]

#calculate TSDs
pagg[, TSD := (CustTrans)/day_count]
pagg <- pagg[TSD>=500&TSD<=1000]
pagg[, CustTrans := NULL];pagg[, day_count := NULL]

#drop if stores don't have LYQuarterlySales
pagg <- na.omit(pagg, cols=c("QuarterlySales", "LYQuarterlySales"))
pagg <- pagg[QuarterlySales>0&LYQuarterlySales>0]

#calculate comps
pagg[, comps := (QuarterlySales-LYQuarterlySales)/LYQuarterlySales]
pagg <- pagg[comps>=-.25&comps<=.25]

#keep only FY17Q4
#pagg <- pagg[FSCL_QTR_IN_YR_NUM==4&FISCAL_YEAR_NUMBER==2017]

#split by cc
prob = c(1/4, 2/4, 3/4, 1)
temp <- pagg %>% group_by(FSCL_YR_NUM) %>% summarise( 
  cc25 = quantile(Q2_2_TB_SCORE, probs = prob[1], na.rm = T), 
  cc50 = quantile(Q2_2_TB_SCORE, probs = prob[2], na.rm = T),
  cc75 = quantile(Q2_2_TB_SCORE, probs = prob[3], na.rm = T),
  cc100 = quantile(Q2_2_TB_SCORE, probs = prob[4], na.rm = T)
)
pagg <- left_join(pagg, temp, by=c("FSCL_YR_NUM"))
setDT(pagg)

##to view quartile cut-offs
#temp2 <- as.data.table(temp)
#temp2[, (names(temp2[,c(2:5)])) := lapply(.SD, function(x) round(x,3)), .SDcols=names(temp2[,c(2:5)])]

#recode cc based on quartiles
pagg[Q2_2_TB_SCORE <= cc25, ccquartile := 1]
pagg[Q2_2_TB_SCORE > cc25 & Q2_2_TB_SCORE <= cc50, ccquartile := 2]
pagg[Q2_2_TB_SCORE > cc50 & Q2_2_TB_SCORE <= cc75, ccquartile := 3]
pagg[Q2_2_TB_SCORE > cc75, ccquartile := 4]
#drop quartile columns
#pagg <- pagg[, !(names(pagg[,c(10:13)])), with=FALSE]

#calculate top box score for cc, by comps quartile
pagg <- pagg[, list(Q2_2_RESPONSE_TOTAL=sum(Q2_2_RESPONSE_TOTAL,na.rm=T),
                    Q2_2_TB_CNT=sum(Q2_2_TB_CNT,na.rm=T),
                    compsavg=(sum(QuarterlySales)-sum(LYQuarterlySales))/sum(LYQuarterlySales)),
             by="ccquartile"]
pagg[, Q2_2_TB_SCORE := Q2_2_TB_CNT/Q2_2_RESPONSE_TOTAL]
#order by quartile
pagg <- setorder(pagg,ccquartile)
pagg <- cbind(pagg,t(temp)[2:5])
setnames(pagg,"V2","cc_q_value")
#make more presentable
pagg[, (colnames(pagg)[4:6]) := lapply(.SD, function(x) round((x*100),1)), .SDcols=colnames(pagg)[4:6]]
pagg[, Q2_2_RESPONSE_TOTAL := NULL]; pagg[, Q2_2_TB_CNT := NULL]

#export data
paggx <- pagg[, .(ccquartile,compsavg,Q2_2_TB_SCORE,cc_q_value)]
setnames(paggx,"cc_q_value","ccquartile_cutoff_value")
paggx[, ccquartile_cutoff_value := as.numeric(ccquartile_cutoff_value)]
paggx[, (colnames(paggx)[2:ncol(paggx)]) := lapply(.SD,function(x) round(x,4)*100),.SDcols=colnames(paggx)[2:ncol(paggx)]]
#write file
write.xlsx(paggx,file="O:/CoOp/CoOp194_PROReportng&OM/Julie/comps_by_cc_quartile_USfy18.xlsx")




#Slide #11 value for change in sales
#load data
#part 1
p1 <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/Comps_by_store_US_pt1.csv")
#part 2
p2 <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/Comps_by_store_US_pt2.csv")

#rename merge id columns to match
setnames(p1,c("STORE_NUM"),c("STORE_NUMBER"))
#change store number and date values to numeric from characters
p1[, STORE_NUMBER := lapply(.SD, as.numeric), .SDcols = "STORE_NUMBER"]
#merge by store number, month, and year
pfull <- Reduce(function(x,y) {merge(x,y,by=c("STORE_NUMBER","FSCL_QTR_IN_YR_NUM","FSCL_YR_NUM"),all.x=TRUE)}, list(p1,p2))

#aggregate for quarter
pagg <- pfull[, lapply(.SD, sum, na.rm=T), .SDcols=c("Q2_2_RESPONSE_TOTAL","Q2_2_TB_CNT",
                                                     "QuarterlySales","LYQuarterlySales","CustTrans","day_count"),
              by=c("STORE_NUMBER","FSCL_QTR_IN_YR_NUM","FSCL_YR_NUM")]

#calculate CC  top box score
pagg[, Q2_2_TB_SCORE := round(Q2_2_TB_CNT/Q2_2_RESPONSE_TOTAL,4)*100]

#drop if stores don't have LYQuarterlySales
pagg <- na.omit(pagg, cols=c("QuarterlySales", "LYQuarterlySales"))
pagg <- pagg[QuarterlySales>0&LYQuarterlySales>0]

#calculate comps
pagg[, comps := (QuarterlySales-LYQuarterlySales)/LYQuarterlySales]
pagg <- pagg[comps>=-.25&comps<=.25]
#calculate TSDs
pagg[, TSD := (CustTrans)/day_count]
pagg <- pagg[TSD>=700&TSD<=1200]

#calculate total impact on quarterly sales by increasing CC one point, via comps
##uses algebra to calculate new sales, based on bumped-up comp score and LY sales

#regression to get CC coefficient on comps
lm1 <- lm(data=pagg, comps ~ Q2_2_TB_SCORE)

#create new relationship variable between comps and quarterly sales
pagg[, compsbump := comps+lm1$coefficients[[2]]]
pagg[, QuarterlySalesbump := (compsbump*LYQuarterlySales)+LYQuarterlySales]
pagg <- pagg[, lapply(.SD, sum, na.rm=T), .SDcols=c("QuarterlySales","QuarterlySalesbump"),
              by=c("FSCL_QTR_IN_YR_NUM","FSCL_YR_NUM")]
pagg[, salesdiff := QuarterlySalesbump-QuarterlySales]


