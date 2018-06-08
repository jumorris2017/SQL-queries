#COSD/COMP and CE Scores

#load libraries
library(data.table)

#read data
ce <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc-so_bystoreperformance_Canada_FY18Q2.csv")
ce <- ce[, .(STORE_NUM,FSCL_YR_NUM,Q2_2_RESPONSE_TOTAL,Q2_2_TB_CNT)]
comp <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/Comps_by_store_ca_q2fy18.csv")

#make consistent
ce[, STORE_NUM := as.numeric(STORE_NUM)]
# ce[, (grep("TB_SCORE",colnames(ce),value=T)) := lapply(.SD,as.numeric), .SDcols=grep("TB_SCORE",colnames(ce),value=T)]
setnames(comp,"STORE_NUMBER","STORE_NUM")

#merge by store number
pfull <- left_join(ce,comp,by=c("STORE_NUM","FSCL_YR_NUM"))
setDT(pfull)

# 
# #change store number and date values to numeric from characters
# p1[, STORE_NUM := lapply(.SD, as.numeric), .SDcols = "STORE_NUM"]
# #merge by store number, month, and year
# pfull <- Reduce(function(x,y) {merge(x,y,by=c("STORE_NUM"),all.x=TRUE)}, list(p1,p2))

#aggregate for quarter
pagg <- pfull[, lapply(.SD, sum, na.rm=T), .SDcols=c("Q2_2_RESPONSE_TOTAL","Q2_2_TB_CNT",
                                                     "Sales","LYSales","CustTrans","day_count"),
              by=c("STORE_NUM","FSCL_YR_NUM")]

#calculate CC  top box score
pagg[, Q2_2_TB_SCORE := round(Q2_2_TB_CNT/Q2_2_RESPONSE_TOTAL,4)*100]

#drop if stores don't have LYSales
pagg <- na.omit(pagg, cols=c("Sales", "LYSales"))
pagg <- pagg[Sales>0&LYSales>0]

#calculate comps
pagg[, comps := (Sales-LYSales)/LYSales]
pagg <- pagg[comps>=-.25&comps<=.25]
# #calculate TSDs
pagg[, TSD := (CustTrans)/day_count]
pagg <- pagg[TSD>=900&TSD<=1800]


#split by cc
prob = c(1/4, 2/4, 3/4, 1)
temp <- pagg %>% group_by(FSCL_YR_NUM) %>% summarise(
  cc25 = quantile(Q2_2_TB_SCORE, probs = prob[1], na.rm = T),
  cc50 = quantile(Q2_2_TB_SCORE, probs = prob[2], na.rm = T),
  cc75 = quantile(Q2_2_TB_SCORE, probs = prob[3], na.rm = T),
  cc100 = quantile(Q2_2_TB_SCORE, probs = prob[4], na.rm = T)
)
pagg <- left_join(pagg, temp,by="FSCL_YR_NUM")
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
                    compsavg=(sum(Sales)-sum(LYSales))/sum(LYSales)),
             by="ccquartile"]
pagg[, Q2_2_TB_SCORE := Q2_2_TB_CNT/Q2_2_RESPONSE_TOTAL]
#order by quartile
pagg <- setorder(pagg,ccquartile)
# pagg <- cbind(pagg,t(temp)[2:5])
# setnames(pagg,"V2","cc_q_value")
#make more presentable
pagg[, (colnames(pagg)[4:5]) := lapply(.SD, function(x) round((x*100),1)), .SDcols=colnames(pagg)[4:5]]
pagg[, Q2_2_RESPONSE_TOTAL := NULL]; pagg[, Q2_2_TB_CNT := NULL]

# #export data
# paggx <- pagg[, .(ccquartile,compsavg,Q2_2_TB_SCORE,cc_q_value)]
# setnames(paggx,"cc_q_value","ccquartile_cutoff_value")
# paggx[, ccquartile_cutoff_value := as.numeric(ccquartile_cutoff_value)]
# paggx[, (colnames(paggx)[2:ncol(paggx)]) := lapply(.SD,function(x) round(x,4)*100),.SDcols=colnames(paggx)[2:ncol(paggx)]]

pagg








#read data
ce <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc-so_bystoreperformance_Canada_FY18Q2.csv")
ce <- ce[, .(STORE_NUM,FSCL_YR_NUM,Q2_2_RESPONSE_TOTAL,Q2_2_TB_CNT)]
comp <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/Comps_by_store_ca_q2fy18.csv")

#make consistent
ce[, STORE_NUM := as.numeric(STORE_NUM)]
# ce[, (grep("TB_SCORE",colnames(ce),value=T)) := lapply(.SD,as.numeric), .SDcols=grep("TB_SCORE",colnames(ce),value=T)]
setnames(comp,"STORE_NUMBER","STORE_NUM")

#merge by store number
pfull <- left_join(ce,comp,by=c("STORE_NUM","FSCL_YR_NUM"))
setDT(pfull)

# 
# #change store number and date values to numeric from characters
# p1[, STORE_NUM := lapply(.SD, as.numeric), .SDcols = "STORE_NUM"]
# #merge by store number, month, and year
# pfull <- Reduce(function(x,y) {merge(x,y,by=c("STORE_NUM"),all.x=TRUE)}, list(p1,p2))

#aggregate for quarter
pagg <- pfull[, lapply(.SD, sum, na.rm=T), .SDcols=c("Q2_2_RESPONSE_TOTAL","Q2_2_TB_CNT",
                                                     "Sales","LYSales","CustTrans","day_count"),
              by=c("STORE_NUM","FSCL_YR_NUM")]

#calculate CC  top box score
pagg[, Q2_2_TB_SCORE := round(Q2_2_TB_CNT/Q2_2_RESPONSE_TOTAL,4)*100]

#drop if stores don't have LYSales
pagg <- na.omit(pagg, cols=c("Sales", "LYSales"))
pagg <- pagg[Sales>0&LYSales>0]

#calculate comps
pagg[, comps := (Sales-LYSales)/LYSales]
pagg <- pagg[comps>=-.25&comps<=.25]
# #calculate TSDs
pagg[, TSD := (CustTrans)/day_count]
pagg <- pagg[TSD>=900&TSD<=1800]


#split by cosd
prob = c(1/4, 2/4, 3/4, 1)
temp <- pagg %>% group_by(FSCL_YR_NUM) %>% summarise(
  TSD25 = quantile(TSD, probs = prob[1], na.rm = T),
  TSD50 = quantile(TSD, probs = prob[2], na.rm = T),
  TSD75 = quantile(TSD, probs = prob[3], na.rm = T),
  TSD100 = quantile(TSD, probs = prob[4], na.rm = T)
)
pagg <- left_join(pagg, temp,by="FSCL_YR_NUM")
setDT(pagg)

##to view quartile cut-offs
#temp2 <- as.data.table(temp)
#temp2[, (names(temp2[,c(2:5)])) := lapply(.SD, function(x) round(x,3)), .SDcols=names(temp2[,c(2:5)])]

#recode TSD based on quartiles
pagg[TSD <= TSD25, TSDquartile := 1]
pagg[TSD > TSD25 & Q2_2_TB_SCORE <= TSD50, TSDquartile := 2]
pagg[TSD > TSD50 & Q2_2_TB_SCORE <= TSD75, TSDquartile := 3]
pagg[TSD > TSD75, TSDquartile := 4]
#drop quartile columns
#pagg <- pagg[, !(names(pagg[,c(10:13)])), with=FALSE]

#calculate top box score for cc, by comps quartile
pagg <- pagg[, list(Q2_2_RESPONSE_TOTAL=sum(Q2_2_RESPONSE_TOTAL,na.rm=T),
                    Q2_2_TB_CNT=sum(Q2_2_TB_CNT,na.rm=T)),
             by="TSDquartile"]
pagg[, Q2_2_TB_SCORE := Q2_2_TB_CNT/Q2_2_RESPONSE_TOTAL]
#order by quartile
pagg <- setorder(pagg,TSDquartile)
# pagg <- cbind(pagg,t(temp)[2:5])
# setnames(pagg,"V2","cc_q_value")
#make more presentable
pagg[, (colnames(pagg)[4:5]) := lapply(.SD, function(x) round((x*100),1)), .SDcols=colnames(pagg)[4:5]]
pagg[, Q2_2_RESPONSE_TOTAL := NULL]; pagg[, Q2_2_TB_CNT := NULL]

# #export data
# paggx <- pagg[, .(ccquartile,compsavg,Q2_2_TB_SCORE,cc_q_value)]
# setnames(paggx,"cc_q_value","ccquartile_cutoff_value")
# paggx[, ccquartile_cutoff_value := as.numeric(ccquartile_cutoff_value)]
# paggx[, (colnames(paggx)[2:ncol(paggx)]) := lapply(.SD,function(x) round(x,4)*100),.SDcols=colnames(paggx)[2:ncol(paggx)]]

pagg

