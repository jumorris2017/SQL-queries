##CC by comps for US stores

##load libraries
library(data.table)
library(xlsx)
library(dplyr)

#slide #11
#load data
#part 1
p1 <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/earnings_Comps_by_store_US_pt1.csv")
#part 2
p2 <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/earnings_CC_by_store_US_pt2.csv")

#rename merge id columns to match
setnames(p2,c("STORE_NUM"),c("STORE_NUMBER"))

#merge by store number, month, and year
pfull <- Reduce(function(x,y) {merge(x,y,by=c("STORE_NUMBER","FSCL_QTR_IN_YR_NUM","FSCL_YR_NUM"),all=TRUE)}, list(p1,p2))

#aggregate
pagg <- pfull[, lapply(.SD, sum, na.rm=T), .SDcols=c("Q2_2_RESPONSE_TOTAL","Q2_2_TB_CNT",
                                                     "QuarterlySales","LYQuarterlySales"),
              by=c("STORE_NUMBER","FSCL_YR_NUM")]
# pagg[, FSCL_YR_NUM := "Rolling 3"]

#calculate CC  top box score
pagg[, Q2_2_TB_SCORE := round(Q2_2_TB_CNT/Q2_2_RESPONSE_TOTAL,4)]

#drop if stores don't have LYQuarterlySales
pagg <- na.omit(pagg, cols=c("QuarterlySales", "LYQuarterlySales"))
pagg <- pagg[QuarterlySales>0&LYQuarterlySales>0]

#calculate comps
pagg[, comps := (QuarterlySales-LYQuarterlySales)/LYQuarterlySales]
pagg <- pagg[comps>=-.275&comps<=.275]

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

#recode cc based on quartiles
pagg[Q2_2_TB_SCORE <= cc25, ccquartile := 1]
pagg[Q2_2_TB_SCORE > cc25 & Q2_2_TB_SCORE <= cc50, ccquartile := 2]
pagg[Q2_2_TB_SCORE > cc50 & Q2_2_TB_SCORE <= cc75, ccquartile := 3]
pagg[Q2_2_TB_SCORE > cc75, ccquartile := 4]

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
setnames(pagg,"cc_q_value","ccquartile_cutoff_value")
