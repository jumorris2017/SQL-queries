##R code for SQL query 
##CC by comps for US stores
##Request from Lisa 11/22/17

##load libraries
library(data.table)
library(xlsx)
library(dplyr)
library(ggplot2)
library(flipRegression)
library(lattice)
#library(RColorBrewer)

#slide #11
#load data
#part 1
p1 <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/Comps_by_store_US_pt1_3mo.csv")
#part 2
p2 <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/Comps_by_store_US_pt2_3mo.csv")

#rename merge id columns to match
setnames(p1,c("STORE_NUM"),
         c("STORE_NUMBER"))
#change store number and date values to numeric from characters
p1[, STORE_NUMBER := lapply(.SD, as.numeric), .SDcols = "STORE_NUMBER"]
#merge by store number, month, and year
pfull <- Reduce(function(x,y) {merge(x,y,by=c("STORE_NUMBER","FSCL_WK_IN_YR_NUM","FSCL_YR_NUM"),all.x=TRUE)}, list(p1,p2))

#aggregate for Q4
# pagg <- pfull[, lapply(.SD, sum, na.rm=T), .SDcols=c("Q2_2_RESPONSE_TOTAL","Q2_2_TB_CNT",
#                                                      "QuarterlySales","LYQuarterlySales","CustTrans","day_count"),
#               by=c("STORE_NUMBER","FSCL_YR_NUM")]

#aggregate for rolling 3
pagg <- pfull[, lapply(.SD, sum, na.rm=T), .SDcols=c("Q2_2_RESPONSE_TOTAL","Q2_2_TB_CNT",
                                                     "QuarterlySales","LYQuarterlySales","CustTrans","day_count"),
              by=c("STORE_NUMBER")]
pagg[, FSCL_YR_NUM := "Rolling 3"]

#calculate CC  top box score
pagg[, Q2_2_TB_SCORE := round(Q2_2_TB_CNT/Q2_2_RESPONSE_TOTAL,4)]

#calculate TSDs
pagg[, TSD := (CustTrans)/day_count]
pagg <- pagg[TSD>=700&TSD<=1200]
pagg[, CustTrans := NULL];pagg[, day_count := NULL]

#drop if stores don't have LYQuarterlySales
pagg <- na.omit(pagg, cols=c("QuarterlySales", "LYQuarterlySales"))
pagg <- pagg[QuarterlySales>0&LYQuarterlySales>0]

#calculate comps
pagg[, comps := (QuarterlySales-LYQuarterlySales)/LYQuarterlySales]
pagg <- pagg[comps>=-.25&comps<=.25]

#keep only FY17Q4
#pagg <- pagg[FSCL_QTR_IN_YR_NUM==4&FISCAL_YEAR_NUMBER==2017]

#
pcor.test(pagg[,comps],pagg[,Q2_2_TB_SCORE],pagg[,TSD],method="pearson")


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


# #split by comp
# prob = c(1/4, 2/4, 3/4, 1)
# temp <- pagg %>% group_by(FISCAL_YEAR_NUMBER) %>% summarise( 
#   comp25 = quantile(comps, probs = prob[1], na.rm = T), 
#   comp50 = quantile(comps, probs = prob[2], na.rm = T),
#   comp75 = quantile(comps, probs = prob[3], na.rm = T),
#   comp100 = quantile(comps, probs = prob[4], na.rm = T)
# )
# pagg <- left_join(pagg, temp, by=c("FISCAL_YEAR_NUMBER"))
# setDT(pagg)
# 
# ##to view quartile cut-offs
# #temp2 <- as.data.table(temp)
# #temp2[, (names(temp2[,c(2:5)])) := lapply(.SD, function(x) round(x,3)), .SDcols=names(temp2[,c(2:5)])]
# 
# #recode comp based on quartiles
# pagg[comps <= comp25, compquartile := 1]
# pagg[comps > comp25 & comps <= comp50, compquartile := 2]
# pagg[comps > comp50 & comps <= comp75, compquartile := 3]
# pagg[comps > comp75, compquartile := 4]
# #drop quartile columns
# #pagg <- pagg[, !(names(pagg[,c(10:13)])), with=FALSE]
# 
# #calculate top box score for cc, by comps quartile
# pagg <- pagg[, list(Q2_2_RESPONSE_TOTAL=sum(Q2_2_RESPONSE_TOTAL,na.rm=T),
#                     Q2_2_TB_CNT=sum(Q2_2_TB_CNT,na.rm=T),
#                     compsavg=(sum(QuarterlySales)-sum(LYQuarterlySales))/sum(LYQuarterlySales)),
#              by="compquartile"]
# pagg[, Q2_2_TB_SCORE := Q2_2_TB_CNT/Q2_2_RESPONSE_TOTAL]
# #order by quartile
# pagg <- setorder(pagg,compquartile)
# pagg <- cbind(pagg,t(temp)[2:5])
# setnames(pagg,"V2","comp_q_value")

# ##make comps quartile factor for grouping
# pagg[, ccquartile := as.factor(ccquartile)]
# #set labels
# lname <- "CC Quartile"
# llabels <- c("25th", "50th", "75th", "100th") 
# #plot of comps quartiles with average CC top box score for each
# #set up unique elements
# DT <- copy(pagg)
# maintitle <- "Comps % by CC Top Box Quartile"
# ylabel <- "Comps"
# xlabel <- "Quartile"
# xvar <- DT[,compquartile]
# yvar <- DT[,ccavg]
# yvarcount <- DT[,Q2_2_TB_SCORE]
# pdata <- DT
# #plot
# ggplot(data = pdata, aes(x = xvar, y = yvar*100, fill = "#ADD8E6")) +
#   geom_bar(stat="identity", width = 0.7) + theme_bw() + 
#   ggtitle(maintitle) + guides(fill=FALSE) +
#   geom_text(size = 5, aes(label=paste0("Comps = ",round(yvar,3)*100,"%"),y=0), stat= "identity", vjust = -1.75) +
#   geom_text(size = 5, aes(label=paste0("CC = ",round(yvarcount,3)*100,"%"),y=0), stat= "identity", vjust = -.5) +
#     theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
#         plot.title = element_text(size = 10, face = "bold")) + 
#   labs(x = xlabel, y = ylabel) 

#export data
paggx <- pagg[, .(ccquartile,compsavg,Q2_2_TB_SCORE,cc_q_value)]
setnames(paggx,"cc_q_value","ccquartile_cutoff_value")
paggx[, ccquartile_cutoff_value := as.numeric(ccquartile_cutoff_value)]
paggx[, (colnames(paggx)[2:ncol(paggx)]) := lapply(.SD,function(x) round(x,4)*100),.SDcols=colnames(paggx)[2:ncol(paggx)]]
#write file
write.xlsx(paggx,file="O:/CoOp/CoOp194_PROReportng&OM/Julie/comps_by_cc_quartile_USfy18.xlsx")





##slide #20 - CC and SO by hour
cc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc-so_byhour_US.csv")
#change CC and SO columns to numeric
cols <- c("Q2_1_TB_SCORE","CC_TB_SCORE","Q2_3_TB_SCORE","Q2_4_TB_SCORE","Q2_5_TB_SCORE","Q2_6_TB_SCORE","Q2_7_TB_SCORE")
cc[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]
#compute SO score
cc[, SO_TB_SCORE := rowMeans(.SD, na.rm = T), .SDcols=c("Q2_1_TB_SCORE","Q2_3_TB_SCORE","Q2_4_TB_SCORE",
                                                 "Q2_5_TB_SCORE","Q2_6_TB_SCORE","Q2_7_TB_SCORE")]

cc <- cc[, .(TRANS_HR,CC_TB_SCORE,SO_TB_SCORE)]
cc <- setorder(cc, TRANS_HR)
#write file
write.xlsx(cc,file="O:/CoOp/CoOp194_PROReportng&OM/Julie/cc-so_byhour_US.xlsx")
