##R code for SQL query 
##CC by comps for Canada stores
##Request from Lisa 11/22/17

##load libraries
library(data.table)
library(xlsx)
library(dplyr)
library(ggplot2)
#library(RColorBrewer)

#load data
#part 1
p1 <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/Comps_by_store_Canada_pt1.csv")
#part 2
p2 <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/Comps_by_store_Canada_pt2.csv")

#rename merge id columns to match
setnames(p1,c("STORE_NUM","FSCL_WK_IN_YR_NUM","FSCL_YR_NUM"),
         c("STORE_NUMBER","FISCAL_WEEK_NUMBER","FISCAL_YEAR_NUMBER"))
#change store number and date values to numeric from characters
p1[, STORE_NUMBER := lapply(.SD, as.numeric), .SDcols = "STORE_NUMBER"]
#merge by store number, month, and year
pfull <- Reduce(function(x,y) {merge(x,y,by=c("STORE_NUMBER","FISCAL_WEEK_NUMBER","FISCAL_YEAR_NUMBER","DIV_ORG_LVL_ID"),all.x=TRUE)}, list(p1,p2))

#aggregate for Q4
pagg <- pfull[, lapply(.SD, sum, na.rm=T), .SDcols=c("Q2_2_RESPONSE_TOTAL","Q2_2_TB_CNT",
                                                     "MonthlySales","LYMonthlySales"),
              by=c("STORE_NUMBER","FSCL_QTR_IN_YR_NUM","FISCAL_YEAR_NUMBER")]

#calculate CC  top box score
pagg[, Q2_2_TB_SCORE := round(Q2_2_TB_CNT/Q2_2_RESPONSE_TOTAL,4)]

#drop if stores don't have LYMonthlySales
pagg <- na.omit(pagg, cols=c("MonthlySales", "LYMonthlySales"))
pagg <- pagg[MonthlySales>0&LYMonthlySales>0]

#calculate comps
pagg[, comps := (MonthlySales-LYMonthlySales)/LYMonthlySales]
pagg <- pagg[comps>=-.25&comps<=.25]

#keep only FY17Q4
pagg <- pagg[FSCL_QTR_IN_YR_NUM==4&FISCAL_YEAR_NUMBER==2017]

#split by cc
prob = c(1/4, 2/4, 3/4, 1)
temp <- pagg %>% group_by(FSCL_QTR_IN_YR_NUM,FISCAL_YEAR_NUMBER) %>% summarise( 
  cc25 = quantile(Q2_2_TB_SCORE, probs = prob[1], na.rm = T), 
  cc50 = quantile(Q2_2_TB_SCORE, probs = prob[2], na.rm = T),
  cc75 = quantile(Q2_2_TB_SCORE, probs = prob[3], na.rm = T),
  cc100 = quantile(Q2_2_TB_SCORE, probs = prob[4], na.rm = T)
)
pagg <- left_join(pagg, temp, by=c("FSCL_QTR_IN_YR_NUM","FISCAL_YEAR_NUMBER"))
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
                    compsavg=(sum(MonthlySales)-sum(LYMonthlySales))/sum(LYMonthlySales)),
             by="ccquartile"]
pagg[, Q2_2_TB_SCORE := Q2_2_TB_CNT/Q2_2_RESPONSE_TOTAL]
#order by quartile
pagg <- setorder(pagg,ccquartile)
pagg <- cbind(pagg,t(temp)[3:6])
setnames(pagg,"V2","cc_q_value")

##make comps quartile factor for grouping
pagg[, ccquartile := as.factor(ccquartile)]
#set labels
lname <- "CC Quartile"
llabels <- c("25th", "50th", "75th", "100th") 
#plot of comps quartiles with average CC top box score for each
#set up unique elements
DT <- copy(pagg)
maintitle <- "Comps % by CC Top Box Quartile"
ylabel <- "Comps"
xlabel <- "Quartile"
xvar <- DT[,ccquartile]
yvar <- DT[,compsavg]
yvarcount <- DT[,Q2_2_TB_SCORE]
pdata <- DT
#plot
ggplot(data = pdata, aes(x = xvar, y = yvar*100, fill = "#ADD8E6")) +
  geom_bar(stat="identity", width = 0.7) + theme_bw() + 
  ggtitle(maintitle) + guides(fill=FALSE) +
  geom_text(size = 5, aes(label=paste0("Comps = ",round(yvar,3)*100,"%"),y=0), stat= "identity", vjust = -1.75) +
  geom_text(size = 5, aes(label=paste0("CC = ",round(yvarcount,3)*100,"%"),y=0), stat= "identity", vjust = -.5) +
    theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
        plot.title = element_text(size = 10, face = "bold")) + 
  labs(x = xlabel, y = ylabel) 

#export data
paggx <- pagg[, .(ccquartile,compsavg,Q2_2_TB_SCORE,cc_q_value)]
setnames(paggx,"cc_q_value","ccquartile_cutoff_value")
#write file
write.xlsx(paggx,file="O:/CoOp/CoOp194_PROReportng&OM/Julie/comps_by_cc_quartile_Canada.xlsx")

##slide #18 - CC and SO by day of week
cc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc_so_bydayofweek_canada.csv")
cc[, SO_Total := rowSums(.SD, na.rm = TRUE), 
   .SDcols = c("Q2_1_RESPONSE_TOTAL", "Q2_3_RESPONSE_TOTAL", "Q2_4_RESPONSE_TOTAL",
               "Q2_5_RESPONSE_TOTAL", "Q2_6_RESPONSE_TOTAL", "Q2_7_RESPONSE_TOTAL")]
cc[, SO_TB_CNT := rowSums(.SD, na.rm = TRUE), 
   .SDcols = c("Q2_1_TB_CNT", "Q2_3_TB_CNT", "Q2_4_TB_CNT",
               "Q2_5_TB_CNT", "Q2_6_TB_CNT", "Q2_7_TB_CNT")]
cc[, SO_TB_SCORE := SO_TB_CNT/SO_Total]
cc <- cc[, .(DAY_ABBR_NM,Q2_2_TB_SCORE,SO_TB_SCORE)]
#write file
write.xlsx(cc,file="O:/CoOp/CoOp194_PROReportng&OM/Julie/cc-so_bydayofweek_Canada.xlsx")


##slide #20 - CC and SO by day of week
cc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc-so_byhour_Canada.csv")
cc[, SO_Total := rowSums(.SD, na.rm = TRUE), 
   .SDcols = c("Q2_1_RESPONSE_TOTAL", "Q2_3_RESPONSE_TOTAL", "Q2_4_RESPONSE_TOTAL",
               "Q2_5_RESPONSE_TOTAL", "Q2_6_RESPONSE_TOTAL", "Q2_7_RESPONSE_TOTAL")]
cc[, SO_TB_CNT := rowSums(.SD, na.rm = TRUE), 
   .SDcols = c("Q2_1_TB_CNT", "Q2_3_TB_CNT", "Q2_4_TB_CNT",
               "Q2_5_TB_CNT", "Q2_6_TB_CNT", "Q2_7_TB_CNT")]
cc[, SO_TB_SCORE := SO_TB_CNT/SO_Total]
cc <- cc[, .(TRANS_HR,Q2_2_TB_SCORE,SO_TB_SCORE)]
cc <- setorder(cc, TRANS_HR)
#write file
write.xlsx(cc,file="O:/CoOp/CoOp194_PROReportng&OM/Julie/cc-so_byhour_Canada.xlsx")


##slide #5 - CC, SO, Q1, and WP by store performance
cc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc-so_bystoreperformance_Canada.csv")
cc[, SO_Total := rowSums(.SD, na.rm = TRUE), 
   .SDcols = c("Q2_1_RESPONSE_TOTAL", "Q2_3_RESPONSE_TOTAL", "Q2_4_RESPONSE_TOTAL",
               "Q2_5_RESPONSE_TOTAL", "Q2_6_RESPONSE_TOTAL", "Q2_7_RESPONSE_TOTAL")]
cc[, SO_TB_CNT := rowSums(.SD, na.rm = TRUE), 
   .SDcols = c("Q2_1_TB_CNT", "Q2_3_TB_CNT", "Q2_4_TB_CNT",
               "Q2_5_TB_CNT", "Q2_6_TB_CNT", "Q2_7_TB_CNT")]
cc[, Q1_TDTMRW_SCORE := Q1_TDTMRW_CNT/Q1_RESPONSE_TOTAL]
cc[, CC_TB_SCORE := Q2_2_TB_CNT/Q2_2_RESPONSE_TOTAL]
cc[, SO_TB_SCORE := SO_TB_CNT/SO_Total]
cc[, WP_TB_SCORE := Q2_8_TB_CNT/Q2_8_RESPONSE_TOTAL]
#SO sub categories
cc[, Q2_1_TB_SCORE := Q2_1_TB_CNT/Q2_1_RESPONSE_TOTAL]
cc[, Q2_3_TB_SCORE := Q2_3_TB_CNT/Q2_3_RESPONSE_TOTAL]
cc[, Q2_4_TB_SCORE := Q2_4_TB_CNT/Q2_4_RESPONSE_TOTAL]
cc[, Q2_5_TB_SCORE := Q2_5_TB_CNT/Q2_5_RESPONSE_TOTAL]
cc[, Q2_6_TB_SCORE := Q2_6_TB_CNT/Q2_6_RESPONSE_TOTAL]
cc[, Q2_7_TB_SCORE := Q2_7_TB_CNT/Q2_7_RESPONSE_TOTAL]
#drop old vars
# dropcols <- grep("TB_CNT$|RESPONSE_TOTAL$", colnames(cc))
# cc <- cc[, !dropcols, with=FALSE] #to drop
#
ccquant <- cc[, list(Q1_10 = quantile(Q1_TDTMRW_SCORE,.1,na.rm=T),
                    Q1_90 = quantile(Q1_TDTMRW_SCORE,.9,na.rm=T),
                    CC_10 = quantile(CC_TB_SCORE,.1,na.rm=T),
                    CC_90 = quantile(CC_TB_SCORE,.9,na.rm=T),
                    SO_10 = quantile(SO_TB_SCORE,.1,na.rm=T),
                    SO_90 = quantile(SO_TB_SCORE,.9,na.rm=T),
                    WP_10 = quantile(WP_TB_SCORE,.1,na.rm=T),
                    WP_90 = quantile(WP_TB_SCORE,.9,na.rm=T),
                    Q2_1_10 = quantile(Q2_1_TB_SCORE,.1,na.rm=T),
                    Q2_1_90 = quantile(Q2_1_TB_SCORE,.9,na.rm=T),
                    Q2_3_10 = quantile(Q2_3_TB_SCORE,.1,na.rm=T),
                    Q2_3_90 = quantile(Q2_3_TB_SCORE,.9,na.rm=T),
                    Q2_4_10 = quantile(Q2_4_TB_SCORE,.1,na.rm=T),
                    Q2_4_90 = quantile(Q2_4_TB_SCORE,.9,na.rm=T),
                    Q2_5_10 = quantile(Q2_5_TB_SCORE,.1,na.rm=T),
                    Q2_5_90 = quantile(Q2_5_TB_SCORE,.9,na.rm=T),
                    Q2_6_10 = quantile(Q2_6_TB_SCORE,.1,na.rm=T),
                    Q2_6_90 = quantile(Q2_6_TB_SCORE,.9,na.rm=T),
                    Q2_7_10 = quantile(Q2_7_TB_SCORE,.1,na.rm=T),
                    Q2_7_90 = quantile(Q2_7_TB_SCORE,.9,na.rm=T))]
#
ccavg <- cc[, list(Q1_TDTMRW_SCORE = sum(Q1_TDTMRW_CNT)/sum(Q1_RESPONSE_TOTAL),
                   CC_TB_SCORE = sum(Q2_2_TB_CNT)/sum(Q2_2_RESPONSE_TOTAL),
                   SO_TB_SCORE = sum(SO_TB_CNT)/sum(SO_Total),
                   WP_TB_SCORE = sum(Q2_8_TB_CNT)/sum(Q2_8_RESPONSE_TOTAL),
                   Q2_1_TB_SCORE = sum(Q2_1_TB_CNT)/sum(Q2_1_RESPONSE_TOTAL),
                   Q2_3_TB_SCORE = sum(Q2_3_TB_CNT)/sum(Q2_3_RESPONSE_TOTAL),
                   Q2_4_TB_SCORE = sum(Q2_4_TB_CNT)/sum(Q2_4_RESPONSE_TOTAL),
                   Q2_5_TB_SCORE = sum(Q2_5_TB_CNT)/sum(Q2_5_RESPONSE_TOTAL),
                   Q2_6_TB_SCORE = sum(Q2_6_TB_CNT)/sum(Q2_6_RESPONSE_TOTAL),
                   Q2_7_TB_SCORE = sum(Q2_7_TB_CNT)/sum(Q2_7_RESPONSE_TOTAL))]

ccprint <- cbind(ccavg,ccquant)
ccprint <- ccprint[, lapply(.SD, function(x) round(x*100,0))]
#write file
write.xlsx(ccprint,file="O:/CoOp/CoOp194_PROReportng&OM/Julie/cc-so_bystoreperformance_Canada.xlsx")


##slide 8 - CC and SO by month
cc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc-so_bymonth_Canada.csv")
#data are in long format (CAW...), so, tidy!
#create new variable for CC vs. SO questions
cc[QSTN_ID=='Q2_2', varname := 'CC']
cc[QSTN_ID=='Q2_1'|QSTN_ID=='Q2_3'|QSTN_ID=='Q2_4'|QSTN_ID=='Q2_5'|QSTN_ID=='Q2_6'|QSTN_ID=='Q2_7', varname := 'SO']
cc <- cc[, lapply(.SD,sum,na.rm=T), .SDcols=c("TOTAL_TB","TOTAL_RSPNS"), by=c("CAL_YR_NUM","CAL_MNTH_IN_YR_NUM","varname")]
cc[, TB_Score := TOTAL_TB/TOTAL_RSPNS]
#cast wide
cc <- dcast.data.table(cc, CAL_YR_NUM + CAL_MNTH_IN_YR_NUM ~ varname, value.var="TB_Score")
cc <- setorder(cc,CAL_YR_NUM,CAL_MNTH_IN_YR_NUM)
cc <- cc[, lapply(.SD, function(x) round(x,3)), .SDcols=c("CC","SO"), by=c("CAL_YR_NUM","CAL_MNTH_IN_YR_NUM")]

