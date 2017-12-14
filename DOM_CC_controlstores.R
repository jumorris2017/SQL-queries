##CC by DOM adoption - case and control stores##

#load libraries
library(data.table)
library(xlsx)
library(ggplot2)

#load data
domc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/DOM_CC_controlstores.csv")
domc[, STORE_CNT_CHECK := NULL]
domt <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/DOM_CC_teststores.csv")

#start with control stores

#CONTROL
#aggregate to get all transaction
colnames <- colnames(domc)[c(1,5:12)]
domcagg <- domc[, colnames, with=F]
domcagg <- domcagg[, lapply(.SD,sum,na.rm=T), by=c("TEST_STORE_NUM")]
#create new "mobile order" pay indicator [ALL TRANS] for binding
domcagg[, MOBILE_ORD_PAY_IND := 2]

#TEST
#aggregate to get all transaction
colnames <- colnames(domt)[c(1,5:12)]
domtagg <- domt[, colnames, with=F]
domtagg <- domtagg[, lapply(.SD,sum,na.rm=T), by=c("TEST_STORE_NUM")]
#create new "mobile order" pay indicator [ALL TRANS] for binding
domtagg[, MOBILE_ORD_PAY_IND := 2]

#restrict variables for rbinding
domcs <- domc[, colnames(domc)[-c(2:3)], with=F]
domts <- domt[, colnames(domt)[-c(2:3)], with=F]

#create "control" and "test" indicator variables
domcs[, testcase := 0]
domts[, testcase := 1]
domcagg[, testcase := 0]
domtagg[, testcase := 1]
l <- list(domcs,domts,domcagg,domtagg)
temp <- rbindlist(l, use.names=T, fill=T)

#aggregate over store numbers
domall <- temp[, TEST_STORE_NUM := NULL]
domall <- domall[, lapply(.SD,sum,na.rm=T), by=c("MOBILE_ORD_PAY_IND","testcase")]

#calculate CC scores

#pre - allday
domall[, pre_cc_allday := round(TB_COUNT_PRE/RSPNS_COUNT_PRE,4)*100]
#pre - peak
domall[, pre_cc_peak := round(PEAK_TB_COUNT_PRE/PEAK_RSPNS_COUNT_PRE,4)*100]
#post - all day
domall[, post_cc_allday := round(TB_COUNT_POST/RSPNS_COUNT_POST,4)*100]
#post - peak
domall[, post_cc_peak := round(PEAK_TB_COUNT_POST/PEAK_RSPNS_COUNT_POST,4)*100]

#calculate deltas
domall[, delta_cc_allday := post_cc_allday - pre_cc_allday]
domall[, delta_cc_peak := post_cc_peak - pre_cc_peak]

#reduce number of variables
temp <- domall[, -colnames(domall)[3:10], with=F]
temp <- setorder(temp, testcase, MOBILE_ORD_PAY_IND)
# #melt for easier viewing
# temp <- melt(temp,id.var=c("MOBILE_ORD_PAY_IND","testcase"),
#              value.name="ccscore")
write.csv(temp,file="C:/Users/jumorris/Documents/temp.csv")

#matched pair t-tests
l <- list(domcs,domts,domcagg,domtagg)
temp2 <- rbindlist(l, use.names=T, fill=T)
#calculate CC scores
#pre - allday
temp2[, pre_cc_allday := round(TB_COUNT_PRE/RSPNS_COUNT_PRE,4)*100]
#pre - peak
temp2[, pre_cc_peak := round(PEAK_TB_COUNT_PRE/PEAK_RSPNS_COUNT_PRE,4)*100]
#post - all day
temp2[, post_cc_allday := round(TB_COUNT_POST/RSPNS_COUNT_POST,4)*100]
#post - peak
temp2[, post_cc_peak := round(PEAK_TB_COUNT_POST/PEAK_RSPNS_COUNT_POST,4)*100]
#calculate deltas
temp2[, delta_cc_allday := post_cc_allday - pre_cc_allday]
temp2[, delta_cc_peak := post_cc_peak - pre_cc_peak]

#paired t-tests
#set confidence level
cl <- 0.85

#test cases / cafe / all day
t.test(temp2[MOBILE_ORD_PAY_IND==0&testcase==1,pre_cc_allday],temp2[MOBILE_ORD_PAY_IND==0&testcase==1,post_cc_allday],paired=TRUE,conf.level = cl)
#test cases / cafe / peak
t.test(temp2[MOBILE_ORD_PAY_IND==0&testcase==1,pre_cc_peak],temp2[MOBILE_ORD_PAY_IND==0&testcase==1,post_cc_peak],paired=TRUE,conf.level = cl)
#test cases / MOP / all day
t.test(temp2[MOBILE_ORD_PAY_IND==1&testcase==1,pre_cc_allday],temp2[MOBILE_ORD_PAY_IND==1&testcase==1,post_cc_allday],paired=TRUE,conf.level = cl)
#test cases / MOP / peak
t.test(temp2[MOBILE_ORD_PAY_IND==1&testcase==1,pre_cc_peak],temp2[MOBILE_ORD_PAY_IND==1&testcase==1,post_cc_peak],paired=TRUE,conf.level = cl)
#test cases / all trans / all day
t.test(temp2[MOBILE_ORD_PAY_IND==2&testcase==1,pre_cc_allday],temp2[MOBILE_ORD_PAY_IND==2&testcase==1,post_cc_allday],paired=TRUE,conf.level = cl)
#test cases / all trans / peak
t.test(temp2[MOBILE_ORD_PAY_IND==2&testcase==1,pre_cc_peak],temp2[MOBILE_ORD_PAY_IND==2&testcase==1,post_cc_peak],paired=TRUE,conf.level = cl)

#control cases / cafe / all day
t.test(temp2[MOBILE_ORD_PAY_IND==0&testcase==0,pre_cc_allday],temp2[MOBILE_ORD_PAY_IND==0&testcase==0,post_cc_allday],paired=TRUE,conf.level = cl)
#control cases / cafe / peak
t.test(temp2[MOBILE_ORD_PAY_IND==0&testcase==0,pre_cc_peak],temp2[MOBILE_ORD_PAY_IND==0&testcase==0,post_cc_peak],paired=TRUE,conf.level = cl)
#control cases / MOP / all day
t.test(temp2[MOBILE_ORD_PAY_IND==1&testcase==0,pre_cc_allday],temp2[MOBILE_ORD_PAY_IND==1&testcase==0,post_cc_allday],paired=TRUE,conf.level = cl)
#control cases / MOP / peak
t.test(temp2[MOBILE_ORD_PAY_IND==1&testcase==0,pre_cc_peak],temp2[MOBILE_ORD_PAY_IND==1&testcase==0,post_cc_peak],paired=TRUE)
#control cases / all trans / all day
t.test(temp2[MOBILE_ORD_PAY_IND==2&testcase==0,pre_cc_allday],temp2[MOBILE_ORD_PAY_IND==2&testcase==0,post_cc_allday],paired=TRUE,conf.level = cl)
#control cases / all trans / peak
t.test(temp2[MOBILE_ORD_PAY_IND==2&testcase==0,pre_cc_peak],temp2[MOBILE_ORD_PAY_IND==2&testcase==0,post_cc_peak],paired=TRUE,conf.level = cl)

#note: data are organized by pairing results from 10 control stores to each test store,
#so, the number of control stores is 10x higher than test stores

#get survey response N's
getNs <- domt[,list(RSPNS_COUNT_PRE = sum(RSPNS_COUNT_PRE,na.rm=T),
                       RSPNS_COUNT_POST = sum(RSPNS_COUNT_POST,na.rm=T),
                       PEAK_RSPNS_COUNT_PRE = sum(PEAK_RSPNS_COUNT_PRE,na.rm=T),
                       PEAK_RSPNS_COUNT_POST = sum(PEAK_RSPNS_COUNT_POST,na.rm=T))]





