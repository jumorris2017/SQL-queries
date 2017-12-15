##Same customer, item-level, CE and behavior pre-and post- scores##
##Goal: Following customers 15 days after Purchase 1, and 15 days after Purchase 2##
####Assess if behavior changes *after* different CE raings##
####e.g., If at Purchase 1, they rate a 7, do they come back more frequenly,
####than if at Purchase 2, they rate a 6. 

#12/14/17 run has 1,119,724 records

#load libraries
library(data.table)
library(xlsx)
library(ggplot2)

#load data
scus <- read.csv("O:/CoOp/CoOp194_PROReportng&OM/Julie/SameCust_02_pre-post-surveypairs.csv")
setDT(scus)

#
##remove once no longer person-date-level
listofvars <- colnames(scus)[c(5:13,22:30)]
scus <- scus[, lapply(.SD, as.numeric)]
scus[, (listofvars) := lapply(.SD, function(x) ifelse(x==9,'NA',x)), .SDcols=listofvars]

#ANALYSIS 1: FOCUS ONLY ON POST-PERIODS

#create subsetting variable for change in CC score
# scus[CC_1==7&CC_2==7, ccdelgrp := "7to7"]
# scus[CC_1==7&CC_2==6, ccdelgrp := "7to6"]
# scus[CC_1==7&CC_2<=5, ccdelgrp := "7to5orless"]
# scus[CC_1==6&CC_2==6, ccdelgrp := "6to6"]
# scus[CC_1==6&CC_2==5, ccdelgrp := "6to5"]
# scus[CC_1==6&CC_2<=4, ccdelgrp := "6to4orless"]
# scus[CC_1<=5&CC_2<=5, ccdelgrp := "5orlessto5orless"]
# scus[CC_2==7&CC_2==6, ccdelgrp := "6to7"]
# scus[CC_2==7&CC_2<=5, ccdelgrp := "5orlessto7"]
# scus[CC_2==6&CC_2==5, ccdelgrp := "5to6"]
# scus[CC_2==6&CC_2<=4, ccdelgrp := "4orlessto6"]

scus[CC_1==CC_2, ccdelgrp := "07-same"]
# scus[CC_1==7&CC_2==7, ccdelgrp := "7_both"]
# scus[CC_1==6&CC_2==6, ccdelgrp := "6_both"]
# scus[CC_1==5&CC_2==5, ccdelgrp := "5_both"]
# scus[CC_1==4&CC_2==4, ccdelgrp := "4_both"]
# scus[CC_1==3&CC_2==3, ccdelgrp := "3_both"]
# scus[CC_1==2&CC_2==2, ccdelgrp := "2_both"]
# scus[CC_1==1&CC_2==1, ccdelgrp := "1_both"]
scus[CC_1+1==CC_2, ccdelgrp := "08-CC2_hi_1pt"]
scus[CC_1+2==CC_2, ccdelgrp := "09-CC2_hi_2pt"]
scus[CC_1+3==CC_2, ccdelgrp := "10-CC2_hi_3pt"]
scus[CC_1+4==CC_2, ccdelgrp := "11-CC2_hi_4pt"]
scus[CC_1+5==CC_2, ccdelgrp := "12-CC2_hi_5pt"]
scus[CC_1+6==CC_2, ccdelgrp := "13-CC2_hi_6pt"]
scus[CC_1-1==CC_2, ccdelgrp := "06-CC2_lo_1pt"]
scus[CC_1-2==CC_2, ccdelgrp := "05-CC2_lo_2pt"]
scus[CC_1-3==CC_2, ccdelgrp := "04-CC2_lo_3pt"]
scus[CC_1-4==CC_2, ccdelgrp := "03-CC2_lo_4pt"]
scus[CC_1-5==CC_2, ccdelgrp := "02-CC2_lo_5pt"]
scus[CC_1-6==CC_2, ccdelgrp := "01-CC2_lo_6pt"]

#assess differences in behavior across *groups* of people based on ccdelgrp
temp <- scus[!is.na(ccdelgrp), list(n = .N,
                 VISITS_POST_15D_1 = sum(VISITS_POST_15D_1,na.rm=T),
                 VISITS_POST_15D_2 = sum(VISITS_POST_15D_2,na.rm=T),
                 SPEND_POST_15D_1 = sum(SPEND_POST_15D_1,na.rm=T),
                 SPEND_POST_15D_2 = sum(SPEND_POST_15D_2,na.rm=T),
                 avg_vis_post_1 = sum(VISITS_POST_15D_1,na.rm=T)/.N,
                 avg_vis_post_2 = sum(VISITS_POST_15D_2,na.rm=T)/.N,
                 avg_spend_post_1 = sum(SPEND_POST_15D_1,na.rm=T)/.N,
                 avg_spend_post_2 = sum(SPEND_POST_15D_2,na.rm=T)/.N),
             by=c("ccdelgrp")]
#calculate spend per visits
temp[, spv_post_1 := SPEND_POST_15D_1/VISITS_POST_15D_1]
temp[, spv_post_2 := SPEND_POST_15D_2/VISITS_POST_15D_2]
#calculate total spend and visit deltas
temp[, visdelta := VISITS_POST_15D_2 - VISITS_POST_15D_1]
temp[, spenddelta := SPEND_POST_15D_2 - SPEND_POST_15D_1]
#calculate spend per visit delta
temp[, spvdelta := spv_post_2 - spv_post_1]
#calculate spend per person delta
temp[, sppdelta := avg_spend_post_2 - avg_spend_post_1]

#use "same" as control
temp[ccdelgrp!="07-same", svpdeltafromsame := spvdelta - temp[ccdelgrp=="07-same",spvdelta]]
temp[ccdelgrp!="07-same", spenddeltafromsame := spenddelta - temp[ccdelgrp=="07-same",spenddelta]]
temp[ccdelgrp!="07-same", visdeltafromsame := visdelta - temp[ccdelgrp=="07-same",visdelta]]
temp[ccdelgrp!="07-same", sppdeltafromsame := sppdelta - temp[ccdelgrp=="07-same",sppdelta]]
temp <- setorder(temp,ccdelgrp)

#
ggplot(data = temp, aes(x = ccdelgrp, y = sppdeltafromsame)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1))








#assess differences in behavior by time 1 CC and time 2 CC
temp2 <- scus[, list(VISITS_POST_15D_1 = sum(VISITS_POST_15D_1,na.rm=T),
                 VISITS_POST_15D_2 = sum(VISITS_POST_15D_2,na.rm=T),
                 SPEND_POST_15D_1 = sum(SPEND_POST_15D_1,na.rm=T),
                 SPEND_POST_15D_2 = sum(SPEND_POST_15D_2,na.rm=T)),
             by=c("CC_1","CC_2")]
temp2[, visdelta := VISITS_POST_15D_2 - VISITS_POST_15D_1]
temp2[, spenddelta := SPEND_POST_15D_2 - SPEND_POST_15D_1]

#ANALYSIS 2: LOOK AT VISIT AND SPEND PRE-TO-POST DELTAS

#assess differences in behavior across *groups* of people based on ccdelgrp
temp <- scus[, list(VISITS_PRE_15D_1 = sum(VISITS_PRE_15D_1,na.rm=T),
                 VISITS_PRE_15D_2 = sum(VISITS_PRE_15D_2,na.rm=T),
                 VISITS_POST_15D_1 = sum(VISITS_POST_15D_1,na.rm=T),
                 VISITS_POST_15D_2 = sum(VISITS_POST_15D_2,na.rm=T),
                 SPEND_PRE_15D_1 = sum(SPEND_PRE_15D_1,na.rm=T),
                 SPEND_PRE_15D_2 = sum(SPEND_PRE_15D_2,na.rm=T),
                 SPEND_POST_15D_1 = sum(SPEND_POST_15D_1,na.rm=T),
                 SPEND_POST_15D_2 = sum(SPEND_POST_15D_2,na.rm=T)),
             by=c("ccdelgrp")]
temp[, visdelta_1 := VISITS_POST_15D_1 - VISITS_PRE_15D_1]
temp[, visdelta_2 := VISITS_POST_15D_2 - VISITS_PRE_15D_2]
temp[, spenddelta_1 := SPEND_POST_15D_1 - SPEND_PRE_15D_1]
temp[, spenddelta_2 := SPEND_POST_15D_2 - SPEND_PRE_15D_2]

#assess differences in behavior by time 1 CC and time 2 CC
temp2 <- scus[, list(VISITS_PRE_15D_1 = sum(VISITS_PRE_15D_1,na.rm=T),
                 VISITS_PRE_15D_2 = sum(VISITS_PRE_15D_2,na.rm=T),
                 VISITS_POST_15D_1 = sum(VISITS_POST_15D_1,na.rm=T),
                 VISITS_POST_15D_2 = sum(VISITS_POST_15D_2,na.rm=T),
                 SPEND_PRE_15D_1 = sum(SPEND_PRE_15D_1,na.rm=T),
                 SPEND_PRE_15D_2 = sum(SPEND_PRE_15D_2,na.rm=T),
                 SPEND_POST_15D_1 = sum(SPEND_POST_15D_1,na.rm=T),
                 SPEND_POST_15D_2 = sum(SPEND_POST_15D_2,na.rm=T)),
             by=c("CC_1","CC_2")]
temp2[, visdelta_1 := VISITS_POST_15D_1 - VISITS_PRE_15D_1]
temp2[, visdelta_2 := VISITS_POST_15D_2 - VISITS_PRE_15D_2]
temp2[, spenddelta_1 := SPEND_POST_15D_1 - SPEND_PRE_15D_1]
temp2[, spenddelta_2 := SPEND_POST_15D_2 - SPEND_PRE_15D_2]

temp2[, visdelta := VISITS_POST_15D_2 - VISITS_POST_15D_1]
temp2[, spenddelta := SPEND_POST_15D_2 - SPEND_POST_15D_1]

