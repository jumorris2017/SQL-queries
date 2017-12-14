##Same customer, item-level, CE and behavior pre-and post- scores##
##Goal: Following customers 15 days after Purchase 1, and 15 days after Purchase 2##
####Assess if behavior changes *after* different CE raings##
####e.g., If at Purchase 1, they rate a 7, do they come back more frequenly,
####than if at Purchase 2, they rate a 6. 

#load libraries
library(data.table)
library(xlsx)
library(ggplot2)

#load data
scus <- read.csv("O:/CoOp/CoOp194_PROReportng&OM/Julie/SameCust_02_pre-post-surveypairs.csv")
setDT(scus)

##remove once no longer person-date-level
scus[, c("GUID_USER_ID","CASE_ID_1","TRANS_DTM_1","TRANS_ID_1",
         "CASE_ID_2","TRANS_DTM_2","TRANS_ID_2") := NULL]


#ANALYSIS 1: FOCUS ONLY ON POST-PERIODS

#create subsetting variable for change in CC score
scus[CC_1==7&CC_2==7, ccdelgrp := "7to7"]
scus[CC_1==7&CC_2==6, ccdelgrp := "7to6"]
scus[CC_1==7&CC_2<=5, ccdelgrp := "7to5orless"]
scus[CC_1==6&CC_2==6, ccdelgrp := "6to6"]
scus[CC_1==6&CC_2==5, ccdelgrp := "6to5"]
scus[CC_1==6&CC_2<=4, ccdelgrp := "6to4orless"]
scus[CC_1<=5&CC_2<=5, ccdelgrp := "5orlessto5orless"]
scus[CC_2==7&CC_2==6, ccdelgrp := "6to7"]
scus[CC_2==7&CC_2<=5, ccdelgrp := "5orlessto7"]
scus[CC_2==6&CC_2==5, ccdelgrp := "5to6"]
scus[CC_2==6&CC_2<=4, ccdelgrp := "4orlessto6"]

#assess differences in behavior across *groups* of people based on ccdelgrp
temp <- scus[, c(VISITS_POST_15D_1 = sum(VISITS_POST_15D_1),
                 VISITS_POST_15D_2 = sum(VISITS_POST_15D_2),
                 SPEND_POST_15D_1 = sum(SPEND_POST_15D_1),
                 SPEND_POST_15D_2 = sum(SPEND_POST_15D_2)),
             by=c("ccdelgrp")]
temp[, visdelta := VISITS_POST_15D_2 - VISITS_POST_15D_1]
temp[, spenddelta := SPEND_POST_15D_2 - SPEND_POST_15D_1]

#assess differences in behavior by time 1 CC and time 2 CC
temp2 <- scus[, c(VISITS_POST_15D_1 = sum(VISITS_POST_15D_1),
                 VISITS_POST_15D_2 = sum(VISITS_POST_15D_2),
                 SPEND_POST_15D_1 = sum(SPEND_POST_15D_1),
                 SPEND_POST_15D_2 = sum(SPEND_POST_15D_2)),
             by=c("CC_1","CC_2")]
temp2[, visdelta := VISITS_POST_15D_2 - VISITS_POST_15D_1]
temp2[, spenddelta := SPEND_POST_15D_2 - SPEND_POST_15D_1]

#ANALYSIS 2: LOOK AT VISIT AND SPEND PRE-TO-POST DELTAS

#assess differences in behavior across *groups* of people based on ccdelgrp
temp <- scus[, c(VISITS_PRE_15D_1 = sum(VISITS_PRE_15D_1),
                 VISITS_PRE_15D_2 = sum(VISITS_PRE_15D_2),
                 VISITS_POST_15D_1 = sum(VISITS_POST_15D_1),
                 VISITS_POST_15D_2 = sum(VISITS_POST_15D_2),
                 SPEND_PRE_15D_1 = sum(SPEND_PRE_15D_1),
                 SPEND_PRE_15D_2 = sum(SPEND_PRE_15D_2),
                 SPEND_POST_15D_1 = sum(SPEND_POST_15D_1),
                 SPEND_POST_15D_2 = sum(SPEND_POST_15D_2)),
             by=c("ccdelgrp")]
temp[, visdelta_1 := VISITS_POST_15D_1 - VISITS_PRE_15D_1]
temp[, visdelta_2 := VISITS_POST_15D_2 - VISITS_PRE_15D_2]
temp[, spenddelta_1 := SPEND_POST_15D_1 - SPEND_PRE_15D_1]
temp[, spenddelta_2 := SPEND_POST_15D_2 - SPEND_PRE_15D_2]

#assess differences in behavior by time 1 CC and time 2 CC
temp2 <- scus[, c(VISITS_PRE_15D_1 = sum(VISITS_PRE_15D_1),
                 VISITS_PRE_15D_2 = sum(VISITS_PRE_15D_2),
                 VISITS_POST_15D_1 = sum(VISITS_POST_15D_1),
                 VISITS_POST_15D_2 = sum(VISITS_POST_15D_2),
                 SPEND_PRE_15D_1 = sum(SPEND_PRE_15D_1),
                 SPEND_PRE_15D_2 = sum(SPEND_PRE_15D_2),
                 SPEND_POST_15D_1 = sum(SPEND_POST_15D_1),
                 SPEND_POST_15D_2 = sum(SPEND_POST_15D_2)),
             by=c("CC_1","CC_2")]
temp2[, visdelta_1 := VISITS_POST_15D_1 - VISITS_PRE_15D_1]
temp2[, visdelta_2 := VISITS_POST_15D_2 - VISITS_PRE_15D_2]
temp2[, spenddelta_1 := SPEND_POST_15D_1 - SPEND_PRE_15D_1]
temp2[, spenddelta_2 := SPEND_POST_15D_2 - SPEND_PRE_15D_2]

temp2[, visdelta := VISITS_POST_15D_2 - VISITS_POST_15D_1]
temp2[, spenddelta := SPEND_POST_15D_2 - SPEND_POST_15D_1]

#
