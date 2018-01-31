##Analysis of Partner Demographics by Dominant Day-Part Worked
##Author: Julie Morris

##load libraries
library(foreign)
library(data.table)
library(ggplot2)
library(tidyverse)
library(xlsx)
set.seed(98115)

#read in daypart data
dp <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/partners_bar-ss_bydaypart_dec4-17_2017.csv") 
# dp <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/partner_domdaypart.csv") #baristas
# dp <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/partner_domdaypart_shifts.csv") #shifts
# setnames(dp,"PRTNR_NUM","PartnerID")

#recalcualate PM_DOM as necessary
dp[, earlyam_prp := round(EARLYAM_SHIFTS/SHIFTS_WORKED,3)]
dp[, am_prp := round(AM_SHIFTS/SHIFTS_WORKED,3)]
dp[, midday_prp := round(MIDDAY_SHIFTS/SHIFTS_WORKED,3)]
dp[, pm_prp := round(PM_SHIFTS/SHIFTS_WORKED,3)]
dp[, latepm_prp := round(LATEPM_SHIFTS/SHIFTS_WORKED,3)]
#calculate dominate daypart
dp[, ddp_tie_first := colnames(.SD)[max.col(.SD, ties.method="first")], .SDcols = c("earlyam_prp","am_prp","midday_prp","pm_prp","latepm_prp")]
dp[, ddp_tie_last := colnames(.SD)[max.col(.SD, ties.method="last")], .SDcols = c("earlyam_prp","am_prp","midday_prp","pm_prp","latepm_prp")]
#create indicator for a tie
dp[ddp_tie_first==ddp_tie_last, ddp_tie_exists := 0]
dp[ddp_tie_first!=ddp_tie_last, ddp_tie_exists := 1]

#write to .csv
write.csv(dp,"O:/CoOp/CoOp194_PROReportng&OM/Julie/baristas-shifts_bydaypart_dec4-17_2017.csv")


#for selecting sample, pull in panel ids
dp <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/baristas-shifts_bydaypart_dec4-17_2017.csv")
dp[, V1 := NULL]
panel <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/panelistsfordaypart.csv") 
setnames(panel,c("PanelistAlternateId"),c("PRTNR_NUM"))
#keep dp rows where partner numbers are in the panelist id file
dp <- dp[PRTNR_NUM %in% unique(panel[,PRTNR_NUM])]
dp <- dp[!duplicated(dp$PRTNR_NUM),]
panel <- panel[PRTNR_NUM %in% unique(dp[,PRTNR_NUM])]
panel <- panel[!duplicated(panel$PRTNR_NUM),]
dpanel <- merge(dp,panel,by="PRTNR_NUM",all=F)

#exclude ties
dpanel <- dpanel[ddp_tie_exists==0]
dpanel[JOB_ID==50000362&ddp_tie_first=="am_prp"&pm_prp==0&latepm_prp==0, group := 1] #AM BAR (NO PM)
dpanel[JOB_ID==50000362&ddp_tie_first=="am_prp"&(pm_prp>0|latepm_prp>0), group := 2] #AM BAR (some PM)
dpanel[JOB_ID==50000358&ddp_tie_first=="am_prp"&pm_prp==0&latepm_prp==0, group := 3] #AM SHIFT (NO PM)
dpanel[JOB_ID==50000358&ddp_tie_first=="am_prp"&(pm_prp>0|latepm_prp>0), group := 4] #AM SHIFT (some PM)
dpanel[JOB_ID==50000362&ddp_tie_first=="pm_prp", group := 5] #PM BAR
dpanel[JOB_ID==50000358&ddp_tie_first=="pm_prp", group := 6] #PM SHIFT
dpanel[JOB_ID==50000358&ddp_tie_first=="latepm_prp", group := 7] #LATE PM SHIFT
dpanel <- na.omit(dpanel,cols="group")

#randomly assign values
randcol <- runif(nrow(dpanel), min=0, max=1)
dpanel <- cbind(dpanel,randcol)

#rank by group
dpanel <- dpanel %>%
  group_by(group) %>%
  mutate(rankcol = order(randcol,decreasing=TRUE))
setDT(dpanel)

#select sample
dpanel <- dpanel[(group==1&rankcol<=300)|(group==2&rankcol<=200)|(group==3&rankcol<=300)|(group==4&rankcol<=200)|(group==5&rankcol<=500)|(group==6&rankcol<=257)|(group==7&rankcol<=243)]

#keep only partner id and panelist id
panelists <- dpanel[, .(PanelistIdQuestion)]

#write to .xlsx
write.xlsx(panelists,"O:/CoOp/CoOp194_PROReportng&OM/Julie/panelists_for_daypart_survey.xlsx")


###SURVEY RESULTS

#for selecting sample, pull in panel ids
dp <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/baristas-shifts_bydaypart_dec4-17_2017.csv")
dp[, V1 := NULL]
panel <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/panelistsfordaypart.csv") 
setnames(panel,c("PanelistAlternateId"),c("PRTNR_NUM"))
#keep dp rows where partner numbers are in the panelist id file
dp <- dp[PRTNR_NUM %in% unique(panel[,PRTNR_NUM])]
dp <- dp[!duplicated(dp$PRTNR_NUM),]
panel <- panel[PRTNR_NUM %in% unique(dp[,PRTNR_NUM])]
panel <- panel[!duplicated(panel$PRTNR_NUM),]
dpanel <- merge(dp,panel,by="PRTNR_NUM",all=F)

#exclude ties
dpanel <- dpanel[ddp_tie_exists==0]
dpanel[JOB_ID==50000362&ddp_tie_first=="am_prp"&pm_prp==0&latepm_prp==0, group := 1] #AM BAR (NO PM)
dpanel[JOB_ID==50000362&ddp_tie_first=="am_prp"&(pm_prp>0|latepm_prp>0), group := 2] #AM BAR (some PM)
dpanel[JOB_ID==50000358&ddp_tie_first=="am_prp"&pm_prp==0&latepm_prp==0, group := 3] #AM SHIFT (NO PM)
dpanel[JOB_ID==50000358&ddp_tie_first=="am_prp"&(pm_prp>0|latepm_prp>0), group := 4] #AM SHIFT (some PM)
dpanel[JOB_ID==50000362&ddp_tie_first=="pm_prp", group := 5] #PM BAR
dpanel[JOB_ID==50000358&ddp_tie_first=="pm_prp", group := 6] #PM SHIFT
dpanel[JOB_ID==50000358&ddp_tie_first=="latepm_prp", group := 7] #LATE PM SHIFT
dpanel <- na.omit(dpanel,cols="group")

#READ IN SURVEY RESULTS
surv <- read.spss("//starbucks/amer/portal/Departments/WMO/Marketing Research/New Q drive/Partner Insights/Partner Perspectives/Research/Day Part Research/Final_30.01.18_12.06.12.PM.sav", use.value.labels = FALSE, to.data.frame=TRUE)
setDT(surv)
# surv <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/Partial_Survey_Results.csv")
dpanel <- merge(dpanel,surv,by="PanelistIdQuestion")
##
dpanel[,.N,by=c("JOB_ID","ddp_tie_first")]
#barista (362): am 124, pm 93 = 217
#shift (368): am 122, pm 44, 52 (pm: 96) = 218
dpanel[,.N,by=c("JOB_ID","Q3_Daypart_MostOften")]
#barista (362): early am 42, am 73, mid 51, pm 26, late pm 25 = 217
#shift (358): early am 76, am 45, mid 35, pm 25, late pm 37 = 218
tempmat <- dpanel[,.N,by=c("JOB_ID","ddp_tie_first","Q3_Daypart_MostOften")]
setorder(tempmat,JOB_ID,ddp_tie_first,Q3_Daypart_MostOften)
write.xlsx(tempmat,"O:/CoOp/CoOp194_PROReportng&OM/Julie/temp_final_survey.xlsx")

#subset to just baristas who work only am
#temp <- dpanel[JOB_ID==50000362&group==1]
temp <- dpanel[,.(JOB_ID,ddp_tie_first,Q2_Dayparts_Worked_EarlyAMbefore7am,
                 Q2_Dayparts_Worked_AM711am,Q2_Dayparts_Worked_Midday11am2pm,Q2_Dayparts_Worked_PM25pm,
                 Q2_Dayparts_Worked_LatePMafter5pm,Q3_Daypart_MostOften,
                 Q4a_Why_NotWork_AM,Q4b_Why_NotWork_PM,
                 Q5a_Why_UnableWork_4a_Iamastudentandattendschoolduringthattimeof,
                 Q5a_Why_UnableWork_4a_Iamresponsiblefortakingcareofotheregmychil,
                 Q5a_Why_UnableWork_4a_Ihaveanotherjobduringthattimeoftheday,
                 Q5b_Why_UnableWork_4b_Iamastudentandattendschoolduringthattimeof,
                 Q5b_Why_UnableWork_4b_Iamresponsiblefortakingcareofotheregmychil,
                 Q5b_Why_UnableWork_4b_Ihaveanotherjobduringthattimeoftheday)]
# temp2 <- temp[,lapply(.SD,sum,na.rm=T),.SDcols=colnames(temp)[3:ncol(temp)],by="JOB_ID"]
nrow(temp[JOB_ID==50000362])
nrow(temp[JOB_ID==50000358])


tempbin <- dpanel %>%
  group_by(JOB_ID) %>%
  summarize(n = n(), 
            q5a_schoolN = sum(Q5a_Why_UnableWork_4a_Iamastudentandattendschoolduringthattimeof,na.rm=T),
            q5a_school = round(mean(Q5a_Why_UnableWork_4a_Iamastudentandattendschoolduringthattimeof,na.rm=T),3),
            q5a_careN = sum(Q5a_Why_UnableWork_4a_Iamresponsiblefortakingcareofotheregmychil,na.rm=T),
            q5a_care = round(mean(Q5a_Why_UnableWork_4a_Iamresponsiblefortakingcareofotheregmychil,na.rm=T),3),
            q5a_jobN = sum(Q5a_Why_UnableWork_4a_Ihaveanotherjobduringthattimeoftheday,na.rm=T),
            q5a_job = round(mean(Q5a_Why_UnableWork_4a_Ihaveanotherjobduringthattimeoftheday,na.rm=T),3),
            q5b_schoolN = sum(Q5b_Why_UnableWork_4b_Iamastudentandattendschoolduringthattimeof,na.rm=T),
            q5b_school = round(mean(Q5b_Why_UnableWork_4b_Iamastudentandattendschoolduringthattimeof,na.rm=T),3),
            q5b_careN = sum(Q5b_Why_UnableWork_4b_Iamresponsiblefortakingcareofotheregmychil,na.rm=T),
            q5b_care = round(mean(Q5b_Why_UnableWork_4b_Iamresponsiblefortakingcareofotheregmychil,na.rm=T),3),
            q5b_jobN = sum(Q5b_Why_UnableWork_4b_Ihaveanotherjobduringthattimeoftheday,na.rm=T),
            q5b_job = round(mean(Q5b_Why_UnableWork_4b_Ihaveanotherjobduringthattimeoftheday,na.rm=T),3))
setDT(tempbin)
write.xlsx(tempbin,"O:/CoOp/CoOp194_PROReportng&OM/Julie/temp_final_survey_q5.xlsx")

tempbin2 <- dpanel %>%
  group_by(JOB_ID,Q3_Daypart_MostOften) %>%
  summarize(n = n(), 
            q5a_schoolN = sum(Q5a_Why_UnableWork_4a_Iamastudentandattendschoolduringthattimeof,na.rm=T),
            q5a_school = round(mean(Q5a_Why_UnableWork_4a_Iamastudentandattendschoolduringthattimeof,na.rm=T),3),
            q5a_careN = sum(Q5a_Why_UnableWork_4a_Iamresponsiblefortakingcareofotheregmychil,na.rm=T),
            q5a_care = round(mean(Q5a_Why_UnableWork_4a_Iamresponsiblefortakingcareofotheregmychil,na.rm=T),3),
            q5a_jobN = sum(Q5a_Why_UnableWork_4a_Ihaveanotherjobduringthattimeoftheday,na.rm=T),
            q5a_job = round(mean(Q5a_Why_UnableWork_4a_Ihaveanotherjobduringthattimeoftheday,na.rm=T),3),
            q5b_schoolN = sum(Q5b_Why_UnableWork_4b_Iamastudentandattendschoolduringthattimeof,na.rm=T),
            q5b_school = round(mean(Q5b_Why_UnableWork_4b_Iamastudentandattendschoolduringthattimeof,na.rm=T),3),
            q5b_careN = sum(Q5b_Why_UnableWork_4b_Iamresponsiblefortakingcareofotheregmychil,na.rm=T),
            q5b_care = round(mean(Q5b_Why_UnableWork_4b_Iamresponsiblefortakingcareofotheregmychil,na.rm=T),3),
            q5b_jobN = sum(Q5b_Why_UnableWork_4b_Ihaveanotherjobduringthattimeoftheday,na.rm=T),
            q5b_job = round(mean(Q5b_Why_UnableWork_4b_Ihaveanotherjobduringthattimeoftheday,na.rm=T),3))
setDT(tempbin2)
write.xlsx(tempbin2,"O:/CoOp/CoOp194_PROReportng&OM/Julie/temp_final_survey_q5_bydaypart.xlsx")


# #afternoon over-performers
# af <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/afternoon_overperformers.csv")
# cc1yr <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC_daypart_bystore_4quarters.csv") #4 quarters
# cc1yr <- cc1yr[STORE_NUM %in% af[,STORE_NUM]]
# cc1yr[, QSTN_ID := NULL]

# #read in daypart data
# dp18 <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/partners_bar-ss_bydaypart_jan8-21_2018.csv") 
# 
# #recalcualate PM_DOM as necessary
# dp18[, earlyam_prp := round(EARLYAM_SHIFTS/SHIFTS_WORKED,3)]
# dp18[, am_prp := round(AM_SHIFTS/SHIFTS_WORKED,3)]
# dp18[, midday_prp := round(MIDDAY_SHIFTS/SHIFTS_WORKED,3)]
# dp18[, pm_prp := round(PM_SHIFTS/SHIFTS_WORKED,3)]
# dp18[, latepm_prp := round(LATEPM_SHIFTS/SHIFTS_WORKED,3)]
# #calculate dominate daypart
# dp18[, ddp_tie_first := colnames(.SD)[max.col(.SD, ties.method="first")], .SDcols = c("earlyam_prp","am_prp","midday_prp","pm_prp","latepm_prp")]
# dp18[, ddp_tie_last := colnames(.SD)[max.col(.SD, ties.method="last")], .SDcols = c("earlyam_prp","am_prp","midday_prp","pm_prp","latepm_prp")]
# #create indicator for a tie
# dp18[ddp_tie_first==ddp_tie_last, ddp_tie_exists := 0]
# dp18[ddp_tie_first!=ddp_tie_last, ddp_tie_exists := 1]
# 
# #write to .csv
# write.csv(dp18,"O:/CoOp/CoOp194_PROReportng&OM/Julie/baristas-shifts_bydaypart_jan8-21_2018.csv")


# #merge into 2017
# dp18 <- dp18[, .(PRTNR_NUM,ddp_tie_first)]
# setnames(dp18,"ddp_tie_first","ddp_tie_first18")
# #left_join
# dp <- left_join(dp,dp18,by="PRTNR_NUM")
# setDT(dp)
# dp <- na.omit(dp,cols="ddp_tie_first18")
# #make a table
# tab <- dp[,.N,by=c("ddp_tie_first","ddp_tie_first18")]
# setDT(tab)
# setorder(tab,ddp_tie_first,ddp_tie_first18)
# tab[ddp_tie_first=="am_prp", pct := round(N/sum(tab[ddp_tie_first=="am_prp",N]),3)*100]
# tab[ddp_tie_first=="earlyam_prp", pct := round(N/sum(tab[ddp_tie_first=="earlyam_prp",N]),3)*100]
# tab[ddp_tie_first=="midday_prp", pct := round(N/sum(tab[ddp_tie_first=="midday_prp",N]),3)*100]
# tab[ddp_tie_first=="pm_prp", pct := round(N/sum(tab[ddp_tie_first=="pm_prp",N]),3)*100]
# tab[ddp_tie_first=="latepm_prp", pct := round(N/sum(tab[ddp_tie_first=="latepm_prp",N]),3)*100]
# 
# #write to .csv
# write.xlsx(tab,"O:/CoOp/CoOp194_PROReportng&OM/Julie/baristas-shifts_bydaypart_dec-jan-comparison.xlsx")


#pull in training data
tr <- read.spss("//starbucks/amer/portal/Departments/WMO/Marketing Research/New Q drive/Partner Insights/Partner Perspectives/Research/Partner Experience Study/11_Wave8_Retail (Sept 2017)/02 Data/PES _Retail_W8_Sept_2017_CLEAN_US.sav", use.value.labels = FALSE, to.data.frame=TRUE)
setDT(tr)
tr <- tr[,.(PartnerID,BlockA_4_TB,BlockA_7_TB,BlockB_7_TB)]
setnames(tr,"PartnerID","PRTNR_NUM")
dpanel <- merge(dpanel,tr,by="PRTNR_NUM")
#a5.	I like working for my manager
#a8.	My work schedule fits my life
#b18.	I receive the training I need to do my job

#frequency table: binary & continuous variables
tempbin <- dpanel %>%
  group_by(JOB_ID,ddp_tie_first) %>%
  summarize(n = n(), 
            likemymgrN = sum(BlockA_4_TB,na.rm=T),
            likemymgr = round(mean(BlockA_4_TB,na.rm=T),3),
            schedulefitslifeN = sum(BlockA_7_TB,na.rm=T),
            schedulefitslife = round(mean(BlockA_7_TB,na.rm=T),3),
            trainingineedN = sum(BlockB_7_TB,na.rm=T),
            trainingineed = round(mean(BlockB_7_TB,na.rm=T),3))
setDT(tempbin)
write.xlsx(tempbin,"O:/CoOp/CoOp194_PROReportng&OM/Julie/partnerexp_grouped.xlsx")



