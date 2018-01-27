##Analysis of Partner Demographics by Dominant Day-Part Worked
##Author: Julie Morris

##load libraries
library(foreign)
library(data.table)
library(ggplot2)
library(tidyverse)
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


#afternoon over-performers
af <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/afternoon_overperformers.csv")
cc1yr <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC_daypart_bystore_4quarters.csv") #4 quarters
cc1yr <- cc1yr[STORE_NUM %in% af[,STORE_NUM]]
cc1yr[, QSTN_ID := NULL]









#read in daypart data
dp18 <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/partners_bar-ss_bydaypart_jan8-21_2018.csv") 

#recalcualate PM_DOM as necessary
dp18[, earlyam_prp := round(EARLYAM_SHIFTS/SHIFTS_WORKED,3)]
dp18[, am_prp := round(AM_SHIFTS/SHIFTS_WORKED,3)]
dp18[, midday_prp := round(MIDDAY_SHIFTS/SHIFTS_WORKED,3)]
dp18[, pm_prp := round(PM_SHIFTS/SHIFTS_WORKED,3)]
dp18[, latepm_prp := round(LATEPM_SHIFTS/SHIFTS_WORKED,3)]
#calculate dominate daypart
dp18[, ddp_tie_first := colnames(.SD)[max.col(.SD, ties.method="first")], .SDcols = c("earlyam_prp","am_prp","midday_prp","pm_prp","latepm_prp")]
dp18[, ddp_tie_last := colnames(.SD)[max.col(.SD, ties.method="last")], .SDcols = c("earlyam_prp","am_prp","midday_prp","pm_prp","latepm_prp")]
#create indicator for a tie
dp18[ddp_tie_first==ddp_tie_last, ddp_tie_exists := 0]
dp18[ddp_tie_first!=ddp_tie_last, ddp_tie_exists := 1]

#write to .csv
write.csv(dp18,"O:/CoOp/CoOp194_PROReportng&OM/Julie/baristas-shifts_bydaypart_jan8-21_2018.csv")


#merge into 2017
dp18 <- dp18[, .(PRTNR_NUM,ddp_tie_first)]
setnames(dp18,"ddp_tie_first","ddp_tie_first18")
#left_join
dp <- left_join(dp,dp18,by="PRTNR_NUM")
setDT(dp)
dp <- na.omit(dp,cols="ddp_tie_first18")
#make a table
tab <- dp[,.N,by=c("ddp_tie_first","ddp_tie_first18")]
setDT(tab)
setorder(tab,ddp_tie_first,ddp_tie_first18)
tab[ddp_tie_first=="am_prp", pct := round(N/sum(tab[ddp_tie_first=="am_prp",N]),3)*100]
tab[ddp_tie_first=="earlyam_prp", pct := round(N/sum(tab[ddp_tie_first=="earlyam_prp",N]),3)*100]
tab[ddp_tie_first=="midday_prp", pct := round(N/sum(tab[ddp_tie_first=="midday_prp",N]),3)*100]
tab[ddp_tie_first=="pm_prp", pct := round(N/sum(tab[ddp_tie_first=="pm_prp",N]),3)*100]
tab[ddp_tie_first=="latepm_prp", pct := round(N/sum(tab[ddp_tie_first=="latepm_prp",N]),3)*100]

#write to .csv
write.xlsx(tab,"O:/CoOp/CoOp194_PROReportng&OM/Julie/baristas-shifts_bydaypart_dec-jan-comparison.xlsx")


