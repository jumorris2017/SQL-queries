#load libraries
library(data.table)
library(lubridate)

#set directory
data_dir <- "O:/CoOp/CoOp194_PROReportng&OM/Julie"

#load data
pt1 <- fread(paste0(data_dir,"/q2fy18storepartners.csv"))
pt2 <- fread(paste0(data_dir,"/partner_pull.csv"))

#manage and merge
setnames(pt1,"PartnerID","PRTNR_ID")
full <- Reduce(function(x, y) {merge(x, y, by=c("PRTNR_ID"), all.x = TRUE)}, list(pt1, pt2))
full[EMP_STAT_CD=='Active', SEPARATION_DT := 'NA']
full <- full[, .(PRTNR_ID,PRTNR_CNTRY_CD,EMP_STAT_CD,SEPARATION_DT,JOB_ID,JOB_NM)]

#write to .csv
write.csv(full,paste0(data_dir,"/Q2_FY18_Stor_PES_PartnerIDs.csv"))