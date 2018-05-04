#load libraries
library(data.table)
library(lubridate)
library(foreign)

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

##data from James Liu (5/3/18)
data_dir_wave13 <- "Q:/Departments/WMO/Marketing Research/New Q drive/Partner Insights/Partner Perspectives/Research/Partner Experience Study/13_Wave5_NonRetail (Mar 18)/Hierarchy"
jl <- fread(paste0(data_dir_wave13,"/PartnerListforSeparation_09.22.17.csv"))
nr13 <- read.spss(paste0(data_dir_wave13,"/FullDataFile_09.04.18_07.40.11.AM.sav"),use.value.labels = FALSE)
setDT(nr13)
nr13 <- nr13[, .(PartnerID,BlockA_6)]
data_dir_wave10 <- "Q:/Departments/WMO/Marketing Research/New Q drive/Partner Insights/Partner Perspectives/Research/Partner Experience Study/10_Wave4_NonRetail (Aug17)/Data/Data Files"
nr10 <- read.spss(paste0(data_dir_wave10,"/NonRetailPEData_CLEAN_09.22.17.sav"),use.value.labels = FALSE)
setDT(nr10)
nr10 <- nr10[, .(PartnerID,BlockA_6)]

#merge
nr <- rbind(nr10)
nr[, PRTNR_ID := as.character(PartnerID)]
nr[, PRTNR_ID := factor(sub(" +$", "", PRTNR_ID))]
nr[, PRTNR_ID := as.numeric(as.character(PRTNR_ID))]
nr[, PartnerID := NULL ]
#merge
nr <- Reduce(function(x, y) {merge(x, y, by=c("PRTNR_ID"), all.x = TRUE)}, list(nr,jl))
#na omit
nr <- na.omit(nr,cols="BlockA_6")
#analysis
nr[ACTIVE==0, SEPARATED := 1]; nr[ACTIVE==1, SEPARATED := 0]
t.test(nr[SEPARATED==0,BlockA_6],nr[SEPARATED==1,BlockA_6])
#engagementTB
nr[BlockA_6==5, EngTB := 1];nr[BlockA_6<=4, EngTB := 0]
#engagementBottom2Box
nr[BlockA_6>=3, EngB2B := 0];nr[BlockA_6<=2, EngB2B := 1]
t.test(nr[EngTB==1,SEPARATED],nr[EngB2B==1,SEPARATED])




