#load libraries
library(data.table)

#load data
data_dir <- "O:/CoOp/CoOp194_PROReportng&OM/Julie"
pre1 <- fread(paste0(data_dir,"/pre-bias_survey_1.csv"))
pre2 <- fread(paste0(data_dir,"/pre-bias_survey_2.csv"))
post1 <- fread(paste0(data_dir,"/post-bias_survey_1.csv"))
post2 <- fread(paste0(data_dir,"/post-bias_survey_2.csv"))
post3 <- fread(paste0(data_dir,"/post-bias_survey_3.csv"))

##pre

#rbind together
l = list(pre1,pre2,post1,post2,post3)
pp <- rbindlist(l, use.names=TRUE, fill=TRUE) 

#use chief flag to make updated job categories
pp[, LEVEL := JOB_GROUP]
pp[CHIEF_FLAG=='N'&JOB_GROUP=='IC', LEVEL := 'IC']
pp[CHIEF_FLAG=='Y'&JOB_GROUP=='IC', LEVEL := 'MGR']
pp[CHIEF_FLAG=='N'&JOB_GROUP=='MGR', LEVEL := 'IC']
pp[CHIEF_FLAG=='Y'&JOB_GROUP=='MGR', LEVEL := 'MGR']

#remove duplicates from pre- and post-surveys
pp <- subset(pp, !duplicated(pp))
#subset
pp <- pp[, .(SAP_PRTNR_ID,LEVEL)]

#WRITE.CSV
write.csv(pp,file=paste0(data_dir,"/bias_survey_partner_levels.csv"))
