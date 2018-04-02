#prepping partner productivity data
#April 2018

#load libraries
library(data.table)
library(lubridate)

#set directory
data_dir <- "O:/CoOp/CoOp194_PROReportng&OM/Julie"

#load data
pt <- fread(paste0(data_dir,"/partnerprod_tenure.csv"))
tr <- fread(paste0(data_dir,"/partnerprod_trans.csv")) 
st <- fread(paste0(data_dir,"/partnerprod_store.csv"))

#calculate tenure
pt[, hiredt := as_date(ymd_hms(MOST_RECENT_HIRE_DT))]
pt[, today := as_date(Sys.Date())]
pt[, tenure := today-hiredt]
pt[, tenure_mnths := as.numeric(tenure, units="days")]
pt[, tenure_mnths := round(tenure_mnths/30.5,2)]
