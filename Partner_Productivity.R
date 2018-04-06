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

#create tenure bins
pt[tenure_mnths<3, ten_bin := 1]
pt[tenure_mnths>=3&tenure_mnths<6, ten_bin := 2]
pt[tenure_mnths>=6&tenure_mnths<9, ten_bin := 3]
pt[tenure_mnths>=9&tenure_mnths<12, ten_bin := 4]
pt[tenure_mnths>=12&tenure_mnths<18, ten_bin := 5]
pt[tenure_mnths>=18&tenure_mnths<24, ten_bin := 6]
pt[tenure_mnths>=24&tenure_mnths<30, ten_bin := 7]
pt[tenure_mnths>=30&tenure_mnths<60, ten_bin := 8]
pt[tenure_mnths>=60, ten_bin := 9]

#focus on baristas
pt <- pt[JOB_ID==50000362]

#organize partners by half hour period
pt[, hh1 := 0];pt[, hh2 := 0];pt[, hh3 := 0];pt[, hh4 := 0]
pt[START_PUNCH_TM<=80000&END_PUNCH_TM>=83000, hh1 := 1] #8-8:30am
pt[START_PUNCH_TM<=83000&END_PUNCH_TM>=90000, hh2 := 1] #8:30-9am
pt[START_PUNCH_TM<=90000&END_PUNCH_TM>=93000, hh3 := 1] #9-9:30am
pt[START_PUNCH_TM<=93000&END_PUNCH_TM>=100000, hh4 := 1] #9:30-10am

#reduce variables so easier to work with
pt <- pt[, .(PRTNR_ID,STORE_NUM,hh1,hh2,hh3,hh4,ten_bin)]

#melt by half hour period
ptm1 <- melt(pt[hh1==1,.(PRTNR_ID,STORE_NUM,hh1,ten_bin)], id.vars=c("PRTNR_ID","STORE_NUM","hh1"), value.name="ten_bin")
ptm2 <- melt(pt[hh2==1,.(PRTNR_ID,STORE_NUM,hh2,ten_bin)], id.vars=c("PRTNR_ID","STORE_NUM","hh2"))
ptm3 <- melt(pt[hh3==1,.(PRTNR_ID,STORE_NUM,hh3,ten_bin)], id.vars=c("PRTNR_ID","STORE_NUM","hh3"))
ptm4 <- melt(pt[hh4==1,.(PRTNR_ID,STORE_NUM,hh4,ten_bin)], id.vars=c("PRTNR_ID","STORE_NUM","hh4"))

ptm1[, halfhour := 1]
ptm2[, halfhour := 2]
ptm3[, halfhour := 3]
ptm4[, halfhour := 4]

ptm1[, hh1 := NULL]
ptm2[, hh2 := NULL]
ptm3[, hh3 := NULL]
ptm4[, hh4 := NULL]

#rbind
ptw <- rbind(ptm1,ptm2,ptm3,ptm4)


pt <- na.omit(pt)
ptw <- dcast.data.table(pt, JOB_ID + STORE_NUM ~ ten_bin)


