#expected CC and speed values
#dec/jan/feb fy2018 scores
#using TSDs, region, and home store % as indicators

#load libaries
library(data.table)
library(nlme)
library(lubridate)

##FEBRUARY
#set path (new Q)
data_dir <- "O:/CoOp/CoOp194_PROReportng&OM/Julie"

#load data
ce <- fread(paste0(data_dir,"/FP3-5_FY18_CC-Speed_StoreLevel.csv"))
hs <- fread(paste0(data_dir,"/FP3-5_FY18_homestore.csv"))
tsd <- fread(paste0(data_dir,"/FP3-5_FY17-18_TSD.csv"))
rural <- fread(paste0(data_dir,"/storenum_urbanity.csv"))
prodmix <- fread(paste0(data_dir,"/product_mix.csv"))
hf <- fread(paste0(data_dir,"/highfreq_prop.csv"))
sr_dt <- fread(paste0(data_dir,"/SRtrans_DTflag.csv"))
sm <- fread(paste0(data_dir,"/sm_tenure_in_store.csv"))
hpten <- fread(paste0(data_dir,"/hourly_partner_tenure.csv"))
hpturn <- fread(paste0(data_dir,"/hourly_partner_turnover.csv"))

#calculate home store percent
hs[, hspct := round(HS_CUST_COUNT/ALL_CUST_COUNT,4)]
hs <- hs[, .(STORE_NUM,hspct)]

#speed
#agg by year
ce <- ce[, list(SP_RESPONSE_TOTAL=sum(SP_RESPONSE_TOTAL,na.rm=T),
                SP_TB_CNT=sum(SP_TB_CNT,na.rm=T)),
           by=c("FSCL_YR_NUM","STORE_NUM")]
ce[, sp_score := round(SP_TB_CNT/SP_RESPONSE_TOTAL,3)]
ce <- ce[, .(STORE_NUM,sp_score)]

#calculate TSDs
#agg by year
tsd <- tsd[, list(CustTrans=sum(CustTrans,na.rm=T),
                  day_count=sum(day_count,na.rm=T)),
           by=c("FSCL_YR_NUM","RGN_ORG_LVL_DESCR","RGN_ORG_LVL_ID","STORE_NUM")]
tsd[, tsd := round(CustTrans/day_count,1)]
#swing wide for comp
tsd <- dcast.data.table(tsd, STORE_NUM + RGN_ORG_LVL_DESCR + RGN_ORG_LVL_ID ~ FSCL_YR_NUM, value.var="tsd")
#setnames
setnames(tsd,c("2017","2018"),c("tsd18","tsd17"))
#calculate comp
tsd[, tsd18comp := round((tsd18-tsd17)/tsd17,3)]

#SR transaction (%)
sr_dt <- dcast.data.table(sr_dt, STORE_NUM + DRIVE_THRU_IND ~ SR_MEMBER, value.var="TTL_TRANS_CNT")
setnames(sr_dt,c("0","1"),c("nonSR","SR"))
sr_dt[, SR_trans_prp := round(SR/(nonSR+SR),3)]
sr_dt <- sr_dt[, .(STORE_NUM,DRIVE_THRU_IND,SR_trans_prp)]

#high freq customers (%)
hf <- dcast.data.table(hf, STORE_NUM ~ HIGH_TRANS_CUST, value.var="N_CUST")
setnames(hf,c("0","1"),c("lowfreq","highfreq"))
hf[, highfreq_cust_prp := round(highfreq/(lowfreq+highfreq),3)]
hf <- hf[, .(STORE_NUM,highfreq_cust_prp)]

#rural flag
setnames(rural,"STORE_NUMBER","STORE_NUM")
rural[, rural_flag := 0]
rural[urbanity=="U6", rural_flag := 1]
rural <- rural[, .(STORE_NUM,rural_flag)]

#product mix
prodmix[, food_prp := round(N_ITEMS_FOOD/N_ITEMS_TOTAL,3)]
prodmix[, bev_prp := round(N_ITEMS_BEV/N_ITEMS_TOTAL,3)]
prodmix <- prodmix[, .(STORE_NUM,food_prp,bev_prp)]

#sm tenure in store
setnames(sm,"Store_Num","STORE_NUM")
sm <- sm[, .(STORE_NUM,SM_Tenure_Store_Yrs)]

#average hourly partner tenure
hpten[, hire_date := as_date(MOST_RECENT_HIRE_DT)]
hpten[, today := as_date(Sys.Date())]
hpten[, tenure := today-hire_date]
hpten <- hpten[JOB_ID==50000362|JOB_ID==50000358, 
               list(avghrlyten_yrs = mean(tenure,na.rm=T)),
               by="STORE_NUM"]
hpten[, avghrlyten_yrs := as.numeric(avghrlyten_yrs, units="days")]
hpten[, avghrlyten_yrs := round(avghrlyten_yrs/365.25,2)]

#calculate hourly partner turnover
#agg by year
setnames(hpturn,"StoreNum","STORE_NUM")
hpturn <- hpturn[Period_Name=="FP-2018-4"|Period_Name=="FP-2018-5", 
                 list(Terms=sum(Terms,na.rm=T),
                        PartnerDays=sum(PartnerDays,na.rm=T),
                        Org_Days=sum(Org_Days,na.rm=T)),
           by=c("STORE_NUM")]
hpturn[, hrlyturnover := round(Terms/(PartnerDays/Org_Days),3)]
hpturn <- hpturn[, .(STORE_NUM,hrlyturnover,Org_Days)]
hpturn <- na.omit(hpturn,cols="STORE_NUM")

#merge
cedt <- Reduce(function(x, y) {merge(x, y, by=c("STORE_NUM"), all = TRUE)},
               list(ce,hs,tsd,rural,prodmix,hf,sr_dt,sm,hpten,hpturn))
cedt <- na.omit(cedt,cols=c("sp_score","tsd18","tsd18comp","rural_flag",
                            "DRIVE_THRU_IND","RGN_ORG_LVL_DESCR"))

#run models split by region
newData <- cedt[, .(STORE_NUM,RGN_ORG_LVL_DESCR,sp_score,tsd18,tsd18comp,
                    rural_flag,DRIVE_THRU_IND)]
ll = lmList(sp_score ~ tsd18 + tsd18comp + rural_flag +
              DRIVE_THRU_IND | RGN_ORG_LVL_DESCR, data=newData)
summary(ll$'FLORIDA')
summary(ll$'HAWAII')
summary(ll$'LA CENTRAL CA')
summary(ll$'MID-AMERICA')
summary(ll$'MID-ATLANTIC')
summary(ll$'MIDWEST')
summary(ll$'NEW YORK METRO')
summary(ll$'NORTHEAST')
summary(ll$'NORTHERN CALIFORNIA')
summary(ll$'PACIFIC NORTHWEST')
summary(ll$'SOUTH CENTRAL')
summary(ll$'SOUTHEAST')
summary(ll$'SOUTHERN CALIFORNIA')
summary(ll$'WESTERN MOUNTAIN')