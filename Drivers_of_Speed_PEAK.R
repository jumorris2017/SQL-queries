#expected CC and speed values
#dec/jan/feb fy2018 scores
#using TSDs, region, and home store % as indicators
#PEAK ONLY

#load libaries
library(data.table)
library(nlme)
library(lubridate)
library(flipRegression)

##MARCH
#set path (new Q)
data_dir <- "O:/CoOp/CoOp194_PROReportng&OM/Julie"

#load data
ce <- fread(paste0(data_dir,"/Q2_FY18_Speed_StoreLevel_bychannel_PEAK.csv")) #rolling 3-month
hs <- fread(paste0(data_dir,"/FP6_FY18_homestore_PEAK.csv"))
units <- fread(paste0(data_dir,"/FP6_FY18_units_PEAK.csv"))
hpten <- fread(paste0(data_dir,"/FP6_FY18_hourly_partner_tenure_PEAK.csv"))
sr <- fread(paste0(data_dir,"/FP6_FY18_SRtrans_PEAK.csv"))
prodmix <- fread(paste0(data_dir,"/FP6_FY18_product_mix.csv")) #all day
hf <- fread(paste0(data_dir,"/FP6_FY18_highfreq_prop_PEAK.csv"))
sm <- fread(paste0(data_dir,"/sm_tenure_in_store.csv"))
chanmix <- fread(paste0(data_dir,"/FP6_FY18_channel_mix_PEAK.csv"))
uplh <- fread(paste0(data_dir,"/FP6_FY18_UPLH_PEAK.csv"))

#calculate home store percent
hs[, hspct := round(HS_CUST_COUNT/ALL_CUST_COUNT,4)]
hs <- hs[, .(STORE_NUM,hspct)]

#speed
#agg by year and order method
ce1 <- ce[, list(SP_RESPONSE_TOTAL=sum(TOTAL_RSPNS,na.rm=T),
                SP_TB_CNT=sum(TOTAL_TB,na.rm=T)),
           by=c("FSCL_YR_NUM","STORE_NUM","ORD_MTHD_CD")]
ce1[, sp_score := round(SP_TB_CNT/SP_RESPONSE_TOTAL,3)]
#swing wide by trans type
ce1 <- dcast.data.table(ce1, STORE_NUM ~ ORD_MTHD_CD, value.var="sp_score")
colnames(ce1)[2:4] <- paste0("sp_",colnames(ce1)[2:4])

#agg by year
ce2 <- ce[, list(SP_RESPONSE_TOTAL=sum(TOTAL_RSPNS,na.rm=T),
                 SP_TB_CNT=sum(TOTAL_TB,na.rm=T)),
          by=c("FSCL_YR_NUM","STORE_NUM")]
ce2[, sp_score := round(SP_TB_CNT/SP_RESPONSE_TOTAL,3)]
ce2 <- ce2[, .(STORE_NUM,sp_score)]

#calculate TSDs
#agg by year
tsd <- tsd[, list(CustTrans=sum(CustTrans,na.rm=T),
                  day_count=sum(day_count,na.rm=T)),
           by=c("FSCL_YR_NUM","RGN_ORG_LVL_DESCR","RGN_ORG_LVL_ID","STORE_NUM")]
tsd[, tsd := round(CustTrans/day_count,1)]
#swing wide for comp
tsd <- dcast.data.table(tsd, STORE_NUM + RGN_ORG_LVL_DESCR + RGN_ORG_LVL_ID ~ FSCL_YR_NUM, value.var=c("tsd","day_count"))
#setnames
setnames(tsd,c("tsd_2018","tsd_2017","day_count_2018"),c("tsd18","tsd17","daycount"))
tsd[, day_count_2017 := NULL]
#calculate comp
tsd[, tsd18comp := round((tsd18-tsd17)/tsd17,3)]

#SR transaction (%)
sr <- dcast.data.table(sr, STORE_NUM ~ SR_MEMBER, value.var="TTL_TRANS_CNT")
setnames(sr,c("0","1"),c("nonSR","SR"))
sr[, SR_trans_prp := round(SR/(nonSR+SR),3)]
sr <- sr[, .(STORE_NUM,SR_trans_prp)]

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

#channel mix
chanmix[CAFE_TRANS_CNT>0&TTL_TRANS_CNT>0, CAFE_PRP := round(CAFE_TRANS_CNT/TTL_TRANS_CNT,3)]
chanmix[CAFE_TRANS_CNT==0|TTL_TRANS_CNT==0, CAFE_PRP := 0]
chanmix[MOP_TRANS_CNT>0&TTL_TRANS_CNT>0, MOP_PRP := round(MOP_TRANS_CNT/TTL_TRANS_CNT,3)]
chanmix[MOP_TRANS_CNT==0|TTL_TRANS_CNT==0, MOP_PRP := 0]
chanmix[OTW_TRANS_CNT>0&TTL_TRANS_CNT>0, OTW_PRP := round(OTW_TRANS_CNT/TTL_TRANS_CNT,3)]
chanmix[OTW_TRANS_CNT==0|TTL_TRANS_CNT==0, OTW_PRP := 0]
chanmix <- chanmix[, .(STORE_NUM,OTW_PRP,MOP_PRP)]

#sm tenure in store
setnames(sm,"TimeInPosition","sm_tenure_in_store")
sm <- sm[, .(STORE_NUM,sm_tenure_in_store)]

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
hpturn[, hrlyturnover := round(SEPCOUNT/HEADCOUNT,3)]
hpturn <- hpturn[, .(STORE_NUM,hrlyturnover)]

#units per labor hour
uplh <- uplh[, .(STORE_NUM,UPLH)]

#merge
cedt <- Reduce(function(x, y) {merge(x, y, by=c("STORE_NUM"), all = TRUE)},
               list(ce1,ce2,hs,tsd,rural,prodmix,chanmix,hf,sr,dt,sm,hpten,hpturn,uplh))
#cedt <- na.omit(cedt)

#relative weights analysis -- library(flipRegression)
#cafe
Regression(sp_score ~ hspct + 
             SR_trans_prp +
             highfreq_cust_prp + 
             avghrlyten_yrs + 
             sm_tenure_in_store + 
             UPLH +
             MOP_PRP +
             bev_prp, data=cedt[ORD_MTHD_CD=='CAFE'],
           output = "Relative Importance Analysis")
lm0c <- lm(sp_score ~ hspct + 
             SR_trans_prp +
             highfreq_cust_prp + 
             avghrlyten_yrs + 
             sm_tenure_in_store + 
             UPLH +
             MOP_PRP +
             bev_prp, data=cedt[ORD_MTHD_CD=='CAFE'])
summary(lm0c)

