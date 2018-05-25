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
hs <- fread(paste0(data_dir,"/FP6_FY18_homestore_PEAK.csv"))
units <- fread(paste0(data_dir,"/FP6_FY18_units_PEAK.csv"))
setnames(units,"STORE_NUMBER","STORE_NUM")
ce <- fread(paste0(data_dir,"/Q2_FY18_Speed_StoreLevel_bychannel_PEAK.csv")) #rolling 3-month
hpten <- fread(paste0(data_dir,"/FP6_FY18_hourly_partner_tenure_PEAK.csv"))
sr <- fread(paste0(data_dir,"/FP6_FY18_SRtrans_PEAK.csv"))
prodmix <- fread(paste0(data_dir,"/FP6_FY18_product_mix.csv")) #all day
setnames(prodmix,"STORE_NUMBER","STORE_NUM")
hf <- fread(paste0(data_dir,"/FP6_FY18_highfreq_prop_PEAK.csv"))
sm <- fread(paste0(data_dir,"/sm_tenure_in_store.csv"))
chanmix <- fread(paste0(data_dir,"/FP6_FY18_channel_mix_PEAK.csv"))
uplh <- fread(paste0(data_dir,"/FP6_FY18_UPLH_PEAK.csv"))
cons <- fread(paste0(data_dir,"/store_constraint_flag.csv"))

#calculate home store percent
hs[, hspct := round(HS_CUST_COUNT/ALL_CUST_COUNT,4)]
hs <- hs[, .(STORE_NUM,hspct)]

#units per ticket & ticket price & region
units[, units_per_ticket := round(NET_UNITS/TRANS,4)]
units[, price_per_ticket := round(NET_SALES/TRANS,4)]
units <- units[, .(STORE_NUM,RGN_ORG_LVL_DESCR,units_per_ticket,price_per_ticket)]

#speed
#agg by store and order method
ce <- ce[, list(SP_RESPONSE_TOTAL=sum(TOTAL_RSPNS,na.rm=T),
                SP_TB_CNT=sum(TOTAL_TB,na.rm=T)),
           by=c("STORE_NUM","ORD_MTHD_CD")]
ce[, sp_score := round(SP_TB_CNT/SP_RESPONSE_TOTAL,3)]
#swing wide by trans type
ce <- dcast.data.table(ce, STORE_NUM ~ ORD_MTHD_CD, value.var="sp_score")
colnames(ce)[2:4] <- paste0("sp_",colnames(ce)[2:4])

#SR transaction (%)
sr <- dcast.data.table(sr, STORE_NUM ~ SR_MEMBER, value.var="TTL_TRANS_CNT")
setnames(sr,c("0","1"),c("nonSR","SR"))
sr[, SR_trans_prp := round(SR/(nonSR+SR),3)]
sr <- sr[, .(STORE_NUM,SR_trans_prp)]

#high freq customers (%)
hf[, hf26 := round(N_CUST_26/N_CUST,3)]
hf[, hf20 := round(N_CUST_20/N_CUST,3)]
hf[, hf15 := round(N_CUST_15/N_CUST,3)]
hf[, hf10 := round(N_CUST_10/N_CUST,3)]
hf[, hf05 := round(N_CUST_5/N_CUST,3)]
hf <- hf[, .(STORE_NUM,hf26,hf20,hf15,hf10,hf05)]

#product mix
prodmix[, brewed_prp := round(PROD_BREWED/PROD_TOTAL,3)]
prodmix[, blended_prp := round(PROD_BLENDED/PROD_TOTAL,3)]
prodmix <- prodmix[, .(STORE_NUM,brewed_prp,blended_prp)]

#channel mix: mop pct
chanmix[PEAK_MOP_TRANS_CNT>0&PEAK_TTL_TRANS_CNT>0, MOP_PRP := round(PEAK_MOP_TRANS_CNT/PEAK_TTL_TRANS_CNT,3)]
chanmix[PEAK_MOP_TRANS_CNT==0|PEAK_TTL_TRANS_CNT==0, MOP_PRP := 0]
chanmix <- chanmix[, .(STORE_NUM,MOP_PRP)]

#sm tenure in store
setnames(sm,"TimeInPosition","sm_tenure_in_store")
sm <- sm[, .(STORE_NUM,sm_tenure_in_store)]

#average hourly partner tenure
hpten[, hire_date := mdy_hm(MOST_RECENT_HIRE_DT)]
hpten[, hire_date := as_date(hire_date)]
hpten[, today := as_date(Sys.Date())]
hpten[, tenure := today-hire_date]
hpten <- hpten[JOB_ID==50000362|JOB_ID==50000358, 
               list(avghrlyten_yrs = mean(tenure,na.rm=T)),
               by="STORE_NUM"]
hpten[, avghrlyten_yrs := as.numeric(avghrlyten_yrs, units="days")]
hpten[, avghrlyten_yrs := round(avghrlyten_yrs/365.25,2)]

#units per labor hour
uplh <- uplh[, .(STORE_NUM,UPLH)]

#merge
cedt <- Reduce(function(x, y) {merge(x, y, by=c("STORE_NUM"), all = TRUE)},
               list(ce,hs,prodmix,chanmix,hf,sr,sm,hpten,uplh,units))
cedt <- na.omit(cedt, cols=c("STORE_NUM","RGN_ORG_LVL_DESCR"))

#cafe
#relative weights analysis -- library(flipRegression)
Regression(sp_CAFE ~ hspct + 
             brewed_prp + 
             blended_prp +
             MOP_PRP +
             hf10 +
             SR_trans_prp +
             sm_tenure_in_store + 
             avghrlyten_yrs + 
             UPLH +
             units_per_ticket +
             price_per_ticket, data=cedt,
           output = "Relative Importance Analysis")
summary(lm(sp_CAFE ~ 
             hspct + 
             brewed_prp + 
             blended_prp +
             MOP_PRP +
             hf10 +
             SR_trans_prp +
             sm_tenure_in_store + 
             avghrlyten_yrs + 
             UPLH +
             units_per_ticket +
             price_per_ticket, data=cedt))

#OTW
#relative weights analysis -- library(flipRegression)
Regression(sp_OTW ~ hspct + 
             brewed_prp + 
             blended_prp +
             MOP_PRP +
             hf10 +
             SR_trans_prp +
             sm_tenure_in_store + 
             avghrlyten_yrs + 
             UPLH +
             units_per_ticket +
             price_per_ticket, data=cedt,
           output = "Relative Importance Analysis")
summary(lm(sp_OTW ~ 
             hspct + 
             brewed_prp + 
             blended_prp +
             MOP_PRP +
             hf10 +
             SR_trans_prp +
             sm_tenure_in_store + 
             avghrlyten_yrs + 
             UPLH +
             units_per_ticket +
             price_per_ticket, data=cedt))

#MOP
#relative weights analysis -- library(flipRegression)
Regression(sp_MOP ~ hspct + 
             brewed_prp + 
             blended_prp +
             MOP_PRP +
             hf10 +
             SR_trans_prp +
             sm_tenure_in_store + 
             avghrlyten_yrs + 
             UPLH +
             units_per_ticket +
             price_per_ticket, data=cedt,
           output = "Relative Importance Analysis")
summary(lm(sp_MOP ~ 
             hspct + 
             brewed_prp + 
             blended_prp +
             MOP_PRP +
             hf10 +
             SR_trans_prp +
             sm_tenure_in_store + 
             avghrlyten_yrs + 
             UPLH +
             units_per_ticket +
             price_per_ticket, data=cedt))
