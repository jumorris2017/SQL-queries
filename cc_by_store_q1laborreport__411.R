## Training Hours Overspend Analysis -- Pulse & CC ##
## Request from Lisa 11/28/17 ##

#load libraries
library(foreign)
library(data.table)
library(dplyr)
library(Hmisc)
library(ggplot2)
library(PerformanceAnalytics)
library(labelled)

#load data
#training hours
# ####CSV flat file from macro-enabled .xlsb (project folder)
# th <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/q1laborreport.csv")
th <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/traininghours_by_store_q1laborreport.csv")
# #setnames
# setnames(th,c("STORE_NUM","FISCAL_WEEK_NUMBER","QTD_ACTUAL_TRAINING","QTD_ACTUAL_OPS2"),
#          c("store_num","fiscalweek","thours","opshours"))
setnames(th,c("STORE_NUM","FSCL_WK_IN_YR_NUM","WEEKLY_ACTUAL_TRAIN_HRS","WEEKLY_ACTUAL_OPS2_HRS"),
         c("store_num","fiscalweek","thours","opshours"))
#aggregate by fiscal week to get a sense of the number of additional hours
tempth <- th[, list(thours = round(sum(thours,na.rm=T),0),
                    opshours = round(sum(opshours,na.rm=T),0)), by="fiscalweek"]
#keep weeks consistent
tempth <- tempth[fiscalweek<=10]

# ####holiday hours: CSV flat file from macro-enabled .xlsb (project folder)
# hol <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/q1laborreport_holiday.csv")
# #setnames
# setnames(hol,c("STORE_NUM","FSCL_WK_IN_YR_NUM","TOTAL_HRS"),
#          c("store_num","fiscalweek","holhours"))
# #aggregate by fiscal week to get a sense of the number of additional hours
# temphol <- hol[, list(holhours = round(sum(holhours,na.rm=T),0)), by="fiscalweek"]
# #subtract 1 week for holiday count, as they prep in advance
# hol[, fiscalweek := fiscalweek-1]
# #keep weeks 5 and 6 budget. agg together. split in half and parse between FW 4 & 5
# # hol <- hol[fiscalweek==5|fiscalweek==6]
# # hol <- hol[, list(holhours = sum(holhours,na.rm=T)), by="store_num"]
# # hol[, holhours := holhours/2]
# # hol <- rbind(hol,hol)
# # fwvec <- rep(4:5,each=length(unique(hol[,store_num])))
# # hol <- cbind(hol,fwvec)
# # setnames(hol,"fwvec","fiscalweek")


#CC data
cc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc_by_store_q1laborreport.csv")
setnames(cc,c("STORE_NUM","FSCL_WK_IN_YR_NUM","CAL_WK_IN_YR_NUM","CCTOTALRESP","CCTBCOUNT"),
         c("store_num","fiscalweek","calweek","totalresp_cc","tbcount_cc"))
cc[, c("store_num") := lapply(.SD, as.numeric), .SDcols=c("store_num")]

#pulse data
# p <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/pulse_by_store_q1laborreport.csv")
# setnames(p,c("PersonnelNumber","survWeek","STORE_NUM_ASSIGNED"),c("pn","calweek","store_num"))
# p <- na.omit(p, cols="store_num")
#only store managers
p <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/pulse_by_store_q1laborreport_SM.csv")
setnames(p,c("PersonnelNumber","survWeek","STORE_NUM_ASSIGNED"),c("pn","calweek","store_num"))
p <- na.omit(p, cols="store_num")
#keep only store managers
p <- p[Job_Key==50000117]
p[, Job_Key := NULL]
#create vector of unique partnerIDs
p_pvec <- unique(p[,pn])

#same partner data
sp <- spss.get("//starbucks/amer/portal/Departments/WMO/Marketing Research/New Q drive/Partner Insights/Partner Perspectives/Research/Partner Experience Study/12_Wave9_Retail (Nov 2017)/02 Data/Q1 FY18 Retail PES US CLEAN.sav", force.single=T, use.value.labels=F, to.data.frame=T)
#remove variable labels
for (i in 1:ncol(sp)) {
  z<-class(sp[[i]])
  if (z[[1]]=='labelled'){
    class(sp[[i]])<-z[-1]
    attr(sp[[i]],'label')<-NULL
  }
}
setDT(sp)
#set names
setnames(sp,c("PartnerID","BlockB.2","BlockB.2.W8","BlockB.2.TB","BlockB.2.TB.W8",
              "BlockC.8","BlockC.8.W8","BlockC.8.TB","BlockC.8.TB.W8",
              "BlockB.1","BlockB.1.W8","BlockB.1.TB","BlockB.1.TB.W8",
              "BlockA.6","BlockA.6.W8","BlockA.6.TB","BlockA.6.TB.W8",
              "BlockC.2","BlockC.2.W8","BlockC.2.TB","BlockC.2.TB.W8",
              "BlockC.4","BlockC.4.W8","BlockC.4.TB","BlockC.4.TB.W8",
              "BlockB.0","BlockB.0.W8","BlockB.0.TB","BlockB.0.TB.W8",
              "BlockB.5","BlockB.5.W8","BlockB.5.TB","BlockB.5.TB.W8",
              "BlockC.9","BlockC.9.W8","BlockC.9.TB","BlockC.9.TB.W8",
              "BlockA.7","BlockA.7.W8","BlockA.7.TB","BlockA.7.TB.W8",
              "BlockB.6","BlockB.6.W8","BlockB.6.TB","BlockB.6.TB.W8"),
         c("pn","pex_resdemands_1","pex_resdemands_0","pex_resdemands_TB_1","pex_resdemands_TB_0",
           "pex_greatplace_1","pex_greatplace_0","pex_greatplace_TB_1","pex_greatplace_TB_0",
           "pex_greatteam_1","pex_greatteam_0","pex_greatteam_TB_1","pex_greatteam_TB_0",
           "pex_rightdec_1","pex_rightdec_0","pex_rightdec_TB_1","pex_rightdec_TB_0",
           "pex_accomp_1","pex_accomp_0","pex_accomp_TB_1","pex_accomp_TB_0",
           "pex_mission_1","pex_mission_0","pex_mission_TB_1","pex_mission_TB_0",
           "pex_career_1","pex_career_0","pex_career_TB_1","pex_career_TB_0",
           "pex_ethical_1","pex_ethical_0","pex_ethical_TB_1","pex_ethical_TB_0",
           "pex_socimpact_1","pex_socimpact_0","pex_socimpact_TB_1","pex_socimpact_TB_0",
           "pex_sched_1","pex_sched_0","pex_sched_TB_1","pex_sched_TB_0",
           "pex_hourswk_1","pex_hourswk_0","pex_hourswk_TB_1","pex_hourswk_TB_0"))
#drop partners who only have one wave
sp <- sp[W8.Flag==1]
sp <- sp[Store.Role==14]
#keep only numeric variables
sp <- sp[, sapply(sp, is.numeric), with=F]
sp <- sp[, -grep("CONCEPT",names(sp)), with=F]
#my job has reasonable demands
# #mean W8: 3.107306, mean: 2.916667, delta: -0.1906393
# sp[, pex_resdemands_delta := pex_resdemands_1 - pex_resdemands_0]
# #mean W8 TB: 0.1643836, mean TB: 0.1369863, delta: -0.02739726
# sp[, pex_resdemands_TB_delta := pex_resdemands_TB_1 - pex_resdemands_TB_0]
# #recommend Starbucks as a great place to work
# #mean W8: 4.383562, mean: 4.299087, delta: -0.08447489
# sp[, pex_greatplace_delta := pex_greatplace_1 - pex_greatplace_0]
# #mean W8 TB: 0.5833333, mean TB: 0.5627854, delta: -0.02054795
# sp[, pex_greatplace_TB_delta := pex_greatplace_TB_1 - pex_greatplace_TB_0]
#subset to SM's in pulse
sp_p_subset <- sp[pn %in% p_pvec]
#create vector of unique partnerIDs
sp_p_subset_pvec <- unique(sp_p_subset[,pn])
#subset questions
sp_p_subset <- sp_p_subset[, c("pn",grep("pex_",colnames(sp_p_subset),value=T)), with=F]
#pull in store numbers
pstores <- p[pn %in% sp_p_subset_pvec, c("pn","store_num"), with=F]
pstores <- subset(pstores, !duplicated(pstores$pn))
#merge in store numbers
sp_p_subset <- merge(sp_p_subset, pstores, by=c("pn"), all.x=T)
#aggregate by store
sp_p_subset[, pn := NULL]
sp_p_subset <- sp_p_subset[, lapply(.SD,sum,na.rm=T), by=c("store_num")]

#hire data
# h <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/hire-rehire_count_bystoreFY18.csv", colClasses=list(character=6))
# h[, store_num := as.numeric(StoreAbbrev)]
# h[, fiscalmonth := as.numeric(gsub('FP-2018-', '', Fiscal_Month))]
# h <- h[, c("store_num","fiscalmonth","tally"), with=F]
# h <- h[, list(tally = sum(tally,na.rm=T)), by=c("store_num","fiscalmonth")]
# h <- na.omit(h, cols="store_num")
hi <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/hiredate_by_storenumber.csv")
setnames(hi,c("STORE_NUM","FSCL_WK_IN_YR_NUM"),c("store_num","fiscalweek"))
#keep new baristas and promoted shifts
hi <- hi[(New_Job_Key==50000362&Move_Type==1)|(New_Job_Key==50000358&Move_Type==2)]
hi <- hi[, c("newhires","store_num","fiscalweek","New_Job_Key"), with=F]
#swing wide
hi <- dcast.data.table(hi, store_num + fiscalweek ~ New_Job_Key, value.var="newhires")
setnames(hi, c("50000362","50000358"),c("nh_bar","nh_shift"))
hi[is.na(hi)] <- 0

#headcount data
he <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/headcount_by_storenumber.csv")
#period 1
he1 <- he[FSCL_WK_IN_QTR_NUM==4]
he1 <- rbind(he1,he1,he1,he1)
fiscalweek <- rep(1:4, each=length(he1[,STORE_NUM]))
he1 <- cbind(he1,fiscalweek)
#period 2
he2 <- he[FSCL_WK_IN_QTR_NUM==8]
he2 <- rbind(he2,he2,he2,he2)
fiscalweek <- rep(5:8, each=length(he2[,STORE_NUM]))
he2 <- cbind(he2,fiscalweek)
#period 3
he3 <- he[FSCL_WK_IN_QTR_NUM==13]
he3 <- rbind(he3,he3,he3,he3,he3)
fiscalweek <- rep(9:13, each=length(he3[,STORE_NUM]))
he3 <- cbind(he3,fiscalweek)
#bind together
he <- rbind(he1,he2,he3)
he[, FSCL_WK_IN_QTR_NUM := NULL] #remove old week variable
setnames(he,"STORE_NUM","store_num") #rename for merging
#average by fiscal week and job code
he <- he[, list(AvgHeadcount = mean(AvgHeadcount,na.rm=T)), by=c("store_num","fiscalweek","Job_Key")]
#swing wide
he <- dcast.data.table(he, store_num + fiscalweek ~ Job_Key, value.var="AvgHeadcount")
setnames(he, c("50000362","50000358","50000117","50000118"),c("hcnt_bar","hcnt_shift","hcnt_sm","hcnt_asm"))
he[is.na(he)] <- 0

#comp data
comp <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/q1laborreport_comp.csv")
setnames(comp,c("STORE_NUMBER","FSCL_WK_IN_YR_NUM"),c("store_num","fiscalweek"))


#organize pulse data
temp1 <- p %>% 
  group_by(calweek, Question_ID, store_num) %>%
  dplyr::summarise(totalresp = n())
setDT(temp1)
temp2 <- p %>% 
  filter(RespID==7) %>%
  group_by(calweek, Question_ID, store_num) %>%
  dplyr::summarise(tbcount = n())
setDT(temp2)
pf <- left_join(temp1,temp2,by=c("calweek","Question_ID","store_num"))
setDT(pf)
pf[is.na(pf[,tbcount]), tbcount := 0]
pf <- dcast.data.table(pf, calweek + store_num ~ Question_ID, value.var=c("tbcount","totalresp"))


#joins
#get rid of calendar week
pfcc <- merge(cc,pf, by=c("store_num","calweek"), all=FALSE)
pfcc[, calweek := NULL]
pfcc <- pfcc[, lapply(.SD,sum,na.rm=T), by=c("store_num","fiscalweek")]
#KEEP SAME WEEKS OF DATA#
#REMOVE THESE LINES OF CODE WHEN WE GET MORE DATA FROM EVERY SOURCE#
# pfcc <- pfcc[fiscalweek<=8]

#join together
full <- merge(th, pfcc, by=c("store_num","fiscalweek"), all=FALSE)
full <- merge(full, hi, by=c("store_num","fiscalweek"), all.x=T)
full <- merge(full, he, by=c("store_num","fiscalweek"), all.x=T)
full <- merge(full, comp, by=c("store_num","fiscalweek"), all.x=T)
# full <- merge(full, hol, by=c("store_num","fiscalweek"), all.x=T)
#aggregate
full <- full[, lapply(.SD,sum,na.rm=T), by=c("store_num","fiscalweek")]

# #create delta variable
# full <- setorder(full,store_num,fiscalweek)
# #this basic shift takes previous store value - *not* necessarily the previous week
# full[, thourslag := shift(thours, 1L, fill=NA, type="lag"), by="store_num"]

## 12/6/17 interjection ##
#combine thours + ops2hours
full[, thours := thours + opshours]

#generate variable for total hours per hourly worker (shift + barista)
full[, thoursphp := thours/(hcnt_shift+hcnt_bar)]

#compare week 8 to average of weeks 1-3
full[fiscalweek>=8, fywkpost := 1]
full[fiscalweek>=1&fiscalweek<=3, fywkpost := 0]
full <- full[fywkpost==0|fywkpost==1]
full[, fiscalweek := NULL]

#calculate weekly cc by store
full[, cc_score := tbcount_cc/totalresp_cc]
#calculate weekly pulse scores by store
full[, q1_score := tbcount_Q1/totalresp_Q1]
full[, q2a_score := tbcount_Q2_A/totalresp_Q2_A]
#full[, q2b_score := tbcount_Q2_B/totalresp_Q2_B]
full[, q2c_score := tbcount_Q2_C/totalresp_Q2_C]
full[, q2d_score := tbcount_Q2_D/totalresp_Q2_D]
#full[, q2e_score := tbcount_Q2_E/totalresp_Q2_E]
#calculate comps
full[, salescomp := round((MonthlySales-LYMonthlySales)/LYMonthlySales,4)]

#aggregate
# full <- full[, lapply(.SD,mean,na.rm=T), .SDcols=c("thours","hcnt_shift","hcnt_bar","thoursphp",grep("score",colnames(full),value=T)), by=c("store_num","fywkpost")]
full <- full[, list(thours = mean(thours,na.rm=T),
                    opshours = mean(opshours,na.rm=T),
                    hcnt_shift = mean(hcnt_shift,na.rm=T),
                    hcnt_bar = mean(hcnt_bar,na.rm=T),
                    thoursphp = mean(thoursphp,na.rm=T),
                    cc_score = mean(cc_score,na.rm=T),
                    q1_score = mean(q1_score,na.rm=T),
                    q2a_score = mean(q2a_score,na.rm=T),
                    q2c_score = mean(q2c_score,na.rm=T),
                    q2d_score = mean(q2d_score,na.rm=T),
                    totalresp_cc = sum(totalresp_cc,na.rm=T),
                    totalresp_Q1 = sum(totalresp_Q1,na.rm=T),
                    totalresp_Q2_A = sum(totalresp_Q2_A,na.rm=T),
                    totalresp_Q2_C = sum(totalresp_Q2_C,na.rm=T),
                    totalresp_Q2_D = sum(totalresp_Q2_D,na.rm=T),
                    MonthlySales = sum(MonthlySales,na.rm=T),
                    LYMonthlySales = sum(LYMonthlySales,na.rm=T),
                    salescomp = mean(salescomp,na.rm=T)), by=c("store_num","fywkpost")]

#create headcount var
full[, hcnt_shiftbar := rowSums(.SD,na.rm=T), .SDcols=c("hcnt_shift","hcnt_bar")]

#swing wide
fullw <- dcast.data.table(full, store_num ~ fywkpost, value.var=c("thours","opshours","thoursphp","hcnt_shiftbar",grep("score",colnames(full),value=T),grep("totalresp",colnames(full),value=T),grep("Monthly",colnames(full),value=T)))
#merge in same partner sentiment
fullw <- merge(fullw, sp_p_subset, by="store_num", all.x=T)

#drop rows without both timepoints for hours and CC
fullw <- na.omit(fullw,cols=c("thours_0","thours_1","cc_score_0","cc_score_1"))
#round
fullw[, (colnames(fullw)[2:11]) := lapply(.SD, function(x) round(x,2)), .SDcols=colnames(fullw)[2:11]]
#make delta between time periods
fullw[, thours_delta := thours_1-thours_0]
fullw[, opshours_delta := opshours_1-opshours_0]
fullw[, thoursphp_delta := thoursphp_1-thoursphp_0]
fullw[, cc_delta := cc_score_1-cc_score_0]
is.na(fullw) <- sapply(fullw, is.infinite)
#summarize min, max, median and mean
#
### MINIMUM VALUES ###
#
fullw[, lapply(.SD,min,na.rm=T), .SDcols=colnames(fullw)[2:ncol(fullw)]]
### MAXIMUM VALUES ###
#
fullw[, lapply(.SD,max,na.rm=T), .SDcols=colnames(fullw)[2:ncol(fullw)]]
#
### MEDIAN VALUES ###
#
fullw[, lapply(.SD,median,na.rm=T), .SDcols=colnames(fullw)[2:ncol(fullw)]]
#
### MEAN VALUES ###
#
fullw[, lapply(.SD,mean,na.rm=T), .SDcols=colnames(fullw)[2:ncol(fullw)]]

# #correlated CC delta and training hour per hourly partner delta
# cor(fullw[,thoursphp_delta],fullw[,cc_delta])
# rcorr(fullw[,thoursphp_delta],fullw[,cc_delta],type="pearson")
# 
# ## correlation matrix with p-values
# cor.prob <- function (X, dfr = nrow(X) - 2) {
#   R <- cor(X, use="pairwise.complete.obs")
#   above <- row(R) < col(R)
#   r2 <- R[above]^2
#   Fstat <- r2 * dfr/(1 - r2)
#   R[above] <- 1 - pf(Fstat, 1, dfr)
#   R[row(R) == col(R)] <- NA
#   R
# }
# ## create function to dump the cor.prob output to a 4 column matrix
# ## with row/column indices, correlation, and p-value.
# flattenSquareMatrix <- function(m) {
#   if( (class(m) != "matrix") | (nrow(m) != ncol(m))) stop("Must be a square matrix.")
#   if(!identical(rownames(m), colnames(m))) stop("Row and column names must be equal.")
#   ut <- upper.tri(m)
#   data.frame(i = rownames(m)[row(m)[ut]],
#              j = rownames(m)[col(m)[ut]],
#              cor=t(m)[ut],
#              p=m[ut])
# }
# #flatten the table
# flattenSquareMatrix(cor.prob(fullw[,2:ncol(fullw)]))
# #plot the data
# chart.Correlation(fullw[,2:ncol(fullw)])


#split by training hours avg weeks 1-3
prob = c(.1,.9)
temp <- fullw %>% summarise( 
  th0_10 = quantile(thours_0, probs = prob[1], na.rm = T), 
  th0_90 = quantile(thours_0, probs = prob[2], na.rm = T)
)
fullw <- cbind(fullw, temp)
#recode based on quartiles
fullw[thours_0 <= th0_10, th0_qtile := 1]
fullw[thours_0 > th0_10 & thours_0 < th0_90, th0_qtile := 2]
fullw[thours_0 >= th0_90, th0_qtile := 3]

#split by training hours week 8
prob = c(.1,.9)
temp <- fullw %>% summarise( 
  th1_10 = quantile(thours_1, probs = prob[1], na.rm = T), 
  th1_90 = quantile(thours_1, probs = prob[2], na.rm = T)
)
fullw <- cbind(fullw, temp)
#recode based on quartiles
fullw[thours_1 <= th1_10, th1_qtile := 1]
fullw[thours_1 > th1_10 & thours_1 < th1_90, th1_qtile := 2]
fullw[thours_1 >= th1_90, th1_qtile := 3]

#split by training hours per partner avg weeks 1-3
prob = c(.1,.9)
temp <- fullw %>% summarise( 
  thphp0_10 = quantile(thoursphp_0, probs = prob[1], na.rm = T), 
  thphp0_90 = quantile(thoursphp_0, probs = prob[2], na.rm = T)
)
fullw <- cbind(fullw, temp)
#recode based on quartiles
fullw[thoursphp_0 <= thphp0_10, thphp0_qtile := 1]
fullw[thoursphp_0 > thphp0_10 & thoursphp_0 < thphp0_90, thphp0_qtile := 2]
fullw[thoursphp_0 >= thphp0_90, thphp0_qtile := 3]

#split by training hours per partner week 8
prob = c(.1,.9)
temp <- fullw %>% summarise( 
  thphp1_10 = quantile(thoursphp_1, probs = prob[1], na.rm = T), 
  thphp1_90 = quantile(thoursphp_1, probs = prob[2], na.rm = T)
)
fullw <- cbind(fullw, temp)
#recode based on quartiles
fullw[thoursphp_1 <= thphp1_10, thphp1_qtile := 1]
fullw[thoursphp_1 > thphp1_10 & thoursphp_1 < thphp1_90, thphp1_qtile := 2]
fullw[thoursphp_1 >= thphp1_90, thphp1_qtile := 3]

#split by training hours delta
prob = c(.1,.9)
temp <- fullw %>% summarise( 
  thdelta_10 = quantile(thours_delta, probs = prob[1], na.rm = T), 
  thdelta_90 = quantile(thours_delta, probs = prob[2], na.rm = T)
)
fullw <- cbind(fullw, temp)
#recode based on quartiles
fullw[thours_delta <= thdelta_10, thdelta_qtile := 1]
fullw[thours_delta > thdelta_10 & thours_delta < thdelta_90, thdelta_qtile := 2]
fullw[thours_delta >= thdelta_90, thdelta_qtile := 3]

#split by training hours per partner delta
prob = c(.1,.9)
temp <- fullw %>% summarise( 
  thphpdelta10 = quantile(thoursphp_delta, probs = prob[1], na.rm = T), 
  thphpdelta90 = quantile(thoursphp_delta, probs = prob[2], na.rm = T)
)
fullw <- cbind(fullw, temp)
#recode based on quartiles
fullw[thoursphp_delta <= thphpdelta10, thphpdelta_qtile := 1]
fullw[thoursphp_delta > thphpdelta10 & thoursphp_delta < thphpdelta90, thphpdelta_qtile := 2]
fullw[thoursphp_delta >= thphpdelta90, thphpdelta_qtile := 3]

# #look at over-spenders in both time periods
# temp <- fullw[, c(colnames(fullw)[1:5],"hcnt_shiftbar_1",grep("qtile",colnames(fullw),value=T)[1:4]),with=F]
# temp <- temp[thphp0_qtile==3&thphp1_qtile==3]
# temp <- setorder(temp,-thoursphp_1)

#split dataset by thoursphp_delta (neg, pos)
fullw[thoursphp_delta<0, thoursphp_delta_grp := "Hours - down"] #negative
fullw[thoursphp_delta==0, thoursphp_delta_grp := "Hours - same"] #zero
fullw[thoursphp_delta>0, thoursphp_delta_grp := "Hours - up"] #positive

#split dataset by thours_delta (neg, pos)
fullw[thours_delta<0, thours_delta_grp := "Hours - down"] #negative
fullw[thours_delta==0, thours_delta_grp := "Hours - same"] #zero
fullw[thours_delta>0, thours_delta_grp := "Hours - up"] #positive

#correlate CC delta and training hour php delta, by positive vs negative thphp delta
# rcorr(fullw[thoursphp_delta_grp=="Hours - down",thoursphp_delta],fullw[thoursphp_delta_grp=="Hours - down",cc_delta],type="pearson")
# rcorr(fullw[thoursphp_delta_grp=="Hours - up",thoursphp_delta],fullw[thoursphp_delta_grp=="Hours - up",cc_delta],type="pearson")
# 
# #correlate CC score and training hour php delta, by positive vs negative thphp delta
# rcorr(fullw[thoursphp_delta_grp=="Hours - down",thoursphp_delta],fullw[thoursphp_delta_grp=="Hours - down",cc_score_1],type="pearson")
# rcorr(fullw[thoursphp_delta_grp=="Hours - up",thoursphp_delta],fullw[thoursphp_delta_grp=="Hours - up",cc_score_1],type="pearson")
# 
# #correlate CC score and training hour php, by positive vs negative thphp delta
# rcorr(fullw[thoursphp_delta_grp=="Hours - down",thoursphp_1],fullw[thoursphp_delta_grp=="Hours - down",cc_score_1],type="pearson")
# rcorr(fullw[thoursphp_delta_grp=="Hours - up",thoursphp_1],fullw[thoursphp_delta_grp=="Hours - up",cc_score_1],type="pearson")

# #aggregate to get average CC score by th_php group
# #means
# tempa <- fullw[, c("thoursphp_delta_grp",grep("score",colnames(fullw),value=T)), with=F]
# tempa <- tempa[, lapply(.SD,function(x) round(mean(x,na.rm=T),4)*100), .SDcols=colnames(tempa)[2:ncol(tempa)], by="thoursphp_delta_grp"]
# #sums
# tempb <- fullw[, c("thoursphp_delta_grp",grep("totalresp",colnames(fullw),value=T),grep("Monthly",colnames(fullw),value=T)), with=F]
# tempb <- tempb[, lapply(.SD,sum,na.rm=T), .SDcols=colnames(tempb)[2:ncol(tempb)], by="thoursphp_delta_grp"]
# #join together
# temp <- left_join(tempa,tempb,by="thoursphp_delta_grp")
# setDT(temp)
# temp[, cc_delta := cc_score_1-cc_score_0]
# temp[, q1_delta := q1_score_1-q1_score_0]
# temp[, q2a_delta := q2a_score_1-q2a_score_0]
# temp[, q2c_delta := q2c_score_1-q2c_score_0]
# temp[, q2d_delta := q2d_score_1-q2d_score_0]
# #comps
# temp[, salescomp_0 := round((MonthlySales_0-LYMonthlySales_0)/LYMonthlySales_0,4)]
# temp[, salescomp_1 := round((MonthlySales_1-LYMonthlySales_1)/LYMonthlySales_1,4)]
# temp[, salescomp_delta := salescomp_1-salescomp_0]
#temp <- setorder(temp,thoursphp_delta_grp)
# fullw[,.N/nrow(fullw),by="thoursphp_delta_grp"]
# #aggregate to get average CC score by th group
# temp2a <- fullw[, c("thours_delta_grp",grep("score",colnames(fullw),value=T)), with=F]
# temp2a <- temp2a[, lapply(.SD,function(x) round(mean(x,na.rm=T),4)*100), .SDcols=colnames(temp2a)[2:ncol(temp2a)], by="thours_delta_grp"]
# temp2b <- fullw[, c("thours_delta_grp",grep("totalresp",colnames(fullw),value=T),grep("Monthly",colnames(fullw),value=T)), with=F]
# temp2b <- temp2b[, lapply(.SD,sum,na.rm=T), .SDcols=colnames(temp2b)[2:ncol(temp2b)], by="thours_delta_grp"]
# temp2 <- left_join(temp2a,temp2b,by="thours_delta_grp")
# setDT(temp2)
# temp2[, cc_delta := cc_score_1-cc_score_0]
# temp2[, q1_delta := q1_score_1-q1_score_0]
# temp2[, q2a_delta := q2a_score_1-q2a_score_0]
# temp2[, q2c_delta := q2c_score_1-q2c_score_0]
# temp2[, q2d_delta := q2d_score_1-q2d_score_0]
# #comps
# temp2[, salescomp_0 := round((MonthlySales_0-LYMonthlySales_0)/LYMonthlySales_0,4)]
# temp2[, salescomp_1 := round((MonthlySales_1-LYMonthlySales_1)/LYMonthlySales_1,4)]
# temp2[, salescomp_delta := salescomp_1-salescomp_0]
# #temp2 <- setorder(temp2,thours_delta_grp)
# # fullw[,.N/nrow(fullw),by="thours_delta_grp"]




#isolate extremes
n <- 500
#decliners
temp3 <- fullw[thphpdelta_qtile==1, c("thoursphp_delta",grep("opshours",colnames(fullw),value=T),grep("score",colnames(fullw),value=T),grep("totalresp",colnames(fullw),value=T),grep("Monthly",colnames(fullw),value=T),grep("pex_",colnames(fullw),value=T)), with=F]
temp3 <- setorder(temp3,-thoursphp_delta)
rankvar <- c(1:nrow(temp3))
temp3 <- cbind(temp3,rankvar)
#keep top 50 stores
temp3 <- temp3[rankvar<=n]
temp3a <- temp3[, c(grep("score",colnames(temp3),value=T),grep("pex_",colnames(temp3),value=T)), with=F]
temp3a <- temp3a[, lapply(.SD,function(x) round(mean(x,na.rm=T),4)*100), .SDcols=colnames(temp3a)[1:ncol(temp3a)]]
temp3b <- temp3[, c(grep("totalresp",colnames(temp3),value=T),grep("opshours",colnames(temp3),value=T),grep("Monthly",colnames(temp3),value=T)), with=F]
temp3b <- temp3b[, lapply(.SD,sum,na.rm=T), .SDcols=colnames(temp3b)[1:ncol(temp3b)]]
temp3 <- cbind(temp3a,temp3b)
setDT(temp3)
#scores
temp3[, cc_delta := cc_score_1-cc_score_0]
temp3[, q1_delta := q1_score_1-q1_score_0]
temp3[, q2a_delta := q2a_score_1-q2a_score_0]
temp3[, q2c_delta := q2c_score_1-q2c_score_0]
temp3[, q2d_delta := q2d_score_1-q2d_score_0]
#loop through elements of same partner survey to make deltas
list0 <- grep('pex_',grep('TB_0',colnames(temp3),value=T),value=T)
list1 <- grep('pex_',grep('TB_1',colnames(temp3),value=T),value=T)
listd <- sub('TB_1','delta',list1)
xvec <- rep(NA,length=length(listd))
for (i in 1:length(listd)) {
  #d[, listd[[i]] := list1[[i]] - list0[[i]]]
  d <- temp3[, c(list1[[i]],list0[[i]]), with=F]
  xvec[[i]] <- d[[1,1]]-d[[1,2]]
  #print(xvec)
}
xmat <- as.data.table(t(xvec))
setnames(xmat,listd)
temp3 <- cbind(xmat,temp3)
# temp3[, pex_resdemands_delta := pex_resdemands_TB_1-pex_resdemands_TB_0]
# temp3[, pex_greatplace_delta := pex_greatplace_TB_1-pex_greatplace_TB_0]
# temp3[, pex_greatteam_delta := pex_greatteam_TB_1-pex_greatteam_TB_0]
# temp3[, pex_rightdec_delta := pex_rightdec_TB_1-pex_rightdec_TB_0]
# temp3[, pex_sched_delta := pex_sched_TB_1-pex_sched_TB_0]
# temp3[, pex_accomp_delta := pex_accomp_TB_1-pex_accomp_TB_0]
# temp3[, pex_ethical_delta := pex_ethical_TB_1-pex_ethical_TB_0]
# temp3[, pex_socimpact_delta := pex_socimpact_TB_1-pex_socimpact_TB_0]
# temp3[, pex_mission_delta := pex_mission_TB_1-pex_mission_TB_0]
# temp3[, pex_career_delta := pex_career_TB_1-pex_career_TB_0]
# temp3[, pex_hourswk_delta := pex_hourswk_TB_1-pex_hourswk_TB_0]
#comps
temp3[, salescomp_0 := round((MonthlySales_0-LYMonthlySales_0)/LYMonthlySales_0,4)]
temp3[, salescomp_1 := round((MonthlySales_1-LYMonthlySales_1)/LYMonthlySales_1,4)]
temp3[, salescomp_delta := salescomp_1-salescomp_0]
#isolate increasers
temp4 <- fullw[thphpdelta_qtile==3, c("thoursphp_delta",grep("opshours",colnames(fullw),value=T),grep("score",colnames(fullw),value=T),grep("totalresp",colnames(fullw),value=T),grep("pex_",colnames(fullw),value=T),grep("Monthly",colnames(fullw),value=T)), with=F]
temp4 <- setorder(temp4,thoursphp_delta)
rankvar <- c(1:nrow(temp4))
temp4 <- cbind(temp4,rankvar)
#keep top 50 stores
temp4 <- temp4[rankvar<=n]
temp4a <- temp4[, c(grep("score",colnames(temp4),value=T),grep("pex_",colnames(temp4),value=T)), with=F]
temp4a <- temp4a[, lapply(.SD,function(x) round(mean(x,na.rm=T),4)*100), .SDcols=colnames(temp4a)[1:ncol(temp4a)]]
temp4b <- temp4[, c(grep("totalresp",colnames(temp4),value=T),grep("opshours",colnames(temp4),value=T),grep("Monthly",colnames(temp4),value=T)), with=F]
temp4b <- temp4b[, lapply(.SD,sum,na.rm=T), .SDcols=colnames(temp4b)[1:ncol(temp4b)]]
temp4 <- cbind(temp4a,temp4b)
setDT(temp4)
#scores
temp4[, cc_delta := cc_score_1-cc_score_0]
temp4[, q1_delta := q1_score_1-q1_score_0]
temp4[, q2a_delta := q2a_score_1-q2a_score_0]
temp4[, q2c_delta := q2c_score_1-q2c_score_0]
temp4[, q2d_delta := q2d_score_1-q2d_score_0]
#loop through elements of same partner survey to make deltas
list0 <- grep('pex_',grep('TB_0',colnames(temp4),value=T),value=T)
list1 <- grep('pex_',grep('TB_1',colnames(temp4),value=T),value=T)
listd <- sub('TB_1','delta',list1)
xvec <- rep(NA,length=length(listd))
for (i in 1:length(listd)) {
  #d[, listd[[i]] := list1[[i]] - list0[[i]]]
  d <- temp4[, c(list1[[i]],list0[[i]]), with=F]
  xvec[[i]] <- d[[1,1]]-d[[1,2]]
  #print(xvec)
}
xmat <- as.data.table(t(xvec))
setnames(xmat,listd)
temp4 <- cbind(xmat,temp4)
#comps
temp4[, salescomp_0 := round((MonthlySales_0-LYMonthlySales_0)/LYMonthlySales_0,4)]
temp4[, salescomp_1 := round((MonthlySales_1-LYMonthlySales_1)/LYMonthlySales_1,4)]
temp4[, salescomp_delta := salescomp_1-salescomp_0]
#top 50 agged
thphp_group <- c(paste0("Top ",n," Decliners"),paste0("Top ",n," Increasers"))
temp5 <- cbind(thphp_group,rbind(temp3,temp4))
temp5 <- temp5[, c("thphp_group",
                   #"opshours_0","opshours_1","opshours_delta",
                   "cc_score_0","cc_score_1","cc_delta",
                   "q1_score_0","q1_score_1","q1_delta",
                   "salescomp_0","salescomp_1","salescomp_delta",
                   list0,list1,listd),with=F]

#roughly the same
temp6 <- fullw[thoursphp_delta>=-0.2&thoursphp_delta<=0.2, c("thoursphp_delta",grep("opshours",colnames(fullw),value=T),grep("score",colnames(fullw),value=T),grep("totalresp",colnames(fullw),value=T),grep("Monthly",colnames(fullw),value=T),grep("pex_",colnames(fullw),value=T)), with=F]
#keep top 50 stores
temp6a <- temp6[, c(grep("score",colnames(temp6),value=T),grep("pex_",colnames(temp6),value=T)), with=F]
temp6a <- temp6a[, lapply(.SD,function(x) round(mean(x,na.rm=T),4)*100), .SDcols=colnames(temp6a)[1:ncol(temp6a)]]
temp6b <- temp6[, c(grep("totalresp",colnames(temp6),value=T),grep("opshours",colnames(temp6),value=T),grep("Monthly",colnames(temp6),value=T)), with=F]
temp6b <- temp6b[, lapply(.SD,sum,na.rm=T), .SDcols=colnames(temp6b)[1:ncol(temp6b)]]
temp6 <- cbind(temp6a,temp6b)
setDT(temp6)
#scores
temp6[, cc_delta := cc_score_1-cc_score_0]
temp6[, q1_delta := q1_score_1-q1_score_0]
temp6[, q2a_delta := q2a_score_1-q2a_score_0]
temp6[, q2c_delta := q2c_score_1-q2c_score_0]
temp6[, q2d_delta := q2d_score_1-q2d_score_0]
#loop through elements of same partner survey to make deltas
list0 <- grep('pex_',grep('TB_0',colnames(temp6),value=T),value=T)
list1 <- grep('pex_',grep('TB_1',colnames(temp6),value=T),value=T)
listd <- sub('TB_1','delta',list1)
xvec <- rep(NA,length=length(listd))
for (i in 1:length(listd)) {
  #d[, listd[[i]] := list1[[i]] - list0[[i]]]
  d <- temp6[, c(list1[[i]],list0[[i]]), with=F]
  xvec[[i]] <- d[[1,1]]-d[[1,2]]
  #print(xvec)
}
xmat <- as.data.table(t(xvec))
setnames(xmat,listd)
temp6 <- cbind(xmat,temp6)
#comps
temp6[, salescomp_0 := round((MonthlySales_0-LYMonthlySales_0)/LYMonthlySales_0,4)]
temp6[, salescomp_1 := round((MonthlySales_1-LYMonthlySales_1)/LYMonthlySales_1,4)]
temp6[, salescomp_delta := salescomp_1-salescomp_0]
temp6 <- temp6[, c("cc_score_0","cc_score_1","cc_delta",
                   "q1_score_0","q1_score_1","q1_delta",
                   #"opshours_0","opshours_1","opshours_delta",
                   "salescomp_0","salescomp_1","salescomp_delta",
                   list0,list1,listd),with=F]

#print
temp6x <- cbind("same",temp6)
setnames(temp6x,"V1","thphp_group")
temp6x <- rbind(temp5,temp6x)
write.csv(temp6x,file="C:/Users/jumorris/Documents/tsp.csv")




####

#aggregate up to get topline
fullwk <- full[, store_num := NULL]
fullwk <- fullwk[, lapply(.SD,sum,na.rm=T), by=c("fiscalweek")]
fullwk <- setorder(fullwk,fiscalweek)
fullwk[, thourslag := shift(thours, 1L, fill=NA, type="lag")]
fullwk[fiscalweek>1, tdelta := thours - thourslag]

#calculate average headcount by adding together headcount of different roles
fullwk[, AvgHeadcount := rowSums(.SD,na.rm=T), by=c("hcnt_bar","hcnt_shift","hcnt_sm","hcnt_asm")]

#calculate training hours
# fullwk[, newhirehalf := newhires/2]
# fullwk[, newhirehalflag := shift(newhirehalf, 1L, fill=NA, type="lag")]
# fullwk[, newhiresplit := ifelse(is.na(newhirehalflag),newhirehalf,newhirehalf+newhirehalflag)]
# fullwk[, newhirethours := newhiresplit * (19.5/2)]
fullwk[, newbaristathours := nh_bar * ((19.5 + 12.5 + 3.25))] #need to split into two weeks
fullwk[, proshiftthours := nh_shift * ((10.5 + 6.5))] #need to split into two weeks
fullwk[, safetytraining := AvgHeadcount * .25]
fullwk[, newsafethours := rowSums(.SD,na.rm=T), .SDcols=c("newbaristathours", "proshiftthours","safetytraining")]

# #create PPK measure (headcount*1 hour)
# fullwk[fiscalweek==4, ppkhours := AvgHeadcount*0.33]
# create PPK measure based on Tea's values 
fullwk[fiscalweek==4, ppkhours := AvgHeadcount*(1.25/2)]
fullwk[fiscalweek==5, ppkhours := AvgHeadcount*(0.75/2)]

#create updated total hour measure, which subtracts out holiday prep hours
# fullwk[, thoursnohol := thours-holhours]
fullwk[, thoursnoppk := ifelse(is.na(ppkhours),thours,thours-ppkhours)]
# #create updated total hour measure, which subtracts out half of holiday prep hours
# fullwk[, thourshalfhol := thours-(holhours/2)]
# #create updated total hour measure, with updated holiday hour measure
# fullwk[, thoursnohol_upd := thours-holhours]

#cc top box
fullwk[, ccscore := tbcount_cc / totalresp_cc]
fullwk[, Q1score := tbcount_Q1 / totalresp_Q1]
fullwk[, Q2_Ascore := tbcount_Q2_A / totalresp_Q2_A]
fullwk[, Q2_Bscore := tbcount_Q2_B / totalresp_Q2_B]
fullwk[, Q2_Cscore := tbcount_Q2_C / totalresp_Q2_C]
fullwk[, Q2_Dscore := tbcount_Q2_D / totalresp_Q2_D]
fullwk[, Q2_Escore := tbcount_Q2_E / totalresp_Q2_E]

#melt for plotting
m <- fullwk[, c("fiscalweek","thours",grep("score", names(fullwk), value=T)), with=F]
m <- melt(m, id=c("fiscalweek","thours"), 
          variable.name = "question", 
          value.name = "tbscore", 
          na.rm = T, #NA's removed from molten data when T
          variable.factor = F) #turns variable into factor

#plot trends
#set labels
xlabel <- "Fiscal Week (Q1 FY18)"
ylabel <- "Top Box Scores"
tlabel <- "Training Hours"
slabel <- "Customer Connection & Pulse Scores"
#set data and variables
pdata <- m
px <- m[, fiscalweek]
py <- m[, tbscore]
groupvar <- m[, question]
#manual legend labels
lname <- "Top Box Variable"
llabels <- c("Customer connection","Lived up to values","Connect with team","Supported","Feel proud",
             "Connect with customers","Reasonable demands") 
xbreaks <- 8
ybreaks <- 7
#line chart, factored by one variable
plot1a <- ggplot() +
  geom_line(data=pdata, aes(x=px, y=py, group=factor(groupvar), colour=factor(groupvar), linetype=factor(groupvar))) + 
  xlab(xlabel) + ylab(ylabel) + theme_bw() +
  scale_colour_discrete(name=lname, labels=llabels, guide=guide_legend(order=1)) +
  scale_linetype_manual(name=lname, labels=llabels, guide=guide_legend(order=1), values=c(5,1,1,1,1,1,1)) +
  scale_x_continuous(limits=c(pdata[,min(px)],pdata[,max(px)]), breaks = scales::pretty_breaks(n = xbreaks)) +
  labs(title = tlabel, subtitle = slabel)
print(plot1a)
plot1b <- ggplot() +
  geom_line(data=m[question=="ccscore"], aes(x=m[question=="ccscore",fiscalweek], y=m[question=="ccscore",thours])) +
  xlab(xlabel) + ylab(ylabel) + theme_bw() +
  scale_colour_discrete(name=lname, labels=llabels, guide=guide_legend(order=1)) +
  scale_y_continuous(limits=c(m[question=="ccscore",min(thours)*.75],m[question=="ccscore",max(thours)*1.15]),
                     breaks = scales::pretty_breaks(n = ybreaks), labels=comma, name="Training Hours") +
  labs(title = tlabel, subtitle = slabel)
print(plot1b)




##observation 1: big spike in week 4 training hours == upcoming holiday launch
##observation 2: precipitous drop-off following week 5

#melt for plotting
n <- fullwk[, c("fiscalweek","thoursnohol",grep("score", names(fullwk), value=T)), with=F]
n <- melt(n, id=c("fiscalweek","thoursnohol"), 
          variable.name = "question", 
          value.name = "tbscore", 
          na.rm = T, #NA's removed from molten data when T
          variable.factor = F) #turns variable into factor

#plot trends
#set labels
xlabel <- "Fiscal Week (Q1 FY18)"
ylabel <- "Top Box Scores"
tlabel <- "Training Hours - with Holiday Additional Hours Removed"
slabel <- "Customer Connection & Pulse Scores"
#set data and variables
pdata <- n
px <- n[, fiscalweek]
py <- n[, tbscore]
groupvar <- n[, question]
#manual legend labels
lname <- "Top Box Variable"
llabels <- c("Customer connection","Lived up to values","Connect with team","Supported","Feel proud",
             "Connect with customers","Reasonable demands") 
xbreaks <- 8
ybreaks <- 7
# #line chart, factored by one variable
# plot2 <- ggplot() +
#   geom_line(data=pdata, aes(x=px, y=py, group=factor(groupvar), colour=factor(groupvar), linetype=factor(groupvar))) + 
#   geom_line(data=n[question=="ccscore"], aes(x=n[question=="ccscore",fiscalweek], y=n[question=="ccscore",(thoursnohol+1155000)/5200000])) +
#   xlab(xlabel) + ylab(ylabel) + theme_bw() +
#   scale_colour_discrete(name=lname, labels=llabels, guide=guide_legend(order=1)) +
#   #scale_colour_discrete("") +
#   scale_linetype_manual(name=lname, labels=llabels, guide=guide_legend(order=1), values=c(5,1,1,1,1,1,1)) +
#   #guides(colour = guide_legend(override.aes = list(size = 7))) + 
#   scale_x_continuous(limits=c(pdata[,min(px)],pdata[,max(px)]), breaks = scales::pretty_breaks(n = xbreaks)) +
#   scale_y_continuous(limits=c(pdata[,min(py)*.75],pdata[,max(py)*1.15]),
#                      breaks = scales::pretty_breaks(n = ybreaks), labels=scales::percent, "TB Score",
#                      sec.axis=sec_axis(~.*5200000 - 1155000, 
#                                        breaks = scales::pretty_breaks(n = ybreaks), labels=comma, name="Training Hours")) +
#   labs(title = tlabel, subtitle = slabel)
# print(plot2)
plot2b <- ggplot() +
  geom_line(data=n[question=="ccscore"], aes(x=n[question=="ccscore",fiscalweek], y=n[question=="ccscore",thoursnohol])) +
  xlab(xlabel) + ylab(ylabel) + theme_bw() +
  scale_colour_discrete(name=lname, labels=llabels, guide=guide_legend(order=1)) +
  scale_y_continuous(limits=c(n[question=="ccscore",min(thoursnohol)*.75],n[question=="ccscore",max(thoursnohol)*1.15]),
                     breaks = scales::pretty_breaks(n = ybreaks), labels=comma, name="Training Hours") +
  labs(title = tlabel, subtitle = slabel)
print(plot2b)


#melt for plotting
o <- fullwk[, c("fiscalweek","thours","ppkhours","thoursnoppk","newsafethours",grep("score", names(fullwk), value=T)), with=F]
o <- melt(o, id=c("fiscalweek","thours","ppkhours","thoursnoppk","newsafethours"), 
          variable.name = "question", 
          value.name = "tbscore", 
          na.rm = T, #NA's removed from molten data when T
          variable.factor = F) #turns variable into factor

#plot trends
#set labels
xlabel <- "Fiscal Week (Q1 FY18)"
ylabel <- "Top Box Scores"
tlabel <- "Training hours, customer connection & pulse scores"
slabel <- "QTD actual (black line), actual minus holiday [JM calc] (red line), \nnew hire and safety training hours [JM calc] (blue line)"
#set data and variables
pdata <- o
px <- o[, fiscalweek]
py <- o[, tbscore]
groupvar <- o[, question]
#manual legend labels
lname <- "Top Box Variable"
llabels <- c("Customer connection","Lived up to values","Connect with team","Supported","Feel proud",
             "Connect with customers","Reasonable demands") 
xbreaks <- 8
ybreaks <- 7

#line chart, factored by one variable
plot3 <- ggplot() +
  geom_line(data=pdata, aes(x=px, y=py, group=factor(groupvar), colour=factor(groupvar), linetype=factor(groupvar))) +
  geom_line(data=o[question=="ccscore"], aes(x=o[question=="ccscore",fiscalweek], y=o[question=="ccscore",(thours+130000)/650000])) +
  geom_line(data=o[question=="ccscore"&fiscalweek>=3&fiscalweek<=6], aes(x=o[question=="ccscore"&fiscalweek>=3&fiscalweek<=6,fiscalweek], y=o[question=="ccscore"&fiscalweek>=3&fiscalweek<=6,(thoursnoppk+130000)/650000]),color='red') +
  geom_line(data=o[question=="ccscore"], aes(x=o[question=="ccscore",fiscalweek], y=o[question=="ccscore",(newsafethours+130000)/650000]),color='blue') +
  #geom_point(data=o[question=="ccscore"&ppkhours>0], aes(x=o[question=="ccscore"&ppkhours>0,fiscalweek], y=o[question=="ccscore"&ppkhours>0,(ppkhours+130000)/650000]),color='red') +
  xlab(xlabel) + ylab(ylabel) + theme_bw() +
  scale_colour_discrete(name=lname, labels=llabels, guide=guide_legend(order=1)) +
  scale_linetype_manual(name=lname, labels=llabels, guide=guide_legend(order=1), values=c(5,1,1,1,1,1,1)) +
  scale_x_continuous(limits=c(pdata[,min(px)],pdata[,max(px)]), breaks = scales::pretty_breaks(n = xbreaks)) +
  scale_y_continuous(limits=c(pdata[,min(py)*.65],pdata[,max(py)*1.25]),
                     breaks = scales::pretty_breaks(n = ybreaks), labels=scales::percent, "TB Score",
                     sec.axis=sec_axis(~.*650000 - 130000, 
                                       breaks = scales::pretty_breaks(n = ybreaks), labels=comma, name="Training Hours")) +
  labs(title = tlabel, subtitle = slabel)
print(plot3)

# #merge together
# tempthhol <- left_join(tempth,temphol,by="fiscalweek")
# setDT(tempthhol)
# tempthhol[, thoursnohol := ifelse(is.na(holhours),thours,thours-holhours)]
# temphol2 <- temphol[, fiscalweek := fiscalweek-1]
# tempthhol2 <- left_join(tempth,temphol2,by="fiscalweek")
# setDT(tempthhol2)
# tempthhol2[, thoursnohol := ifelse(is.na(holhours),thours,thours-holhours)]



temp <- copy(fullw)
temp[, cc_delta := cc_score_1-cc_score_0]
temp[, q1_delta := q1_score_1-q1_score_0]
temp[, q2a_delta := q2a_score_1-q2a_score_0]
temp[, q2c_delta := q2c_score_1-q2c_score_0]
temp[, q2d_delta := q2d_score_1-q2d_score_0]
#comps
temp[, salescomp_0 := round((MonthlySales_0-LYMonthlySales_0)/LYMonthlySales_0,4)]
temp[, salescomp_1 := round((MonthlySales_1-LYMonthlySales_1)/LYMonthlySales_1,4)]
temp[, salescomp_delta := salescomp_1-salescomp_0]
#correlations
cor(temp[,cc_score_1],temp[,thours_1])
cor(temp[,cc_delta],temp[,thours_delta])
cor(temp[,q1_delta],temp[,thours_delta])

temp2 <- temp[, c("salescomp_delta","thours_delta"), with=F]
is.na(temp2) <- sapply(temp2, is.infinite)
temp2 <- na.omit(temp2, cols=c("salescomp_delta"))
cor(temp2[,salescomp_delta],temp2[,thours_delta])
