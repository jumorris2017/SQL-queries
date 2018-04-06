##Speed by MOP percentage##

#load libraries
library(tidyverse)
library(xlsx)
library(ggplot2)
library(ggthemes)

#Speed score for NON-peak MOP transactions, locking stores into FY18Q1 mop adoptions rates
#load data
mop <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/MOP_percent_by_store.csv")
mop18q2 <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/MOP_percent_by_store_FY18Q2.csv")
mopsp <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/Speed_by_store_cafetrans_FY18Q2.csv")
mopsp[, STORE_NUM := as.numeric(STORE_NUM)]

#calculate MOP percentage
mop[, moppct := PEAK_MOP_TRANS_CNT/PEAK_TTL_TRANS_CNT]
mop <- na.omit(mop,cols="moppct")

#organize stores into core, high, and super high
#group by FY 18 Q1
mop17 <- mop[FSCL_YR_NUM==2018&FSCL_QTR_IN_YR_NUM==1]
mop17[moppct>=0&moppct<.10, mopgroup := "1 - core"] #core
mop17[moppct>=.10&moppct<.20, mopgroup := "2 - high"] #high
mop17[moppct>=.20, mopgroup := "3 - super high"] #super high
mop17 <- mop17[, c("STORE_NUM","mopgroup")]

#merge
mop18q2 <- left_join(mop18q2,mop17,by="STORE_NUM")
setDT(mop18q2)
mop18q2 <- na.omit(mop18q2,cols="mopgroup")


#organize stores into core, high, and super high
mop[PEAK_MOP_PCT>=0&PEAK_MOP_PCT<10, mopgroup := "1 - core"] #core
mop[PEAK_MOP_PCT>=10&PEAK_MOP_PCT<20, mopgroup := "2 - high"] #high
mop[PEAK_MOP_PCT>=20, mopgroup := "3 - super high"] #super high
mop <- mop[, c("STORE_NUM","mopgroup")]

#Speed - CAFE transactions only
mopsp <- mopsp[ORD_MTHD_CD=="CAFE"]

#merge
mop <- left_join(mop,mopsp,by=c("STORE_NUM"))
setDT(mop)
mop <- na.omit(mop)

#aggregate
temp <- mop[, list(
  TOTAL_TB = sum(TOTAL_TB,na.rm=T),
  TOTAL_RSPNS = sum(TOTAL_RSPNS,na.rm=T),
  n = .N,
  tbsp = round(sum(TOTAL_TB,na.rm=T)/sum(TOTAL_RSPNS,na.rm=T),3)*100),
  by=c("mopgroup","FSCL_YR_NUM","FSCL_QTR_IN_YR_NUM")]
temp <- setorder(temp,mopgroup,FSCL_YR_NUM,FSCL_QTR_IN_YR_NUM)
# write.xlsx(temp,"O:/CoOp/CoOp194_PROReportng&OM/Julie/CCscore_byFY18Q1_MOPgroup-NONpeak.xlsx")

#set labels
xlabels <- c("Core","High","Super High")
ylabel <- "Speed TB Score"
tlabel <- "Speed scores for Cafe Transactions by MOP Percent at Peak"
# #manual legend labels
# lname <- "Benefit Utilization"
# llabels <- c("Non-Participant", "Participant")
#values
pdata1a <- temp
px1a <- temp[,mopgroup]
py1a <- temp[,tbsp]
nvar <- temp[,n]
#plot itself
plot1a <- ggplot(data=pdata1a,aes(y=py1a,x=as.factor(px1a))) + 
  geom_bar(stat="identity", width = 0.95, position=position_dodge(), colour="black") +
  #scale_fill_brewer(palette = 2, guide=FALSE) + 
  theme_economist() +
  scale_x_discrete(name="",labels=xlabels) +
  scale_y_continuous(limits=c(0,100)) +
  xlab("") + ylab(ylabel) + ggtitle(tlabel) +
  geom_text(size = 3, aes(label=paste0("n=",nvar),y=0), stat="identity", vjust = -1, position = position_dodge(0.95)) +
  geom_text(size = 3.5, aes(label=paste0(py1a,"%")), stat="identity", vjust = -1, position = position_dodge(0.95)) 
print(plot1a)