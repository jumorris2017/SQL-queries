#Testing intent to return CE ratings against actual return visitation behavior

#load libraries
library(data.table)
library(ggplot2)
library(ggthemes)
library(ppcor)
library(scales)
library(stringr)

#load data
data_dir <- "O:/CoOp/CoOp194_PROReportng&OM/Julie"
ir60 <- fread(paste0(data_dir,"/intent_to_return_visits_60days.csv"))
ir30 <- fread(paste0(data_dir,"/intent_to_return_visits_30days.csv"))
ir30 <- ir30[, .(GUID_USER_ID,SPEND_POST30D,VISITS_POST30D)]
ir7 <- fread(paste0(data_dir,"/intent_to_return_visits_7days.csv"))
ir7 <- ir7[, .(GUID_USER_ID,SPEND_POST7D,VISITS_POST7D)]
ir1 <- fread(paste0(data_dir,"/intent_to_return_visits_1day.csv"))
ir1 <- ir1[, .(GUID_USER_ID,SPEND_POST1D,VISITS_POST1D)]
#TB versus TB2
irtb <- fread(paste0(data_dir,"/intent_to_return_TBvTB2.csv"))
#response counts
irr <- fread(paste0(data_dir,"/intent_to_return_resp.csv"))
irr[, QSTN_ID := NULL]
#Sales and TSDs
irs <- fread(paste0(data_dir,"/intent_to_return_sales.csv"))

#merge together
ir <- Reduce(function(x, y) {merge(x, y, by=c("GUID_USER_ID"), all = TRUE)}, list(ir60,ir30,ir7,ir1))
irtb <- Reduce(function(x, y) {merge(x, y, by=c("FSCL_YR_NUM","FSCL_WK_IN_YR_NUM"), all = TRUE)}, list(irtb,irr,irs))
irtb <- irtb[!(FSCL_YR_NUM==2018&FSCL_WK_IN_YR_NUM==29)]

#sales data 
irtb[, tsd := round(CustTrans/day_count,2)]
irtb[, CustTrans := as.numeric(CustTrans)]

#create top box var
ir[RETURN==5, returnTB := 1]; ir[RETURN<=4&RETURN>=1, returnTB := 0]

#create top 2 box var
ir[RETURN>=4, returnTB2 := 1]; ir[RETURN<=3&RETURN>=1, returnTB2 := 0]

#create top 3 box var
ir[RETURN>=3, returnTB3 := 1]; ir[RETURN<=2&RETURN>=1, returnTB3 := 0]

#create indicator for actual returns in 30 days
ir[VISITS_POST30D>=1, returnin30days := 1]; ir[VISITS_POST30D==0, returnin30days := 0]
#create indicator for actual returns in 7 days
ir[VISITS_POST7D>=1, returnin7days := 1]; ir[VISITS_POST7D==0, returnin7days := 0]
#create indicator for actual returns in 1 day
ir[VISITS_POST1D>=1, returnin1day := 1]; ir[VISITS_POST1D==0, returnin1day := 0]

#create indicator for actual returns; EXCLUDING lesser returns
#actual returns in 30 days #excluding returns in 7 days and 1 day
ir[, returnnever_excl := 0]
ir[VISITS_POST60D==0, returnnever_excl := 1]
#actual returns in 60 days #excluding returns in 30 days, 7 days and 1 day
ir[, returnin60days_excl := 0]
ir[returnin7days==0&returnin1day==0&returnin30days==0&VISITS_POST60D>=1, returnin60days_excl := 1]
#actual returns in 30 days #excluding returns in 7 days and 1 day
ir[, returnin30days_excl := 0]
ir[returnin7days==0&returnin1day==0&returnin30days==1, returnin30days_excl := 1]
#actual returns in 7 days #excluding returns in 1 day
ir[, returnin7days_excl := 0]
ir[returnin1day==0&returnin7days==1, returnin7days_excl := 1]

#remove outliers... (71 people visited more than 200x in 60 days)
ir <- ir[VISITS_POST60D<=200]

#test accuracy
#mututally exclusive
ir[RETURN==5, mean(returnin1day)]
ir[RETURN==4, mean(returnin7days_excl)]
ir[RETURN==3, mean(returnin30days_excl)]
ir[RETURN==2, mean(returnin60days_excl)]
ir[RETURN==1, mean(returnnever_excl)]

# #combined top boxes
# ir[RETURN==5, mean(returnin1day)]
# ir[RETURN>=4, mean(returnin7days)]
# ir[RETURN>=3, mean(returnin30days)]

#t.tests
#60 days: number of visits
ir[RETURN==5, mean(VISITS_POST60D,na.rm=T)]
ir[RETURN==4, mean(VISITS_POST60D,na.rm=T)]
ir[RETURN==3, mean(VISITS_POST60D,na.rm=T)]
ir[RETURN==2, mean(VISITS_POST60D,na.rm=T)]
ir[RETURN==1, mean(VISITS_POST60D,na.rm=T)]
t.test(ir[returnTB==0, VISITS_POST60D],ir[returnTB==1, VISITS_POST60D])
t.test(ir[returnTB2==0, VISITS_POST60D],ir[returnTB2==1, VISITS_POST60D])
# t.test(ir[returnTB3==0, VISITS_POST60D],ir[returnTB3==1, VISITS_POST60D])
#30 days: number of visits
ir[RETURN==5, mean(VISITS_POST30D,na.rm=T)]
ir[RETURN==4, mean(VISITS_POST30D,na.rm=T)]
ir[RETURN==3, mean(VISITS_POST30D,na.rm=T)]
ir[RETURN==2, mean(VISITS_POST30D,na.rm=T)]
ir[RETURN==1, mean(VISITS_POST30D,na.rm=T)]
t.test(ir[returnTB==0, VISITS_POST30D],ir[returnTB==1, VISITS_POST30D])
t.test(ir[returnTB2==0, VISITS_POST30D],ir[returnTB2==1, VISITS_POST30D])
#7 days: number of visits
ir[RETURN==5, mean(VISITS_POST7D,na.rm=T)]
ir[RETURN==4, mean(VISITS_POST7D,na.rm=T)]
ir[RETURN==3, mean(VISITS_POST7D,na.rm=T)]
ir[RETURN==2, mean(VISITS_POST7D,na.rm=T)]
ir[RETURN==1, mean(VISITS_POST7D,na.rm=T)]
t.test(ir[returnTB==0, VISITS_POST7D],ir[returnTB==1, VISITS_POST7D])
t.test(ir[returnTB2==0, VISITS_POST7D],ir[returnTB2==1, VISITS_POST7D])
# t.test(ir[returnTB3==0, VISITS_POST7D],ir[returnTB3==1, VISITS_POST7D])


#30 days: binary visitation
t.test(ir[returnTB3==0, returnin30days],ir[returnTB3==1, returnin30days])
#7 days: binary visitation
t.test(ir[returnTB2==0, returnin7days],ir[returnTB2==1, returnin7days])
#1 day: binary visitation
t.test(ir[returnTB==0, returnin1day],ir[returnTB==1, returnin1day])

#melt data
irtbm <- irtb[,.(FSCL_YR_NUM,FSCL_WK_IN_YR_NUM,TB_SCORE,TB2_SCORE)]
irtbm <- melt(irtbm, id=c("FSCL_YR_NUM","FSCL_WK_IN_YR_NUM"))

#create an x-variable
irtbm[, fyfw := paste0(FSCL_YR_NUM,".",str_pad(irtbm[,FSCL_WK_IN_YR_NUM],2,pad="0"))]

#set up line chart
pdata <- irtbm
px <- irtbm[, fyfw]
py <- irtbm[, value]
groupvar <- irtbm[, variable]
#set labels
xlabel <- "Time"
ylabel <- "Score (%)"
tlabel <- "Intent to Return"
#manual legend labels
lname <- "Metric"
llabels <- c("Top Box (5 out of 5)","Top 2 Box (4+ out of 5)") 

#line chart
plot2 <- ggplot() +
  geom_line(data=pdata, aes(x=factor(px), y=py, group=factor(groupvar), colour=factor(groupvar))) + 
  xlab(xlabel) + ylab(ylabel) + theme_economist_white(gray_bg = FALSE) +
  scale_colour_discrete(name=lname, labels=llabels, guide=guide_legend(order=1)) +
  scale_x_discrete(breaks = px[seq(1, length(px), by = 4)]) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  theme(axis.text.x = element_text(size = 7, angle = 90, hjust = 1)) +
  ggtitle(tlabel)
print(plot2)

#set up line chart
pdata <- irtbm[variable=="TB_SCORE"]
px <- irtbm[variable=="TB_SCORE", fyfw]
py <- irtbm[variable=="TB_SCORE", value]
#set labels
xlabel <- "Time"
ylabel <- "Score (%)"
tlabel <- "Intent to Return\nTop Box Score"

#line chart
plot2 <- ggplot() +
  geom_line(data=pdata, aes(x=factor(px), y=py, group=1)) + 
  xlab(xlabel) + ylab(ylabel) + theme_economist_white(gray_bg = FALSE) +
  #scale_colour_discrete(name=lname, labels=llabels, guide=guide_legend(order=1)) +
  scale_y_continuous(limits=c(pdata[,min(py)*.85],pdata[,max(py)*1.05])) +
  scale_x_discrete(breaks = px[seq(1, length(px), by = 4)]) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  theme(axis.text.x = element_text(size = 7, angle = 90, hjust = 1)) +
  ggtitle(tlabel)
print(plot2)

#set up line chart
pdata <- irtbm[variable=="TB2_SCORE"]
px <- irtbm[variable=="TB2_SCORE", fyfw]
py <- irtbm[variable=="TB2_SCORE", value]
#set labels
xlabel <- "Time"
ylabel <- "Score (%)"
tlabel <- "Intent to Return\nTop 2 Box Score"

#line chart
plot2 <- ggplot() +
  geom_line(data=pdata, aes(x=factor(px), y=py, group=1)) + 
  xlab(xlabel) + ylab(ylabel) + theme_economist_white(gray_bg = FALSE) +
  #scale_colour_discrete(name=lname, labels=llabels, guide=guide_legend(order=1)) +
  scale_y_continuous(limits=c(pdata[,min(py)*.85],pdata[,max(py)*1.05])) +
  scale_x_discrete(breaks = px[seq(1, length(px), by = 4)]) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  theme(axis.text.x = element_text(size = 7, angle = 90, hjust = 1)) +
  ggtitle(tlabel)
print(plot2)



#merge together
cor.test(irtb[,TB_SCORE],irtb[,tsd])
cor.test(irtb[,TB2_SCORE],irtb[,tsd])
cor.test(irtb[,TB_SCORE],irtb[,CustTrans])
cor.test(irtb[,TB2_SCORE],irtb[,CustTrans])

#melt data
irsm <- irtb[,.(FSCL_YR_NUM,FSCL_WK_IN_YR_NUM,CustTrans,tsd)]
# irsm <- melt(irsm, id=c("FSCL_YR_NUM","FSCL_WK_IN_YR_NUM"))

#create an x-variable
irsm[, fyfw := paste0(FSCL_YR_NUM,".",str_pad(irsm[,FSCL_WK_IN_YR_NUM],2,pad="0"))]

#set up line chart
pdata <- irsm
px <- irsm[, fyfw]
py <- irsm[, CustTrans]
#set labels
xlabel <- "Time"
ylabel <- "Transactions"
tlabel <- "Customer Transactions"
#line chart
plot1 <- ggplot() +
  geom_line(data=pdata, aes(x=factor(px), y=py, group = 1)) + 
  xlab(xlabel) + ylab(ylabel) + theme_economist_white(gray_bg = FALSE) +
  scale_x_discrete(breaks = px[seq(1, length(px), by = 4)]) +
  scale_y_continuous(limits=c(0,55000000), breaks = scales::pretty_breaks(n = 5), labels = comma) +
  theme(axis.text.x = element_text(size = 7, angle = 90, hjust = 1)) +
  ggtitle(tlabel)
print(plot1)

#set up line chart
pdata <- irsm
px <- irsm[, fyfw]
py <- irsm[, tsd]
#set labels
xlabel <- "Time"
ylabel <- "TSDs"
tlabel <- "TSDs"
#line chart
plot1 <- ggplot() +
  geom_line(data=pdata, aes(x=factor(px), y=py, group = 1)) + 
  xlab(xlabel) + ylab(ylabel) + theme_economist_white(gray_bg = FALSE) +
  scale_x_discrete(breaks = px[seq(1, length(px), by = 4)]) +
  scale_y_continuous(limits=c(pdata[,min(py)*.85],pdata[,max(py)*1.1])) +
  #scale_y_continuous(limits=c(0,1000), breaks = scales::pretty_breaks(n = 5), labels = comma) +
  theme(axis.text.x = element_text(size = 7, angle = 90, hjust = 1)) +
  ggtitle(tlabel)
print(plot1)



#make plot of different survey responses
#melt data
irrm <- irtb[,.(FSCL_YR_NUM,FSCL_WK_IN_YR_NUM,RSP_SCORE5,RSP_SCORE4,RSP_SCORE3,RSP_SCORE2,RSP_SCORE1)]
irrm <- melt(irrm, id=c("FSCL_YR_NUM","FSCL_WK_IN_YR_NUM"))

#create an x-variable
irrm[, fyfw := paste0(FSCL_YR_NUM,".",str_pad(irtbm[,FSCL_WK_IN_YR_NUM],2,pad="0"))]

#set up line chart
pdata <- irrm
px <- irrm[, fyfw]
py <- irrm[, value]
groupvar <- irrm[, variable]
#set labels
xlabel <- "Time"
ylabel <- "Percent of CE Responses (%)"
tlabel <- "Intent to Return\nDistribution of Responses"
#manual legend labels
lname <- "Response"
llabels <- c("5","4","3","2","1") 

#line chart
plot2 <- ggplot() +
  geom_line(data=pdata, size=1, aes(x=factor(px), y=py, group=factor(groupvar), colour=factor(groupvar))) + 
  xlab(xlabel) + ylab(ylabel) + theme_economist_white(gray_bg = FALSE) +
  scale_colour_discrete(name=lname, labels=llabels, guide=guide_legend(order=1)) +
  scale_x_discrete(breaks = px[seq(1, length(px)/5, by = 4)]) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  theme(axis.text.x = element_text(size = 7, angle = 90, hjust = 1)) +
  ggtitle(tlabel)
print(plot2)


#change in response scores by specific values
irtb2 <- fread(paste0(data_dir,"/intent_to_return_RespNums.csv"))

#calculate percent
irtb2[, (grep("TOTAL",names(irtb2),value=T)) := lapply(.SD, function(x) x*100/RSPNS_COUNT),
     .SDcols=c(grep("TOTAL",names(irtb2),value=T))]

#melt data
irtb2 <- irtb2[,.(FSCL_YR_NUM,FSCL_PER_NM,FSCL_WK_IN_YR_NUM,TOTAL5,TOTAL4,TOTAL3,TOTAL2,TOTAL1)]
irtb2 <- melt(irtb2, id=c("FSCL_YR_NUM","FSCL_PER_NM","FSCL_WK_IN_YR_NUM"))

#create an x-variable
irtb2[, fyfw := paste0(FSCL_YR_NUM,".",str_pad(irtb2[,FSCL_WK_IN_YR_NUM],2,pad="0"))]

#create row numbers
irtb2[, ID := .I]

#set up line chart
pdata <- irtb2
px <- irtb2[, fyfw]
py <- irtb2[, value]
groupvar <- irtb2[, variable]
#breaks & labels
irtb2[, fyfp := paste0(FSCL_YR_NUM,".",str_pad(irtb2[,FSCL_PER_NM],2,pad="0"))]
temp <- irtb2[!duplicated(irtb2$fyfp)&variable=="TOTAL5",]
pxlabels <- temp[, fyfw]
xlabels <- temp[, FSCL_PER_NM]
#set labels
xlabel <- ""
ylabel <- "Percent of CE Responses (%)"
tlabel <- "Intent to Return"
sublabel <- "Weekly Distribution of Response Scores from Customer Experience Survey"
caption <- "U.S. Company-Operated Stores\n5 = Today or tomorrow; 4 = Within the next week; 3 = Within the next month; 2 = More than a month from now; 1 = Never"
#manual legend labels
lname <- "Response Score"
llabels <- c("5","4","3","2","1") 

#line chart
plot2 <- ggplot() +
  geom_line(data=pdata, size=1, aes(x=factor(px), y=py, group=factor(groupvar), colour=factor(groupvar))) + 
  xlab(xlabel) + ylab(ylabel) + theme_economist_white(gray_bg = FALSE) +
  scale_colour_discrete(name=lname, labels=llabels, guide=guide_legend(order=1)) +
  scale_x_discrete(labels = xlabels, breaks = pxlabels[seq(1, length(pxlabels), by = 1)]) +
  geom_vline(aes(xintercept = 10)) + annotate(geom = "text", x=12, y=12, hjust=0, vjust=0.2, label = "2016", angle=90, size=4) +
  geom_vline(aes(xintercept = 63)) +annotate(geom = "text", x=65, y=12, hjust=0, vjust=0.2, label = "2017", angle=90, size=4) +
  geom_vline(aes(xintercept = 115)) +annotate(geom = "text", x=117, y=12, hjust=0, vjust=0.2, label = "2018", angle=90, size=4) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  theme(axis.text.x = element_text(size=9, angle=90, hjust=1, vjust=.75)) +
  ggtitle(tlabel,subtitle=sublabel) + labs(caption=caption)
print(plot2)


