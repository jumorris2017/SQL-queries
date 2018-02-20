##making plots for deployment plays w/ CE indicators
##weekly trends and YoY comparison
##with YoY deltas called out
##CE measures: CC, SO, Speed, Cleanliness

#load libraries
library(data.table)
library(ggplot2)
library(tidyverse)

#group by stores that *are* or *are not* using the new plays

#load data
dp <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CE_deploymentplays.csv")
# strlist <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/APT_Test_and_Control_site_mapping.csv")

#group by stores using/not using plays
###this is fake for setting up code
dp[STORE_NUM %in% strlist[,stores_test], useploy := 0]; dp[STORE_NUM %in% strlist[,stores_control], useploy := 1]
dp <- na.omit(dp,cols="useploy")

#agg by week
dp <- dp[, lapply(.SD,sum,na.rm=T), .SDcols=grep("Q",names(dp),value=T), by=c("FSCL_WK_IN_YR_NUM","FSCL_YR_NUM","useploy")]
dp[, cc_sc := round(Q2_2_TB_CNT/Q2_2_RESPONSE_TOTAL*100,1)]
dp[, sp_sc := round(Q2_1_TB_CNT/Q2_1_RESPONSE_TOTAL*100,1)]
dp[, ab_sc := round(Q2_3_TB_CNT/Q2_3_RESPONSE_TOTAL*100,1)]
dp[, ac_sc := round(Q2_4_TB_CNT/Q2_4_RESPONSE_TOTAL*100,1)]
dp[, bt_sc := round(Q2_5_TB_CNT/Q2_5_RESPONSE_TOTAL*100,1)]
dp[, ft_sc := round(Q2_6_TB_CNT/Q2_6_RESPONSE_TOTAL*100,1)]
dp[, cl_sc := round(Q2_7_TB_CNT/Q2_7_RESPONSE_TOTAL*100,1)]
dp[, so_sc := round(rowMeans(.SD, na.rm = TRUE),1), .SDcols=c("sp_sc","ab_sc","ac_sc",
                                                    "bt_sc","ft_sc","cl_sc")]
#make year and wek indicator for plotting
dp[, fyfw := paste0(FSCL_YR_NUM,"-",FSCL_WK_IN_YR_NUM)]
#calculate delta
#ensure sorted properly
setorder(dp,useploy,FSCL_WK_IN_YR_NUM,FSCL_YR_NUM)
dp[, cc_lag := lapply(.SD, function(x) c(NA, x[-.N])), by=c("useploy","FSCL_WK_IN_YR_NUM"), .SDcols="cc_sc"]
dp[, so_lag := lapply(.SD, function(x) c(NA, x[-.N])), by=c("useploy","FSCL_WK_IN_YR_NUM"), .SDcols="so_sc"]

#calcualte delta
dp[, ccdelta := round(cc_sc-cc_lag,1)]
dp[, sodelta := round(so_sc-so_lag,1)]

#reduce variables
dp <- dp[, .(FSCL_WK_IN_YR_NUM,FSCL_YR_NUM,fyfw,useploy,cc_sc,so_sc,sp_sc,cl_sc,ccdelta,sodelta)]

##CC plot
#set labels
xlabels1a <- c("FW 14","FW 15","FW 16","FW 17","FW 18","FW 19","FW 20","FW 21")
xlabel1a <- c("Fiscal Week")
ylabel1a <- "TB Score"
tlabel1a <- "Deployment Play Utilization on Customer Experience"
sublabel1a <- "Customer Connection and Store Operations"
caption1a <- "Deployment Activities Timeline sent to SMs:\nFW17 - Understand the change for yourself and your team\nFW18 - Prepare to lead the change for your team\nFW20 - Lead the change for your team"
# caption1a <- "U.S. Company Operated Stores"
#manual legend labels
lname11a <- "Fiscal Year"
llabels11a <- c("2017","2018")
lname21a <- "Deployment Play Utilization"
llabels21a <- c("Not utilized","Utilized")
#values
pdata1a <- dp
px1a <- dp[,FSCL_WK_IN_YR_NUM]
py1a <- dp[,cc_sc]
py1b <- dp[,so_sc]
groupvar1a <- dp[,useploy]
colourvar1a <- dp[,FSCL_YR_NUM]
#plot itself
plot1a <- ggplot(data=pdata1a, aes(x=px1a, y=py1a, colour=factor(colourvar1a), linetype=factor(groupvar1a), group=interaction(groupvar1a, colourvar1a))) + 
  geom_line(size=1) + annotate(geom = "text", x=14, y=70, hjust=0, label = "Store Operations") +
  geom_line(y=py1b,size=1) + annotate(geom = "text", x=14, y=40, hjust=0, label = "Customer Connection") +
  geom_vline(aes(xintercept = 17)) + annotate(geom = "text", x=17, y=3, hjust=0, vjust=-0.5, label = "Understand", angle=90, size=3, fontface = "italic") +
  geom_vline(aes(xintercept = 18)) + annotate(geom = "text", x=18, y=3, hjust=0, vjust=-0.5, label = "Prepare", angle=90, size=3, fontface = "italic") +
  geom_vline(aes(xintercept = 20)) + annotate(geom = "text", x=20, y=3, hjust=0, vjust=-0.5, label = "Lead", angle=90, size=3, fontface = "italic") +
  xlab(xlabel1a) + ylab(ylabel1a) + 
  scale_colour_discrete(name=lname11a, labels=llabels11a, guide=guide_legend(order=1)) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  scale_linetype_discrete(name=lname21a, labels=llabels21a, guide=guide_legend(order=1)) +
  scale_x_continuous(breaks=14:21) +
  scale_y_continuous(limits=c(0,70)) + theme_economist() +
  ggtitle(tlabel1a) + labs(subtitle=sublabel1a,caption=caption1a)
print(plot1a)

#melt for delta plot
dpm <- melt(dp[FSCL_YR_NUM==2018,.(useploy,FSCL_WK_IN_YR_NUM,ccdelta,sodelta)], id=c("useploy","FSCL_WK_IN_YR_NUM"))

##CC plot
#set labels
xlabels2a <- c("FW 14","FW 15","FW 16","FW 17","FW 18","FW 19","FW 20")
xlabel2a <- c("Fiscal Week")
ylabel2a <- "YoY Delta"
tlabel2a <- "Deployment Play Utilization on Customer Experience"
sublabel2a <- "Customer Connection and Store Operations YoY Deltas"
caption2a <- "Deployment Activities Timeline sent to SMs:\nFW17 - Understand the change for yourself and your team\nFW18 - Prepare to lead the change for your team\nFW20 - Lead the change for your team"
#manual legend labels
lname12a <- "CE Metric"
llabels12a <- c("Customer Connection","Store Operations")
lname22a <- "Deployment Play Utilization"
llabels22a <- c("Not utilized","Utilized")
#values
pdata2a <- dpm
px2a <- dpm[,FSCL_WK_IN_YR_NUM]
py2a <- dpm[,value]
# py2b <- dpm[variable=="sodelta",value]
groupvar2a <- dpm[,useploy]
colourvar2a <- dpm[,variable]
#plot itself
plot2a <- ggplot(data=pdata2a, aes(x=px2a, y=py2a, colour=factor(colourvar2a), linetype=factor(groupvar2a), group=interaction(groupvar2a, colourvar2a))) + 
  geom_line(size=1) +
  geom_vline(aes(xintercept = 17)) + annotate(geom = "text", x=17, y=-4, hjust=0, vjust=-0.5, label = "Understand", angle=90, size=3, fontface = "italic") +
  geom_vline(aes(xintercept = 18)) + annotate(geom = "text", x=18, y=-4, hjust=0, vjust=-0.5, label = "Prepare", angle=90, size=3, fontface = "italic") +
  geom_vline(aes(xintercept = 20)) + annotate(geom = "text", x=20, y=-4, hjust=0, vjust=-0.5, label = "Lead", angle=90, size=3, fontface = "italic") +
  xlab(xlabel2a) + ylab(ylabel2a) + 
  scale_colour_discrete(name=lname12a, labels=llabels12a, guide=guide_legend(order=1)) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  scale_linetype_discrete(name=lname22a, labels=llabels22a, guide=guide_legend(order=1)) +
  scale_x_continuous(breaks=14:21) +
  scale_y_continuous(limits=c(-4,4)) + theme_economist() +
  ggtitle(tlabel2a) + labs(subtitle=sublabel2a,caption=caption2a)
print(plot2a)
