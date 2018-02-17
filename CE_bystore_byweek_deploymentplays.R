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

#group by stores using/not using plays
###this is fake for setting up code
dp[STORE_NUM<10000, useploy := 0]; dp[STORE_NUM>=10000, useploy := 1]

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

#MELT
dp <- dp[, .(FSCL_WK_IN_YR_NUM,FSCL_YR_NUM,fyfw,useploy,cc_sc,so_sc,sp_sc,cl_sc,ccdelta,sodelta)]
dp <- melt(dp, id=c("fyfw","useploy","FSCL_WK_IN_YR_NUM","FSCL_YR_NUM"))

##CC plot
#set labels
xlabels1a <- c("FW 14","FW 15","FW 16","FW 17","FW 18","FW 19","FW 20","FW 21")
# xlabel1a <- c("Fiscal Week")
ylabel1a <- "TB Score"
tlabel1a <- "Deployment Play Utilization on Customer Experience"
sublabel1a <- "Customer Connection"
# caption1a <- "U.S. Company Operated Stores"
#manual legend labels
lname11a <- "Fiscal Year"
llabels11a <- c("2017","2018")
lname21a <- "Deployment Play Utilization"
llabels21a <- c("Not utilized","Utilized")
#values
pdata1a <- dp[variable=="cc_sc"]
px1a <- dp[variable=="cc_sc",FSCL_WK_IN_YR_NUM]
py1a <- dp[variable=="cc_sc",value]
groupvar1a <- dp[variable=="cc_sc",useploy]
colourvar1a <- dp[variable=="cc_sc",FSCL_YR_NUM]
#plot itself
plot1a <- ggplot(data=pdata1a, aes(x=px1a, y=py1a, colour=factor(colourvar1a), linetype=factor(groupvar1a), group=interaction(groupvar1a, colourvar1a))) + 
  geom_line(size=1) +
  xlab("") + ylab(ylabel1a) + 
  scale_colour_discrete(name=lname11a, labels=llabels11a, guide=guide_legend(order=1)) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  scale_linetype_discrete(name=lname21a, labels=llabels21a, guide=guide_legend(order=1)) +
  scale_x_continuous(breaks=14:21) +
  scale_y_continuous(limits=c(20,40)) + theme_economist() +
  ggtitle(tlabel1a) + labs(subtitle=sublabel1a)
print(plot1a)


##CC plot
#set labels
xlabels2a <- c("FW 14","FW 15","FW 16","FW 17","FW 18","FW 19","FW 20","FW 21")
xlabel2a <- c("Fiscal Week")
ylabel2a <- "TB Score"
# tlabel2a <- "Deployment Play Utilization on Customer Experience"
sublabel2a <- "Store Operations"
caption2a <- "U.S. Company Operated Stores"
#manual legend labels
# lname12a <- "Fiscal Year"
# llabels12a <- c("2017","2018")
# lname22a <- "Deployment Play Utilization"
# llabels22a <- c("Not utilized","Utilized")
#values
pdata2a <- dp[variable=="so_sc"]
px2a <- dp[variable=="so_sc",FSCL_WK_IN_YR_NUM]
py2a <- dp[variable=="so_sc",value]
groupvar2a <- dp[variable=="so_sc",useploy]
colourvar2a <- dp[variable=="so_sc",FSCL_YR_NUM]
#plot itself
plot2a <- ggplot(data=pdata2a, aes(x=px2a, y=py2a, colour=factor(colourvar2a), linetype=factor(groupvar2a), group=interaction(groupvar2a, colourvar2a))) + 
  geom_line(size=1) +
  xlab(xlabel2a) + ylab(ylabel2a) + 
  scale_colour_discrete(name="", labels="", guide=FALSE) +
  scale_linetype_discrete(name="", labels="", guide=FALSE) +
  scale_x_continuous(breaks=14:21) +
  scale_y_continuous(limits=c(55,65)) + theme_economist() +
  ggtitle("") + labs(subtitle=sublabel2a,caption=caption2a)
print(plot2a)

plot1a / plot2a
