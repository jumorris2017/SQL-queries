##pulling CE for unicorn frapp and HH for Strat Plan
##12/18/17

#load libraries
library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)

#read file
#ce <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc_bycalweek_121817.csv") #edw
caw <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc_bycalweek_121917.csv") #caw

###PUT CAW INTO EDW'S FORMAT
caw <- setnames(caw,c("FSCL_YR_NUM","FSCL_WK_IN_YR_NUM"),c("FSCL_YEAR","FSCL_WEEK"))
#calculate TB score
caw[, tbscore := TOTAL_TB/TOTAL_RSPNS]
#swing wide
caw <- dcast.data.table(caw, FSCL_YEAR + FSCL_WEEK ~ QSTN_ID, value.var="tbscore")
###

# #convert TB scores to numeric
# ce[, (grep("TB_SCORE",colnames(ce),value=T)) := lapply(.SD,as.numeric), .SDcols=grep("TB_SCORE",colnames(ce),value=T)]

#set order
ce <- copy(caw)
ce <- setorder(ce,FSCL_YEAR,FSCL_WEEK)

#keep only weeks we want
ce <- ce[FSCL_WEEK<=52]
ce <- ce[FSCL_YEAR>=2015]

#assign cal year
ce[FSCL_YEAR==2015&FSCL_WEEK>=14&FSCL_WEEK<=52, calyear := 2015]
ce[FSCL_YEAR==2016&FSCL_WEEK>=1&FSCL_WEEK<=13, calyear := 2015]

ce[FSCL_YEAR==2016&FSCL_WEEK>=14&FSCL_WEEK<=52, calyear := 2016]
ce[FSCL_YEAR==2017&FSCL_WEEK>=1&FSCL_WEEK<=13, calyear := 2016]

ce[FSCL_YEAR==2017&FSCL_WEEK>=14&FSCL_WEEK<=52, calyear := 2017]
ce[FSCL_YEAR==2018&FSCL_WEEK>=1&FSCL_WEEK<=13, calyear := 2017]

#assign cal week
ce[FSCL_WEEK>=14&FSCL_WEEK<=52, calweek := FSCL_WEEK-13]
ce[FSCL_WEEK>=1&FSCL_WEEK<=13, calweek := FSCL_WEEK+39]

#graphs of CC, Speed, and WP by year
#WP
#set labels
xlabel <- "Fiscal Week"
ylabel <- "Worth Top Box Score"
tlabel <- "Purchase Worth Top Box Score"
slabel <- "Note: Unicorn Frappuccino launched 04-19-17 (FW 29),  Happy Hour launched 05-05-17 (FW 31)"
#set data and variables
pdata <- ce
px <- ce[, calweek]
pxmap <- c(c(14:52),c(1:13))
py <- ce[, Q2_8]
groupvar <- ce[, calyear]
#manual legend labels
lname <- "Calendar Year"
llabels <- c("2015","2016","2017")
xbreaks <- 9
#line chart, factored by one variable
#cal weeks 16, 17, 20 -> fiscal weeks 29, 30, 33
plot2 <- ggplot() +
  geom_line(data=pdata, aes(x=px, y=py, group=factor(groupvar), colour=factor(groupvar))) + 
  xlab(xlabel) + ylab(ylabel) + theme_bw() +
  scale_colour_manual(values=c("royalblue3","orangered","forestgreen"), name=lname, labels=llabels, guide=guide_legend(reverse=T)) +
  scale_x_continuous(breaks=1:52,labels=pxmap) +
  theme(axis.text.x=element_text(size=7)) +
  #scale_y_continuous(limits=c(0,.7),labels=scales::percent) +
  scale_y_continuous(limits=c(pdata[,min(py)]*.75,pdata[,max(py)]*1.05),labels=scales::percent) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  #geom_vline(data=pdata, aes(xintercept=16), size=.5, colour="black") +
  geom_vline(data=pdata, aes(xintercept=18), linetype="dashed", size=.5, colour="black") +
  geom_vline(data=pdata, aes(xintercept=16), linetype="dashed", size=.5, colour="black") +
  labs(title = tlabel)
  #labs(title = tlabel, subtitle = slabel)
print(plot2)




#CC
#set labels
xlabel <- "Fiscal Week"
ylabel <- "CC Top Box Score"
tlabel <- "Customer Connection Top Box Score"
slabel <- "Note: Unicorn Frappuccino launched 04-19-17 (FW 29),  Happy Hour launched 05-05-17 (FW 31)"
#set data and variables
pdata <- ce
px <- ce[, calweek]
pxmap <- c(c(14:52),c(1:13))
py <- ce[, Q2_2]
groupvar <- ce[, calyear]
#manual legend labels
lname <- "Calendar Year"
llabels <- c("2015","2016","2017")
xbreaks <- 9
#line chart, factored by one variable
#cal weeks 16, 17, 20 -> fiscal weeks 29, 30, 33
plot2 <- ggplot() +
  geom_line(data=pdata, aes(x=px, y=py, group=factor(groupvar), colour=factor(groupvar))) + 
  xlab(xlabel) + ylab(ylabel) + theme_bw() +
  scale_colour_manual(values=c("royalblue3","orangered","forestgreen"), name=lname, labels=llabels, guide=guide_legend(reverse=T)) +
  scale_x_continuous(breaks=1:52,labels=pxmap) +
  theme(axis.text.x=element_text(size=7)) +
  scale_y_continuous(limits=c(pdata[,min(py)]*.75,.36), breaks = scales::pretty_breaks(n = 6), labels=scales::percent) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  #geom_vline(data=pdata, aes(xintercept=16), size=.5, colour="black") +
  geom_vline(data=pdata, aes(xintercept=18), linetype="dashed", size=.5, colour="black") +
  geom_vline(data=pdata, aes(xintercept=16), linetype="dashed", size=.5, colour="black") +
  labs(title = tlabel)
#labs(title = tlabel, subtitle = slabel)
print(plot2)





#Speed
#set labels
xlabel <- "Fiscal Week"
ylabel <- "Speed Top Box Score"
tlabel <- "Speed of Service Top Box Score"
slabel <- "Note: Unicorn Frappuccino launched 04-19-17 (FW 29),  Happy Hour launched 05-05-17 (FW 31)"
#set data and variables
pdata <- ce
px <- ce[, calweek]
pxmap <- c(c(14:52),c(1:13))
py <- ce[, Q2_1]
groupvar <- ce[, calyear]
#manual legend labels
lname <- "Calendar Year"
llabels <- c("2015","2016","2017")
xbreaks <- 9
#line chart, factored by one variable
#cal weeks 16, 17, 20 -> fiscal weeks 29, 30, 33
plot2 <- ggplot() +
  geom_line(data=pdata, aes(x=px, y=py, group=factor(groupvar), colour=factor(groupvar))) + 
  xlab(xlabel) + ylab(ylabel) + theme_bw() +
  scale_colour_manual(values=c("royalblue3","orangered","forestgreen"), name=lname, labels=llabels, guide=guide_legend(reverse=T)) +
  scale_x_continuous(breaks=1:52,labels=pxmap) +
  theme(axis.text.x=element_text(size=7)) +
  #scale_y_continuous(limits=c(0,.75),labels=scales::percent) +
  scale_y_continuous(limits=c(pdata[,min(py)]*.85,pdata[,max(py)]*1.05),breaks = scales::pretty_breaks(n = 6), labels=scales::percent) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  #geom_vline(data=pdata, aes(xintercept=16), size=.5, colour="black") +
  geom_vline(data=pdata, aes(xintercept=18), linetype="dashed", size=.5, colour="black") +
  geom_vline(data=pdata, aes(xintercept=16), linetype="dashed", size=.5, colour="black") +
  labs(title = tlabel)
#labs(title = tlabel, subtitle = slabel)
print(plot2)






#read file
caw <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/ce_last3years.csv") #caw

###PUT CAW INTO EDW'S FORMAT
caw <- setnames(caw,c("FSCL_YR_NUM","FSCL_WK_IN_YR_NUM"),c("FSCL_YEAR","FSCL_WEEK"))
#calculate TB score
caw[, tbscore := round(TOTAL_TB/TOTAL_RSPNS,4)*100]
#swing wide
caw <- dcast.data.table(caw, FSCL_YEAR + FSCL_WEEK ~ QSTN_ID, value.var="tbscore")
###
caw <- setorder(caw,FSCL_YEAR,FSCL_WEEK)
#write file
write.xlsx(caw,file="C:/Users/jumorris/Documents/CE_Jan15-Dec17.xlsx")