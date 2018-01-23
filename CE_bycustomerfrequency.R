##CE Trend by Customer Transaction Frequency##
##To assess if low frequency customers differ from higher frequency, with the goal
###of extrapolating finding to non-SR members (since we assume they are lower freq)##
##From Lisa 1/3/18##

#load data
#load libraries
library(data.table)
library(ggplot2)
library(ggthemes)
library(stringr)
library(dplyr)

#load data
cc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc_by_customer_frequency.csv")
#calculate CC score (percent)
cc[, CC_SCORE := (TB_COUNT/RSPSN_COUNT)*100]
#group 26+ as 26 (N's get very low at the higher end)
cc[TRANS>=26, transgrp := 26]; cc[TRANS<26, transgrp := TRANS]
#pads numbers - add 0 before number
cc[, monthvar := str_pad(cc[,FSCL_PER_IN_YR_NUM], 2, pad = "0")]
#create new year/month var for plotting
cc[, fpfy := paste(FSCL_YR_NUM,monthvar,sep="-")]

#aggreagate by transgrp
cc <- cc[, list(USER_COUNT = sum(USER_COUNT,na.rm=T),
          TB_COUNT = sum(TB_COUNT,na.rm=T),
          RSPSN_COUNT = sum(RSPSN_COUNT,na.rm=T),
          CC_SCORE = sum(TB_COUNT,na.rm=T)/sum(RSPSN_COUNT,na.rm=T)),
   by=c("transgrp","fpfy")]

#plot
#set labels
xlabel <- "Monthly Transactions"
ylabel <- "CC Score"
tlabel <- "CC by Customer Frequency"
caption <- "Note: 26+ transactions grouped as 26"
sublabel <- "January FY 2017 - December FY 2018"
#set data and variables
pdata <- cc
px <- cc[, fpfy]
py <- cc[, CC_SCORE]
groupvar <- cc[, transgrp]
#manual legend labels
lname <- "Monthly Trans"
#plot
p1 <- ggplot(data=pdata) +
  geom_line(aes(x=px, y=py, group=factor(groupvar), colour=factor(groupvar))) + 
  xlab(xlabel) + ylab(ylabel) + theme_economist() + 
  scale_colour_discrete(name=lname) + guides(col = guide_legend(ncol = 13)) +
  ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption)
print(p1)



#load data
cc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc_by_customer_frequency.csv")
#calculate CC score (percent)
cc[, CC_SCORE := (TB_COUNT/RSPSN_COUNT)*100]
#group 26+ as 26 (N's get very low at the higher end)
cc[TRANS>=26, transgrp := 26]; cc[TRANS<26, transgrp := TRANS]
#create trans groups that match survey dist
cc[TRANS>=1&TRANS<=5, transgrp2 := 1]
cc[TRANS>=6&TRANS<=10, transgrp2 := 2]
cc[TRANS>=11&TRANS<=15, transgrp2 := 3]
cc[TRANS>=16&TRANS<=25, transgrp2 := 4]
cc[TRANS>=26, transgrp2 := 5]
#pads numbers - add 0 before number
cc[, monthvar := str_pad(cc[,FSCL_PER_IN_YR_NUM], 2, pad = "0")]
#create new year/month var for plotting
cc[, fpfy := paste(FSCL_YR_NUM,monthvar,sep="-")]

#aggreagate by transgrp
cc <- cc[, list(USER_COUNT = sum(USER_COUNT,na.rm=T),
                TB_COUNT = sum(TB_COUNT,na.rm=T),
                RSPSN_COUNT = sum(RSPSN_COUNT,na.rm=T),
                CC_SCORE = sum(TB_COUNT,na.rm=T)/sum(RSPSN_COUNT,na.rm=T)),
         by=c("transgrp2","fpfy")]

#plot
#set labels
xlabel <- "Monthly Transactions"
ylabel <- "CC Score"
tlabel <- "CC by Customer Frequency"
sublabel <- "January FY 2017 - December FY 2018"
#set data and variables
pdata <- cc
px <- cc[, fpfy]
py <- cc[, CC_SCORE]
groupvar <- cc[, transgrp2]
#manual legend labels
lname <- "Monthly Trans"
llabels <- c("1-5","6-10","11-15","16-25","26+") 
#plot
p1 <- ggplot(data=pdata) +
  geom_line(aes(x=px, y=py, group=factor(groupvar), colour=factor(groupvar))) + 
  xlab(xlabel) + ylab(ylabel) + theme_economist() + 
  scale_colour_discrete(name=lname,labels=llabels) + guides(col = guide_legend(ncol = 13)) +
  ggtitle(tlabel) + labs(subtitle=sublabel)
print(p1)






#load data
cc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc_by_customer_frequency.csv")
cc[, CC_SCORE := (TB_COUNT/RSPSN_COUNT)*100]
#pads numbers - add 0 before number
cc[, monthvar := str_pad(cc[,FSCL_PER_IN_YR_NUM], 2, pad = "0")]
#create new year/month var for plotting
cc[, fpfy := paste(FSCL_YR_NUM,monthvar,sep="-")]

#aggreagate by transgrp
cc <- cc[, list(USER_COUNT = sum(USER_COUNT,na.rm=T),
                TB_COUNT = sum(TB_COUNT,na.rm=T),
                RSPSN_COUNT = sum(RSPSN_COUNT,na.rm=T),
                CC_SCORE = sum(TB_COUNT,na.rm=T)/sum(RSPSN_COUNT,na.rm=T)),
         by=c("fpfy")]
#plot
#set labels
xlabel <- "Monthly Transactions"
ylabel <- "CC Score"
tlabel <- "CC by Customer Frequency"
sublabel <- "January FY 2017 - December FY 2018"
#set data and variables
pdata <- cc
px <- cc[, fpfy]
py <- cc[, CC_SCORE]
#plot
p2 <- ggplot() +
  geom_line(data=pdata,aes(x=px, y=py, group = 1)) + 
  xlab(xlabel) + ylab(ylabel) + theme_economist() + 
  scale_y_continuous(limits=c(0,.4)) +
  ggtitle(tlabel) + labs(subtitle=sublabel)
print(p2)
