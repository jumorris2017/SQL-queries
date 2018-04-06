#makes a plot of AM/PM Speed and CC monthly scores
#for Brittney; Strat Plan

#load libraries
library(data.table)
library(ggplot2)
library(ggthemes)

#set path (new Q)
data_dir <- "O:/CoOp/CoOp194_PROReportng&OM/Julie"

#load data
ce <- fread(paste0(data_dir,"/2018-04-06_speed-cc_am-pm_byperiod.csv"))

#drop April FY18
ce <- ce[!(FSCL_YR_NUM==2018&FSCL_PER_IN_YR_NUM==7)]

#make percent
ce[, TB_SCORE := TB_SCORE*100]

#make year-month variable for plotting
ce[, fy := stringi::stri_sub(FSCL_YR_NUM,-2,-1)]
ce[, fyfp := paste0(fy,".",stringr::str_pad(ce[,FSCL_PER_IN_YR_NUM],2,pad="0"))]

#make a plotting height label
ce[QSTN_ID=="Q2_1"&DAY_PART=="am", value_y := TB_SCORE+3.5]
ce[QSTN_ID=="Q2_1"&DAY_PART=="pm", value_y := TB_SCORE-3.5]
ce[QSTN_ID=="Q2_2"&DAY_PART=="am", value_y := TB_SCORE+3.5]
ce[QSTN_ID=="Q2_2"&DAY_PART=="pm", value_y := TB_SCORE-3.5]

#set labels
# xlabels <- c("Mar 17", "Apr 17", "May 17", "June 17", "July 17", "Aug 17", 
#              "Sep 17", "Oct 17", "Nov 17", "Dec 17", "Jan 18", "Feb 18")
ylabel <- "TB Score"
tlabel <- "AM/PM Customer Experience: Speed and CC"
sublabel <- "US Company-Operated Stores, June 2016 - March 2018"
#manual legend labels
lname1 <- "Day Part"
llabels1 <- c("AM","PM")
lname2 <- "Metric"
llabels2 <- c("Speed","Customer Connection")
#values
pdata <- ce
px <- ce[,fyfp]
py <- ce[,TB_SCORE]
groupvar <- ce[,QSTN_ID]
colourvar <- ce[,DAY_PART]
#plot itself
plot2 <- ggplot(data=pdata, aes(x=px, y=py, colour=factor(colourvar), group=interaction(groupvar, colourvar))) + 
  geom_line(size=1) +
  xlab("") + ylab(ylabel) + 
  #scale_x_discrete(labels="") + 
  scale_colour_discrete(name="", labels=llabels1, guide=guide_legend(order=1)) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  scale_y_continuous(limits=c(0,90)) + theme_economist_white(gray_bg = FALSE) +
  ggtitle(tlabel) + labs(subtitle=sublabel) +
  annotate(size=5, geom="text", x=1, y=70, label= "Speed",hjust = 0) +
  annotate(size=5, geom="text", x=1, y=40, label= "Customer Connection",hjust = 0) +
  geom_text(size = 3.5, aes(label=py,y=value_y), stat="identity")
print(plot2)


#load data
ce <- fread(paste0(data_dir,"/2018-04-06_speed-cc_am-pm_byperiod.csv"))

#agg by fiscal year
ce <- ce[, list(TOTAL_TB = sum(TOTAL_TB),
                TOTAL_RSPNS = sum(TOTAL_RSPNS),
                TB_SCORE = round(sum(TOTAL_TB)/sum(TOTAL_RSPNS),3)*100),
         by=c("QSTN_ID","DAY_PART","FSCL_YR_NUM")]


#plot 1
xlabels <- c("FY16","FY17","FY18")
ylabel <- "TB Score"
tlabel <- "AM/PM Customer Experience: Speed"
sublabel <- "US Company-Operated Stores, 2016 - 2018"
#manual legend labels
lname <- "Day Part"
llabels <- c("AM","PM")
#values
pdata1a <- ce[QSTN_ID=="Q2_1"]
px1a <- ce[QSTN_ID=="Q2_1",FSCL_YR_NUM]
py1a <- ce[QSTN_ID=="Q2_1",TB_SCORE]
groupvar1a <- ce[QSTN_ID=="Q2_1",DAY_PART]
#plot itself
plot1a <- ggplot(data=pdata1a,aes(y=py1a,x=as.factor(px1a),fill=as.factor(groupvar1a))) + 
  geom_bar(stat="identity", width = 0.95, position=position_dodge(), colour="black") +
  scale_fill_brewer(palette = 1, name=lname, labels=llabels) + theme_economist() +
  scale_x_discrete(name="",labels=xlabels) +
  xlab("") + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel) +
  geom_text(size = 4, aes(label=py1a,y=0), stat="identity", vjust = -2, position = position_dodge(0.95))
print(plot1a)

#plot 2
xlabels <- c("FY16","FY17","FY18")
ylabel <- "TB Score"
tlabel <- "AM/PM Customer Experience: Customer Connection"
sublabel <- "US Company-Operated Stores, 2016 - 2018"
#manual legend labels
lname <- "Day Part"
llabels <- c("AM","PM")
#values
pdata1a <- ce[QSTN_ID=="Q2_2"]
px1a <- ce[QSTN_ID=="Q2_2",FSCL_YR_NUM]
py1a <- ce[QSTN_ID=="Q2_2",TB_SCORE]
groupvar1a <- ce[QSTN_ID=="Q2_2",DAY_PART]
#plot itself
plot1a <- ggplot(data=pdata1a,aes(y=py1a,x=as.factor(px1a),fill=as.factor(groupvar1a))) + 
  geom_bar(stat="identity", width = 0.95, position=position_dodge(), colour="black") +
  scale_fill_brewer(palette = 1, name=lname, labels=llabels) + theme_economist() +
  scale_x_discrete(name="",labels=xlabels) +
  xlab("") + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel) +
  geom_text(size = 4, aes(label=py1a,y=0), stat="identity", vjust = -2, position = position_dodge(0.95))
print(plot1a)




