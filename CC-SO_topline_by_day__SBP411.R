##request: CC and SO pre- and post- holiday launch for Lisa request 11/15/17

#load libraries
library(data.table)
library(xlsx)
library(ggplot2)

#Request #1: CC & SO perceptions pre- and post- holiday launch

#ALL DATA - FROM SUBQUERIED SQL

#load data
tpl <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC-SO_topline_by_day_120517.csv")

#CC
tpl[, Q2_2_TB_SCORE := Q2_2_TB_CNT/Q2_2_RESPONSE_TOTAL]
#SO sub categories
tpl[, Q2_1_TB_SCORE := Q2_1_TB_CNT/Q2_1_RESPONSE_TOTAL]
tpl[, Q2_3_TB_SCORE := Q2_3_TB_CNT/Q2_3_RESPONSE_TOTAL]
tpl[, Q2_4_TB_SCORE := Q2_4_TB_CNT/Q2_4_RESPONSE_TOTAL]
tpl[, Q2_5_TB_SCORE := Q2_5_TB_CNT/Q2_5_RESPONSE_TOTAL]
tpl[, Q2_6_TB_SCORE := Q2_6_TB_CNT/Q2_6_RESPONSE_TOTAL]
tpl[, Q2_7_TB_SCORE := Q2_7_TB_CNT/Q2_7_RESPONSE_TOTAL]
#average the SO scores
tpl[, SO_TB_SCORE := rowMeans(.SD, na.rm = TRUE),
    .SDcols = c("Q2_1_TB_SCORE", "Q2_3_TB_SCORE", "Q2_4_TB_SCORE",
                "Q2_5_TB_SCORE", "Q2_6_TB_SCORE", "Q2_7_TB_SCORE")]

#keep only variables we need
#tpl <- tpl[, (colnames(tpl[,-c(1:18)])), with=FALSE]
tpl1 <- tpl[, c("CAL_DATE","CAL_YEAR","CAL_DAY","Q2_2_TB_SCORE","SO_TB_SCORE"), with=FALSE]
tpl1[, CAL_YEAR := as.character(CAL_YEAR)]
tpl1[, CAL_DATE := as.Date(CAL_DATE, "%d-%b-%y")]

#subset dates
tpl1 <- tpl1[CAL_DAY>=307&CAL_DAY<=323]

#create new holiday launch date variable
tpl1[CAL_YEAR==2015, hollaunch := CAL_DAY-314]
tpl1[CAL_YEAR==2016, hollaunch := CAL_DAY-315]
tpl1[CAL_YEAR==2017, hollaunch := CAL_DAY-313]

#subset dates
tpl1 <- tpl1[hollaunch>=-3&hollaunch<=7]

#CC
#set labels
xlabel <- "Days Pre- and Post-Launch"
ylabel <- "Top Box Scores"
tlabel <- "Holiday Launch - Customer Connection"
slabel <- ""
#set data and variables
pdata <- tpl1
px <- tpl1[, hollaunch]
py <- tpl1[, Q2_2_TB_SCORE]
py2 <- tpl1[, SO_TB_SCORE]
groupvar <- tpl1[, CAL_YEAR]
#manual legend labels
lname <- "Calendar Year"
llabels <- c("2015","2016","2017") 
xbreaks <- 10
ybreaks <- 6
#line chart, factored by one variable
plot1 <- ggplot() +
  geom_line(data=pdata, aes(x=px, y=py, group=factor(groupvar), colour=factor(groupvar))) + 
  xlab(xlabel) + ylab(ylabel) + theme_bw() +
  scale_colour_discrete(name=lname, labels=llabels, guide=guide_legend(order=1)) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  scale_x_continuous(limits=c(pdata[,min(px)],pdata[,max(px)]), breaks = scales::pretty_breaks(n = xbreaks)) +
  scale_y_continuous(limits=c(pdata[,min(py)*.85],pdata[,max(py)]*1.1),labels=scales::percent, breaks = scales::pretty_breaks(n = ybreaks)) +
  geom_vline(data=pdata, aes(xintercept=0), size=.5, colour="black") +
  #geom_vline(data=pdata, aes(xintercept=315), size=.5, colour="green") +
  #geom_vline(data=pdata, aes(xintercept=313), size=.5, colour="blue") +
  #geom_vline(data=pdata, aes(xintercept=45), linetype="dashed", size=.5, colour="black") +
  labs(title = tlabel, subtitle = slabel)
print(plot1)

#SO
#set labels
xlabel <- "Days Pre- and Post-Launch"
ylabel <- "Top Box Scores"
tlabel <- "Holiday Launch - Store Operations"
slabel <- ""
#set data and variables
pdata <- tpl1
px <- tpl1[, hollaunch]
py <- tpl1[, SO_TB_SCORE]
groupvar <- tpl1[, CAL_YEAR]
#manual legend labels
lname <- "Calendar Year"
llabels <- c("2015","2016","2017") 
xbreaks <- 10
ybreaks <- 6
#line chart, factored by one variable
plot2 <- ggplot() +
  geom_line(data=pdata, aes(x=px, y=py, group=factor(groupvar), colour=factor(groupvar))) + 
  xlab(xlabel) + ylab(ylabel) + theme_bw() +
  scale_colour_discrete(name=lname, labels=llabels, guide=guide_legend(order=1)) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  scale_x_continuous(limits=c(pdata[,min(px)],pdata[,max(px)]), breaks = scales::pretty_breaks(n = xbreaks)) +
  scale_y_continuous(limits=c(pdata[,min(py)*.85],pdata[,max(py)]*1.1),labels=scales::percent, breaks = scales::pretty_breaks(n = ybreaks)) +
  geom_vline(data=pdata, aes(xintercept=0), size=.5, colour="black") +
  labs(title = tlabel, subtitle = slabel)
print(plot2)


#keep only variables we need
#tpl <- tpl[, (colnames(tpl[,-c(1:18)])), with=FALSE]
tpl2 <- tpl[, c("CAL_DATE","CAL_YEAR","CAL_DAY","DAY_ABBR_NM","Q2_2_TB_SCORE","SO_TB_SCORE"), with=FALSE]
tpl2[, CAL_YEAR := as.character(CAL_YEAR)]
tpl2[, CAL_DATE := as.Date(CAL_DATE, "%d-%b-%y")]

#subset dates
tpl2 <- tpl2[CAL_DAY>=301&CAL_DAY<=327]

#create new holiday launch date variable
# tpl2[CAL_YEAR==2015, hollaunch := CAL_DAY-314]
# tpl2[CAL_YEAR==2016, hollaunch := CAL_DAY-315]
# tpl2[CAL_YEAR==2017, hollaunch := CAL_DAY-313]

#create new day of week of launch week variable
tpl2[CAL_YEAR==2015, hollaunch := CAL_DAY-314] #day 314 is tuesday
tpl2[CAL_YEAR==2016, hollaunch := CAL_DAY-313] #day 313 is tuesday
tpl2[CAL_YEAR==2017, hollaunch := CAL_DAY-311] #day 311 is tuesday

#subset dates
tpl2 <- tpl2[hollaunch>=-3&hollaunch<=7]

#CC
#set labels
xlabel <- "Day of Launch Week"
ylabel <- "Top Box Scores"
tlabel <- "Holiday Launch - Customer Connection"
slabel <- "2015 launched on a Tuesday, 2016 and 2017 launched on Thursdays"
#set data and variables
pdata <- tpl2
px <- tpl2[, hollaunch]
py <- tpl2[, Q2_2_TB_SCORE]
py2 <- tpl2[, SO_TB_SCORE]
groupvar <- tpl2[, CAL_YEAR]
#manual legend labels
lname <- "Calendar Year"
llabels <- c("2015","2016","2017") 
ybreaks <- 6
xbreaks <- 11
xaxislabels <- c("Sa","Su","Mo","Tu","We","Th","Fr","Sa","Su","Mo","Tu")
#line chart, factored by one variable
plot3 <- ggplot() +
  geom_line(data=pdata, aes(x=px, y=py, group=factor(groupvar), colour=factor(groupvar))) + 
  xlab(xlabel) + ylab(ylabel) + theme_bw() +
  scale_colour_discrete(name=lname, labels=llabels, guide=guide_legend(order=1)) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  scale_x_continuous(limits=c(pdata[,min(px)],pdata[,max(px)]), breaks = c(-3:7), labels=xaxislabels) +
  scale_y_continuous(limits=c(pdata[,min(py)*.85],pdata[,max(py)]*1.1),labels=scales::percent, breaks = scales::pretty_breaks(n = ybreaks)) +
  geom_vline(data=pdata, aes(xintercept=0), size=.5, colour="red") +
  geom_vline(data=pdata, aes(xintercept=2), size=.5, colour="blue") +
  labs(title = tlabel, subtitle = slabel)
print(plot3)

#SO
#set labels
xlabel <- "Day of Launch Week"
ylabel <- "Top Box Scores"
tlabel <- "Holiday Launch - Store Operations"
slabel <- "2015 launched on a Tuesday, 2016 and 2017 launched on Thursdays"
#set data and variables
pdata <- tpl2
px <- tpl2[, hollaunch]
py <- tpl2[, SO_TB_SCORE]
groupvar <- tpl2[, CAL_YEAR]
#manual legend labels
lname <- "Calendar Year"
llabels <- c("2015","2016","2017") 
ybreaks <- 6
xbreaks <- 11
xaxislabels <- c("Sa","Su","Mo","Tu","We","Th","Fr","Sa","Su","Mo","Tu")
#line chart, factored by one variable
plot4 <- ggplot() +
  geom_line(data=pdata, aes(x=px, y=py, group=factor(groupvar), colour=factor(groupvar))) + 
  xlab(xlabel) + ylab(ylabel) + theme_bw() +
  scale_colour_discrete(name=lname, labels=llabels, guide=guide_legend(order=1)) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  scale_x_continuous(limits=c(pdata[,min(px)],pdata[,max(px)]), breaks = c(-3:7), labels=xaxislabels) +
  scale_y_continuous(limits=c(pdata[,min(py)*.85],pdata[,max(py)]*1.1),labels=scales::percent, breaks = scales::pretty_breaks(n = ybreaks)) +
  geom_vline(data=pdata, aes(xintercept=0), size=.5, colour="red") +
  geom_vline(data=pdata, aes(xintercept=2), size=.5, colour="blue") +
  labs(title = tlabel, subtitle = slabel)
print(plot4)





