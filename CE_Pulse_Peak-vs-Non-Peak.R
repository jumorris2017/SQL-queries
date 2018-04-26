
#load libraries
library(data.table)
library(lubridate)
library(stringr)

#load data
data_dir <- "O:/CoOp/CoOp194_PROReportng&OM/Julie"
ce <- fread(paste0(data_dir,"/ce_byperiod_peak-vs-nonpeak.csv"))
pu <- fread(paste0(data_dir,"/pulse_byperiod_peak-vs-nonpeak.csv"))

#make year and period variable for plotting
ce[, fyfp := paste0(FSCL_YR_NUM,".",str_pad(ce[,FSCL_PER_IN_YR_NUM],2,pad="0"))]

#restrict to only P9 FY 16 on
ce <- ce[fyfp>=2016.09]

#set labels
xlabels <- c(ce[,fyfp])
ylabel <- "TB Score"
tlabel <- "Customer Experience by Peak and Non-Peak"
sublabel <- "U.S. Company-Operated Stores"
caption <- "'Peak' defined as 7-11am; 'Non-Peak' all other time periods"
#manual legend labels
lname2 <- "Day Part"
llabels2 <- c("Non-Peak","Peak")
lname1 <- "CE Metric"
llabels1 <- c("Speed","Customer Connection","Cleanliness")
#plot by question and day part
pdata <- ce
xvar <- ce[,fyfp]
yvar <- ce[,TB_SCORE]
groupvar <- ce[,QSTN_ID]
colourvar <- ce[,DAY_PART]
#plot
plot1 <- ggplot(pdata,aes(x=factor(xvar),y=yvar,colour=factor(groupvar),point=factor(colourvar),group=interaction(groupvar, colourvar))) + 
  geom_point(size=2) + geom_line(size=1) +
  xlab("") + ylab(ylabel) + 
  scale_x_discrete(breaks = px[seq(1, length(px), by = 4)]) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_colour_discrete(name="", labels=llabels1, guide=guide_legend(order=1)) +
  #guides(colour = guide_legend(override.aes = list(size = 7))) + 
  theme_economist_white(gray_bg = FALSE) +
  ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) 
print(plot1)


#set labels
xlabels <- c(ce[,fyfp])
ylabel <- "TB Score"
tlabel <- "Speed by Peak and Non-Peak"
sublabel <- "U.S. Company-Operated Stores"
caption <- "'Peak' defined as 7-11am; 'Non-Peak' all other time periods"
#manual legend labels
lname2 <- "Day Part"
llabels2 <- c("Non-Peak","Peak")
#plot by question and day part
pdata <- ce[QSTN_ID=='Q2_1']
px <- ce[QSTN_ID=='Q2_1',fyfp]
py <- ce[QSTN_ID=='Q2_1',TB_SCORE]
groupvar <- ce[QSTN_ID=='Q2_1',DAY_PART]
#plot
plot2 <- ggplot() +
  geom_line(data=pdata, aes(x=px, y=py, group=factor(groupvar), colour=factor(groupvar))) + 
  xlab("") + ylab(ylabel) + theme_bw() +
  scale_x_discrete(breaks = px[seq(1, length(px), by = 2)]) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_colour_discrete(name="", labels=llabels1, guide=guide_legend(order=1)) +
  #guides(colour = guide_legend(override.aes = list(size = 7))) + 
  scale_y_continuous(limits=c(.7,.9)) +
  theme_economist_white(gray_bg = FALSE) +
  ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) 
print(plot2)

write.csv(ce,file=paste0(data_dir,"/forlisa-ce_byperiod_peak-vs-nonpeak.csv"))
write.csv(temp,file=paste0(data_dir,"/forlisa-pulse_byperiod_peak-vs-nonpeak.csv"))








#edit partner data
temp <- pu[QUESTION_ID=='Q2_D']
temp[, start_hour := hour(SHIFT_START_DTM_LCL)]
#recode into peak and non-peak
#peak if shift time starts 6-10am (1 hour pre-7 to 11am windwo)
temp[, DAY_PART := 'NON-PEAK']
temp[start_hour>=6&start_hour<=10, DAY_PART := 'PEAK']
#recode to TB
temp[RESP_ID<7, TB_COUNT := 0];temp[RESP_ID==7, TB_COUNT := 1]
#recode to TB3
temp[RESP_ID<5, TB3_COUNT := 0];temp[RESP_ID>=5, TB3_COUNT := 1]
#aggregate by period
temp <- temp[, list(RSPNS_COUNT = .N,
                    TB_COUNT = sum(TB_COUNT,na.rm=T),
                    TB3_COUNT = sum(TB3_COUNT,na.rm=T)), 
             by=c("SHIFT_FSCL_YR_NUM","SHIFT_FSCL_PER_IN_YR_NUM","DAY_PART")]
temp[, TB_SCORE := round(TB_COUNT/RSPNS_COUNT,3)]
temp[, TB3_SCORE := round(TB3_COUNT/RSPNS_COUNT,3)]
#set order
temp <- setorder(temp,SHIFT_FSCL_YR_NUM,SHIFT_FSCL_PER_IN_YR_NUM,-DAY_PART)
#make year and period variable for plotting
temp[, fyfp := paste0(SHIFT_FSCL_YR_NUM,".",str_pad(temp[,SHIFT_FSCL_PER_IN_YR_NUM],2,pad="0"))]

#set labels
llabels1 <- c("Non-Peak", "Peak")
ylabel <- "Top Box Score"
tlabel <- "Pulse (Q2_D) 'Connect with Customers' by Day Part"
sublabel <- "U.S. Company-Operated Stores"
caption <- "'Peak' defined as 7-11am; 'Non-Peak' all other time periods"
#values
pdata1a <- temp
px1a <- temp[,fyfp]
py1a <- temp[,TB_SCORE]
groupvar1a <- temp[,DAY_PART]
#plot itself
plot2 <- ggplot() +
  geom_line(data=pdata1a, aes(x=px1a, y=py1a, group=factor(groupvar1a), colour=factor(groupvar1a))) + 
  xlab("") + ylab(ylabel) + theme_bw() +
  scale_x_discrete(breaks = px[seq(1, length(px), by = 2)]) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_colour_discrete(name="", labels=llabels1, guide=guide_legend(order=1)) +
  #guides(colour = guide_legend(override.aes = list(size = 7))) + 
  scale_y_continuous(limits=c(.3,.6)) +
  theme_economist_white(gray_bg = FALSE) +
  ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) 
print(plot2)






#edit partner data
temp <- pu[QUESTION_ID=='Q1']
temp[, start_hour := hour(SHIFT_START_DTM_LCL)]
#recode into peak and non-peak
#peak if shift time starts 6-10am (1 hour pre-7 to 11am windwo)
temp[, DAY_PART2 := 'REST-OF-DAY']
temp[start_hour>=6&start_hour<=10, DAY_PART2 := 'AM']
temp[start_hour>=13&start_hour<=16, DAY_PART2 := 'PM']
#recode to TB
temp[RESP_ID<7, TB_COUNT := 0];temp[RESP_ID==7, TB_COUNT := 1]
#recode to TB3
temp[RESP_ID<5, TB3_COUNT := 0];temp[RESP_ID>=5, TB3_COUNT := 1]
#aggregate by period
temp <- temp[, list(RSPNS_COUNT = .N,
                    AVG_RESP = mean(RESP_ID,na.rm=T),
                    TB_COUNT = sum(TB_COUNT,na.rm=T),
                    TB3_COUNT = sum(TB3_COUNT,na.rm=T)), 
             by=c("SHIFT_FSCL_YR_NUM","SHIFT_FSCL_PER_IN_YR_NUM","DAY_PART2")]
temp[, TB_SCORE := round(TB_COUNT/RSPNS_COUNT,3)]
temp[, TB3_SCORE := round(TB3_COUNT/RSPNS_COUNT,3)]
#set order
temp <- setorder(temp,SHIFT_FSCL_YR_NUM,SHIFT_FSCL_PER_IN_YR_NUM,-DAY_PART2)
#make year and period variable for plotting
temp[, fyfp := paste0(SHIFT_FSCL_YR_NUM,".",str_pad(temp[,SHIFT_FSCL_PER_IN_YR_NUM],2,pad="0"))]

#set labels
llabels1 <- c("AM", "PM", "Rest of Day")
ylabel <- "Top 3 Box Score"
tlabel <- "Pulse (Q1) by Day Part"
sublabel <- "U.S. Company-Operated Stores"
caption <- "'Peak' defined as 7-11am; 'Non-Peak' all other time periods"
#values
pdata1a <- temp
px1a <- temp[,fyfp]
py1a <- temp[,TB3_SCORE]
groupvar1a <- temp[,DAY_PART2]
#plot itself
plot2 <- ggplot() +
  geom_line(data=pdata1a, aes(x=px1a, y=py1a, group=factor(groupvar1a), colour=factor(groupvar1a))) + 
  xlab("") + ylab(ylabel) + theme_bw() +
  scale_x_discrete(breaks = px[seq(1, length(px), by = 2)]) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_colour_discrete(name="", labels=llabels1, guide=guide_legend(order=1)) +
  #guides(colour = guide_legend(override.aes = list(size = 7))) + 
  #scale_y_continuous(limits=c(.35,.55)) +
  theme_economist_white(gray_bg = FALSE) +
  ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) 
print(plot2)







#edit partner data
temp <- pu[QUESTION_ID=='Q2_C']
temp[, start_hour := hour(SHIFT_START_DTM_LCL)]
#recode into peak and non-peak
#peak if shift time starts 6-10am (1 hour pre-7 to 11am windwo)
temp[, DAY_PART2 := 'REST-OF-DAY']
temp[start_hour>=6&start_hour<=10, DAY_PART2 := 'AM']
temp[start_hour>=13&start_hour<=16, DAY_PART2 := 'PM']
#recode to TB
temp[RESP_ID<7, TB_COUNT := 0];temp[RESP_ID==7, TB_COUNT := 1]
#recode to TB3
temp[RESP_ID<5, TB3_COUNT := 0];temp[RESP_ID>=5, TB3_COUNT := 1]
#aggregate by period
temp <- temp[, list(RSPNS_COUNT = .N,
                    AVG_RESP = mean(RESP_ID,na.rm=T),
                    TB_COUNT = sum(TB_COUNT,na.rm=T),
                    TB3_COUNT = sum(TB3_COUNT,na.rm=T)), 
             by=c("SHIFT_FSCL_YR_NUM","SHIFT_FSCL_PER_IN_YR_NUM","DAY_PART2")]
temp[, TB_SCORE := round(TB_COUNT/RSPNS_COUNT,3)]
temp[, TB3_SCORE := round(TB3_COUNT/RSPNS_COUNT,3)]
#set order
temp <- setorder(temp,SHIFT_FSCL_YR_NUM,SHIFT_FSCL_PER_IN_YR_NUM,-DAY_PART2)
#make year and period variable for plotting
temp[, fyfp := paste0(SHIFT_FSCL_YR_NUM,".",str_pad(temp[,SHIFT_FSCL_PER_IN_YR_NUM],2,pad="0"))]

#set labels
llabels1 <- c("AM", "PM", "Rest of Day")
ylabel <- "Top 3 Box Score"
tlabel <- "Pulse by Day Part"
sublabel <- "U.S. Company-Operated Stores"
caption <- "'Peak' defined as 7-11am; 'Non-Peak' all other time periods"
#values
pdata1a <- temp
px1a <- temp[,fyfp]
py1a <- temp[,TB3_SCORE]
groupvar1a <- temp[,DAY_PART2]
#plot itself
plot2 <- ggplot() +
  geom_line(data=pdata1a, aes(x=px1a, y=py1a, group=factor(groupvar1a), colour=factor(groupvar1a))) + 
  xlab("") + ylab(ylabel) + theme_bw() +
  scale_x_discrete(breaks = px[seq(1, length(px), by = 2)]) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_colour_discrete(name="", labels=llabels1, guide=guide_legend(order=1)) +
  #guides(colour = guide_legend(override.aes = list(size = 7))) + 
  #scale_y_continuous(limits=c(.35,.55)) +
  theme_economist_white(gray_bg = FALSE) +
  ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) 
print(plot2)