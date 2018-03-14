#makes line chart of CC by daypart for AM/PM

#load libraries
library(data.table)
library(ggplot2)
library(ggthemes)
library(lubridate)

#load data
data_dir <- "O:/CoOp/CoOp194_PROReportng&OM/Julie"
cedp <- fread(paste0(data_dir,"/CC_daypart_OCTFY17-FEBFY18.csv"))
#keep only CC
ccdp <- cedp[QSTN_ID=="Q2_2"]

#reduce to just AM and PM
ccdp <- ccdp[DAY_PART==2|DAY_PART==4]

#turn CC to percent
ccdp[, CC_SCORE := TB_SCORE*100]

#make a plotting height label
ccdp[FSCL_YR_NUM==2018&DAY_PART==2, value_y := CC_SCORE+1]
ccdp[FSCL_YR_NUM==2017&DAY_PART==2, value_y := CC_SCORE-1]
ccdp[FSCL_YR_NUM==2018&DAY_PART==4, value_y := CC_SCORE+1]
ccdp[FSCL_YR_NUM==2017&DAY_PART==4, value_y := CC_SCORE-1]

#set labels
xlabels <- c("Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","June","July","Aug","Sept")
ylabel <- "TB Score"
tlabel <- "Customer Connection by Day Part"
sublabel <- "US Company-Operated Stores, October FY17 - February FY18"
caption <- "AM = 7-11am\nPM = 2-5pm"
#manual legend labels
lname1 <- "Fiscal Year"
llabels1 <- c("2017","2018")
lname2 <- "Day Part"
llabels2 <- c("AM","PM")
#values
pdata <- ccdp
px <- ccdp[,FSCL_PER_IN_YR_NUM]
py <- ccdp[,CC_SCORE]
groupvar <- ccdp[,DAY_PART]
colourvar <- ccdp[,FSCL_YR_NUM]
value_y <- ccdp[,value_y]
#plot itself
plot2 <- ggplot(data=pdata, aes(x=as.factor(px), y=py, colour=factor(colourvar), group=interaction(groupvar, colourvar))) + 
  geom_line(size=1) +
  xlab("") + ylab(ylabel) + 
  scale_x_discrete(breaks=c(1:12), labels=factor(xlabels)) +
  scale_colour_discrete(name="", labels=llabels1, guide=guide_legend(order=1)) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  scale_y_continuous(limits=c(22,37)) + theme_economist_white(gray_bg = FALSE) +
  ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) +
  annotate(size=5, geom="text", x=1, y=36, label= "AM",hjust = 0) +
  annotate(size=5, geom="text", x=1, y=23, label= "PM",hjust = 0) +
  geom_text(size = 2.5, aes(label=py,y=value_y), stat="identity")
print(plot2)

#keep only Speed
spdp <- cedp[QSTN_ID=="Q2_1"]

#reduce to just AM and PM
spdp <- spdp[DAY_PART==2|DAY_PART==4]

#turn speed to percent
spdp[, TB_SCORE := TB_SCORE*100]

#make a plotting height label
spdp[FSCL_YR_NUM==2018&DAY_PART==2, value_y := TB_SCORE+1]
spdp[FSCL_YR_NUM==2017&DAY_PART==2, value_y := TB_SCORE-1]
spdp[FSCL_YR_NUM==2018&DAY_PART==4, value_y := TB_SCORE+1]
spdp[FSCL_YR_NUM==2017&DAY_PART==4, value_y := TB_SCORE-1]

#set labels
xlabels <- c("Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","June","July","Aug","Sept")
ylabel <- "TB Score"
tlabel <- "Speed by Day Part"
sublabel <- "US Company-Operated Stores, October FY17 - February FY18"
caption <- "AM = 7-11am\nPM = 2-5pm"
#manual legend labels
lname1 <- "Fiscal Year"
llabels1 <- c("2017","2018")
lname2 <- "Day Part"
llabels2 <- c("AM","PM")
#values
pdata <- spdp
px <- spdp[,FSCL_PER_IN_YR_NUM]
py <- spdp[,TB_SCORE]
groupvar <- spdp[,DAY_PART]
colourvar <- spdp[,FSCL_YR_NUM]
value_y <- spdp[,value_y]
#plot itself
plot2 <- ggplot(data=pdata, aes(x=as.factor(px), y=py, colour=factor(colourvar), group=interaction(groupvar, colourvar))) + 
  geom_line(size=1) +
  xlab("") + ylab(ylabel) + 
  scale_x_discrete(breaks=c(1:12), labels=factor(xlabels)) +
  scale_colour_discrete(name="", labels=llabels1, guide=guide_legend(order=1)) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  scale_y_continuous(limits=c(54,71)) + theme_economist_white(gray_bg = FALSE) +
  ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) +
  annotate(size=5, geom="text", x=1, y=69, label= "AM",hjust = 0) +
  annotate(size=5, geom="text", x=1, y=57, label= "PM",hjust = 0) +
  geom_text(size = 2.5, aes(label=py,y=value_y), stat="identity")
print(plot2)


#keep only Cleanliness
cldp <- cedp[QSTN_ID=="Q2_7"]

#reduce to just AM and PM
cldp <- cldp[DAY_PART==2|DAY_PART==4]

#turn speed to percent
cldp[, TB_SCORE := TB_SCORE*100]

#make a plotting height label
cldp[FSCL_YR_NUM==2018&DAY_PART==2, value_y := TB_SCORE+1]
cldp[FSCL_YR_NUM==2017&DAY_PART==2, value_y := TB_SCORE-1]
cldp[FSCL_YR_NUM==2018&DAY_PART==4, value_y := TB_SCORE+1]
cldp[FSCL_YR_NUM==2017&DAY_PART==4, value_y := TB_SCORE-1]

#set labels
xlabels <- c("Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","June","July","Aug","Sept")
ylabel <- "TB Score"
tlabel <- "Cleanliness by Day Part"
sublabel <- "US Company-Operated Stores, October FY17 - February FY18"
caption <- "AM = 7-11am\nPM = 2-5pm"
#manual legend labels
lname1 <- "Fiscal Year"
llabels1 <- c("2017","2018")
lname2 <- "Day Part"
llabels2 <- c("AM","PM")
#values
pdata <- cldp
px <- cldp[,FSCL_PER_IN_YR_NUM]
py <- cldp[,TB_SCORE]
groupvar <- cldp[,DAY_PART]
colourvar <- cldp[,FSCL_YR_NUM]
value_y <- cldp[,value_y]
#plot itself
plot2 <- ggplot(data=pdata, aes(x=as.factor(px), y=py, colour=factor(colourvar), group=interaction(groupvar, colourvar))) + 
  geom_line(size=1) +
  xlab("") + ylab(ylabel) + 
  scale_x_discrete(breaks=c(1:12), labels=factor(xlabels)) +
  scale_colour_discrete(name="", labels=llabels1, guide=guide_legend(order=1)) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  scale_y_continuous(limits=c(50,65)) + theme_economist_white(gray_bg = FALSE) +
  ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) +
  annotate(size=5, geom="text", x=1, y=64, label= "AM",hjust = 0) +
  annotate(size=5, geom="text", x=1, y=52, label= "PM",hjust = 0) +
  geom_text(size = 2.5, aes(label=py,y=value_y), stat="identity")
print(plot2)




#pulse by day part for q1 fy 18 - from partner-level data
data_dir <- "O:/CoOp/CoOp194_PROReportng&OM/Julie"
pulse <- fread(paste0(data_dir,"/pulse_q2d_byhour_bypartner_q1fy18.csv"))
pulse[shift_start_hour>=7&shift_start_hour<=11, dp := 2]
pulse[shift_start_hour>=14&shift_start_hour<=17, dp := 4]
pulse <- pulse[, list(q2dscore = sum(Q2D_TB,na.rm=T)/sum(Q2D_RESP,na.rm=T)), by="dp"]

#pulse by day part for q1 fy 18
pulse <- fread(paste0(data_dir,"/pulse_q2d_byhour_q1fy18.csv"))
pulse[shift_start_hour>=7&shift_start_hour<=11, dp := 2]
pulse[shift_start_hour>=14&shift_start_hour<=17, dp := 4]
pulse <- pulse[, list(q2dscore = sum(Q2D_TB3,na.rm=T)/sum(Q2D_RESP,na.rm=T)), by="dp"]

#pulse by day part by month for 6 months (q4 fy 17 and q1 fy 18)
pulse <- fread(paste0(data_dir,"/pulse_q2d_byhour_OCTFY17-FEBFY18.csv"))
setnames(pulse,c("SHIFT_FSCL_YR_NUM","SHIFT_FSCL_PER_IN_YR_NUM"),c("FY","FP"))
pulse[, shift_start_hour := mdy_hm(SHIFT_START_DTM_LCL)]
pulse[, shift_start_hour := hour(shift_start_hour)]
pulse[shift_start_hour>=7&shift_start_hour<=11, dp := 2]
pulse[shift_start_hour>=14&shift_start_hour<=17, dp := 4]
pulse <- na.omit(pulse,cols="dp")

#drop march
pulse <- pulse[FY==2017|(FY==2018&FP<=5)]

#aggregate
pulse[QUESTION_ID=='Q2_D', Q2D_RESP := 1]
pulse[RESP_ID>=5, Q2D_TB3 := 1]; pulse[RESP_ID<5, Q2D_TB3 := 0]
pulse <- pulse[, list(q2dtb3 = round(sum(Q2D_TB3,na.rm=T)/sum(Q2D_RESP,na.rm=T)*100,1)), by=c("dp","FP","FY")]
pulse <- setorder(pulse,FY,FP)

#map fiscal periods to order for plot
pulse[FY==2017, FPplot := FP-3]
pulse[FY==2018, FPplot := FP+9]

#make a plotting height label
pulse[dp==2, value_y := q2dtb3-1.5]
pulse[dp==4, value_y := q2dtb3+1.5]
pulse[FPplot==4&dp==2, value_y := q2dtb3+1.5]
pulse[FPplot==4&dp==4, value_y := q2dtb3-1.5]

#set labels
xlabels <- c("Apr","May","June","July","Aug","Sept","Oct","Nov","Dec","Jan","Feb")
ylabel <- "Top 3 Box Score"
tlabel <- "Pulse by Day Part"
sublabel <- "Q2_D: 'Connect with Customers'"
caption <- "AM = 7-11am\nPM = 2-5pm"
#manual legend labels
lname1 <- "Day Part"
llabels1 <- c("AM","PM")
#values
pdata <- pulse
px <- pulse[,FPplot]
py <- pulse[,q2dtb3]
colourvar <- pulse[,dp]
value_y <- pulse[,value_y]
#plot itself
plot2 <- ggplot(data=pdata, aes(x=as.factor(px), y=py, group=factor(colourvar), colour=factor(colourvar))) + 
  geom_line(size=1) +
  xlab("") + ylab(ylabel) + 
  scale_x_discrete(labels=factor(xlabels)) +
  scale_colour_discrete(name=lname1, labels=llabels1, guide=guide_legend(order=1)) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  scale_y_continuous(brea=c(70,80,90), limits=c(70,90)) + theme_economist_white(gray_bg = FALSE) +
  ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) +
  #annotate(size=5, geom="text", x=1, y=75, label= "AM",hjust = 0) +
  #annotate(size=5, geom="text", x=1, y=85, label= "PM",hjust = 0) +
  geom_text(size = 4, aes(label=py,y=value_y), stat="identity")
print(plot2)


#pulse by day part by month for 6 months (q4 fy 17 and q1 fy 18)
pulse <- fread(paste0(data_dir,"/pulse_q2d_byhour_q4fy17-q1fy18.csv"))
pulse[shift_start_hour>=7&shift_start_hour<=11, dp := 2]
pulse[shift_start_hour>=14&shift_start_hour<=17, dp := 4]
pulse <- na.omit(pulse,cols="dp")
#create FPs
  pulse[, bdate := mdy(BUS_DT)]
pulse[bdate>='2017-07-03'&bdate<='2017-07-30', mnth := 1] #july
pulse[bdate>='2017-07-31'&bdate<='2017-08-27', mnth := 2] #aug
pulse[bdate>='2017-08-28'&bdate<='2017-10-01', mnth := 3] #sept
pulse[bdate>='2017-10-02'&bdate<='2017-10-29', mnth := 4] #oct
pulse[bdate>='2017-10-30'&bdate<='2017-11-26', mnth := 5] #nov
pulse[bdate>='2017-11-27'&bdate<='2017-12-31', mnth := 6] #dec
#aggregate
  pulse <- pulse[, list(q2dtb1 = sum(Q2D_TB1,na.rm=T)/sum(Q2D_RESP,na.rm=T)*100,
                        q2dtb2 = sum(Q2D_TB2,na.rm=T)/sum(Q2D_RESP,na.rm=T)*100,
                        q2dtb3 = round(sum(Q2D_TB3,na.rm=T)/sum(Q2D_RESP,na.rm=T)*100,1)), by=c("dp","mnth")]

#make a plotting height label
pulse[dp==2, value_y := q2dtb3-1]
pulse[dp==4, value_y := q2dtb3+1]
  
#set labels
xlabels <- c("July","Aug","Sept","Oct","Nov","Dec")
ylabel <- "Top 3 Box Score"
tlabel <- "Pulse by Day Part"
sublabel <- "Q2_D: 'Connect with Customers'"
caption <- "Q4 FY17 & Q1 FY18\nAM = 7-11am\nPM = 2-5pm"
#manual legend labels
lname1 <- "Day Part"
llabels1 <- c("AM","PM")
#values
pdata <- pulse
px <- pulse[,mnth]
py <- pulse[,q2dtb3]
colourvar <- pulse[,dp]
value_y <- pulse[,value_y]
#plot itself
plot2 <- ggplot(data=pdata, aes(x=as.factor(px), y=py, group=factor(colourvar), colour=factor(colourvar))) + 
 geom_line(size=1) +
 xlab("") + ylab(ylabel) + 
 scale_x_discrete(labels=factor(xlabels)) +
 scale_colour_discrete(name="", labels="", guide=FALSE) +
 #guides(colour = guide_legend(override.aes = list(size = 7))) + 
 scale_y_continuous(breaks=c(40,50,60), limits=c(40,60)) + theme_economist_white(gray_bg = FALSE) +
 ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) +
 annotate(size=5, geom="text", x=1, y=55, label= "PM",hjust = 0) +
 annotate(size=5, geom="text", x=1, y=43, label= "AM",hjust = 0) +
 geom_text(size = 3.5, aes(label=py,y=value_y), stat="identity")
print(plot2)