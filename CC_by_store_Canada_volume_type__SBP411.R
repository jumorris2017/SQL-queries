##R code for SQL query 
##CC by store volume and cafe/drive-thru type for Canada stores
##Request from Lisa 11/16/17

##load libraries
library(data.table)
library(xlsx)
library(dplyr)
library(ggplot2)
#library(RColorBrewer)

#load data
#part 1
p1 <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC_by_store_Canada_volume_type_pt1.csv")
#part 2
p2 <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC_by_store_Canada_volume_type_pt2.csv")

#rename merge id columns to match
setnames(p1,c("STORE_NUM","CAL_MNTH_IN_YR_NUM","CAL_YR_NUM"),
         c("STORE_NUMBER","DATE_MONTH","DATE_YEAR"))
#change store number and date values to numeric from characters
p1[, STORE_NUMBER := lapply(.SD, as.numeric), .SDcols = "STORE_NUMBER"]
p2[, (grep("DATE",names(p2),value=T)) := lapply(.SD, as.numeric), .SDcols=grep("DATE",names(p2),value=T)]
#merge by store number, month, and year
pfull <- Reduce(function(x,y) {merge(x,y,by=c("STORE_NUMBER","DATE_MONTH","DATE_YEAR"),all.x=TRUE)}, list(p1,p2))
#drop stores with no active days
pfull <- pfull[ACTIVE_STORE_DAY_CNT>0]

##aggregate for two year rolling-averages
#create new variable for start month to agg over two month-blocks
pfull[DATE_MONTH%%2!=0, month_start := DATE_MONTH]
pfull[DATE_MONTH%%2==0, month_start := DATE_MONTH-1]
#agg over
pagg1 <- pfull[, list(Q2_2_RESPONSE_TOTAL=sum(Q2_2_RESPONSE_TOTAL,na.rm=T),
             Q2_2_TB_CNT=sum(Q2_2_TB_CNT,na.rm=TRUE),
             Q2_2_TB_SCORE=round(sum(Q2_2_TB_CNT,na.rm=TRUE)/sum(Q2_2_RESPONSE_TOTAL,na.rm=T),4),
             CUST_TRANS_CNT=sum(CUST_TRANS_CNT,na.rm=T),
             ACTIVE_STORE_DAY_CNT=sum(ACTIVE_STORE_DAY_CNT,na.rm=T),
             COSD=round(sum(CUST_TRANS_CNT,na.rm=T)/sum(ACTIVE_STORE_DAY_CNT,na.rm=T),2)),
      by=c("STORE_NUMBER","month_start","DATE_YEAR","DRIVE_THRU_IND")]

###NEW! - START! ###

##year-over-year change
pagg1_yoy <- dcast.data.table(pagg1, STORE_NUMBER + month_start + DRIVE_THRU_IND ~ DATE_YEAR, value.var="Q2_2_TB_SCORE")
setnames(pagg1_yoy,c("2016","2017"),c("yr2016","yr2017"))
pagg1_yoy[, yoy := yr2017/yr2016]
pagg1_yoy[mapply(is.infinite, pagg1_yoy)] <- NA

#create indicator for "is positive"
pagg1_yoy <- na.omit(pagg1_yoy, cols="yoy")
pagg1_yoy[yoy==1, yoy_posneg := 0]
pagg1_yoy[yoy<1, yoy_posneg := 1]
pagg1_yoy[yoy>1, yoy_posneg := 2]

#split by yoy
prob = c(1/4, 2/4, 3/4, 1)
temp <- pagg1_yoy %>% group_by(DRIVE_THRU_IND) %>% summarise( 
  yoy25 = quantile(yoy, probs = prob[1], na.rm = T), 
  yoy50 = quantile(yoy, probs = prob[2], na.rm = T),
  yoy75 = quantile(yoy, probs = prob[3], na.rm = T),
  yoy100 = quantile(yoy, probs = prob[4], na.rm = T)
)
pagg1_yoy <- left_join(pagg1_yoy, temp,by="DRIVE_THRU_IND")
setDT(pagg1_yoy)

##to view quartile cut-offs
temp2 <- as.data.table(temp)
temp2[, (names(temp2[,c(2:5)])) := lapply(.SD, function(x) round(x,3)), .SDcols=names(temp2[,c(2:5)])]

#recode yoy based on quartiles
pagg1_yoy[yoy <= yoy25, yoyquartile := 1]
pagg1_yoy[yoy > yoy25 & yoy <= yoy50, yoyquartile := 2]
pagg1_yoy[yoy > yoy50 & yoy <= yoy75, yoyquartile := 3]
pagg1_yoy[yoy > yoy75, yoyquartile := 4]
#drop quartile columns
pagg1_yoy <- pagg1_yoy[, !(names(pagg1_yoy[,c(7:10)])), with=FALSE]
#drop november-december (don't have much data for 2017)
pagg1_yoy <- pagg1_yoy[month_start<11]

##box plots
##make month_start factor for grouping
pagg1_yoy[, month_start := as.factor(month_start)]
#set labels
lname <- "Store Type"
llabels <- c("Cafe", "Drive-Thru") 
#plot
ggplot(pagg1_yoy, aes(x = month_start, y = yoy, fill = DRIVE_THRU_IND)) +
  geom_boxplot(alpha=0.7) +
  scale_y_continuous(name = "YoY Change in CC Top Box Score") +
  scale_x_discrete(name = "Two-Month Period",
                   labels=c("Jan-Feb","Mar-Apr","May-June","July-Aug","Sept-Oct")) +
  ggtitle("YoY Change in CC Top Box Scores by Store Type (Canada)") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11),
        legend.position = "bottom") +
  scale_fill_manual(values=c("#af3596","#00b1ff"), name=lname,labels=llabels)

#histogram
hist(pagg1_yoy[month_start==9,yoy])


### NEW! - END! ##

#split by COSD
prob = c(1/3, 2/3, 1)
temp <- pagg1 %>% group_by(DRIVE_THRU_IND) %>% summarise( 
  cosd33 = quantile(COSD, probs = prob[1], na.rm = T), 
  cosd67 = quantile(COSD, probs = prob[2], na.rm = T),
  cosd100 = quantile(COSD, probs = prob[3], na.rm = T)
)
pagg1 <- left_join(pagg1, temp,by="DRIVE_THRU_IND")
setDT(pagg1)
#recode COSD based on quartiles
pagg1[COSD <= cosd33, vol := 1]
pagg1[COSD > cosd33 & COSD <= cosd67, vol := 2]
pagg1[COSD > cosd67, vol := 3]

#create single indicator for volume and store type
pagg1[DRIVE_THRU_IND=="N"&vol==1, storetype_vol := 1]
pagg1[DRIVE_THRU_IND=="N"&vol==2, storetype_vol := 2]
pagg1[DRIVE_THRU_IND=="N"&vol==3, storetype_vol := 3]
pagg1[DRIVE_THRU_IND=="Y"&vol==1, storetype_vol := 4]
pagg1[DRIVE_THRU_IND=="Y"&vol==2, storetype_vol := 5]
pagg1[DRIVE_THRU_IND=="Y"&vol==3, storetype_vol := 6]

#create single indicator for date & year
pagg1[month_start<10, mmyr := paste0(DATE_YEAR,"-0",month_start)]
pagg1[month_start>=10, mmyr := paste0(DATE_YEAR,"-",month_start)]
#drop november 2017
pagg1 <- pagg1[mmyr!="2017-11"]

##box plots
##make store type factor for grouping
pagg1[, storetype_vol := as.factor(storetype_vol)]
#set labels
lname <- "Store Type & Volume"
llabels <- c("Cafe, Low Volume", "Cafe, Med Volume", "Cafe, High Volume",
             "Drive-Thru, Low Volume", "Drive-Thru, Med Volume", "Drive-Thru, High Volume") 
#plot
ggplot(pagg1, aes(x = mmyr, y = Q2_2_TB_SCORE, fill = storetype_vol)) +
  geom_boxplot(alpha=0.7) +
  scale_y_continuous(name = "CC Top Box Score") +
  scale_x_discrete(name = "Two-Month Period") +
  ggtitle("CC Top Box Scores by Store Type & Volume (Canada)") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11, angle = 45, hjust = 1),
        legend.position = "bottom") +
  scale_fill_manual(values=c("#ffc8d2", "#af3596", "#731982","#00e4ff","#00b1ff","#0070ff"), 
                    name=lname,labels=llabels)


#aggregate for plot
pagg2 <- pagg1[, list(Q2_2_RESPONSE_TOTAL=sum(Q2_2_RESPONSE_TOTAL,na.rm=T),
                     Q2_2_TB_CNT=sum(Q2_2_TB_CNT,na.rm=TRUE),
                     Q2_2_TB_SCORE=round(sum(Q2_2_TB_CNT,na.rm=TRUE)/sum(Q2_2_RESPONSE_TOTAL,na.rm=T),4),
                     CUST_TRANS_CNT=sum(CUST_TRANS_CNT,na.rm=T),
                     ACTIVE_STORE_DAY_CNT=sum(ACTIVE_STORE_DAY_CNT,na.rm=T),
                     COSD=round(sum(CUST_TRANS_CNT,na.rm=T)/sum(ACTIVE_STORE_DAY_CNT,na.rm=T),2)),
              by=c("mmyr","storetype_vol")]

#set labels
xlabel <- "Time Period"
ylabel <- "CC Top Box Score"
tlabel <- "CC Top Box Score by Store Type & Volume"
#set data and variables
pdata <- pagg2
px <- pagg2[, mmyr]
py <- pagg2[, Q2_2_TB_SCORE]
groupvar <- pagg2[, storetype_vol]
#manual legend labels
lname <- "Store Type & Volume"
llabels <- c("Cafe, Low Volume", "Cafe, Med Volume", "Cafe, High Volume",
             "Drive-Thru, Low Volume", "Drive-Thru, Med Volume", "Drive-Thru, High Volume") 
#line chart, factored by one variable
plot2 <- ggplot() +
  geom_line(data=pdata, aes(x=px, y=py, group=factor(groupvar), colour=factor(groupvar))) + 
  xlab(xlabel) + ylab(ylabel) + theme_bw() +
  scale_colour_discrete(name=lname, labels=llabels, guide=guide_legend(order=1)) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  scale_y_continuous(limits=c(pdata[,min(py)]*.85,pdata[,max(py)]*1.15)) + 
  ggtitle(tlabel)
print(plot2)

#subset variables for .xslx
#store level
pagg1_print <- pagg1[, c("STORE_NUMBER","month_start","DATE_YEAR",
                   "DRIVE_THRU_IND","Q2_2_TB_SCORE",
                   "COSD"), with=FALSE]
pagg1_print[, months_inc := paste0(month_start,"-",month_start+1)]
#store type and volume level
#store level
pagg2_print <- pagg2[, c("mmyr","storetype_vol","Q2_2_TB_SCORE",
                         "COSD"), with=FALSE]

#write to .xslx
write.xlsx(pagg1_print,file="O:/CoOp/CoOp194_PROReportng&OM/Julie/CC_COSD_Canada_store_level.xlsx")
write.xlsx(pagg2_print,file="O:/CoOp/CoOp194_PROReportng&OM/Julie/CC_COSD_Canada_storetypevol_level.xlsx")



##re-do at single-month level for plot
##aggregate for two year rolling-averages
#create new variable for start month to agg over two month-blocks
#agg over
pmon1 <- pfull[, list(Q2_2_RESPONSE_TOTAL=sum(Q2_2_RESPONSE_TOTAL,na.rm=T),
                      Q2_2_TB_CNT=sum(Q2_2_TB_CNT,na.rm=TRUE),
                      Q2_2_TB_SCORE=round(sum(Q2_2_TB_CNT,na.rm=TRUE)/sum(Q2_2_RESPONSE_TOTAL,na.rm=T),4),
                      CUST_TRANS_CNT=sum(CUST_TRANS_CNT,na.rm=T),
                      ACTIVE_STORE_DAY_CNT=sum(ACTIVE_STORE_DAY_CNT,na.rm=T),
                      COSD=round(sum(CUST_TRANS_CNT,na.rm=T)/sum(ACTIVE_STORE_DAY_CNT,na.rm=T),2)),
               by=c("STORE_NUMBER","DATE_MONTH","DATE_YEAR","DRIVE_THRU_IND")]

#split by COSD
prob = c(1/3, 2/3, 1)
temp <- pmon1 %>% group_by(DRIVE_THRU_IND) %>% summarise( 
  cosd33 = quantile(COSD, probs = prob[1], na.rm = T), 
  cosd67 = quantile(COSD, probs = prob[2], na.rm = T),
  cosd100 = quantile(COSD, probs = prob[3], na.rm = T)
)
pmon1 <- left_join(pmon1, temp,by="DRIVE_THRU_IND")
setDT(pmon1)
#recode COSD based on quartiles
pmon1[COSD <= cosd33, vol := 1]
pmon1[COSD > cosd33 & COSD <= cosd67, vol := 2]
pmon1[COSD > cosd67, vol := 3]

#create single indicator for volume and store type
pmon1[DRIVE_THRU_IND=="N"&vol==1, storetype_vol := 1]
pmon1[DRIVE_THRU_IND=="N"&vol==2, storetype_vol := 2]
pmon1[DRIVE_THRU_IND=="N"&vol==3, storetype_vol := 3]
pmon1[DRIVE_THRU_IND=="Y"&vol==1, storetype_vol := 4]
pmon1[DRIVE_THRU_IND=="Y"&vol==2, storetype_vol := 5]
pmon1[DRIVE_THRU_IND=="Y"&vol==3, storetype_vol := 6]

#create single indicator for date & year
pmon1[DATE_MONTH<10, mmyr := paste0(DATE_YEAR,"-0",DATE_MONTH)]
pmon1[DATE_MONTH>=10, mmyr := paste0(DATE_YEAR,"-",DATE_MONTH)]

#create single indicator for date & year
#drop november 2017
pmon1 <- pmon1[!which(DATE_MONTH==11&DATE_YEAR==2017)]

#aggregate for plot
pmon2 <- pmon1[, list(Q2_2_RESPONSE_TOTAL=sum(Q2_2_RESPONSE_TOTAL,na.rm=T),
                      Q2_2_TB_CNT=sum(Q2_2_TB_CNT,na.rm=TRUE),
                      Q2_2_TB_SCORE=round(sum(Q2_2_TB_CNT,na.rm=TRUE)/sum(Q2_2_RESPONSE_TOTAL,na.rm=T),4),
                      CUST_TRANS_CNT=sum(CUST_TRANS_CNT,na.rm=T),
                      ACTIVE_STORE_DAY_CNT=sum(ACTIVE_STORE_DAY_CNT,na.rm=T),
                      COSD=round(sum(CUST_TRANS_CNT,na.rm=T)/sum(ACTIVE_STORE_DAY_CNT,na.rm=T),2)),
               by=c("mmyr","storetype_vol")]

#set labels
xlabel <- "Time Period"
ylabel <- "CC Top Box Score"
tlabel <- "CC Top Box Score by Store Type & Volume"
#set data and variables
pdata <- pmon2
px <- pmon2[, mmyr]
py <- pmon2[, Q2_2_TB_SCORE]
groupvar <- pmon2[, storetype_vol]
#manual legend labels
lname <- "Store Type & Volume"
llabels <- c("Cafe, Low Volume", "Cafe, Med Volume", "Cafe, High Volume",
             "Drive-Thru, Low Volume", "Drive-Thru, Med Volume", "Drive-Thru, High Volume") 
#line chart, factored by one variable
plot2 <- ggplot() +
  geom_line(data=pdata, aes(x=px, y=py, group=factor(groupvar), colour=factor(groupvar))) + 
  xlab(xlabel) + ylab(ylabel) + theme_bw() +
  scale_colour_discrete(name=lname, labels=llabels, guide=guide_legend(order=1)) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(limits=c(pdata[,min(py)]*.85,pdata[,max(py)]*1.15)) + 
  ggtitle(tlabel)
print(plot2)



#part 3
p3 <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC_by_store_Canada_volume_type_pt3.csv")

#agg over
pmon3 <- p3[, list(Q2_2_RESPONSE_TOTAL=sum(Q2_2_RESPONSE_TOTAL,na.rm=T),
                      Q2_2_TB_CNT=sum(Q2_2_TB_CNT,na.rm=TRUE),
                      Q2_2_TB_SCORE=round(sum(Q2_2_TB_CNT,na.rm=TRUE)/sum(Q2_2_RESPONSE_TOTAL,na.rm=T),4)),
               by=c("STORE_NUM","FSCL_PER_IN_YR_NUM","FSCL_YR_NUM","DRIVE_THRU_IND")]

##year-over-year change
pagg3_yoy <- dcast.data.table(pmon3, STORE_NUM + DRIVE_THRU_IND ~ FSCL_YR_NUM, value.var="Q2_2_TB_SCORE")
setnames(pagg3_yoy,c("2016","2017"),c("yr2016","yr2017"))
pagg3_yoy[, yoy := yr2017/yr2016]
pagg3_yoy[mapply(is.infinite, pagg3_yoy)] <- NA
pagg3_yoy[, val := yr2017-yr2016]
pagg3_yoy[mapply(is.infinite, pagg3_yoy)] <- NA

#create indicator for "is positive"
pagg3_yoy <- na.omit(pagg3_yoy, cols="yoy")
pagg3_yoy[yoy==1, yoy_posneg := 0]
pagg3_yoy[yoy<1, yoy_posneg := 1]
pagg3_yoy[yoy>1, yoy_posneg := 2]

#create indicator for 10-point change
pagg3_yoy <- na.omit(pagg3_yoy, cols="val")
pagg3_yoy[val<.1, val10 := 0]
pagg3_yoy[val>=.1, val10 := 1]


#split by yoy
prob = c(1/4, 2/4, 3/4, 1)
temp1 <- pagg3_yoy %>% group_by(DRIVE_THRU_IND) %>% summarise( 
  yoy25 = quantile(yoy, probs = prob[1], na.rm = T), 
  yoy50 = quantile(yoy, probs = prob[2], na.rm = T),
  yoy75 = quantile(yoy, probs = prob[3], na.rm = T),
  yoy100 = quantile(yoy, probs = prob[4], na.rm = T)
  )
pagg1_yoy <- left_join(pagg3_yoy, temp,by="DRIVE_THRU_IND")
setDT(pagg3_yoy)

prob = c(.6,.7,.8,.9)
temp2 <- pagg3_yoy %>% group_by(DRIVE_THRU_IND) %>% summarise( 
  yoy60 = quantile(yoy, probs = prob[1], na.rm = T), 
  yoy70 = quantile(yoy, probs = prob[2], na.rm = T),
  yoy80 = quantile(yoy, probs = prob[3], na.rm = T),
  yoy90 = quantile(yoy, probs = prob[4], na.rm = T)
)
pagg1_yoy <- left_join(pagg3_yoy, temp,by="DRIVE_THRU_IND")
setDT(pagg3_yoy)


##box plots
##make store type factor for grouping
pagg3_yoy[, DRIVE_THRU_IND := as.factor(DRIVE_THRU_IND)]
#set labels
lname <- "Store Type"
llabels <- c("Cafe", "Drive-Thru") 
#plot
ggplot(pagg3_yoy, aes(x = DRIVE_THRU_IND, y = yoy, fill = DRIVE_THRU_IND)) +
  geom_boxplot(alpha=0.7) +
  scale_y_continuous(name = "YoY Change in CC Box Score",breaks=c(0:6.5)) +
  scale_x_discrete(name = "September",labels=llabels) +
  ggtitle("YoY Change in CC Top Box Scores by Store Type (Canada)") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11)) + guides(fill=FALSE)
#plot
ggplot(pagg3_yoy[yoy<4], aes(x = DRIVE_THRU_IND, y = yoy, fill = DRIVE_THRU_IND)) +
  geom_boxplot(alpha=0.7) +
  scale_y_continuous(name = "YoY Change in CC Top Box Score",breaks=c(0:6.5)) +
  scale_x_discrete(name = "September",labels=llabels) +
  ggtitle("YoY Change in CC Top Box Scores by Store Type (Canada)") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11)) + guides(fill=FALSE)
