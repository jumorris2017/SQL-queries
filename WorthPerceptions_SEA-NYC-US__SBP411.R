##request 1: worth perceptions for Lisa request 11/15/17
##request 2: CC and SO correlations for Lisa request 11/15/17

#load libraries
library(data.table)
library(xlsx)
library(ggplot2)

#Request #1: worth perceptions pre- and post- price change by store

#load data
wp <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/worth_perception_for_R_111517.csv")
#load NYC store data
nypz <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/nycpricezonestore.csv")
nyvec <- unique(nypz[,store_num])

##analysis 1

#convert date to R date type
wp[, TRANS_DATE := as.Date(TRANS_DATE, "%d-%b-%y")]
#create pre- and post-weeks
wp[TRANS_DATE>='2016-11-05'&TRANS_DATE<='2016-11-08', post_week := 0]
wp[TRANS_DATE>='2016-11-12'&TRANS_DATE<='2016-11-15', post_week := 1]
wp[TRANS_DATE>='2017-11-04'&TRANS_DATE<='2017-11-07', post_week := 0]
wp[TRANS_DATE>='2017-11-11'&TRANS_DATE<='2017-11-14', post_week := 1]
wp <- na.omit(wp, cols="post_week")

#subset to Seattle stores
sea <- wp[AREA_ORG_LVL_VERS_SID==10]
#subset to NYC stores
ny <- wp[STORE_NUM %in% nyvec]
#wp is all of US

#calculate week average
seaag <- sea[, list(Q2_8_RESPONSE_TOTAL=sum(Q2_8_RESPONSE_TOTAL),
                   Q2_8_TB_CNT=sum(Q2_8_TB_CNT),
                   Q2_8_TB_SCORE=sum(Q2_8_TB_CNT)/sum(Q2_8_RESPONSE_TOTAL),
                   num_stores=length(unique(STORE_NUM))),
            by=c("FSCL_YR_NUM","post_week")]
seaag[, loc := "SEA"]

#calculate week average
nyag <- ny[, list(Q2_8_RESPONSE_TOTAL=sum(Q2_8_RESPONSE_TOTAL),
                  Q2_8_TB_CNT=sum(Q2_8_TB_CNT),
                  Q2_8_TB_SCORE=sum(Q2_8_TB_CNT)/sum(Q2_8_RESPONSE_TOTAL),
                  num_stores=length(unique(STORE_NUM))),
             by=c("FSCL_YR_NUM","post_week")]
nyag[, loc := "NYC"]

#calculate week average
wpag <- wp[, list(Q2_8_RESPONSE_TOTAL=sum(Q2_8_RESPONSE_TOTAL),
                  Q2_8_TB_CNT=sum(Q2_8_TB_CNT),
                  Q2_8_TB_SCORE=sum(Q2_8_TB_CNT)/sum(Q2_8_RESPONSE_TOTAL),
                  num_stores=length(unique(STORE_NUM))),
           by=c("FSCL_YR_NUM","post_week")]
wpag[, loc := "US"]

#rbind together
l = list(seaag,nyag,wpag)
totalag <- rbindlist(l,use.names=T,fill=T)

#write file
write.xlsx(totalag,file="O:/CoOp/CoOp194_PROReportng&OM/Julie/worth_perception_pricezones.xlsx")


##analysis 2

#SEATTLE

#load data
sea <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/worth_perception_for_R_v2_111617.csv")

#convert date to R date type
sea[, TRANS_DATE := as.Date(TRANS_DATE, "%d-%b-%y")]
sea[, CAL_WK_IN_YR_NUM := as.numeric(CAL_WK_IN_YR_NUM)]

#subset to Seattle stores
sea <- sea[AREA_ORG_LVL_VERS_SID==10]

#create weeks that start on Saturdays
#in 2016, weeks start on Fridays
sea[CAL_YR_NUM==2016&DAY_ABBR_NM=="FR", new_week := CAL_WK_IN_YR_NUM-1]
sea[CAL_YR_NUM==2016&DAY_ABBR_NM!="FR", new_week := CAL_WK_IN_YR_NUM]
#in 2017, weeks start on Sundays
sea[CAL_YR_NUM==2017&DAY_ABBR_NM=="SA", new_week := CAL_WK_IN_YR_NUM+1]
sea[CAL_YR_NUM==2017&DAY_ABBR_NM!="SA", new_week := CAL_WK_IN_YR_NUM]

#calculate week average
seaag <- sea[, list(Q2_8_RESPONSE_TOTAL=sum(Q2_8_RESPONSE_TOTAL),
                    Q2_8_TB_CNT=sum(Q2_8_TB_CNT),
                    Q2_8_TB_SCORE=sum(Q2_8_TB_CNT)/sum(Q2_8_RESPONSE_TOTAL),
                    num_stores=length(unique(STORE_NUM)),
                    start_date=min(TRANS_DATE)),
             by=c("CAL_YR_NUM","new_week")]
seaag[, loc := "SEA"]
setorder(seaag,CAL_YR_NUM,start_date)

# #create single indicator for date & year
# seaag[new_week<10, mmyr := paste0(CAL_YR_NUM,"-0",new_week)]
# seaag[new_week>=10, mmyr := paste0(CAL_YR_NUM,"-",new_week)]

#capture only the past 6 months
seaag <- seaag[new_week>=19]

#set labels
xlabel <- "Week in Calendar Year"
ylabel <- "WP Top Box Score"
tlabel <- "Seattle: WP Top Box Score \nNote: weeks start on price-increase days"
#set data and variables
pdata <- seaag
px <- seaag[, new_week]
py <- seaag[, Q2_8_TB_SCORE]
groupvar <- seaag[, CAL_YR_NUM]
#manual legend labels
lname <- "Calendar Year"
llabels <- c("2016","2017") 
#line chart, factored by one variable
plot2 <- ggplot() +
  geom_line(data=pdata, aes(x=px, y=py, group=factor(groupvar), colour=factor(groupvar))) + 
  xlab(xlabel) + ylab(ylabel) + theme_bw() +
  scale_colour_discrete(name=lname, labels=llabels, guide=guide_legend(order=1)) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  scale_y_continuous(limits=c(pdata[,min(py)]*.85,pdata[,max(py)]*1.15)) + 
  geom_vline(data=pdata, aes(xintercept=46), size=.5, colour="black") +
  geom_vline(data=pdata, aes(xintercept=45), linetype="dashed", size=.5, colour="black") +
  ggtitle(tlabel)
print(plot2)


#ALL DATA - FROM SUBQUERIED SQL

#load data
wp4 <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/worth_perception_for_R_v4_112017.csv")
#load NYC store data
nypz <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/nycpricezonestore.csv")
nyvec <- unique(nypz[,store_num])

#convert store number to numeric
wp4[, STORE_NUM := as.numeric(STORE_NUM)]

#subset to Seattle stores
sea <- wp4[AREA_ORG_LVL_VERS_SID==10]
#subset to NYC stores
ny <- wp4[STORE_NUM %in% nyvec]
#wp4 is all of US

#calculate week average
seaag <- sea[, list(Q2_8_RESPONSE_TOTAL=sum(Q2_8_RESPONSE_TOTAL),
                    Q2_8_TB_CNT=sum(Q2_8_TB_CNT),
                    Q2_8_TB_SCORE=sum(Q2_8_TB_CNT)/sum(Q2_8_RESPONSE_TOTAL),
                    num_stores=length(unique(STORE_NUM))),
             by=c("CAL_YR_NUM","NEW_WEEK")]
seaag[, loc := "SEA"]
seaag[mapply(is.infinite, seaag)] <- NA
seaag <- seaag[NEW_WEEK>19]

#calculate week average
nyag <- ny[, list(Q2_8_RESPONSE_TOTAL=sum(Q2_8_RESPONSE_TOTAL),
                  Q2_8_TB_CNT=sum(Q2_8_TB_CNT),
                  Q2_8_TB_SCORE=sum(Q2_8_TB_CNT)/sum(Q2_8_RESPONSE_TOTAL),
                  num_stores=length(unique(STORE_NUM))),
           by=c("CAL_YR_NUM","NEW_WEEK")]
nyag[, loc := "NYC"]
nyag[mapply(is.infinite, nyag)] <- NA
nyag <- nyag[NEW_WEEK>19]

#calculate week average
wpag <- wp4[, list(Q2_8_RESPONSE_TOTAL=sum(Q2_8_RESPONSE_TOTAL),
                  Q2_8_TB_CNT=sum(Q2_8_TB_CNT),
                  Q2_8_TB_SCORE=sum(Q2_8_TB_CNT)/sum(Q2_8_RESPONSE_TOTAL),
                  num_stores=length(unique(STORE_NUM))),
           by=c("CAL_YR_NUM","NEW_WEEK")]
wpag[, loc := "US"]
wpag[mapply(is.infinite, wpag)] <- NA
wpag <- wpag[NEW_WEEK>19]

#rbind together
l = list(seaag,nyag,wpag)
totalag <- rbindlist(l,use.names=T,fill=T)

#write file
#write.xlsx(totalag,file="O:/CoOp/CoOp194_PROReportng&OM/Julie/worth_perception_pricezones_112017.xlsx")

#SEATTLE
#set labels
xlabel <- "Week in Calendar Year"
ylabel <- "WP Top Box Score"
tlabel <- "Seattle: WP Top Box Score"
slabel <- "*Note: weeks start on price-increase days"
#set data and variables
pdata <- seaag
px <- seaag[, NEW_WEEK]
py <- seaag[, Q2_8_TB_SCORE]
groupvar <- seaag[, CAL_YR_NUM]
#manual legend labels
lname <- "Calendar Year"
llabels <- c("2016","2017") 
#line chart, factored by one variable
plot1 <- ggplot() +
  geom_line(data=pdata, aes(x=px, y=py, group=factor(groupvar), colour=factor(groupvar))) + 
  xlab(xlabel) + ylab(ylabel) + theme_bw() +
  scale_colour_discrete(name=lname, labels=llabels, guide=guide_legend(order=1)) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  scale_y_continuous(limits=c(0,.4),labels=scales::percent) +
  geom_vline(data=pdata, aes(xintercept=46), size=.5, colour="black") +
  geom_vline(data=pdata, aes(xintercept=45), linetype="dashed", size=.5, colour="black") +
  labs(title = tlabel, subtitle = slabel)
print(plot1)

#NEW YORK
#set labels
xlabel <- "Week in Calendar Year"
ylabel <- "WP Top Box Score"
tlabel <- "New York: WP Top Box Score"
slabel <- "*Note: weeks start on price-increase days"
#set data and variables
pdata <- nyag
px <- nyag[, NEW_WEEK]
py <- nyag[, Q2_8_TB_SCORE]
groupvar <- nyag[, CAL_YR_NUM]
#manual legend labels
lname <- "Calendar Year"
llabels <- c("2016","2017") 
#line chart, factored by one variable
plot2 <- ggplot() +
  geom_line(data=pdata, aes(x=px, y=py, group=factor(groupvar), colour=factor(groupvar))) + 
  xlab(xlabel) + ylab(ylabel) + theme_bw() +
  scale_colour_discrete(name=lname, labels=llabels, guide=guide_legend(order=1)) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  scale_y_continuous(limits=c(0,.4),labels=scales::percent) +
  geom_vline(data=pdata, aes(xintercept=46), size=.5, colour="black") +
  geom_vline(data=pdata, aes(xintercept=45), linetype="dashed", size=.5, colour="black") +
  labs(title = tlabel, subtitle = slabel)
print(plot2)

#UNITED STATES
#set labels
xlabel <- "Week in Calendar Year"
ylabel <- "WP Top Box Score"
tlabel <- "United States: WP Top Box Score"
slabel <- "*Note: weeks start on price-increase days"
#set data and variables
pdata <- wpag
px <- wpag[, NEW_WEEK]
py <- wpag[, Q2_8_TB_SCORE]
groupvar <- wpag[, CAL_YR_NUM]
#manual legend labels
lname <- "Calendar Year"
llabels <- c("2016","2017") 
#line chart, factored by one variable
plot3 <- ggplot() +
  geom_line(data=pdata, aes(x=px, y=py, group=factor(groupvar), colour=factor(groupvar))) + 
  xlab(xlabel) + ylab(ylabel) + theme_bw() +
  scale_colour_discrete(name=lname, labels=llabels, guide=guide_legend(order=1)) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  scale_y_continuous(limits=c(0,.4),labels=scales::percent) + 
  geom_vline(data=pdata, aes(xintercept=46), size=.5, colour="black") +
  geom_vline(data=pdata, aes(xintercept=45), linetype="dashed", size=.5, colour="black") +
  labs(title = tlabel, subtitle = slabel)
print(plot3)

#NYC, SEATTLE, AND UNITED STATES - 2017
#set labels
xlabel <- "Week in Calendar Year"
ylabel <- "WP Top Box Score"
tlabel <- "NYC, Seattle, and United States: WP Top Box Score"
slabel <- "*Note: weeks start on price-increase days"
#set data and variables
pdata <- totalag[CAL_YR_NUM==2017]
px <- totalag[CAL_YR_NUM==2017, NEW_WEEK]
py <- totalag[CAL_YR_NUM==2017, Q2_8_TB_SCORE]
groupvar <- totalag[CAL_YR_NUM==2017, loc]
#manual legend labels
lname <- "Location"
llabels <- c("NYC","Seattle","US") 
#line chart, factored by one variable
plot4 <- ggplot() +
  geom_line(data=pdata, aes(x=px, y=py, group=factor(groupvar), colour=factor(groupvar))) + 
  xlab(xlabel) + ylab(ylabel) + theme_bw() +
  scale_colour_discrete(name=lname, labels=llabels, guide=guide_legend(order=1)) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  scale_y_continuous(limits=c(0,.4),labels=scales::percent) + 
  geom_vline(data=pdata, aes(xintercept=46), size=.5, colour="black") +
  geom_vline(data=pdata, aes(xintercept=45), linetype="dashed", size=.5, colour="black") +
  labs(title = tlabel, subtitle = slabel)
print(plot4)
