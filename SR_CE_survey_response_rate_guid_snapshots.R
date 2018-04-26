#tracking SR CE response repeats by GUID
#P12 FY16 & P6 FY17 & P12 FY18 & P6 FY18

#load libraries
library(data.table)
library(lubridate)
library(ggplot2)

#load data
data_dir <- "O:/CoOp/CoOp194_PROReportng&OM/Julie"
cer <- fread(paste0(data_dir,"/total_ce_resp_by_storeandmonth.csv"))


#drop FY 2014 and latest month
cer <- cer[!(FSCL_YR_NUM==2014|(FSCL_YR_NUM==2018&FSCL_PER_IN_YR_NUM==7))]

#get average number of responses by store
cer <- cer[, list(avg_resp = round(mean(TOTAL_RSPNS_CNT,na.rm=T),1)),
           by=c("COUNTRY_CD","FSCL_YR_NUM","FSCL_PER_IN_YR_NUM")]

#create an x-variable
cer[, fyfp := paste0(FSCL_YR_NUM,".",str_pad(cer[,FSCL_PER_IN_YR_NUM],2,pad="0"))]

#set up line chart
pdata <- cer
px <- cer[, fyfp]
py <- cer[, avg_resp]
groupvar <- cer[, COUNTRY_CD]
#set labels
xlabel <- "Time"
ylabel <- "Average Responses (#)"
tlabel <- "Average CE Responses by Store"
#manual legend labels
lname <- "Country"
llabels <- c("Canada","United States") 

#line chart
plot2 <- ggplot() +
  geom_line(data=pdata, aes(x=factor(px), y=py, group=factor(groupvar), colour=factor(groupvar))) + 
  xlab(xlabel) + ylab(ylabel) + theme_economist_white(gray_bg = FALSE) +
  scale_colour_discrete(name=lname, labels=llabels, guide=guide_legend(order=1)) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  theme(axis.text.x = element_text(size = 7, angle = 90, hjust = 1)) +
  ggtitle(tlabel)
print(plot2)

#load data
data_dir <- "O:/CoOp/CoOp194_PROReportng&OM/Julie"
sr <- fread(paste0(data_dir,"/ce_survey_guids_p6fy16and17_p12fy17and18.csv"))
ex <- fread(paste0(data_dir,"/ce_survey_guids_experian.csv"))
ten <- fread(paste0(data_dir,"/ce_survey_guids_sr_tenure.csv"))

#SR COMPARISON
sr_sr <- fread(paste0(data_dir,"/sr_guids_p6fy16and17_p12fy17and18.csv"))
sr_ex <- fread(paste0(data_dir,"/sr_guids_experian.csv"))
sr_ten <- fread(paste0(data_dir,"/sr_guids_sr_tenure.csv"))

#merge together
srfull <- Reduce(function(x, y) {merge(x, y, 
                                       by=c("GUID_USER_ID","FSCL_YR_NUM","FSCL_PER_IN_YR_NUM"), 
                                       all = TRUE)}, list(sr,ex))
srfull <- Reduce(function(x, y) {merge(x, y, by=c("GUID_USER_ID"), all = TRUE)}, list(srfull,ten))

#vectors of guids by period
g_p12fy16 <- srfull[FSCL_YR_NUM==2016&FSCL_PER_IN_YR_NUM==12,GUID_USER_ID]
g_p6fy17 <- srfull[FSCL_YR_NUM==2017&FSCL_PER_IN_YR_NUM==6,GUID_USER_ID]
g_p12fy17 <- srfull[FSCL_YR_NUM==2017&FSCL_PER_IN_YR_NUM==12,GUID_USER_ID]
g_p6fy18 <- srfull[FSCL_YR_NUM==2018&FSCL_PER_IN_YR_NUM==6,GUID_USER_ID]

#percent repeat
#first period to next 3 periods
length(intersect(g_p12fy16,g_p6fy17))/length(g_p12fy16) #1 period later
length(intersect(g_p12fy16,g_p12fy17))/length(g_p12fy16)
length(intersect(g_p12fy16,g_p6fy18))/length(g_p12fy16)
#second period to next 2 periods
length(intersect(g_p6fy17,g_p12fy17))/length(g_p6fy17) #1 period later
length(intersect(g_p6fy17,g_p6fy18))/length(g_p6fy17)
#third period to next 1 period
length(intersect(g_p12fy17,g_p6fy18))/length(g_p12fy17) #1 period later

#calculate tenure
srfull[, joindt := as_date(ymd_hms(MBR_JOIN_DT))]
srfull[FSCL_YR_NUM==2016&FSCL_PER_IN_YR_NUM==12, last_per_date := as_date("2016-10-02")] #update for each period
srfull[FSCL_YR_NUM==2017&FSCL_PER_IN_YR_NUM==6, last_per_date := as_date("2017-04-02")] #update for each period
srfull[FSCL_YR_NUM==2017&FSCL_PER_IN_YR_NUM==12, last_per_date := as_date("2017-10-01")] #update for each period
srfull[FSCL_YR_NUM==2018&FSCL_PER_IN_YR_NUM==6, last_per_date := as_date("2018-04-01")] #update for each period
srfull[, srtenure := last_per_date-joindt]
srfull[, srtenure_yrs := as.numeric(srtenure, units="days")]
srfull[, srtenure_yrs := round(srtenure_yrs/365.25,2)]

#clean up experian data
srfull[, fyfp := paste0(FSCL_YR_NUM,".",str_pad(srfull[,FSCL_PER_IN_YR_NUM],2,pad="0"))]
srfull[, COMBINED_AGE := factor(sub("E", "", COMBINED_AGE))]
srfull[GENDER=="U", GENDER := NA]

temp <- srfull[, list(srtenure_yrs = mean(srtenure_yrs,na.rm=T)),
               by=c("FSCL_YR_NUM","FSCL_PER_IN_YR_NUM")]

srfull %>%
  filter(!is.na(GENDER)) %>%
  group_by(fyfp,GENDER) %>%
  summarise (n = n()) %>%
  mutate(pct = n / sum(n))

#mode function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#calculate percent in modal category
na.omit(srfull[fyfp==2016.12,(.N/nrow(na.omit(srfull[fyfp==2016.12])))*100,by="EDUCATION_MODEL"])
na.omit(srfull[fyfp==2017.06,(.N/nrow(na.omit(srfull[fyfp==2017.06])))*100,by="EDUCATION_MODEL"])
na.omit(srfull[fyfp==2017.12,(.N/nrow(na.omit(srfull[fyfp==2017.12])))*100,by="EDUCATION_MODEL"])
na.omit(srfull[fyfp==2018.06,(.N/nrow(na.omit(srfull[fyfp==2018.06])))*100,by="EDUCATION_MODEL"])

na.omit(srfull[fyfp==2016.12,(.N/nrow(na.omit(srfull[fyfp==2016.12])))*100,by="EST_HOUSEHOLD_INCOME_V5"])
na.omit(srfull[fyfp==2017.06,(.N/nrow(na.omit(srfull[fyfp==2017.06])))*100,by="EST_HOUSEHOLD_INCOME_V5"])
na.omit(srfull[fyfp==2017.12,(.N/nrow(na.omit(srfull[fyfp==2017.12])))*100,by="EST_HOUSEHOLD_INCOME_V5"])
na.omit(srfull[fyfp==2018.06,(.N/nrow(na.omit(srfull[fyfp==2018.06])))*100,by="EST_HOUSEHOLD_INCOME_V5"])

na.omit(srfull[fyfp==2016.12,(.N/nrow(na.omit(srfull[fyfp==2016.12])))*100,by="OCCUPATION_GROUP_V2"])
na.omit(srfull[fyfp==2017.06,(.N/nrow(na.omit(srfull[fyfp==2017.06])))*100,by="OCCUPATION_GROUP_V2"])
na.omit(srfull[fyfp==2017.12,(.N/nrow(na.omit(srfull[fyfp==2017.12])))*100,by="OCCUPATION_GROUP_V2"])
na.omit(srfull[fyfp==2018.06,(.N/nrow(na.omit(srfull[fyfp==2018.06])))*100,by="OCCUPATION_GROUP_V2"])

na.omit(srfull[fyfp==2016.12,(.N/nrow(na.omit(srfull[fyfp==2016.12])))*100,by="MARITAL_STATUS"])
na.omit(srfull[fyfp==2017.06,(.N/nrow(na.omit(srfull[fyfp==2017.06])))*100,by="MARITAL_STATUS"])
na.omit(srfull[fyfp==2017.12,(.N/nrow(na.omit(srfull[fyfp==2017.12])))*100,by="MARITAL_STATUS"])
na.omit(srfull[fyfp==2018.06,(.N/nrow(na.omit(srfull[fyfp==2018.06])))*100,by="MARITAL_STATUS"])

#52 = Some College - Likely
Mode(na.omit(srfull[fyfp==2016.12,EDUCATION_MODEL]))
Mode(na.omit(srfull[fyfp==2017.06,EDUCATION_MODEL]))
Mode(na.omit(srfull[fyfp==2017.12,EDUCATION_MODEL]))
Mode(na.omit(srfull[fyfp==2018.06,EDUCATION_MODEL]))
#F = 75000 - 99999
Mode(na.omit(srfull[fyfp==2016.12,EST_HOUSEHOLD_INCOME_V5]))
Mode(na.omit(srfull[fyfp==2017.06,EST_HOUSEHOLD_INCOME_V5]))
Mode(na.omit(srfull[fyfp==2017.12,EST_HOUSEHOLD_INCOME_V5]))
Mode(na.omit(srfull[fyfp==2018.06,EST_HOUSEHOLD_INCOME_V5]))
#I8 = Other - Unknown 
Mode(na.omit(srfull[fyfp==2016.12,OCCUPATION_GROUP_V2]))
Mode(na.omit(srfull[fyfp==2017.06,OCCUPATION_GROUP_V2]))
Mode(na.omit(srfull[fyfp==2017.12,OCCUPATION_GROUP_V2]))
Mode(na.omit(srfull[fyfp==2018.06,OCCUPATION_GROUP_V2]))
#1M = Married Extremely Likely
Mode(na.omit(srfull[fyfp==2016.12,MARITAL_STATUS]))
Mode(na.omit(srfull[fyfp==2017.06,MARITAL_STATUS]))
Mode(na.omit(srfull[fyfp==2017.12,MARITAL_STATUS]))
Mode(na.omit(srfull[fyfp==2018.06,MARITAL_STATUS]))