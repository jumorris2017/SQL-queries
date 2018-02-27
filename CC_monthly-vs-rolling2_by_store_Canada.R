##CC by daypart and store
##identifying minimum timeframe
##ideal cell minimum = 70 CE responses

#load libraries
library(data.table)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(xlsx)


###CANADA

#2 month snapshots for stores
str2m <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC_r2m_Canada.csv")
#drop earliest month
str2m <- str2m[(FSCL_YR_NUM==2017&FSCL_PER_IN_YR_NUM>=3)|(FSCL_YR_NUM==2018&FSCL_PER_IN_YR_NUM<=3)]
str2m[, cc_score := round(TOTAL_TB/TOTAL_RSPNS,3)*100]
#make month year var
str2m[, FPFY := paste0(FSCL_YR_NUM,".",FSCL_PER_IN_YR_NUM)]
str2m[, FPFY := as.numeric(FPFY)]
str2m[, FPFYlabel := paste0(FSCL_YR_NUM,"-",FSCL_PER_IN_YR_NUM)]
#calculate cc delta
str2m[, lag_R2MCC :=lapply(.SD, function(x) c(NA, x[-.N])), by="STORE_NUM", .SDcols="cc_score"]
str2m[, r2mccdel := cc_score-lag_R2MCC]
#keep only the latest period for plot
str2m <- str2m[FSCL_YR_NUM==2018&FSCL_PER_IN_YR_NUM==3]

#set labels
xlabel <- "CC Score Delta"
ylabel <- "Number of Stores"
sublabel <- "FY18 Nov and Dec"
tlabel <- "Delta in CC Score across 2 Months"
caption <- "Canada"
#plot itself
plot2 <- ggplot(str2m,aes(r2mccdel)) + 
  geom_histogram(binwidth=1,show.legend=FALSE,fill="lightgrey",col=I("black")) + 
  theme_economist() + scale_colour_brewer(palette = 1, name="", labels="") +
  scale_x_continuous(limits=c(-55,55), breaks = scales::pretty_breaks(35)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption)
print(plot2)


#rolling 2 for stores
str2m <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC_r2m_Canada.csv")
#calculate rolling two by day part
str2m[, lag_TB :=lapply(.SD, function(x) c(NA, x[-.N])), by="STORE_NUM", .SDcols="TOTAL_TB"]
str2m[, lag_RSPNS :=lapply(.SD, function(x) c(NA, x[-.N])), by="STORE_NUM", .SDcols="TOTAL_RSPNS"]
#sum together
str2m[, R2MTB := rowSums(.SD, na.rm = TRUE), .SDcols=c("TOTAL_TB","lag_TB")]
str2m[, R2MRSPNS := rowSums(.SD, na.rm = TRUE), .SDcols=c("TOTAL_RSPNS","lag_RSPNS")]
#drop earliest month
str2m <- str2m[(FSCL_YR_NUM==2017&FSCL_PER_IN_YR_NUM>=3)|(FSCL_YR_NUM==2018&FSCL_PER_IN_YR_NUM<=3)]
str2m[, R2MCC := round(R2MTB/R2MRSPNS,3)*100]
#make month year var
str2m[, FPFY := paste0(FSCL_YR_NUM,".",FSCL_PER_IN_YR_NUM)]
str2m[, FPFY := as.numeric(FPFY)]
str2m[, FPFYlabel := paste0(FSCL_YR_NUM,"-",FSCL_PER_IN_YR_NUM)]
#calculate cc delta
str2m[, lag_R2MCC :=lapply(.SD, function(x) c(NA, x[-.N])), by="STORE_NUM", .SDcols="R2MCC"]
str2m[, r2mccdel := R2MCC-lag_R2MCC]
#keep only the latest period for plot
str2m <- str2m[FSCL_YR_NUM==2018&FSCL_PER_IN_YR_NUM==3]

#set labels
xlabel <- "CC Score Delta"
ylabel <- "Number of Stores"
sublabel <- "FY18 Oct-Nov and Nov-Dec"
tlabel <- "Delta in CC Score across Rolling-2 Month Window"
caption <- "Canada"
#plot itself
plot2 <- ggplot(str2m,aes(r2mccdel)) + 
  geom_histogram(binwidth=1,show.legend=FALSE,fill="lightgrey",col=I("black")) + 
  theme_economist() + scale_colour_brewer(palette = 1, name="", labels="") +
  scale_x_continuous(limits=c(-35,35), breaks = scales::pretty_breaks(35)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption)
print(plot2)
