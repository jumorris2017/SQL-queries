##CE by Channel
##1/4/17

#load libraries
library(data.table)
library(ggplot2)
library(xlsx)

##CE AND COMP TRENDS FOR BROOKE/KELLY - 1/8/17

#PART 1: load data - CC/SO
cc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc-so_bystore-daypart_FY17Q1-FY18Q1.csv")
setnames(cc,"STORE_NUM","STORE_NUMBER")

#aggregate by daypart
cc <- cc[, list(TOTAL_TB = sum(TOTAL_TB,na.rm=T),
                TOTAL_RSPNS = sum(TOTAL_RSPNS,na.rm=T)),
         by=c("FSCL_YR_NUM","FSCL_QTR_IN_YR_NUM","DAY_PART","STORE_NUMBER","QSTN_ID")]

#reshape from long to wide by question
cc <- dcast.data.table(cc, FSCL_YR_NUM + FSCL_QTR_IN_YR_NUM + DAY_PART + STORE_NUMBER ~ QSTN_ID, value.var=c("TOTAL_TB","TOTAL_RSPNS"))

#loop through to create top box scores
tbcols <- grep("TOTAL_TB",colnames(cc),value=T)
trcols <- grep("TOTAL_RSPNS",colnames(cc),value=T)
for (i in c(1:7)){
  cc[, paste0("tbscore_q2_",i)] <- cc[, tbcols[[i]], with=F] / cc[, trcols[[i]], with=F]
}
#convert to percentages and round
cc[, (grep("tbscore",colnames(cc),value=T)) := lapply(.SD,function(x) round(x,2)*100), 
   .SDcols=grep("tbscore",colnames(cc),value=T)]
#calculate SO
sovars <- paste0("tbscore_q2_",c(1,3,4,5,6,7))
cc[, tbscore_so := round(rowMeans(cc[, c(sovars), with=F]),0)]


#PART 2: load data - transactions by day/part
tsdhr <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc-so_bystore-daypart_FY17Q1-FY18Q1-TSDhourly.csv")

#aggregate by daypart and FY/FQ
tsdhr <- tsdhr[, list(TRANS = sum(TRANS,na.rm=T)), 
               by=c("FSCL_YR_NUM","FSCL_QTR_IN_YR_NUM","DAY_PART","STORE_NUMBER")]

#create an indicator for unique dayparts by store and FY/FQ
dp <- tsdhr %>% 
  group_by(STORE_NUMBER,FSCL_YR_NUM,FSCL_QTR_IN_YR_NUM) %>%
  count(DAY_PART) %>% mutate(pct = round(n/sum(n),4))
setDT(dp)

#merge into daypart transaction data
tsdhr <- left_join(tsdhr,dp,by=c("FSCL_YR_NUM","FSCL_QTR_IN_YR_NUM","DAY_PART","STORE_NUMBER"))
setDT(tsdhr)

#PART 3: load data - day count
tsddc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc-so_bystore-daypart_FY17Q1-FY18Q1-TSDdaycount.csv")

#merge into daypart transaction data
tsdhr <- left_join(tsdhr,tsddc,by=c("FSCL_YR_NUM","FSCL_QTR_IN_YR_NUM","STORE_NUMBER"))
setDT(tsdhr)

#multiply day_count by percent of day_count
tsdhr[, daycount_scaled := day_count*pct]

#reduce columns
tsdhr <- tsdhr[, c("FSCL_YR_NUM","FSCL_QTR_IN_YR_NUM","TRANS","STORE_NUMBER","DAY_PART","daycount_scaled"), with=F]

#calculate COSD by daypart and store
tsdhr[, cosd := round(TRANS/daycount_scaled,1)]
tsdhr[mapply(is.infinite, tsdhr)] <- NA
#merge together CC-SO with tsdhr
full <- left_join(cc,tsdhr,by=c("STORE_NUMBER","DAY_PART","FSCL_YR_NUM","FSCL_QTR_IN_YR_NUM"))
setDT(full)

#aggregate at day_part level
fullagg <- full[, lapply(.SD,sum,na.rm=T), by=c("DAY_PART","FSCL_YR_NUM","FSCL_QTR_IN_YR_NUM")]
fullagg[, cosd := round(TRANS/daycount_scaled,1)]
#loop through to create top box scores
tbcols <- grep("TOTAL_TB",colnames(fullagg),value=T)
trcols <- grep("TOTAL_RSPNS",colnames(fullagg),value=T)
for (i in c(1:7)){
  fullagg[, paste0("tbscore_q2_",i)] <- fullagg[, tbcols[[i]], with=F] / fullagg[, trcols[[i]], with=F]
}
#calculate SO
sovars <- paste0("tbscore_q2_",c(1,3,4,5,6,7))
fullagg[, tbscore_so := rowMeans(fullagg[, c(sovars), with=F])]
#remove store var
fullagg[, STORE_NUMBER := NULL]
#keep only variables needed
fullagg <- fullagg[, c("DAY_PART","FSCL_YR_NUM","FSCL_QTR_IN_YR_NUM","tbscore_q2_2","tbscore_q2_1","tbscore_so","cosd"), with=F]
#setnames(fullagg,"tbscore_q2_2","tbscore_cc")
#write file
write.xlsx(fullagg,"O:/CoOp/CoOp194_PROReportng&OM/Julie/DayPart_CC-SO_FY17Q1-FY18Q1.xlsx")



















##ADDITIONAL WORK, FOR MY OWN EDIFICATION

#CORRELATIONS - partial, controlling for day part
#drop cases with missing cc/so or cosd
full <- na.omit(full,cols=c("tbscore_q2_2","tbscore_so","TRANS","TRANScomp"))
#COSD
pcor.test(full[,tbscore_q2_2],full[,cosd],full[,DAY_PART],method="pearson")
pcor.test(full[,tbscore_so],full[,cosd],full[,DAY_PART],method="pearson")
#TRANSACTION COUNT
pcor.test(full[,tbscore_q2_2],full[,TRANS],full[,DAY_PART],method="pearson")
pcor.test(full[,tbscore_so],full[,TRANS],full[,DAY_PART],method="pearson")
#TRANSACTION COMP
pcor.test(full[,tbscore_q2_2],full[,TRANScomp],full[,DAY_PART],method="pearson")
pcor.test(full[,tbscore_so],full[,TRANScomp],full[,DAY_PART],method="pearson")

#AM/M DELTAS: load data - CC
dcc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc_bystore-hour_FY17Q2-FY18Q1.csv")
setnames(dcc,"STORE_NUM","STORE_NUMBER")

#create an AM and PM delta (7-10) vs (2-5)
dcc[HOUR>=7&HOUR<=10, pmflag := 0]
dcc[HOUR>=14&HOUR<=17, pmflag := 1]
#
dcc <- dcc[(pmflag==0|pmflag==1)&QSTN_ID=="Q2_2"]

#aggregate by daypart
dcc <- dcc[, list(TOTAL_TB = sum(TOTAL_TB,na.rm=T),
                TOTAL_RSPNS = sum(TOTAL_RSPNS,na.rm=T)),
         by=c("pmflag","STORE_NUMBER","QSTN_ID")]
#tb score
dcc[, cctbscore := TOTAL_TB/TOTAL_RSPNS]

#reshape from long to wide by question
dcc <- dcast.data.table(dcc, STORE_NUMBER ~ pmflag, value.var=c("cctbscore"))
setnames(dcc,c("0","1"),c("cc_am","cc_pm"))

#look at AM to PM deltas
dcc[, ccdelta := cc_pm-cc_am]
#categorize deltas
dcc[ccdelta>=.05, ccdelgrp := 1]
dcc[ccdelta>=0&ccdelta<.05, ccdelgrp := 2]
dcc[ccdelta>-.05&ccdelta<0, ccdelgrp := 3]
dcc[ccdelta>-.15&ccdelta<=-.05, ccdelgrp := 4]
dcc[ccdelta<=-.15, ccdelgrp := 5]
dcc %>% 
  filter(!is.na(ccdelgrp)) %>%
  count(ccdelgrp) %>% mutate(pct = round(n/sum(n),4))

##JUST FOR FY18 Q1##
##FOR COMP##

#PART 1: load data - CC/SO
cc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc-so_bystore-daypart_FY18Q1.csv")
setnames(cc,"STORE_NUM","STORE_NUMBER")

#aggregate by daypart
cc <- cc[, list(TOTAL_TB = sum(TOTAL_TB,na.rm=T),
                TOTAL_RSPNS = sum(TOTAL_RSPNS,na.rm=T)),
         by=c("DAY_PART","STORE_NUMBER","QSTN_ID")]

#reshape from long to wide by question
cc <- dcast.data.table(cc, DAY_PART + STORE_NUMBER ~ QSTN_ID, value.var=c("TOTAL_TB","TOTAL_RSPNS"))

#loop through to create top box scores
tbcols <- grep("TOTAL_TB",colnames(cc),value=T)
trcols <- grep("TOTAL_RSPNS",colnames(cc),value=T)
for (i in c(1:7)){
  cc[, paste0("tbscore_q2_",i)] <- cc[, tbcols[[i]], with=F] / cc[, trcols[[i]], with=F]
}
#convert to percentages and round
cc[, (grep("tbscore",colnames(cc),value=T)) := lapply(.SD,function(x) round(x,2)*100), 
   .SDcols=grep("tbscore",colnames(cc),value=T)]
#calculate SO
sovars <- paste0("tbscore_q2_",c(1,3,4,5,6,7))
cc[, tbscore_so := round(rowMeans(cc[, c(sovars), with=F]),0)]

#PART 2: load data - transactions by day/part
tsdhr <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc-so_bystore-daypart_FY18Q1-TSDhourly.csv")
#LAST YEAR TRANS
tsdhrly <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc-so_bystore-daypart_FY17Q1-TSDhourly.csv")
setnames(tsdhrly,"TRANS","TRANS_LY")
tsdhrly[, FSCL_YR_NUM := NULL]

#merge into last-year trans
tsdhr <- left_join(tsdhr,tsdhrly,by=c("STORE_NUMBER","DAY_PART","SALE_HOUR"))
setDT(tsdhr)

#aggregate by daypart
tsdhr <- tsdhr[, list(TRANS = sum(TRANS,na.rm=T),
                      TRANS_LY = sum(TRANS_LY,na.rm=T)), by=c("DAY_PART","STORE_NUMBER")]

#create an indicator for unique dayparts by store
dp <- tsdhr %>% 
  group_by(STORE_NUMBER) %>%
  count(DAY_PART) %>% mutate(pct = round(n/sum(n),4))
setDT(dp)

#merge into daypart transaction data
tsdhr <- left_join(tsdhr,dp,by=c("DAY_PART","STORE_NUMBER"))
setDT(tsdhr)

#PART 3: load data - day count
tsddc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc-so_bystore-daypart_FY18Q1-TSDdaycount.csv")
tsddcly <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc-so_bystore-daypart_FY17Q1-TSDdaycount.csv")
setnames(tsddcly,"day_count","day_count_LY")
tsddcly[, FSCL_YR_NUM := NULL]

#merge into last-year daycount
tsddc <- left_join(tsddc,tsddcly,by=c("STORE_NUMBER"))
setDT(tsddc)
tsddc <- left_join(tsddc,dp,by=c("STORE_NUMBER"))
setDT(tsddc)

#multiply day_count by percent of day_count
tsddc[, daycount_scaled := day_count*pct]
tsddc[, daycount_scaled_LY := day_count_LY*pct]

#reduce columns
tsddc <- tsddc[, c("STORE_NUMBER","DAY_PART","daycount_scaled","daycount_scaled_LY"), with=F]

#merge into daypart transaction data
tsdhr <- left_join(tsdhr,tsddc,by=c("STORE_NUMBER","DAY_PART"))
setDT(tsdhr)

#calculate COSD by daypart and store
tsdhr[, cosd := round(TRANS/daycount_scaled,1)]
#calculate trans comp
tsdhr[, TRANScomp := (TRANS-TRANS_LY)/TRANS_LY]
tsdhr[mapply(is.infinite, tsdhr)] <- NA

# #merge together CC-SO with tsdhr
# full <- left_join(cc,tsdhr,by=c("STORE_NUMBER","DAY_PART"))
# setDT(full)



##COSD
#merge together CC-SO with tsdhr
full <- left_join(cc,tsdhr,by=c("STORE_NUMBER","DAY_PART"))
setDT(full)

##BY DAYPART

#split by cc
prob = c(1/4, 2/4, 3/4, 1)
temp <- full %>% group_by(DAY_PART) %>% summarise( 
  cc25 = quantile(tbscore_q2_2, probs = prob[1], na.rm = T), 
  cc50 = quantile(tbscore_q2_2, probs = prob[2], na.rm = T),
  cc75 = quantile(tbscore_q2_2, probs = prob[3], na.rm = T),
  cc100 = quantile(tbscore_q2_2, probs = prob[4], na.rm = T)
)
full <- left_join(full, temp, by=c("DAY_PART"))
setDT(full)

#recode cc based on quartiles
full[tbscore_q2_2 <= cc25, ccquartile := 1]
full[tbscore_q2_2 > cc25 & tbscore_q2_2 <= cc50, ccquartile := 2]
full[tbscore_q2_2 > cc50 & tbscore_q2_2 <= cc75, ccquartile := 3]
full[tbscore_q2_2 > cc75, ccquartile := 4]
#drop quartile columns
#full <- full[, !(names(full[,c(10:13)])), with=FALSE]

#calculate top box score for cc, by comps quartile
full <- full[, list(TOTAL_RSPNS_Q2_2=sum(TOTAL_RSPNS_Q2_2,na.rm=T),
                    TOTAL_TB_Q2_2=sum(TOTAL_TB_Q2_2,na.rm=T),
                    COSDavg=(sum(TRANS))/sum(daycount_scaled)),
             by=c("ccquartile","DAY_PART")]
full[, tbscore_q2_2 := TOTAL_TB_Q2_2/TOTAL_RSPNS_Q2_2]
#order by quartile
full <- setorder(full,ccquartile)
full[, TOTAL_RSPNS_Q2_2 := NULL]; full[, TOTAL_TB_Q2_2 := NULL]

#export data
fullx <- full[, .(DAY_PART,ccquartile,COSDavg,tbscore_q2_2)]
fullx <- setorder(fullx,DAY_PART,ccquartile)
#fullx[, (colnames(fullx)[2:ncol(fullx)]) := lapply(.SD,function(x) round(x,4)*100),.SDcols=colnames(fullx)[2:ncol(fullx)]]
#write file
write.xlsx(fullx,file="O:/CoOp/CoOp194_PROReportng&OM/Julie/cosd_by_cc_quartile_byDAYPART_FY18Q1.xlsx")


##*not* BY DAYPART
#merge together CC-SO with tsdhr
full <- left_join(cc,tsdhr,by=c("STORE_NUMBER","DAY_PART"))
setDT(full)
full[, FSCL_YR_NUM := 2018]

#split by cc
prob = c(1/4, 2/4, 3/4, 1)
temp <- full %>% group_by(FSCL_YR_NUM) %>% summarise( 
  cc25 = quantile(tbscore_q2_2, probs = prob[1], na.rm = T), 
  cc50 = quantile(tbscore_q2_2, probs = prob[2], na.rm = T),
  cc75 = quantile(tbscore_q2_2, probs = prob[3], na.rm = T),
  cc100 = quantile(tbscore_q2_2, probs = prob[4], na.rm = T)
)
full <- left_join(full, temp, by=c("FSCL_YR_NUM"))
setDT(full)

#recode cc based on quartiles
full[tbscore_q2_2 <= cc25, ccquartile := 1]
full[tbscore_q2_2 > cc25 & tbscore_q2_2 <= cc50, ccquartile := 2]
full[tbscore_q2_2 > cc50 & tbscore_q2_2 <= cc75, ccquartile := 3]
full[tbscore_q2_2 > cc75, ccquartile := 4]
#drop quartile columns
#full <- full[, !(names(full[,c(10:13)])), with=FALSE]

#calculate top box score for cc, by comps quartile
full <- full[, list(TOTAL_RSPNS_Q2_2=sum(TOTAL_RSPNS_Q2_2,na.rm=T),
                    TOTAL_TB_Q2_2=sum(TOTAL_TB_Q2_2,na.rm=T),
                    COSDavg=(sum(TRANS))/sum(daycount_scaled)),
             by=c("ccquartile")]
full[, tbscore_q2_2 := TOTAL_TB_Q2_2/TOTAL_RSPNS_Q2_2]
#order by quartile
full <- setorder(full,ccquartile)
full[, TOTAL_RSPNS_Q2_2 := NULL]; full[, TOTAL_TB_Q2_2 := NULL]

#export data
fullx <- full[, .(ccquartile,COSDavg,tbscore_q2_2)]
fullx <- setorder(fullx,ccquartile)
#fullx[, (colnames(fullx)[2:ncol(fullx)]) := lapply(.SD,function(x) round(x,4)*100),.SDcols=colnames(fullx)[2:ncol(fullx)]]
#write file
write.xlsx(fullx,file="O:/CoOp/CoOp194_PROReportng&OM/Julie/cosd_by_cc_quartile_FY18Q1.xlsx")



##TRANS COMP

##BY DAYPART
#merge together CC-SO with tsdhr
full <- left_join(cc,tsdhr,by=c("STORE_NUMBER","DAY_PART"))
setDT(full)

#agg by store
full <- full[, list(TOTAL_RSPNS_Q2_2=sum(TOTAL_RSPNS_Q2_2,na.rm=T),
                    TOTAL_TB_Q2_2=sum(TOTAL_TB_Q2_2,na.rm=T),
                    TRANS=sum(TRANS,na.rm=T),
                    TRANS_LY=sum(TRANS_LY,na.rm=T),
                    daycount=sum(daycount_scaled,na.rm=T)),
                    #TRANScompavg=(sum(TRANS)-sum(TRANS_LY))/sum(TRANS_LY)),
             by=c("STORE_NUMBER")]
#cc tb score
full[, tbscore_q2_2 := TOTAL_TB_Q2_2/TOTAL_RSPNS_Q2_2]
full[, TRANScompavg := (TRANS-TRANS_LY)/TRANS_LY]
#tsd
full[, tsd := TRANS/daycount]

#correlation
full[mapply(is.infinite, full)] <- NA
full <- na.omit(full,cols="TRANScompavg")
cor(full[,tbscore_q2_2],full[,TRANScompavg],use="complete.obs")
pcor.test(full[,tbscore_q2_2],full[,TRANScompavg],full[,tsd],method="pearson")

#limit by tsd
full <- full[tsd>=0&tsd<=850]
#make fake variable for merge
full[, FSCL_YR_NUM := 2018]

#split by cc
prob = c(1/4, 2/4, 3/4, 1)
temp <- full %>% group_by(FSCL_YR_NUM) %>% summarise( 
  cc25 = quantile(tbscore_q2_2, probs = prob[1], na.rm = T), 
  cc50 = quantile(tbscore_q2_2, probs = prob[2], na.rm = T),
  cc75 = quantile(tbscore_q2_2, probs = prob[3], na.rm = T),
  cc100 = quantile(tbscore_q2_2, probs = prob[4], na.rm = T)
)
full <- left_join(full, temp, by=c("FSCL_YR_NUM"))
setDT(full)

#recode cc based on quartiles
full[tbscore_q2_2 <= cc25, ccquartile := 1]
full[tbscore_q2_2 > cc25 & tbscore_q2_2 <= cc50, ccquartile := 2]
full[tbscore_q2_2 > cc50 & tbscore_q2_2 <= cc75, ccquartile := 3]
full[tbscore_q2_2 > cc75, ccquartile := 4]
#drop quartile columns
#full <- full[, !(names(full[,c(10:13)])), with=FALSE]

#calculate top box score for cc, by comps quartile
full <- full[, list(TOTAL_RSPNS_Q2_2=sum(TOTAL_RSPNS_Q2_2,na.rm=T),
                    TOTAL_TB_Q2_2=sum(TOTAL_TB_Q2_2,na.rm=T),
                    TRANScompavg=(sum(TRANS)-sum(TRANS_LY))/sum(TRANS_LY)),
             by=c("ccquartile")]
full[, tbscore_q2_2 := TOTAL_TB_Q2_2/TOTAL_RSPNS_Q2_2]
#order by quartile
full <- setorder(full,ccquartile)
full[, TOTAL_RSPNS_Q2_2 := NULL]; full[, TOTAL_TB_Q2_2 := NULL]

#export data
fullx <- full[, .(ccquartile,TRANScompavg,tbscore_q2_2)]
fullx <- setorder(fullx,ccquartile)
#fullx[, (colnames(fullx)[2:ncol(fullx)]) := lapply(.SD,function(x) round(x,4)*100),.SDcols=colnames(fullx)[2:ncol(fullx)]]
#write file
write.xlsx(fullx,file="O:/CoOp/CoOp194_PROReportng&OM/Julie/transcomp_by_cc_quartile_FY18Q1.xlsx")



# #aggregate at day_part level
# fullagg <- full[, lapply(.SD,sum,na.rm=T), by=c("DAY_PART")]
# fullagg[, cosd := round(TRANS/daycount_scaled,1)]
# #loop through to create top box scores
# tbcols <- grep("TOTAL_TB",colnames(fullagg),value=T)
# trcols <- grep("TOTAL_RSPNS",colnames(fullagg),value=T)
# for (i in c(1:7)){
#   fullagg[, paste0("tbscore_q2_",i)] <- fullagg[, tbcols[[i]], with=F] / fullagg[, trcols[[i]], with=F]
# }
# #calculate SO
# sovars <- paste0("tbscore_q2_",c(1,3,4,5,6,7))
# fullagg[, tbscore_so := rowMeans(fullagg[, c(sovars), with=F])]
# #remove store var
# fullagg[, STORE_NUMBER := NULL]
# #keep only variables needed
# fullagg <- fullagg[, c("DAY_PART","tbscore_q2_2","tbscore_so","cosd"), with=F]
# #write file
# write.xlsx(fullagg,"O:/CoOp/CoOp194_PROReportng&OM/Julie/DayPart_CC-SO_FY18Q1.xlsx")





