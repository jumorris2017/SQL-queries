##request 1: worth perceptions for Lisa request 11/15/17
##request 2: CC and SO correlations for Lisa request 11/15/17

#load libraries
library(data.table)
library(xlsx)

#Request #1: worth perceptions pre- and post- price change by store

#load data
wp <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/worth_perception_for_R_111517.csv")
#load NYC store data
nypz <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/nycpricezonestore.csv")
nyvec <- unique(nypz[,store_num])

#create pre- and post-weeks
wp[, TRANS_DATE := as.Date(TRANS_DATE, "%d-%b-%y")]
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

