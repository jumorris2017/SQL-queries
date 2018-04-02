#load libaries
library(data.table)

#set path (new Q)
data_dir <- "O:/CoOp/CoOp194_PROReportng&OM/Julie"

#load data
wp <- fread(paste0(data_dir,"/FY16-18_WPscores_PriceZones.csv"))
zones <- fread(paste0(data_dir,"/base_price_zone.csv"))

#merge
full <- Reduce(function(x,y) {merge(x,y,all=T,by="STORE_NUM")}, list(wp,zones))

#aggregate
full <- full[, list(TOTAL_TB = sum(TOTAL_TB,na.rm=T),
                    TOTAL_RSPNS = sum(TOTAL_RSPNS,na.rm=T),
                    WP_TB = sum(TOTAL_TB,na.rm=T)/sum(TOTAL_RSPNS,na.rm=T)),
             by=c("zone_nm","FSCL_YR_NUM","FSCL_WK_IN_YR_NUM")]
full[, WP_TB := round(WP_TB,3)]
full <- full[zone_nm!='#N/A']
full <- na.omit(full)
#set order
setorder(full,zone_nm,FSCL_YR_NUM,FSCL_WK_IN_YR_NUM)
full <- full[,.(zone_nm,FSCL_YR_NUM,FSCL_WK_IN_YR_NUM,WP_TB)]

#export
write.csv(full,paste0(data_dir,"/FY16-18_WPscores_by_pricezone.csv"))