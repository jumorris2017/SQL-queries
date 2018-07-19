#worth scores by price zone
#by week and year
#US CO stores

#load data
data_dir <- "O:/CoOp/CoOp194_PROReportng&OM/Julie"
wp <- fread(paste0(data_dir,"/20180719_wp_by_store_and_week.csv"))
pz <- fread(paste0(data_dir,"/price_zones.csv"))
setnames(pz,"store_num","STORE_NUM")

#merge
wp <- Reduce(function(x,y) {merge(x,y,by="STORE_NUM",all.x=T)},list(wp,pz))

# #canadian stores, eh...
# pz[!price_zone %in% wp[,price_zone],.N,by="price_zone"]

#aggregate by price zone
wp <- wp[, list(wp_tb_score = round(sum(WP_TB,na.rm=T)/sum(WP_RESP,na.rm=T),4)),
         by=c("FSCL_YR_NUM","FSCL_WK_IN_YR_NUM","price_zone")]

#swing wide
wp <- dcast.data.table(wp, FSCL_WK_IN_YR_NUM + price_zone ~ FSCL_YR_NUM, value.var="wp_tb_score")

#setnames
setnames(wp,c("2017","2018"),c("wp_2017","wp_2018"))

#set order
setcolorder(wp,c("price_zone","FSCL_WK_IN_YR_NUM","wp_2018","wp_2017"))
wp <- setorder(wp,price_zone,FSCL_WK_IN_YR_NUM)

#drop missing
wp <- na.omit(wp)

#write to .csv
write.csv(wp,file=paste0(data_dir,"/20180719_wp_by_pricezones.csv"),row.names=F)