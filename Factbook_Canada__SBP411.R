##R code for SQL query 
##CC by comps for Canada stores
##Request from Lisa 11/22/17

##load libraries
library(data.table)
library(xlsx)
library(dplyr)
library(ggplot2)
library(flipRegression)
library(lattice)
#library(RColorBrewer)

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#load data
#part 1
p1 <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/Comps_by_store_Canada_pt1.csv")
#part 2
p2 <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/Comps_by_store_Canada_pt2.csv")

#rename merge id columns to match
setnames(p1,c("STORE_NUM","FSCL_WK_IN_YR_NUM","FSCL_YR_NUM"),
         c("STORE_NUMBER","FISCAL_WEEK_NUMBER","FISCAL_YEAR_NUMBER"))
#change store number and date values to numeric from characters
p1[, STORE_NUMBER := lapply(.SD, as.numeric), .SDcols = "STORE_NUMBER"]
#merge by store number, month, and year
pfull <- Reduce(function(x,y) {merge(x,y,by=c("STORE_NUMBER","FISCAL_WEEK_NUMBER","FISCAL_YEAR_NUMBER","DIV_ORG_LVL_ID"),all.x=TRUE)}, list(p1,p2))

#aggregate for Q4
pagg <- pfull[, lapply(.SD, sum, na.rm=T), .SDcols=c("Q2_2_RESPONSE_TOTAL","Q2_2_TB_CNT",
                                                     "MonthlySales","LYMonthlySales"),
              by=c("STORE_NUMBER","FSCL_QTR_IN_YR_NUM","FISCAL_YEAR_NUMBER")]

#calculate CC  top box score
pagg[, Q2_2_TB_SCORE := round(Q2_2_TB_CNT/Q2_2_RESPONSE_TOTAL,4)]

#drop if stores don't have LYMonthlySales
pagg <- na.omit(pagg, cols=c("MonthlySales", "LYMonthlySales"))
pagg <- pagg[MonthlySales>0&LYMonthlySales>0]

#calculate comps
pagg[, comps := (MonthlySales-LYMonthlySales)/LYMonthlySales]
pagg <- pagg[comps>=-.25&comps<=.25]

#keep only FY17Q4
pagg <- pagg[FSCL_QTR_IN_YR_NUM==4&FISCAL_YEAR_NUMBER==2017]

#split by cc
prob = c(1/4, 2/4, 3/4, 1)
temp <- pagg %>% group_by(FSCL_QTR_IN_YR_NUM,FISCAL_YEAR_NUMBER) %>% summarise( 
  cc25 = quantile(Q2_2_TB_SCORE, probs = prob[1], na.rm = T), 
  cc50 = quantile(Q2_2_TB_SCORE, probs = prob[2], na.rm = T),
  cc75 = quantile(Q2_2_TB_SCORE, probs = prob[3], na.rm = T),
  cc100 = quantile(Q2_2_TB_SCORE, probs = prob[4], na.rm = T)
)
pagg <- left_join(pagg, temp, by=c("FSCL_QTR_IN_YR_NUM","FISCAL_YEAR_NUMBER"))
setDT(pagg)

##to view quartile cut-offs
#temp2 <- as.data.table(temp)
#temp2[, (names(temp2[,c(2:5)])) := lapply(.SD, function(x) round(x,3)), .SDcols=names(temp2[,c(2:5)])]

#recode cc based on quartiles
pagg[Q2_2_TB_SCORE <= cc25, ccquartile := 1]
pagg[Q2_2_TB_SCORE > cc25 & Q2_2_TB_SCORE <= cc50, ccquartile := 2]
pagg[Q2_2_TB_SCORE > cc50 & Q2_2_TB_SCORE <= cc75, ccquartile := 3]
pagg[Q2_2_TB_SCORE > cc75, ccquartile := 4]
#drop quartile columns
#pagg <- pagg[, !(names(pagg[,c(10:13)])), with=FALSE]

#calculate top box score for cc, by comps quartile
pagg <- pagg[, list(Q2_2_RESPONSE_TOTAL=sum(Q2_2_RESPONSE_TOTAL,na.rm=T),
                    Q2_2_TB_CNT=sum(Q2_2_TB_CNT,na.rm=T),
                    compsavg=(sum(MonthlySales)-sum(LYMonthlySales))/sum(LYMonthlySales)),
             by="ccquartile"]
pagg[, Q2_2_TB_SCORE := Q2_2_TB_CNT/Q2_2_RESPONSE_TOTAL]
#order by quartile
pagg <- setorder(pagg,ccquartile)
pagg <- cbind(pagg,t(temp)[3:6])
setnames(pagg,"V2","cc_q_value")

##make comps quartile factor for grouping
pagg[, ccquartile := as.factor(ccquartile)]
#set labels
lname <- "CC Quartile"
llabels <- c("25th", "50th", "75th", "100th") 
#plot of comps quartiles with average CC top box score for each
#set up unique elements
DT <- copy(pagg)
maintitle <- "Comps % by CC Top Box Quartile"
ylabel <- "Comps"
xlabel <- "Quartile"
xvar <- DT[,ccquartile]
yvar <- DT[,compsavg]
yvarcount <- DT[,Q2_2_TB_SCORE]
pdata <- DT
#plot
ggplot(data = pdata, aes(x = xvar, y = yvar*100, fill = "#ADD8E6")) +
  geom_bar(stat="identity", width = 0.7) + theme_bw() + 
  ggtitle(maintitle) + guides(fill=FALSE) +
  geom_text(size = 5, aes(label=paste0("Comps = ",round(yvar,3)*100,"%"),y=0), stat= "identity", vjust = -1.75) +
  geom_text(size = 5, aes(label=paste0("CC = ",round(yvarcount,3)*100,"%"),y=0), stat= "identity", vjust = -.5) +
    theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
        plot.title = element_text(size = 10, face = "bold")) + 
  labs(x = xlabel, y = ylabel) 

#export data
paggx <- pagg[, .(ccquartile,compsavg,Q2_2_TB_SCORE,cc_q_value)]
setnames(paggx,"cc_q_value","ccquartile_cutoff_value")
#write file
write.xlsx(paggx,file="O:/CoOp/CoOp194_PROReportng&OM/Julie/comps_by_cc_quartile_Canada.xlsx")

##slide #18 - CC and SO by day of week
cc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc_so_bydayofweek_canada.csv")
cc[, SO_Total := rowSums(.SD, na.rm = TRUE), 
   .SDcols = c("Q2_1_RESPONSE_TOTAL", "Q2_3_RESPONSE_TOTAL", "Q2_4_RESPONSE_TOTAL",
               "Q2_5_RESPONSE_TOTAL", "Q2_6_RESPONSE_TOTAL", "Q2_7_RESPONSE_TOTAL")]
cc[, SO_TB_CNT := rowSums(.SD, na.rm = TRUE), 
   .SDcols = c("Q2_1_TB_CNT", "Q2_3_TB_CNT", "Q2_4_TB_CNT",
               "Q2_5_TB_CNT", "Q2_6_TB_CNT", "Q2_7_TB_CNT")]
cc[, SO_TB_SCORE := SO_TB_CNT/SO_Total]
cc <- cc[, .(DAY_ABBR_NM,Q2_2_TB_SCORE,SO_TB_SCORE)]
#write file
write.xlsx(cc,file="O:/CoOp/CoOp194_PROReportng&OM/Julie/cc-so_bydayofweek_Canada.xlsx")


##slide #20 - CC and SO by hour
cc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc-so_byhour_Canada.csv")
#change CC and SO columns to numeric
cols <- c("Q2_1_TB_SCORE","CC_TB_SCORE","Q2_3_TB_SCORE","Q2_4_TB_SCORE","Q2_5_TB_SCORE","Q2_6_TB_SCORE","Q2_7_TB_SCORE")
cc[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]
#compute SO score
cc[, SO_TB_SCORE := rowMeans(.SD, na.rm = T), .SDcols=c("Q2_1_TB_SCORE","Q2_3_TB_SCORE","Q2_4_TB_SCORE",
                                                 "Q2_5_TB_SCORE","Q2_6_TB_SCORE","Q2_7_TB_SCORE")]

cc <- cc[, .(TRANS_HR,CC_TB_SCORE,SO_TB_SCORE)]
cc <- setorder(cc, TRANS_HR)
#write file
write.xlsx(cc,file="O:/CoOp/CoOp194_PROReportng&OM/Julie/cc-so_byhour_Canada.xlsx")


##slide #5 - CC, SO, Q1, and WP by store performance
cc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc-so_bystoreperformance_Canada_FY18Q2.csv")
cc1 <- copy(cc)
#remove store number variable for topline
cc1[, STORE_NUM := NULL]
#sum - to get one row for the quarter
cc1 <- cc1[, lapply(.SD, sum, na.rm=TRUE)]
#will return
cc1[, RETURN_TB_SCORE := Q1_TB_CNT/Q1_RESPONSE_TOTAL]
#customer connection
cc1[, CC_TB_SCORE := Q2_2_TB_CNT/Q2_2_RESPONSE_TOTAL]
#worth perceptions
cc1[, WP_TB_SCORE := Q2_8_TB_CNT/Q2_8_RESPONSE_TOTAL]
#SO sub categories
cc1[, Q2_1_TB_SCORE := Q2_1_TB_CNT/Q2_1_RESPONSE_TOTAL]
cc1[, Q2_3_TB_SCORE := Q2_3_TB_CNT/Q2_3_RESPONSE_TOTAL]
cc1[, Q2_4_TB_SCORE := Q2_4_TB_CNT/Q2_4_RESPONSE_TOTAL]
cc1[, Q2_5_TB_SCORE := Q2_5_TB_CNT/Q2_5_RESPONSE_TOTAL]
cc1[, Q2_6_TB_SCORE := Q2_6_TB_CNT/Q2_6_RESPONSE_TOTAL]
cc1[, Q2_7_TB_SCORE := Q2_7_TB_CNT/Q2_7_RESPONSE_TOTAL]
#average the SO scores
cc1[, SO_TB_SCORE := rowMeans(.SD, na.rm = TRUE),
    .SDcols = c("Q2_1_TB_SCORE", "Q2_3_TB_SCORE", "Q2_4_TB_SCORE",
                "Q2_5_TB_SCORE", "Q2_6_TB_SCORE", "Q2_7_TB_SCORE")]
#keep only variables we need
cc1 <- cc1[, (grep("TB_SCORE",colnames(cc1),value=T)), with=FALSE]
cc1[, Q1_TB_SCORE := NULL]
cc1 <- cc1[, lapply(.SD,function(x) round(x,2)*100)]

#quantile stores
cc2 <- copy(cc)
#will return
cc2[, RETURN_TB_SCORE := Q1_TB_CNT/Q1_RESPONSE_TOTAL]
#customer connection
cc2[, CC_TB_SCORE := Q2_2_TB_CNT/Q2_2_RESPONSE_TOTAL]
#worth perceptions
cc2[, WP_TB_SCORE := Q2_8_TB_CNT/Q2_8_RESPONSE_TOTAL]
#SO sub categories
cc2[, Q2_1_TB_SCORE := Q2_1_TB_CNT/Q2_1_RESPONSE_TOTAL]
cc2[, Q2_3_TB_SCORE := Q2_3_TB_CNT/Q2_3_RESPONSE_TOTAL]
cc2[, Q2_4_TB_SCORE := Q2_4_TB_CNT/Q2_4_RESPONSE_TOTAL]
cc2[, Q2_5_TB_SCORE := Q2_5_TB_CNT/Q2_5_RESPONSE_TOTAL]
cc2[, Q2_6_TB_SCORE := Q2_6_TB_CNT/Q2_6_RESPONSE_TOTAL]
cc2[, Q2_7_TB_SCORE := Q2_7_TB_CNT/Q2_7_RESPONSE_TOTAL]
#average the SO scores
cc2[, SO_TB_SCORE := rowMeans(.SD, na.rm = TRUE),
    .SDcols = c("Q2_1_TB_SCORE", "Q2_3_TB_SCORE", "Q2_4_TB_SCORE",
                "Q2_5_TB_SCORE", "Q2_6_TB_SCORE", "Q2_7_TB_SCORE")]
#
ccquant <- cc2[, list(RETURN_10 = quantile(RETURN_TB_SCORE,.1,na.rm=T),
                      RETURN_90 = quantile(RETURN_TB_SCORE,.9,na.rm=T),
                    CC_10 = quantile(CC_TB_SCORE,.1,na.rm=T),
                    CC_90 = quantile(CC_TB_SCORE,.9,na.rm=T),
                    SO_10 = quantile(SO_TB_SCORE,.1,na.rm=T),
                    SO_90 = quantile(SO_TB_SCORE,.9,na.rm=T),
                    WP_10 = quantile(WP_TB_SCORE,.1,na.rm=T),
                    WP_90 = quantile(WP_TB_SCORE,.9,na.rm=T),
                    Q2_1_10 = quantile(Q2_1_TB_SCORE,.1,na.rm=T),
                    Q2_1_90 = quantile(Q2_1_TB_SCORE,.9,na.rm=T),
                    Q2_3_10 = quantile(Q2_3_TB_SCORE,.1,na.rm=T),
                    Q2_3_90 = quantile(Q2_3_TB_SCORE,.9,na.rm=T),
                    Q2_4_10 = quantile(Q2_4_TB_SCORE,.1,na.rm=T),
                    Q2_4_90 = quantile(Q2_4_TB_SCORE,.9,na.rm=T),
                    Q2_5_10 = quantile(Q2_5_TB_SCORE,.1,na.rm=T),
                    Q2_5_90 = quantile(Q2_5_TB_SCORE,.9,na.rm=T),
                    Q2_6_10 = quantile(Q2_6_TB_SCORE,.1,na.rm=T),
                    Q2_6_90 = quantile(Q2_6_TB_SCORE,.9,na.rm=T),
                    Q2_7_10 = quantile(Q2_7_TB_SCORE,.1,na.rm=T),
                    Q2_7_90 = quantile(Q2_7_TB_SCORE,.9,na.rm=T))]
ccquant <- ccquant[, lapply(.SD,function(x) round(x,2)*100)]


# ccavg <- cc2[, list(RETURN_TB_SCORE = sum(Q1_TDTMRW_CNT)/sum(Q1_RESPONSE_TOTAL),
#                    CC_TB_SCORE = sum(Q2_2_TB_CNT)/sum(Q2_2_RESPONSE_TOTAL),
#                    SO_TB_SCORE = sum(SO_TB_CNT)/sum(SO_Total),
#                    WP_TB_SCORE = sum(Q2_8_TB_CNT)/sum(Q2_8_RESPONSE_TOTAL),
#                    Q2_1_TB_SCORE = sum(Q2_1_TB_CNT)/sum(Q2_1_RESPONSE_TOTAL),
#                    Q2_3_TB_SCORE = sum(Q2_3_TB_CNT)/sum(Q2_3_RESPONSE_TOTAL),
#                    Q2_4_TB_SCORE = sum(Q2_4_TB_CNT)/sum(Q2_4_RESPONSE_TOTAL),
#                    Q2_5_TB_SCORE = sum(Q2_5_TB_CNT)/sum(Q2_5_RESPONSE_TOTAL),
#                    Q2_6_TB_SCORE = sum(Q2_6_TB_CNT)/sum(Q2_6_RESPONSE_TOTAL),
#                    Q2_7_TB_SCORE = sum(Q2_7_TB_CNT)/sum(Q2_7_RESPONSE_TOTAL))]

ccprint <- cbind(cc1,ccquant)
ccprint <- ccprint[, lapply(.SD, function(x) round(x*100,0))]
#write file
write.xlsx(ccprint,file="O:/CoOp/CoOp194_PROReportng&OM/Julie/cc-so_bystoreperformance_Canada.xlsx")


##slide 8 - CC and SO by month

#fiscal month
cc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc-so_byfiscalmonth_Canada.csv")
#data are in long format (CAW...), so, tidy!
#create new variable for CC vs. SO questions
cc[, TB_Score := TOTAL_TB/TOTAL_RSPNS]
cc[QSTN_ID=='Q2_2', varname := 'CC']
cc[QSTN_ID=='Q2_1'|QSTN_ID=='Q2_3'|QSTN_ID=='Q2_4'|QSTN_ID=='Q2_5'|QSTN_ID=='Q2_6'|QSTN_ID=='Q2_7', varname := 'SO']
cc <- cc[, lapply(.SD,mean,na.rm=T), .SDcols=c("TB_Score"), by=c("FSCL_YR_NUM","FSCL_PER_IN_YR_NUM","varname")]
#cast wide
cc <- dcast.data.table(cc, FSCL_YR_NUM + FSCL_PER_IN_YR_NUM ~ varname, value.var="TB_Score")
cc <- setorder(cc,FSCL_YR_NUM,FSCL_PER_IN_YR_NUM)
cc <- cc[, lapply(.SD, function(x) round(x,3)), .SDcols=c("CC","SO"), by=c("FSCL_YR_NUM","FSCL_PER_IN_YR_NUM")]
#write to .csv
write.xlsx(cc,file="O:/CoOp/CoOp194_PROReportng&OM/Julie/cc-so_byfiscalmonth_Canada.xlsx")


##slide #28 -- CC and SO by store volume

#load data
#part 1
p1 <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc-so_by_store_Canada_volume_type_pt1.csv")
#part 2
p2 <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc-so_by_store_Canada_volume_type_pt2.csv")
#rename merge id columns to match
setnames(p1,"STORE_NUM","STORE_NUMBER")
#change store number and TB scores to numeric from characters
p1[, STORE_NUMBER := lapply(.SD, as.numeric), .SDcols = "STORE_NUMBER"]
#[, (grep("TB_SCORE",names(p1),value=T)) := lapply(.SD, as.numeric), .SDcols=grep("TB_SCORE",names(p1),value=T)]
#merge by store number
pfull <- Reduce(function(x,y) {merge(x,y,by=c("STORE_NUMBER"),all.x=TRUE)}, list(p1,p2))
#drop stores with no active days
pfull <- na.omit(pfull, cols="COSD")
#split by volume
prob = c(1/4, 2/4, 3/4, 1)
temp <- pfull %>% summarise( 
  cosd25 = quantile(COSD, probs = prob[1], na.rm = T), 
  cosd50 = quantile(COSD, probs = prob[2], na.rm = T),
  cosd75 = quantile(COSD, probs = prob[3], na.rm = T),
  cosd100 = quantile(COSD, probs = prob[4], na.rm = T)
)
pfull <- cbind(pfull, temp)
#recode based on quartiles
pfull[COSD <= cosd25, cosdquartile := 1]
pfull[COSD > cosd25 & COSD <= cosd50, cosdquartile := 2]
pfull[COSD > cosd50 & COSD <= cosd75, cosdquartile := 3]
pfull[COSD > cosd75, cosdquartile := 4]
#sum by cosdquartile
pfull <- pfull[, lapply(.SD,sum,na.rm=T), by="cosdquartile"]
pfull[, CC_TB_SCORE := Q2_2_TB_CNT/Q2_2_RESPONSE_TOTAL]
pfull[, Q2_1_TB_SCORE := Q2_1_TB_CNT/Q2_1_RESPONSE_TOTAL]
pfull[, Q2_3_TB_SCORE := Q2_3_TB_CNT/Q2_3_RESPONSE_TOTAL]
pfull[, Q2_4_TB_SCORE := Q2_4_TB_CNT/Q2_4_RESPONSE_TOTAL]
pfull[, Q2_5_TB_SCORE := Q2_5_TB_CNT/Q2_5_RESPONSE_TOTAL]
pfull[, Q2_6_TB_SCORE := Q2_6_TB_CNT/Q2_6_RESPONSE_TOTAL]
pfull[, Q2_7_TB_SCORE := Q2_7_TB_CNT/Q2_7_RESPONSE_TOTAL]
pfull[, SO_TB_SCORE := rowMeans(.SD, na.rm = TRUE),
    .SDcols = c("Q2_1_TB_SCORE", "Q2_3_TB_SCORE", "Q2_4_TB_SCORE",
                "Q2_5_TB_SCORE", "Q2_6_TB_SCORE", "Q2_7_TB_SCORE")]
cols <- c("cosdquartile","CC_TB_SCORE","SO_TB_SCORE")
pfull <- pfull[, cols, with=FALSE]
pfull <- setorder(pfull,cosdquartile)
#write to .csv
write.xlsx(pfull,file="O:/CoOp/CoOp194_PROReportng&OM/Julie/cc-so_by_store_Canada_volume.xlsx")


#slide #31 - CE by channel
ce <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/ce_by_channel_Canada.csv")
ce <- ce[, list(TB_COUNT = sum(TB_COUNT,na.rm=T),
                RSPNS_COUNT = sum(RSPNS_COUNT,na.rm=T)), by=c("QSTN_ID","ORD_MTHD_CD")]
ce[, TB_SCORE := TB_COUNT/RSPNS_COUNT]
#get rid of variables no longer needed
ce[, c("TB_COUNT","RSPNS_COUNT") := NULL]
#calculate SO total
sosub <- ce[QSTN_ID!="Q2_2"]
sosub <- sosub[, list(TB_SCORE = mean(TB_SCORE)), by="ORD_MTHD_CD"]
#create a variable for merge
sosub[, QSTN_ID := "StoreOps"]
#rbind together
l = list(ce,sosub)
ce <- rbindlist(l, use.names=T, fill=F)
ce <- setorder(ce,QSTN_ID,ORD_MTHD_CD)
#write to .csv
write.xlsx(ce,file="O:/CoOp/CoOp194_PROReportng&OM/Julie/ce_by_channel_Canada.xlsx")

#slide #32 - CE by company ownership
ce <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/ce_by_ownership_Canada.csv")
ce[, TB_SCORE := TOTAL_TB/TOTAL_RSPNS]
ce[, c("FSCL_YR_NUM","FSCL_QTR_IN_YR_NUM") := NULL]
ce <- setorder(ce, QSTN_ID,OWNR_TYPE_CD)
#write to .csv
write.xlsx(ce,file="O:/CoOp/CoOp194_PROReportng&OM/Julie/ce_by_ownership_Canada.xlsx")

#slide #15 - CC & SO by % store home store
#quantile stores by % home store
cc1 <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc-so_by_homestore_Canada_pt1.csv")
cc1 <- cc1[, list(HS_CUST_COUNT = sum(HS_CUST_COUNT,na.rm=T),
                  ALL_CUST_COUNT = sum(ALL_CUST_COUNT,na.rm=T)), by="STORE_NUM"]
#cc & so by store
cc2 <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc-so_by_store_Canada_volume_type_pt1.csv")
cc2[, STORE_NUM := as.integer(STORE_NUM)]
#merge by store number
pfull <- Reduce(function(x,y) {merge(x,y,by=c("STORE_NUM"),all.y=TRUE)}, list(cc1,cc2))
#calulate home store %
pfull[, homestore_pct := HS_CUST_COUNT/ALL_CUST_COUNT]
#split by home store %
prob = c(1/4, 2/4, 3/4, 1)
temp <- pfull %>% summarise( 
  hs25 = quantile(homestore_pct, probs = prob[1], na.rm = T), 
  hs50 = quantile(homestore_pct, probs = prob[2], na.rm = T),
  hs75 = quantile(homestore_pct, probs = prob[3], na.rm = T),
  hs100 = quantile(homestore_pct, probs = prob[4], na.rm = T)
)
pfull <- cbind(pfull, temp)
#recode based on quartiles
pfull[homestore_pct <= hs25, hsquartile := 1]
pfull[homestore_pct > hs25 & homestore_pct <= hs50, hsquartile := 2]
pfull[homestore_pct > hs50 & homestore_pct <= hs75, hsquartile := 3]
pfull[homestore_pct > hs75, hsquartile := 4]
#sum by hsquartile
pfull <- pfull[, lapply(.SD,sum,na.rm=T), by="hsquartile"]
pfull[, CC_TB_SCORE := Q2_2_TB_CNT/Q2_2_RESPONSE_TOTAL]
pfull[, Q2_1_TB_SCORE := Q2_1_TB_CNT/Q2_1_RESPONSE_TOTAL]
pfull[, Q2_3_TB_SCORE := Q2_3_TB_CNT/Q2_3_RESPONSE_TOTAL]
pfull[, Q2_4_TB_SCORE := Q2_4_TB_CNT/Q2_4_RESPONSE_TOTAL]
pfull[, Q2_5_TB_SCORE := Q2_5_TB_CNT/Q2_5_RESPONSE_TOTAL]
pfull[, Q2_6_TB_SCORE := Q2_6_TB_CNT/Q2_6_RESPONSE_TOTAL]
pfull[, Q2_7_TB_SCORE := Q2_7_TB_CNT/Q2_7_RESPONSE_TOTAL]
pfull[, SO_TB_SCORE := rowMeans(.SD, na.rm = TRUE),
      .SDcols = c("Q2_1_TB_SCORE", "Q2_3_TB_SCORE", "Q2_4_TB_SCORE",
                  "Q2_5_TB_SCORE", "Q2_6_TB_SCORE", "Q2_7_TB_SCORE")]
cols <- c("hsquartile","CC_TB_SCORE","SO_TB_SCORE")
pfull <- pfull[, cols, with=FALSE]
pfull <- setorder(pfull,hsquartile)
#write to .csv
write.xlsx(pfull,file="O:/CoOp/CoOp194_PROReportng&OM/Julie/cc-so_by_homestore_Canada.xlsx")


## Slide #13 
pt <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/partnertenure_by_store_Canada.csv")
setnames(pt,c("STORE_NUM","avg_tenure"))
cc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc-so_bystoreperformance_Canada.csv")
cc <- cc[, lapply(.SD,sum,na.rm=T),by="STORE_NUM"]
cc[, STORE_NUM := as.numeric(STORE_NUM)]
pfull <- merge(pt,cc,by="STORE_NUM")
#quartile partner tenure
prob = c(1/4, 2/4, 3/4, 1)
temp <- pfull %>% summarise( 
  avg_tenure25 = quantile(avg_tenure, probs = prob[1], na.rm = T), 
  avg_tenure50 = quantile(avg_tenure, probs = prob[2], na.rm = T),
  avg_tenure75 = quantile(avg_tenure, probs = prob[3], na.rm = T),
  avg_tenure100 = quantile(avg_tenure, probs = prob[4], na.rm = T)
)
pfull <- cbind(pfull, temp)
#recode based on quartiles
pfull[avg_tenure <= avg_tenure25, avg_tenurequartile := 1]
pfull[avg_tenure > avg_tenure25 & avg_tenure <= avg_tenure50, avg_tenurequartile := 2]
pfull[avg_tenure > avg_tenure50 & avg_tenure <= avg_tenure75, avg_tenurequartile := 3]
pfull[avg_tenure > avg_tenure75, avg_tenurequartile := 4]
#sum by avg_tenurequartile
pfull <- pfull[, lapply(.SD,sum,na.rm=T), by="avg_tenurequartile"]
pfull[, CC_TB_SCORE := Q2_2_TB_CNT/Q2_2_RESPONSE_TOTAL]
#SO sub categories
pfull[, Q2_1_TB_SCORE := Q2_1_TB_CNT/Q2_1_RESPONSE_TOTAL]
pfull[, Q2_3_TB_SCORE := Q2_3_TB_CNT/Q2_3_RESPONSE_TOTAL]
pfull[, Q2_4_TB_SCORE := Q2_4_TB_CNT/Q2_4_RESPONSE_TOTAL]
pfull[, Q2_5_TB_SCORE := Q2_5_TB_CNT/Q2_5_RESPONSE_TOTAL]
pfull[, Q2_6_TB_SCORE := Q2_6_TB_CNT/Q2_6_RESPONSE_TOTAL]
pfull[, Q2_7_TB_SCORE := Q2_7_TB_CNT/Q2_7_RESPONSE_TOTAL]
#average the SO scores
pfull[, SO_TB_SCORE := rowMeans(.SD, na.rm = TRUE),
    .SDcols = c("Q2_1_TB_SCORE", "Q2_3_TB_SCORE", "Q2_4_TB_SCORE",
                "Q2_5_TB_SCORE", "Q2_6_TB_SCORE", "Q2_7_TB_SCORE")]
pfull <- pfull[, c("avg_tenurequartile","SO_TB_SCORE","CC_TB_SCORE"), with=FALSE]
#write to .csv
write.xlsx(pfull,file="O:/CoOp/CoOp194_PROReportng&OM/Julie/partnertenure_by_store_Canada.xlsx")


##recreating slide # 13 ##
#regression (outcome = CC) based on % home store, average partner tenure, and COSD#
#needs: store-level CC, % home store, partner tenure, COSD for FY 17 Q4#


#cc & cosd
#CC
p1 <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc-so_by_store_Canada_volume_type_pt1.csv")
#COSD
p2 <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc-so_by_store_Canada_volume_type_pt2.csv")
#rename merge id columns to match
setnames(p2,"STORE_NUMBER","STORE_NUM")
#change store number and TB scores to numeric from characters
p1[, STORE_NUM := lapply(.SD, as.numeric), .SDcols = "STORE_NUM"]
#% home store
cc1 <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc-so_by_homestore_Canada_pt1.csv")
cc1 <- cc1[, list(HS_CUST_COUNT = sum(HS_CUST_COUNT,na.rm=T),
                  ALL_CUST_COUNT = sum(ALL_CUST_COUNT,na.rm=T)), by="STORE_NUM"]
#partner tenure
pt <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/partnertenure_by_store_Canada.csv")
setnames(pt,c("STORE_NUM","avg_tenure"))
#merge by store number
pfull <- Reduce(function(x,y) {merge(x,y,by=c("STORE_NUM"),all.x=TRUE)}, list(p1,p2,cc1,pt))

#calculate CC tb score
pfull[, cc_tb_score := Q2_2_TB_CNT/Q2_2_RESPONSE_TOTAL]
#calulate home store %
pfull[, homestore_pct := HS_CUST_COUNT/ALL_CUST_COUNT]
#reduce number of variables
pfull <- pfull[, c("STORE_NUM","cc_tb_score","homestore_pct","COSD","avg_tenure","Q2_2_TB_CNT","Q2_2_RESPONSE_TOTAL"), with=F]
#regression model
lm1 <- lm(cc_tb_score ~ homestore_pct + COSD + avg_tenure, data=pfull)
# summary(lm1)
# summary(lm(cc_tb_score ~ homestore_pct, data=pfull))
# summary(lm(cc_tb_score ~ COSD, data=pfull))
# summary(lm(cc_tb_score ~ avg_tenure, data=pfull))
#relative weights analysis -- library(flipRegression)
Regression(cc_tb_score ~ homestore_pct + COSD + avg_tenure, data=pfull,
           output = "Relative Importance Analysis")

summary(lm(cc_tb_score ~ homestore_pct*COSD + avg_tenure, data=pfull))
summary(lm(cc_tb_score ~ homestore_pct + COSD*avg_tenure, data=pfull))
summary(lm(cc_tb_score ~ homestore_pct*avg_tenure + COSD, data=pfull))
summary(lm(cc_tb_score ~ homestore_pct*COSD*avg_tenure, data=pfull))

pfull[, h_a_int := homestore_pct*avg_tenure]
pfull[, h_a_c_int := homestore_pct*avg_tenure*COSD]
Regression(cc_tb_score ~ homestore_pct + avg_tenure + COSD + h_a_int, data=pfull,
           output = "Relative Importance Analysis")
Regression(cc_tb_score ~ homestore_pct + avg_tenure + COSD + h_a_c_int, data=pfull,
           output = "Relative Importance Analysis")


#quartile everything
pq <- copy(pfull)
#quartile partner tenure
prob = c(1/4, 2/4, 3/4, 1)
temp <- pq %>% summarise( 
  avg_tenure25 = quantile(avg_tenure, probs = prob[1], na.rm = T), 
  avg_tenure50 = quantile(avg_tenure, probs = prob[2], na.rm = T),
  avg_tenure75 = quantile(avg_tenure, probs = prob[3], na.rm = T),
  avg_tenure100 = quantile(avg_tenure, probs = prob[4], na.rm = T)
)
pq <- cbind(pq, temp)
#recode based on quartiles
pq[avg_tenure <= avg_tenure25, avg_tenurequartile := 1]
pq[avg_tenure > avg_tenure25 & avg_tenure <= avg_tenure50, avg_tenurequartile := 2]
pq[avg_tenure > avg_tenure50 & avg_tenure <= avg_tenure75, avg_tenurequartile := 3]
pq[avg_tenure > avg_tenure75, avg_tenurequartile := 4]
#split by home store %
prob = c(1/4, 2/4, 3/4, 1)
temp <- pq %>% summarise( 
  hs25 = quantile(homestore_pct, probs = prob[1], na.rm = T), 
  hs50 = quantile(homestore_pct, probs = prob[2], na.rm = T),
  hs75 = quantile(homestore_pct, probs = prob[3], na.rm = T),
  hs100 = quantile(homestore_pct, probs = prob[4], na.rm = T)
)
pq <- cbind(pq, temp)
#recode based on quartiles
pq[homestore_pct <= hs25, hsquartile := 1]
pq[homestore_pct > hs25 & homestore_pct <= hs50, hsquartile := 2]
pq[homestore_pct > hs50 & homestore_pct <= hs75, hsquartile := 3]
pq[homestore_pct > hs75, hsquartile := 4]
#split by volume
prob = c(1/4, 2/4, 3/4, 1)
temp <- pq %>% summarise( 
  cosd25 = quantile(COSD, probs = prob[1], na.rm = T), 
  cosd50 = quantile(COSD, probs = prob[2], na.rm = T),
  cosd75 = quantile(COSD, probs = prob[3], na.rm = T),
  cosd100 = quantile(COSD, probs = prob[4], na.rm = T)
)
pq <- cbind(pq, temp)
#recode based on quartiles
pq[COSD <= cosd25, cosdquartile := 4]
pq[COSD > cosd25 & COSD <= cosd50, cosdquartile := 3]
pq[COSD > cosd50 & COSD <= cosd75, cosdquartile := 2]
pq[COSD > cosd75, cosdquartile := 1]
#sum by quartiles
pq <- pq[, lapply(.SD,sum,na.rm=T), by=c("avg_tenurequartile","hsquartile","cosdquartile")]
pq[, cc_tb_score := Q2_2_TB_CNT/Q2_2_RESPONSE_TOTAL]
#reduce number of variables
pq <- pq[, c("STORE_NUM","cc_tb_score","homestore_pct","COSD","avg_tenure","Q2_2_TB_CNT","Q2_2_RESPONSE_TOTAL",
             "avg_tenurequartile","hsquartile","cosdquartile"), with=F]
#make factorered group indicator
pq[, group := as.factor(paste(avg_tenurequartile,hsquartile,cosdquartile,sep="-"))]#sum by group
pq <- pq[, lapply(.SD,sum,na.rm=T), by=c("group")]
pq[, cc_tb_score := Q2_2_TB_CNT/Q2_2_RESPONSE_TOTAL]
pq <- setorder(pq,group)
barchart(cc_tb_score~group,data=pq,ylim=c(.15,.45),scales = list(x = list(rot = 90)))

# barchart(cc_tb_score~group,data=pq[hsquartile==1],ylim=c(.15,.45),scales = list(x = list(rot = 90)))
# barchart(cc_tb_score~group,data=pq[hsquartile==2],ylim=c(.15,.45),scales = list(x = list(rot = 90)))
# barchart(cc_tb_score~group,data=pq[hsquartile==3],ylim=c(.15,.45),scales = list(x = list(rot = 90)))
# barchart(cc_tb_score~group,data=pq[hsquartile==4],ylim=c(.15,.45),scales = list(x = list(rot = 90)))

plots <- list()  # new empty list
for (i in 1:4) {
  p1 = barchart(cc_tb_score~group,data=pq[hsquartile==i],ylim=c(.15,.45),scales = list(x = list(rot = 90)))
  plots[[i]] <- p1  # add each plot into plot list
}
layout <- matrix(c(1, 2, 3, 4), nrow = 4, byrow = TRUE)
multiplot(plotlist = plots, layout = layout)


# barchart(cc_tb_score~group,data=pq[avg_tenurequartile==1],ylim=c(.15,.45))
# barchart(cc_tb_score~group,data=pq[avg_tenurequartile==4],ylim=c(.15,.45))
# barchart(cc_tb_score~group,data=pq[hsquartile==4],ylim=c(.15,.45))
# barchart(cc_tb_score~group,data=pq[cosdquartile==4],ylim=c(.15,.45))
