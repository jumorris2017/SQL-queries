##request: CC and SO for licensed stores, Lisa request 12/5/17
##part 0: pull CC and SO data for licensed stores - rolling two months Oct/Nov FY18
##part 1: create expected CC and SO values. (a) get avg CC & SO by segment. (b) get % of DM's portfolio's by segment. 
##part 2: (a) rank DM's by delta from expected. (b) calculate YoY growth. (c) profile top 10 DM's.
##part 3: create heatmaps - Segment on top, Area on the left, average scores in the boxes (color-coded)

#load libraries
library(data.table)
library(ggplot2)
library(xlsx)

#load data
lss <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/licensee_cc_bystore_withsegment.csv")
#drop if segment is missing
lss[, LS_SEGMENT := ifelse(LS_SEGMENT %in% c(""," ","NA"), NA, LS_SEGMENT)]
lss <- na.omit(lss,cols="LS_SEGMENT")
#drop if DM is missing
lss <- lss[DIST_DESCR!="LS TBD"]
#remove closed areas
lss <- lss[!grepl("CLOSED",lss$AREA_DESCR),]
lss <- lss[!grepl("CLOSED",lss$DIST_DESCR),]

#part 1: average CC & SO by segment
#drop variables we don't need & keep only 2018 data
lss0 <- lss[FSCL_YR_NUM==2018, !c("DIST_NUM","STORE_NUM","DIST_DESCR","AREA_DESCR","AREA_NUM","RGN_NUM","RGN_DESCR","LS_PARENT"), with=FALSE]

#aggregate
lss0 <- lss0[, lapply(.SD,sum,na.rm=T), by=c("LS_SEGMENT")]

#CC
lss0[, CC_TB_SCORE := Q2_2_TB_CNT/Q2_2_RESPONSE_TOTAL]
#SO sub categories
lss0[, Q2_1_TB_SCORE := Q2_1_TB_CNT/Q2_1_RESPONSE_TOTAL]
lss0[, Q2_3_TB_SCORE := Q2_3_TB_CNT/Q2_3_RESPONSE_TOTAL]
lss0[, Q2_4_TB_SCORE := Q2_4_TB_CNT/Q2_4_RESPONSE_TOTAL]
lss0[, Q2_5_TB_SCORE := Q2_5_TB_CNT/Q2_5_RESPONSE_TOTAL]
lss0[, Q2_6_TB_SCORE := Q2_6_TB_CNT/Q2_6_RESPONSE_TOTAL]
lss0[, Q2_7_TB_SCORE := Q2_7_TB_CNT/Q2_7_RESPONSE_TOTAL]
#average the SO scores
lss0[, SO_TB_SCORE := rowMeans(.SD, na.rm = TRUE),
     .SDcols = c("Q2_1_TB_SCORE", "Q2_3_TB_SCORE", "Q2_4_TB_SCORE",
                 "Q2_5_TB_SCORE", "Q2_6_TB_SCORE", "Q2_7_TB_SCORE")]

#keep only variables we need
lss0 <- lss0[, c("LS_SEGMENT","CC_TB_SCORE","SO_TB_SCORE"), with=FALSE]
setnames(lss0,c("CC_TB_SCORE","SO_TB_SCORE"),c("cc_exp","so_exp"))
lss0 <- lss0[, lapply(.SD, function(x) round(x,2)), .SDcols=c("cc_exp","so_exp"), by="LS_SEGMENT"]

#write to .xlsx
write.xlsx(lss0, file="O:/CoOp/CoOp194_PROReportng&OM/Julie/licensee_CC-SO_avgbysegment.xlsx")
# 
# #part 1: average CC & SO by segment
# #drop variables we don't need & keep only 2018 data
# lss0x <- lss[FSCL_YR_NUM==2018, !c("DIST_NUM","STORE_NUM","DIST_DESCR","AREA_NUM","RGN_NUM","RGN_DESCR","LS_SEGMENT"), with=FALSE]
# 
# #aggregate
# lss0x <- lss0x[, lapply(.SD,sum,na.rm=T), by=c("LS_PARENT","AREA_DESCR")]
# 
# #CC
# lss0x[, CC_TB_SCORE := Q2_2_TB_CNT/Q2_2_RESPONSE_TOTAL]
# #SO sub categories
# lss0x[, Q2_1_TB_SCORE := Q2_1_TB_CNT/Q2_1_RESPONSE_TOTAL]
# lss0x[, Q2_3_TB_SCORE := Q2_3_TB_CNT/Q2_3_RESPONSE_TOTAL]
# lss0x[, Q2_4_TB_SCORE := Q2_4_TB_CNT/Q2_4_RESPONSE_TOTAL]
# lss0x[, Q2_5_TB_SCORE := Q2_5_TB_CNT/Q2_5_RESPONSE_TOTAL]
# lss0x[, Q2_6_TB_SCORE := Q2_6_TB_CNT/Q2_6_RESPONSE_TOTAL]
# lss0x[, Q2_7_TB_SCORE := Q2_7_TB_CNT/Q2_7_RESPONSE_TOTAL]
# #average the SO scores
# lss0x[, SO_TB_SCORE := rowMeans(.SD, na.rm = TRUE),
#      .SDcols = c("Q2_1_TB_SCORE", "Q2_3_TB_SCORE", "Q2_4_TB_SCORE",
#                  "Q2_5_TB_SCORE", "Q2_6_TB_SCORE", "Q2_7_TB_SCORE")]
# 
# #keep only variables we LS_PARENT
# lss0x <- lss0x[, c("LS_PARENT","AREA_DESCR","CC_TB_SCORE","SO_TB_SCORE"), with=FALSE]
# setnames(lss0x,c("CC_TB_SCORE","SO_TB_SCORE"),c("cc_avg","so_avg"))
# lss0x <- lss0x[, lapply(.SD, function(x) round(x,2)), .SDcols=c("cc_avg","so_avg"), by=c("LS_PARENT","AREA_DESCR")]
# 
# #write to .xlsx
# write.xlsx(lss0x, file="O:/CoOp/CoOp194_PROReportng&OM/Julie/licensee_CC-SO_avgbyareaandparent.xlsx")


#### use lss0 as the table for expected ####

## PROFILE DM'S portfolio
#keep only variables we need
lss4 <- lss[, c("STORE_NUM","DIST_DESCR","LS_SEGMENT"), with=FALSE]
#count number of stores in each segment per DM
lss4 <- lss4 %>%
  group_by(DIST_DESCR, LS_SEGMENT) %>%
  summarise(total = n())
setDT(lss4)
#swing wide
lss4 <- dcast.data.table(lss4, DIST_DESCR ~ LS_SEGMENT, value.var="total")
setnames(lss4,"OTHER RETAILERS","OTH_RETAILERS")
lss4[is.na(lss4)] <- 0

#aggregate
lss4[, totalstores := rowSums(.SD,na.rm=T), .SDcols=names(lss4[,2:7])]
lss4[, pfo_gr := round(GROCERY/totalstores,2)];lss4[, pfo_lo := round(LODGING/totalstores,2)]
lss4[, pfo_on := round(ONSITE/totalstores,2)];lss4[, pfo_or := round(OTH_RETAILERS/totalstores,2)]
lss4[, pfo_rec := round(RECREATION/totalstores,2)];lss4[, pfo_tr := round(TRAVEL/totalstores,2)]

#keep only variables we need
lss4 <- lss4[, c("DIST_DESCR","totalstores",grep("pfo_",names(lss4),value=T)),with=F]

#calculate each DM's expected CC and SO values
ccexpdt <- as.data.table(t(lss0))
ccexpdt <- ccexpdt[2,]
setnames(ccexpdt,c("ccexp_lo","ccexp_gr","ccexp_on","ccexp_tr","ccexp_or","ccexp_rec"))
ccexpdt <- ccexpdt[, lapply(.SD, function(x) as.numeric(x))]
soexpdt <- as.data.table(t(lss0))
soexpdt <- soexpdt[3,]
setnames(soexpdt,c("soexp_lo","soexp_gr","soexp_on","soexp_tr","soexp_or","soexp_rec"))
soexpdt <- soexpdt[, lapply(.SD, function(x) as.numeric(x))]

#merge in CC & SO expected values to DMs
lss4 <- cbind(lss4,ccexpdt,soexpdt)

#calculate each DM's expected CC & SO scores
lss4[, cc_exp := (pfo_gr*ccexp_gr) + (pfo_lo*ccexp_lo) + (pfo_on*ccexp_on) + (pfo_or*ccexp_or) + 
       (pfo_rec*ccexp_rec) + (pfo_tr*ccexp_tr)]
lss4[, so_exp := (pfo_gr*soexp_gr) + (pfo_lo*soexp_lo) + (pfo_on*soexp_on) + (pfo_or*soexp_or) + 
       (pfo_rec*soexp_rec) + (pfo_tr*soexp_tr)]

#keep only variables we need
lss4 <- lss4[, c("DIST_DESCR","totalstores",grep("pfo_",names(lss4),value=T),"cc_exp","so_exp"),with=F]

## RANK DM's BY CC & SO ##

#drop variables we don't need
lss1 <- lss[, !c("DIST_NUM","LS_SEGMENT","STORE_NUM","AREA_DESCR","AREA_NUM","RGN_NUM","RGN_DESCR","LS_PARENT"), with=FALSE]

#aggregate
lss1 <- lss1[, lapply(.SD,sum,na.rm=T), by=c("DIST_DESCR","FSCL_YR_NUM")]

#CC
lss1[, CC_TB_SCORE := Q2_2_TB_CNT/Q2_2_RESPONSE_TOTAL]
#SO sub categories
lss1[, Q2_1_TB_SCORE := Q2_1_TB_CNT/Q2_1_RESPONSE_TOTAL]
lss1[, Q2_3_TB_SCORE := Q2_3_TB_CNT/Q2_3_RESPONSE_TOTAL]
lss1[, Q2_4_TB_SCORE := Q2_4_TB_CNT/Q2_4_RESPONSE_TOTAL]
lss1[, Q2_5_TB_SCORE := Q2_5_TB_CNT/Q2_5_RESPONSE_TOTAL]
lss1[, Q2_6_TB_SCORE := Q2_6_TB_CNT/Q2_6_RESPONSE_TOTAL]
lss1[, Q2_7_TB_SCORE := Q2_7_TB_CNT/Q2_7_RESPONSE_TOTAL]
#average the SO scores
lss1[, SO_TB_SCORE := rowMeans(.SD, na.rm = TRUE),
    .SDcols = c("Q2_1_TB_SCORE", "Q2_3_TB_SCORE", "Q2_4_TB_SCORE",
                "Q2_5_TB_SCORE", "Q2_6_TB_SCORE", "Q2_7_TB_SCORE")]

#keep only variables we need
lss1 <- lss1[, c("DIST_DESCR","FSCL_YR_NUM","CC_TB_SCORE","SO_TB_SCORE"), with=FALSE]

#swing years wide
lss1 <- dcast.data.table(lss1, DIST_DESCR ~ FSCL_YR_NUM, value.var=c("CC_TB_SCORE","SO_TB_SCORE"))

#drop rows without both years of data
lss1 <- na.omit(lss1, cols=c("CC_TB_SCORE_2017","SO_TB_SCORE_2017","CC_TB_SCORE_2018","SO_TB_SCORE_2018"))

#calulcate YoY
lss1[, cc_yoy := CC_TB_SCORE_2018/CC_TB_SCORE_2017]
lss1[, so_yoy := SO_TB_SCORE_2018/SO_TB_SCORE_2017]

#assign YoY rank
rank_var <- c(1:nrow(lss1))
#cc
lss1 <- setorder(lss1,-cc_yoy)
lss1 <- cbind(lss1,rank_var)
setnames(lss1,"rank_var","cc_yoy_rank")
#so
lss1 <- setorder(lss1,-so_yoy)
lss1 <- cbind(lss1,rank_var)
setnames(lss1,"rank_var","so_yoy_rank")

#merge in expected CC & SO scores
lss1 <- left_join(lss1,lss4,by="DIST_DESCR")
setDT(lss1)

#calculate delta observed vs expected
lss1[, cc_delta := round(CC_TB_SCORE_2018 - cc_exp,2)]
lss1[, so_delta := round(SO_TB_SCORE_2018 - so_exp,2)]

#assign delta rank
rank_var <- c(1:nrow(lss1))
#cc
lss1 <- setorder(lss1,-cc_delta)
lss1 <- cbind(lss1,rank_var)
setnames(lss1,"rank_var","cc_delta_rank")
#so
lss1 <- setorder(lss1,-so_delta)
lss1 <- cbind(lss1,rank_var)
setnames(lss1,"rank_var","so_delta_rank")

#merge in district number and region description
lssareas <- lss[FSCL_YR_NUM==2018, c("DIST_NUM","DIST_DESCR","RGN_DESCR"), with=FALSE]
lssareas <- subset(lssareas, !duplicated(lssareas))
lss1 <- left_join(lssareas,lss1,by="DIST_DESCR")
setDT(lss1)

#set order to show top CC delta rankers
lss1 <- setorder(lss1,cc_delta_rank)

#write to .xlsx
write.xlsx(lss1, file="O:/CoOp/CoOp194_PROReportng&OM/Julie/licensee_DM_CC-SOdelta_ranked.xlsx")


##AVERAGE CC & SO BY AREAS & SEGMENTS##

#group stores into C&U sub-segment
cuareas <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cu_storelist.csv")
cuvec <- c(unique(cuareas[,STORE_NUM]))
lss2 <- lss[STORE_NUM %in% cuvec, LS_PARENT := "C&U"]
#keep big 4 and C&U
areavec <- c("C&U","Albertsons, LLC","Kroger Co", "TARGET CORPORATION","Host International")
lss2 <- lss2[LS_PARENT %in% areavec]

#keep only variables we need
lss2x <- lss2[, c("STORE_NUM","LS_PARENT","AREA_DESCR","LS_SEGMENT"), with=FALSE]
#count number of stores in each segment per DM
lss2x <- lss2x %>%
  group_by(LS_PARENT, AREA_DESCR, LS_SEGMENT) %>%
  summarise(total = n())
setDT(lss2x)
#swing wide
lss2x <- dcast.data.table(lss2x, LS_PARENT + AREA_DESCR ~ LS_SEGMENT, value.var="total")
setnames(lss2x,"OTHER RETAILERS","OTH_RETAILERS")
lss2x[is.na(lss2x)] <- 0

#aggregate
lss2x[, totalstores := rowSums(.SD,na.rm=T), .SDcols=names(lss2x[,3:6])]
lss2x[, pfo_gr := round(GROCERY/totalstores,2)];
lss2x[, pfo_on := round(ONSITE/totalstores,2)];
lss2x[, pfo_or := round(OTH_RETAILERS/totalstores,2)]
lss2x[, pfo_tr := round(TRAVEL/totalstores,2)]

#keep only variables we need
lss2x <- lss2x[, c("LS_PARENT","AREA_DESCR","totalstores",grep("pfo_",names(lss2x),value=T)),with=F]

#calculate each segment's expected CC and SO values
ccexpdt <- as.data.table(t(lss0))
ccexpdt <- ccexpdt[2,]
setnames(ccexpdt,c("ccexp_lo","ccexp_gr","ccexp_on","ccexp_tr","ccexp_or","ccexp_rec"))
ccexpdt <- ccexpdt[, lapply(.SD, function(x) as.numeric(x))]
soexpdt <- as.data.table(t(lss0))
soexpdt <- soexpdt[3,]
setnames(soexpdt,c("soexp_lo","soexp_gr","soexp_on","soexp_tr","soexp_or","soexp_rec"))
soexpdt <- soexpdt[, lapply(.SD, function(x) as.numeric(x))]

#merge in CC & SO expected values to DMs
lss2x <- cbind(lss2x,ccexpdt,soexpdt)

#calculate each DM's expected CC & SO scores
lss2x[, cc_exp := (pfo_gr*ccexp_gr) + (pfo_on*ccexp_on) + (pfo_or*ccexp_or) + (pfo_tr*ccexp_tr)]
lss2x[, so_exp := (pfo_gr*soexp_gr) + (pfo_on*soexp_on) + (pfo_or*soexp_or) + (pfo_tr*soexp_tr)]

#keep only variables we need
lss2x <- lss2x[, c("LS_PARENT","AREA_DESCR","totalstores",grep("pfo_",names(lss2x),value=T),"cc_exp","so_exp"),with=F]


## RANK areas BY CC & SO ##

#drop variables we don't need
lss3 <- lss2[, !c("DIST_NUM","LS_SEGMENT","STORE_NUM","DIST_DESCR","AREA_NUM","RGN_NUM","RGN_DESCR"), with=FALSE]

#aggregate
lss3 <- lss3[, lapply(.SD,sum,na.rm=T), by=c("LS_PARENT","AREA_DESCR","FSCL_YR_NUM")]

#CC
lss3[, CC_TB_SCORE := Q2_2_TB_CNT/Q2_2_RESPONSE_TOTAL]
#SO sub categories
lss3[, Q2_1_TB_SCORE := Q2_1_TB_CNT/Q2_1_RESPONSE_TOTAL]
lss3[, Q2_3_TB_SCORE := Q2_3_TB_CNT/Q2_3_RESPONSE_TOTAL]
lss3[, Q2_4_TB_SCORE := Q2_4_TB_CNT/Q2_4_RESPONSE_TOTAL]
lss3[, Q2_5_TB_SCORE := Q2_5_TB_CNT/Q2_5_RESPONSE_TOTAL]
lss3[, Q2_6_TB_SCORE := Q2_6_TB_CNT/Q2_6_RESPONSE_TOTAL]
lss3[, Q2_7_TB_SCORE := Q2_7_TB_CNT/Q2_7_RESPONSE_TOTAL]
#average the SO scores
lss3[, SO_TB_SCORE := rowMeans(.SD, na.rm = TRUE),
     .SDcols = c("Q2_1_TB_SCORE", "Q2_3_TB_SCORE", "Q2_4_TB_SCORE",
                 "Q2_5_TB_SCORE", "Q2_6_TB_SCORE", "Q2_7_TB_SCORE")]

#keep only variables we need
lss3 <- lss3[, c("LS_PARENT","AREA_DESCR","FSCL_YR_NUM","CC_TB_SCORE","SO_TB_SCORE"), with=FALSE]

#swing years wide
lss3 <- dcast.data.table(lss3, LS_PARENT + AREA_DESCR ~ FSCL_YR_NUM, value.var=c("CC_TB_SCORE","SO_TB_SCORE"))

#drop rows without both years of data
lss3 <- na.omit(lss3, cols=c("CC_TB_SCORE_2017","SO_TB_SCORE_2017","CC_TB_SCORE_2018","SO_TB_SCORE_2018"))

#calulcate YoY
lss3[, cc_yoy := CC_TB_SCORE_2018/CC_TB_SCORE_2017]
lss3[, so_yoy := SO_TB_SCORE_2018/SO_TB_SCORE_2017]

#assign YoY rank
rank_var <- c(1:nrow(lss3))
#cc
lss3 <- setorder(lss3,-cc_yoy)
lss3 <- cbind(lss3,rank_var)
setnames(lss3,"rank_var","cc_yoy_rank")
#so
lss3 <- setorder(lss3,-so_yoy)
lss3 <- cbind(lss3,rank_var)
setnames(lss3,"rank_var","so_yoy_rank")

#merge in expected CC & SO scores
lss3 <- left_join(lss3,lss2x,by=c("LS_PARENT","AREA_DESCR"))
setDT(lss3)

#calculate delta observed vs expected
lss3[, cc_delta := round(CC_TB_SCORE_2018 - cc_exp,2)]
lss3[, so_delta := round(SO_TB_SCORE_2018 - so_exp,2)]

#assign delta rank
rank_var <- c(1:nrow(lss3))
#cc
lss3 <- setorder(lss3,-cc_delta)
lss3 <- cbind(lss3,rank_var)
setnames(lss3,"rank_var","cc_delta_rank")
#so
lss3 <- setorder(lss3,-so_delta)
lss3 <- cbind(lss3,rank_var)
setnames(lss3,"rank_var","so_delta_rank")

# #merge in district number and region description
# lssareas <- lss[FSCL_YR_NUM==2018, c("DIST_NUM","LS_PARENT","RGN_DESCR"), with=FALSE]
# lssareas <- subset(lssareas, !duplicated(lssareas))
# lss3 <- left_join(lssareas,lss3,by="LS_PARENT")
# setDT(lss3)

#set order to show top CC delta rankers
lss3 <- setorder(lss3,cc_delta_rank)

#write to .xlsx
#write.xlsx(lss3, file="O:/CoOp/CoOp194_PROReportng&OM/Julie/licensee_segment-area_CC-SOdelta_ranked.xlsx")
lss3 <- read.xlsx("O:/CoOp/CoOp194_PROReportng&OM/Julie/licensee_segment-area_CC-SOdelta_ranked.xlsx",1)
setDT(lss3)

#recode area names (remove LS - )
lss3[, AREA_DESCR := sub("LS - ", "", lss3[,AREA_DESCR])]

#make a positive or posative delta flag variable
lss3[, cc_posdelta := ifelse(cc_delta>=0,1,0)]
lss3[, so_posdelta := ifelse(so_delta>=0,1,0)]
#make a positive or posative YoY flag variable
lss3[, cc_posyoy := ifelse(cc_yoy>=1,1,0)]
lss3[, so_posyoy := ifelse(so_yoy>=1,1,0)]


##bring in RD name
#load data
rd <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/licensee_cc_bystore_RDname.csv")
#recode area names (remove LS - )
rd[, AREA_DESCR := sub("LS - ", "", rd[,AREA_DESCR])]
#remove closed areas
rd <- rd[!grepl("CLOSED",rd$AREA_DESCR),]
#convert names to lower case
rd[, DIV_MGR_NM := tolower(DIV_MGR_NM)]
#make new var for plotting
rd[, area_div := paste0(AREA_DESCR," - ",DIV_MGR_NM)]

#join together
lss3 <- left_join(lss3,rd,by="AREA_DESCR")
setDT(lss3)

#set labels
xlabel <- "Segment"
ylabel <- "Area"
tlabel <- "Customer connection year-over-year by segment and area"
slabel <- "Delta: Oct/Nov FY18 \nYoY: Oct/Nov FY18 vs Oct/Nov FY17"
#set data and variables
pdata <- lss3
px <- lss3[, LS_PARENT]
py <- lss3[, area_div]
fillvar <- lss3[, cc_delta]
boxvar <- lss3[, cc_posyoy]
#manual legend labels
lname1 <- "YoY change"
lname2 <- "Delta from expected"
xaxislabels <- c("Albertson's","C & U", "Host", "Kroger", "Target")
colorlabels <- c("Negative","Positive")
#plot
plot7 <- ggplot(pdata, aes(px, py)) + 
  geom_tile(aes(fill = fillvar, color = as.factor(boxvar)), size=.95) + 
  scale_color_discrete(name=lname1, labels=colorlabels, guide=guide_legend(order=1)) +
  scale_fill_gradientn(colours=brewer.pal(10,"RdYlBu"),
                       values=rescale(c(-0.1, -0.05,
                                        0,
                                        .05, .1)),
                       guide="colorbar", name=lname2) +
  scale_x_discrete(labels=xaxislabels) +
  xlab(xlabel) + ylab(ylabel) + theme_bw() +
  labs(title = tlabel, subtitle = slabel)
print(plot7)


#set labels
xlabel <- "Segment"
ylabel <- "Area"
tlabel <- "Store operations year-over-year by segment and area"
slabel <- "Delta: Oct/Nov FY18 \nYoY: Oct/Nov FY18 vs Oct/Nov FY17"
#set data and variables
pdata <- lss3
px <- lss3[, LS_PARENT]
py <- lss3[, area_div]
fillvar <- lss3[, so_delta]
boxvar <- lss3[, so_posyoy]
#manual legend labels
lname1 <- "YoY change"
lname2 <- "Delta from expected"
xaxislabels <- c("Albertson's","C & U", "Host", "Kroger", "Target")
colorlabels <- c("Negative","Positive")
#plot
plot8 <- ggplot(pdata, aes(px, py)) + 
  geom_tile(aes(fill = fillvar, color = as.factor(boxvar)), size=.95) + 
  scale_color_discrete(name=lname1, labels=colorlabels, guide=guide_legend(order=1)) +
  scale_fill_gradientn(colours=brewer.pal(10,"RdYlBu"),
                       values=rescale(c(-0.1, -0.05,
                                        0,
                                        .05, .1)),
                       guide="colorbar", name=lname2) +
  scale_x_discrete(labels=xaxislabels) +
  xlab(xlabel) + ylab(ylabel) + theme_bw() +
  labs(title = tlabel, subtitle = slabel)
print(plot8)
