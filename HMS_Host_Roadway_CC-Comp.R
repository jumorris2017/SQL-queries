#HMS Host Roadway Stores
#4/19/18
#for Krista Kondray

#load libraries
library(data.table)
library(ggplot2)
library(ggthemes)
library(flipRegression)

#load data
pagg <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/roadway_comp_cc_march.csv")

#recode as pos/neg comp
pagg[comp<=0, pos_comp := 0];pagg[comp>0, pos_comp := 1]

#calculate top box score for cc, by comp tertile
pagg <- pagg[, list(cc=round(mean(cc,na.rm=T),1),
                    comp=round(mean(comp,na.rm=T),3)*100),
             by="pos_comp"]

#bar chart; remove legend
#set labels
xlabel <- "Average Customer Connection"
xlabels <- c("Negative Comp Stores","Positive Comp Stores")
ylabel <- "Sales Comp"
tlabel <- "HMS Host Sales Comp and Customer Connection"
sublabel <- "HMS Host Roadway Stores, March FY 18 (N=73)"
caption <- "Removed stores that were outliers on comp\nMarch scores represent rolling 3-month results"
#values
pdata1a <- pagg
px1a <- pagg[,pos_comp]
py1a <- pagg[,comp]
nvar1a <- pagg[,cc]
#plot itself
plot1a <- ggplot(data=pdata1a,aes(y=py1a,x=as.factor(px1a))) + 
  geom_bar(stat="identity", width = 0.95, position=position_dodge(), fill="lightblue", colour="black") +
  guides(fill=F) +
  theme_economist_white(gray_bg = FALSE) +
  scale_x_discrete(labels=xlabels) +
  xlab("") + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) +
  geom_text(size = 4, aes(label=paste0("CC: ",nvar1a),y=0), stat="identity", vjust = -1, position = position_dodge(0.95)) +
  geom_text(size = 3.5, aes(label=py1a,y=0), stat="identity", vjust = 1.5, position = position_dodge(0.95))
print(plot1a)

#load data
pco <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/co-ls-cafe-dt_comp_q2fy18.csv")
pcc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/co-ls-cafe-dt_cc_q2fy18.csv")

#rename merge id columns to match
setnames(pcc,c("STORE_NUM"),c("STORE_NUMBER"))

#merge by store number, month, and year
pfull <- Reduce(function(x,y) {merge(x,y,by=c("STORE_NUMBER","FSCL_QTR_IN_YR_NUM","FSCL_YR_NUM"),all=TRUE)}, list(pco,pcc))

#aggregate
pagg <- pfull[, lapply(.SD, sum, na.rm=T), .SDcols=c("CC_RESPONSE_TOTAL","CC_TB_CNT",
                                                     "Sales","LYSales"),
              by=c("STORE_NUMBER","FSCL_YR_NUM")]

#calculate CC  top box score
pagg[, CC_TB_SCORE := round(CC_TB_CNT/CC_RESPONSE_TOTAL,4)]

#drop if stores don't have LYQuarterlySales
pagg <- na.omit(pagg, cols=c("Sales", "LYSales"))
pagg <- pagg[Sales>0&LYSales>0]

#calculate comps
pagg[, comp := (Sales-LYSales)/LYSales]
pagg <- pagg[comp>=-.275&comp<=.275]
#recode as pos/neg comp
pagg[comp<=0, pos_comp := 0];pagg[comp>0, pos_comp := 1]

#split by cc
prob = c(1/4, 2/4, 3/4, 1)
temp <- pagg %>% group_by(FSCL_YR_NUM) %>% summarise( 
  cc25 = quantile(CC_TB_SCORE, probs = prob[1], na.rm = T), 
  cc50 = quantile(CC_TB_SCORE, probs = prob[2], na.rm = T),
  cc75 = quantile(CC_TB_SCORE, probs = prob[3], na.rm = T),
  cc100 = quantile(CC_TB_SCORE, probs = prob[4], na.rm = T)
)
pagg <- left_join(pagg, temp, by=c("FSCL_YR_NUM"))
setDT(pagg)

#recode cc based on quartiles
pagg[CC_TB_SCORE <= cc25, ccquartile := 1]
pagg[CC_TB_SCORE > cc25 & CC_TB_SCORE <= cc50, ccquartile := 2]
pagg[CC_TB_SCORE > cc50 & CC_TB_SCORE <= cc75, ccquartile := 3]
pagg[CC_TB_SCORE > cc75, ccquartile := 4]

#calculate top box score for cc, by comps quartile
pagg <- pagg[, list(CC_RESPONSE_TOTAL=sum(CC_RESPONSE_TOTAL,na.rm=T),
                    CC_TB_CNT=sum(CC_TB_CNT,na.rm=T),
                    compsavg=(sum(Sales)-sum(LYSales))/sum(LYSales)),
             by="ccquartile"]
pagg[, CC_TB_SCORE := CC_TB_CNT/CC_RESPONSE_TOTAL]
#order by quartile
pagg <- setorder(pagg,ccquartile)
pagg <- cbind(pagg,t(temp)[2:5])
setnames(pagg,"V2","cc_q_value")
#make more presentable
pagg[, (colnames(pagg)[4:6]) := lapply(.SD, function(x) round((x*100),1)), .SDcols=colnames(pagg)[4:6]]
pagg[, CC_RESPONSE_TOTAL := NULL]; pagg[, CC_TB_CNT := NULL]
setnames(pagg,"cc_q_value","ccquartile_cutoff_value")

#bar chart; remove legend
#set labels
xlabel <- "Average Customer Connection"
xlabels <- c("1","2","3","4")
ylabel <- "Sales Comp"
tlabel <- "CO Comp and Customer Connection"
sublabel <- "US CO Stores, March FY 18"
caption <- "Stores grouped by Customer Connection quartile\nRemoved stores that were outliers on comp\nMarch scores represent rolling 3-month results"
#values
pdata1a <- pagg
px1a <- pagg[,ccquartile]
py1a <- pagg[,compsavg]
nvar1a <- pagg[, CC_TB_SCORE]
#plot itself
plot1a <- ggplot(data=pdata1a,aes(y=py1a,x=as.factor(px1a))) + 
  geom_bar(stat="identity", width = 0.95, position=position_dodge(), fill="lightblue", colour="black") +
  guides(fill=F) +
  theme_economist_white(gray_bg = FALSE) +
  scale_x_discrete(labels=xlabels) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) +
  geom_text(size = 4, aes(label=paste0("CC: ",nvar1a),y=0), stat="identity", vjust = -1, position = position_dodge(0.95)) +
  geom_text(size = 3.5, aes(label=py1a), stat="identity", vjust = 1.5, position = position_dodge(0.95))
print(plot1a)


#load data
pce <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/co-ls_ce_returnvisits_q2fy18.csv")
st <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/store_types.csv")
pce <- left_join(pce,st,by=c("STORE_NUM","OWNR_TYPE_CD"))
setDT(pce)
roadway <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/roadway_comp_cc_march.csv")
pce[STORE_NUM %in% roadway[,STORE_NUM], roadway := 1]
segments <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/store_segments_LS.csv")
pce <- left_join(pce,segments,by=c("STORE_NUM"))
setDT(pce)

#replace 9's
pce[, (colnames(pce)[6:13]) := lapply(.SD, function(x) ifelse(x==9,NA,x)), .SDcols=colnames(pce)[6:13]]

#
summary(lm(RETURN ~ Q2_2 + Q2_1 + Q2_3 + 
             Q2_4 + Q2_5 + Q2_6 + Q2_7,
           data=pce[OWNR_TYPE_CD=='CO']))

#ALL STORES: 
Regression(VISITS_POST60D ~ Q2_2 + Q2_1 + Q2_3 + 
             Q2_4 + Q2_5 + Q2_6 + Q2_7,
           data=pce[DRIVE_THRU_IND==0],
           output = "Relative Importance Analysis")
#CO
Regression(VISITS_POST60D ~ Q2_2 + Q2_1 + Q2_3 + 
             Q2_4 + Q2_5 + Q2_6 + Q2_7,
           data=pce[OWNR_TYPE_CD=='CO'&DRIVE_THRU_IND==0],
           output = "Relative Importance Analysis")
Regression(VISITS_POST60D ~ Q2_2 + Q2_1 + Q2_3 + 
             Q2_4 + Q2_5 + Q2_6 + Q2_7,
           data=pce[OWNR_TYPE_CD=='CO'&DRIVE_THRU_IND==1],
           output = "Relative Importance Analysis")
#LS
Regression(VISITS_POST60D ~ Q2_2 + Q2_1 + Q2_3 + 
             Q2_4 + Q2_5 + Q2_6 + Q2_7,
           data=pce[OWNR_TYPE_CD=='LS'],
           output = "Relative Importance Analysis")

#roadway
Regression(VISITS_POST60D ~ Q2_2 + Q2_1 + Q2_3 + 
             Q2_4 + Q2_5 + Q2_6 + Q2_7,
           data=pce[roadway==1],
           output = "Relative Importance Analysis")

#segments
Regression(VISITS_POST60D ~ Q2_2 + Q2_1 + Q2_3 + 
             Q2_4 + Q2_5 + Q2_6 + Q2_7,
           data=pce[STORE_LOC_TYPE=="AIRPORT"],
           output = "Relative Importance Analysis")
# Regression(VISITS_POST60D ~ Q2_2 + Q2_1 + Q2_3 + 
#              Q2_4 + Q2_5 + Q2_6 + Q2_7,
#            data=pce[STORE_LOC_TYPE=="TRAVEL"],
#            output = "Relative Importance Analysis")
Regression(VISITS_POST60D ~ Q2_2 + Q2_1 + Q2_3 + 
             Q2_4 + Q2_5 + Q2_6 + Q2_7,
           data=pce[STORE_LOC_TYPE=="GROCERY"],
           output = "Relative Importance Analysis")
Regression(VISITS_POST60D ~ Q2_2 + Q2_1 + Q2_3 + 
             Q2_4 + Q2_5 + Q2_6 + Q2_7,
           data=pce[STORE_LOC_TYPE=="SUPRSTOR"],
           output = "Relative Importance Analysis")
Regression(VISITS_POST60D ~ Q2_2 + Q2_1 + Q2_3 + 
             Q2_4 + Q2_5 + Q2_6 + Q2_7,
           data=pce[STORE_LOC_TYPE=="SCHOOL"],
           output = "Relative Importance Analysis")


temp <- pce[, list(VISITS_POST60D = mean(VISITS_POST60D,na.rm=T)),
    by=c("STORE_LOC_TYPE")]


#segments
Regression(VISITS_POST60D ~ Q2_2 + Q2_1 + Q2_3 + 
             Q2_4 + Q2_5 + Q2_6 + Q2_7,
           data=pce[STORE_BUS_CD=="TARGET"],
           output = "Relative Importance Analysis")
Regression(VISITS_POST60D ~ Q2_2 + Q2_1 + Q2_3 + 
             Q2_4 + Q2_5 + Q2_6 + Q2_7,
           data=pce[STORE_BUS_CD=="HOST"],
           output = "Relative Importance Analysis")
Regression(VISITS_POST60D ~ Q2_2 + Q2_1 + Q2_3 + 
             Q2_4 + Q2_5 + Q2_6 + Q2_7,
           data=pce[STORE_BUS_CD=="ALBERTS"|STORE_BUS_CD=="ALBERTSN"],
           output = "Relative Importance Analysis")
Regression(VISITS_POST60D ~ Q2_2 + Q2_1 + Q2_3 + 
             Q2_4 + Q2_5 + Q2_6 + Q2_7,
           data=pce[STORE_BUS_CD=="KROGER"|STORE_BUS_CD=="KROGERCO"],
           output = "Relative Importance Analysis")