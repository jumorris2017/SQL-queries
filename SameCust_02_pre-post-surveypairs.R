##Same customer, item-level, CE and behavior pre-and post- scores##
##Goal: Following customers 15 days after Purchase 1, and 15 days after Purchase 2##
####Assess if behavior changes *after* different CE raings##
####e.g., If at Purchase 1, they rate a 7, do they come back more frequenly,
####than if at Purchase 2, they rate a 6. 

#12/14/17 run has 1,119,724 records

#load libraries
library(data.table)
library(ggplot2)
library(scales)
library(patchwork)

#load data
scus <- read.csv("O:/CoOp/CoOp194_PROReportng&OM/Julie/SameCust_02_pre-post-surveypairsFULL.csv")
setDT(scus)

#create a copy
#scust <- copy(scust)

#convert 9's to NA
listofvars <- colnames(scust)[c(5:13,22:30)]
scus[, (listofvars) := lapply(.SD, function(x) ifelse(x==9,NA,x)), .SDcols=listofvars]

#BASED ON CC TIME 1
temp <- scus[!is.na(CC_1), list(n = .N,
                                     VISITS_POST_15D_1 = sum(VISITS_POST_15D_1,na.rm=T),
                                     VISITS_POST_15D_2 = sum(VISITS_POST_15D_2,na.rm=T),
                                     SPEND_POST_15D_1 = sum(SPEND_POST_15D_1,na.rm=T),
                                     SPEND_POST_15D_2 = sum(SPEND_POST_15D_2,na.rm=T),
                                     avg_vis_post_1 = sum(VISITS_POST_15D_1,na.rm=T)/.N,
                                     avg_vis_post_2 = sum(VISITS_POST_15D_2,na.rm=T)/.N,
                                     avg_spend_post_1 = sum(SPEND_POST_15D_1,na.rm=T)/.N,
                                     avg_spend_post_2 = sum(SPEND_POST_15D_2,na.rm=T)/.N),
              by=c("CC_1")]
#calculate spend per visits
temp[, spv_post_1 := SPEND_POST_15D_1/VISITS_POST_15D_1]
temp[, spv_post_2 := SPEND_POST_15D_2/VISITS_POST_15D_2]
#calculate total spend and visit deltas
temp[, visdelta := VISITS_POST_15D_2 - VISITS_POST_15D_1]
temp[, spenddelta := SPEND_POST_15D_2 - SPEND_POST_15D_1]
#calculate spend per visit delta
temp[, spvdelta := spv_post_2 - spv_post_1]
#calculate spend per person delta
temp[, sppdelta := avg_spend_post_2 - avg_spend_post_1]
#calculate visits per person delta
temp[, vppdelta := avg_vis_post_2 - avg_vis_post_1]

#
tlabel <- "15-Day Spend-Per-Visit (SPV) after time 1 by CC time 1"
xlabel <- "Time 1 CC score"
ylabel <- "15-Day SPV (USD)"
xlabels <- c(sort(unique(temp[CC_1<=7,CC_1])))

ggplot(data = temp[CC_1<=7], aes(x = factor(CC_1), y = spv_post_1)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) +
  theme(axis.text.x = element_text(face = "bold", size = 12, vjust = 1.5)) +
  geom_text(size = 5, aes(label=round(spv_post_1,2),y=0), 
            stat= "identity", vjust = -1.5)

#
tlabel <- "15-Day Visits-Per-Visit (VPP) after time 1 by CC time 1"
xlabel <- "Time 1 CC score"
ylabel <- "15-Day VPP"
xlabels <- c(sort(unique(temp[CC_1<=7,CC_1])))

ggplot(data = temp[CC_1<=7], aes(x = factor(CC_1), y = avg_vis_post_1)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) +
  theme(axis.text.x = element_text(face = "bold", size = 12, vjust = 1.5)) +
  geom_text(size = 5, aes(label=round(avg_vis_post_1,2),y=0), 
            stat= "identity", vjust = -1.5)

#
tlabel <- "15-Day Total Spend after time 1 by CC time 1"
xlabel <- "Time 1 CC score"
ylabel <- "15-Day Total Spend (USD)"

ggplot(data = temp[CC_1<=7], aes(x = factor(CC_1), y = SPEND_POST_15D_1)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) +
  scale_y_continuous(labels=comma) +
  theme(axis.text.x = element_text(face = "bold", size = 12, vjust = 1.5)) +
  geom_text(size = 3, aes(label=round(SPEND_POST_15D_1,0),y=0), 
            stat= "identity", vjust = -1)

#
tlabel <- "15-Day Total Visits after time 1 by CC time 1"
xlabel <- "Time 1 CC score"
ylabel <- "15-Day Total Visits (USD)"

ggplot(data = temp[CC_1<=7], aes(x = factor(CC_1), y = VISITS_POST_15D_1)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) +
  scale_y_continuous(labels=comma) +
  theme(axis.text.x = element_text(face = "bold", size = 12, vjust = 1.5)) +
  geom_text(size = 3, aes(label=round(VISITS_POST_15D_1,0),y=0), 
            stat= "identity", vjust = -1)

#
tlabel <- "15-Day Average Spend-per-Person (SPP) by CC time 1"
xlabel <- "Time 1 CC score"
ylabel <- "15-Day SPP (USD)"

ggplot(data = temp[CC_1<=7], aes(x = factor(CC_1), y = avg_spend_post_1)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) +
  scale_y_continuous(labels=comma) +
  theme(axis.text.x = element_text(face = "bold", size = 12, vjust = 1.5)) +
  geom_text(size = 3, aes(label=round(avg_spend_post_1,2),y=0), 
            stat= "identity", vjust = -1)

#
tlabel <- "Transaction count by CC time 1"
xlabel <- "Time 1 CC score"
ylabel <- "N"

ggplot(data = temp[CC_1<=7], aes(x = factor(CC_1), y = n)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) +
  scale_y_continuous(labels=comma) +
  theme(axis.text.x = element_text(face = "bold", size = 12, vjust = 1.5)) +
  geom_text(size = 3, aes(label=round(n,0),y=0), 
            stat= "identity", vjust = -1)






##subset to JUST those who dropped 1 point and those who stayed the same
#SPLIT INTO POINT CHANGE - CC
scust <- copy(scus)
#ANALYSIS 1: FOCUS ONLY ON POST-PERIODS
#create subsetting variable for change in CC score
scust[CC_1==CC_2, ccdelgrp := "02-same"]
scust[CC_1-1>=CC_2, ccdelgrp := "01-CC2_1pt_lo"]

#assess differences in behavior across *groups* of people based on ccdelgrp
temp <- scust[!is.na(ccdelgrp), list(n = .N,
                                     VISITS_POST_15D_1 = sum(VISITS_POST_15D_1,na.rm=T),
                                     VISITS_POST_15D_2 = sum(VISITS_POST_15D_2,na.rm=T),
                                     SPEND_POST_15D_1 = sum(SPEND_POST_15D_1,na.rm=T),
                                     SPEND_POST_15D_2 = sum(SPEND_POST_15D_2,na.rm=T),
                                     avg_vis_post_1 = sum(VISITS_POST_15D_1,na.rm=T)/.N,
                                     avg_vis_post_2 = sum(VISITS_POST_15D_2,na.rm=T)/.N,
                                     avg_spend_post_1 = sum(SPEND_POST_15D_1,na.rm=T)/.N,
                                     avg_spend_post_2 = sum(SPEND_POST_15D_2,na.rm=T)/.N),
              by=c("ccdelgrp")]
#calculate spend per visits
temp[, spv_post_1 := SPEND_POST_15D_1/VISITS_POST_15D_1]
temp[, spv_post_2 := SPEND_POST_15D_2/VISITS_POST_15D_2]
#calculate total spend and visit deltas
temp[, visdelta := VISITS_POST_15D_2 - VISITS_POST_15D_1]
temp[, spenddelta := SPEND_POST_15D_2 - SPEND_POST_15D_1]
#calculate spend per visit delta
temp[, spvdelta := spv_post_2 - spv_post_1]
#calculate spend per person delta
temp[, sppdelta := avg_spend_post_2 - avg_spend_post_1]
#calculate visits per person delta
temp[, vppdelta := avg_vis_post_2 - avg_vis_post_1]
#
tlabel <- "15-Day Spend-per-Visit Delta by Change in CC Score"
xlabel <- "Delta in CC score between time 1 and time 2"
ylabel <- "Delta in post-15-day spending-per-visit\nbetween time 1 and 2 (USD)"
xlabels <- c("1pt Lower","Same")
pdata <- temp
px <- temp[,ccdelgrp]
py <- temp[,spvdelta]

ggplot(data = pdata, aes(x = px, y = py)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  scale_x_discrete(label=xlabels) + 
  #ylim(c(-1.25,1.25)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) +
  theme(axis.text.x = element_text(face = "bold", size = 12)) +
  geom_text(size = 3, aes(label=round(py,3),y=0), stat= "identity", vjust = -1)



















#SPLIT INTO POINT CHANGE - CC
scust <- copy(scus)
#ANALYSIS 1: FOCUS ONLY ON POST-PERIODS
#create subsetting variable for change in CC score
scust[CC_1==CC_2, ccdelgrp := "07-same"]
scust[CC_1+1==CC_2, ccdelgrp := "08-CC2_hi_1pt"]
scust[CC_1+2==CC_2, ccdelgrp := "09-CC2_hi_2pt"]
scust[CC_1+3==CC_2, ccdelgrp := "10-CC2_hi_3pt"]
scust[CC_1+4==CC_2, ccdelgrp := "11-CC2_hi_4pt"]
scust[CC_1+5==CC_2, ccdelgrp := "12-CC2_hi_5pt"]
scust[CC_1+6==CC_2, ccdelgrp := "13-CC2_hi_6pt"]
scust[CC_1-1==CC_2, ccdelgrp := "06-CC2_lo_1pt"]
scust[CC_1-2==CC_2, ccdelgrp := "05-CC2_lo_2pt"]
scust[CC_1-3==CC_2, ccdelgrp := "04-CC2_lo_3pt"]
scust[CC_1-4==CC_2, ccdelgrp := "03-CC2_lo_4pt"]
scust[CC_1-5==CC_2, ccdelgrp := "02-CC2_lo_5pt"]
scust[CC_1-6==CC_2, ccdelgrp := "01-CC2_lo_6pt"]

#assess differences in behavior across *groups* of people based on ccdelgrp
temp <- scust[!is.na(ccdelgrp), list(n = .N,
                 VISITS_POST_15D_1 = sum(VISITS_POST_15D_1,na.rm=T),
                 VISITS_POST_15D_2 = sum(VISITS_POST_15D_2,na.rm=T),
                 SPEND_POST_15D_1 = sum(SPEND_POST_15D_1,na.rm=T),
                 SPEND_POST_15D_2 = sum(SPEND_POST_15D_2,na.rm=T),
                 avg_vis_post_1 = sum(VISITS_POST_15D_1,na.rm=T)/.N,
                 avg_vis_post_2 = sum(VISITS_POST_15D_2,na.rm=T)/.N,
                 avg_spend_post_1 = sum(SPEND_POST_15D_1,na.rm=T)/.N,
                 avg_spend_post_2 = sum(SPEND_POST_15D_2,na.rm=T)/.N),
             by=c("ccdelgrp")]
#calculate spend per visits
temp[, spv_post_1 := SPEND_POST_15D_1/VISITS_POST_15D_1]
temp[, spv_post_2 := SPEND_POST_15D_2/VISITS_POST_15D_2]
#calculate total spend and visit deltas
temp[, visdelta := VISITS_POST_15D_2 - VISITS_POST_15D_1]
temp[, spenddelta := SPEND_POST_15D_2 - SPEND_POST_15D_1]
#calculate spend per visit delta
temp[, spvdelta := spv_post_2 - spv_post_1]
#calculate spend per person delta
temp[, sppdelta := avg_spend_post_2 - avg_spend_post_1]
#calculate visits per person delta
temp[, vppdelta := avg_vis_post_2 - avg_vis_post_1]

#use "same" as control
temp[ccdelgrp!="07-same", svpdeltafromsame := spvdelta - temp[ccdelgrp=="07-same",spvdelta]]
temp[ccdelgrp!="07-same", spenddeltafromsame := spenddelta - temp[ccdelgrp=="07-same",spenddelta]]
temp[ccdelgrp!="07-same", visdeltafromsame := visdelta - temp[ccdelgrp=="07-same",visdelta]]
temp[ccdelgrp!="07-same", sppdeltafromsame := sppdelta - temp[ccdelgrp=="07-same",sppdelta]]
temp[ccdelgrp!="07-same", vppdeltafromsame := vppdelta - temp[ccdelgrp=="07-same",vppdelta]]
temp <- setorder(temp,ccdelgrp)

##spend per visit
tlabel <- "15-Day Spend-per-Visit Delta by Change in CC Score"
xlabel <- "Delta in CC score between time 1 and time 2,\n(net of delta for 'same' scoring customers)"
ylabel <- "Delta in post-15-day spending-per-visit\nbetween time 1 and 2 (USD)"
xlabels <- c("-6pts","-5pts","-4pts","-3pts","-2pts","-1pt",
             "+1pt","+2pts","+3pts","+4pts","+5pts","+6pts")
pdata <- temp[ccdelgrp!="07-same"]
px <- temp[ccdelgrp!="07-same",ccdelgrp]
py <- temp[ccdelgrp!="07-same",svpdeltafromsame]

ggplot(data = pdata, aes(x = px, y = py)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  scale_x_discrete(label=xlabels) + ylim(c(-.05,.05)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) +
  theme(axis.text.x = element_text(face = "bold", size = 12)) +
  geom_text(size = 3, aes(label=round(py,2),y=0), stat= "identity", vjust = -1)

##spend per person
tlabel <- "15-Day Spend-per-Person Delta by Change in CC Score"
xlabel <- "Delta in CC score between time 1 and time 2,\n(net of delta for 'same' scoring customers)"
ylabel <- "Delta in post-15-day spending-per-person\nbetween time 1 and 2 (USD)"
xlabels <- c("-6pts","-5pts","-4pts","-3pts","-2pts","-1pt",
             "+1pt","+2pts","+3pts","+4pts","+5pts","+6pts")
temp[ccdelgrp!="07-same"]
px <- temp[ccdelgrp!="07-same",ccdelgrp]
py <- temp[ccdelgrp!="07-same",sppdeltafromsame]

ggplot(data = pdata, aes(x = px, y = py)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  scale_x_discrete(label=xlabels) + ylim(c(-1.25,1.25)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) +
  theme(axis.text.x = element_text(face = "bold", size = 12)) +
  geom_text(size = 3, aes(label=round(py,2),y=0), stat= "identity", vjust = -1)

##visits per person
tlabel <- "15-Day Visits-per-Person Delta by Change in CC Score"
xlabel <- "Delta in CC score between time 1 and time 2,\n(net of delta for 'same' scoring customers)"
ylabel <- "Delta in post-15-day visits-per-person\nbetween time 1 and 2 (USD)"
xlabels <- c("-6pts","-5pts","-4pts","-3pts","-2pts","-1pt",
             "+1pt","+2pts","+3pts","+4pts","+5pts","+6pts")
pdata <- temp[ccdelgrp!="07-same"]
px <- temp[ccdelgrp!="07-same",ccdelgrp]
py <- temp[ccdelgrp!="07-same",vppdeltafromsame]

ggplot(data = pdata, aes(x = px, y = py)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  scale_x_discrete(label=xlabels) + 
  #ylim(c(-1.25,1.25)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) +
  theme(axis.text.x = element_text(face = "bold", size = 12)) +
  geom_text(size = 3, aes(label=round(py,2),y=0), stat= "identity", vjust = -1)


#SPLIT INTO SPECIFIC MOVEMENTS
scust <- copy(scus)

#create subsetting variable for change in CC score
scust[CC_1==7&CC_2==6, ccdelgrp := "08-7to6"]
scust[CC_1==7&CC_2==5, ccdelgrp := "09-7to5"]
scust[CC_1==7&CC_2==4, ccdelgrp := "10-7to4"]
scust[CC_1==7&CC_2==3, ccdelgrp := "11-7to3"]
scust[CC_1==7&CC_2==2, ccdelgrp := "12-7to2"]
scust[CC_1==7&CC_2==1, ccdelgrp := "13-7to1"]
scust[CC_1==6&CC_2==5, ccdelgrp := "14-6to5"]
scust[CC_1==6&CC_2==4, ccdelgrp := "15-6to4"]
scust[CC_1==6&CC_2==3, ccdelgrp := "16-6to3"]
scust[CC_1==6&CC_2==2, ccdelgrp := "17-6to2"]
scust[CC_1==6&CC_2==1, ccdelgrp := "18-6to1"]
scust[CC_1==5&CC_2==4, ccdelgrp := "19-5to4"]
scust[CC_1==5&CC_2==3, ccdelgrp := "20-5to3"]
scust[CC_1==5&CC_2==2, ccdelgrp := "21-5to2"]
scust[CC_1==5&CC_2==1, ccdelgrp := "22-5to1"]
scust[CC_1<CC_2, ccdelgrp := "23-growth"]
scust[CC_1==7&CC_2==7, ccdelgrp := "07-7_both"]
scust[CC_1==6&CC_2==6, ccdelgrp := "06-6_both"]
scust[CC_1==5&CC_2==5, ccdelgrp := "05-5_both"]
scust[CC_1==4&CC_2==4, ccdelgrp := "04-4_both"]
scust[CC_1==3&CC_2==3, ccdelgrp := "03-3_both"]
scust[CC_1==2&CC_2==2, ccdelgrp := "02-2_both"]
scust[CC_1==1&CC_2==1, ccdelgrp := "01-1_both"]

#assess differences in behavior across *groups* of people based on ccdelgrp
temp <- scust[!is.na(ccdelgrp), list(n = .N,
                                     VISITS_POST_15D_1 = sum(VISITS_POST_15D_1,na.rm=T),
                                     VISITS_POST_15D_2 = sum(VISITS_POST_15D_2,na.rm=T),
                                     SPEND_POST_15D_1 = sum(SPEND_POST_15D_1,na.rm=T),
                                     SPEND_POST_15D_2 = sum(SPEND_POST_15D_2,na.rm=T),
                                     avg_vis_post_1 = sum(VISITS_POST_15D_1,na.rm=T)/.N,
                                     avg_vis_post_2 = sum(VISITS_POST_15D_2,na.rm=T)/.N,
                                     avg_spend_post_1 = sum(SPEND_POST_15D_1,na.rm=T)/.N,
                                     avg_spend_post_2 = sum(SPEND_POST_15D_2,na.rm=T)/.N),
              by=c("ccdelgrp")]
#calculate spend per visits
temp[, spv_post_1 := SPEND_POST_15D_1/VISITS_POST_15D_1]
temp[, spv_post_2 := SPEND_POST_15D_2/VISITS_POST_15D_2]
#calculate total spend and visit deltas
temp[, visdelta := VISITS_POST_15D_2 - VISITS_POST_15D_1]
temp[, spenddelta := SPEND_POST_15D_2 - SPEND_POST_15D_1]
#calculate spend per visit delta
temp[, spvdelta := spv_post_2 - spv_post_1]
#calculate spend per person delta
temp[, sppdelta := avg_spend_post_2 - avg_spend_post_1]
#calculate visits per person delta
temp[, vppdelta := avg_vis_post_2 - avg_vis_post_1]

#spend per visit
tlabel <- "15-Day Spend-per-Visit Delta by Change in CC Score"
xlabel <- "Delta in CC score between time 1 and time 2"
ylabel <- "Delta in post-15-day spending-per-visit\nbetween time 1 and 2 (USD)"
pdata <- temp
px <- temp[,ccdelgrp]
py <- temp[,spvdelta]

ggplot(data = pdata, aes(x = px, y = py)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  #scale_x_discrete(label=xlabels) + 
  #ylim(c(-1.25,1.25)) + 
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) +
  theme(axis.text.x = element_text(face = "bold", size = 8, angle = 45, hjust = 1)) +
  geom_text(size = 2, aes(label=round(sppdelta,2),y=0), 
            stat= "identity", vjust = -1.5)

##spend per person
tlabel <- "15-Day Spend-per-Person Delta by Change in CC Score"
xlabel <- "Delta in CC score between time 1 and time 2"
ylabel <- "Delta in post-15-day spending-per-person\nbetween time 1 and 2 (USD)"
pdata <- temp
px <- temp[,ccdelgrp]
py <- temp[,sppdelta]

ggplot(data = pdata, aes(x = px, y = py)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  ylim(c(-1.25,1.25)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) +
  theme(axis.text.x = element_text(face = "bold", size = 7, angle=45)) +
  geom_text(size = 3, aes(label=round(py,2),y=0), stat= "identity", vjust = -1)

##visits per person
tlabel <- "15-Day Visits-per-Person Delta by Change in CC Score"
xlabel <- "Delta in CC score between time 1 and time 2"
ylabel <- "Delta in post-15-day visits-per-person\nbetween time 1 and 2 (USD)"
pdata <- temp
px <- temp[,ccdelgrp]
py <- temp[,vppdelta]

ggplot(data = pdata, aes(x = px, y = py)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  #ylim(c(-1.25,1.25)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) +
  theme(axis.text.x = element_text(face = "bold", size = 7, angle=45)) +
  geom_text(size = 3, aes(label=round(py,2),y=0), stat= "identity", vjust = -1)





#SPLIT INTO HIGHER VS LOWER
scust <- copy(scus)
#create subsetting variable for change in CC score
scust[CC_1==CC_2, ccdelgrp := "02-same"]
scust[CC_1>CC_2, ccdelgrp := "01-CC2_lower"]
scust[CC_1<CC_2, ccdelgrp := "03-CC2_higher"]

#assess differences in behavior across *groups* of people based on ccdelgrp
temp <- scust[!is.na(ccdelgrp), list(n = .N,
                                    VISITS_POST_15D_1 = sum(VISITS_POST_15D_1,na.rm=T),
                                    VISITS_POST_15D_2 = sum(VISITS_POST_15D_2,na.rm=T),
                                    SPEND_POST_15D_1 = sum(SPEND_POST_15D_1,na.rm=T),
                                    SPEND_POST_15D_2 = sum(SPEND_POST_15D_2,na.rm=T),
                                    avg_vis_post_1 = sum(VISITS_POST_15D_1,na.rm=T)/.N,
                                    avg_vis_post_2 = sum(VISITS_POST_15D_2,na.rm=T)/.N,
                                    avg_spend_post_1 = sum(SPEND_POST_15D_1,na.rm=T)/.N,
                                    avg_spend_post_2 = sum(SPEND_POST_15D_2,na.rm=T)/.N),
             by=c("ccdelgrp")]
#calculate spend per visits
temp[, spv_post_1 := SPEND_POST_15D_1/VISITS_POST_15D_1]
temp[, spv_post_2 := SPEND_POST_15D_2/VISITS_POST_15D_2]
#calculate total spend and visit deltas
temp[, visdelta := VISITS_POST_15D_2 - VISITS_POST_15D_1]
temp[, spenddelta := SPEND_POST_15D_2 - SPEND_POST_15D_1]
#calculate spend per visit delta
temp[, spvdelta := spv_post_2 - spv_post_1]
#calculate spend per person delta
temp[, sppdelta := avg_spend_post_2 - avg_spend_post_1]
#calculate visits per person delta
temp[, vppdelta := avg_vis_post_2 - avg_vis_post_1]

#use "same" as control
temp[ccdelgrp!="02-same", svpdeltafromsame := spvdelta - temp[ccdelgrp=="02-same",spvdelta]]
temp[ccdelgrp!="02-same", spenddeltafromsame := spenddelta - temp[ccdelgrp=="02-same",spenddelta]]
temp[ccdelgrp!="02-same", visdeltafromsame := visdelta - temp[ccdelgrp=="02-same",visdelta]]
temp[ccdelgrp!="02-same", sppdeltafromsame := sppdelta - temp[ccdelgrp=="02-same",sppdelta]]
temp[ccdelgrp!="02-same", vppdeltafromsame := vppdelta - temp[ccdelgrp=="02-same",vppdelta]]
temp <- setorder(temp,ccdelgrp)

#
tlabel <- "15-Day Spend-per-Visit Delta by Change in CC Score"
xlabel <- "Delta in CC score between time 1 and time 2,\n(net of delta for 'same' scoring customers)"
ylabel <- "Delta in post-15-day spending-per-visit\nbetween time 1 and 2 (USD)"
xlabels <- c("Lower","Higher")
pdata <- temp[ccdelgrp!="02-same"]
px <- temp[ccdelgrp!="02-same",ccdelgrp]
py <- temp[ccdelgrp!="02-same",svpdeltafromsame]

ggplot(data = pdata, aes(x = px, y = py)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  scale_x_discrete(label=xlabels) + 
  #ylim(c(-1.25,1.25)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) +
  theme(axis.text.x = element_text(face = "bold", size = 12)) +
  geom_text(size = 3, aes(label=round(py,3),y=0), stat= "identity", vjust = -1)

##spend per person
tlabel <- "15-Day Spend-per-Person Delta by Change in CC Score"
xlabel <- "Delta in CC score between time 1 and time 2,\n(net of delta for 'same' scoring customers)"
ylabel <- "Delta in post-15-day spending-per-person\nbetween time 1 and 2 (USD)"
pdata <- temp[ccdelgrp!="02-same"]
px <- temp[ccdelgrp!="02-same",ccdelgrp]
py <- temp[ccdelgrp!="02-same",sppdeltafromsame]

ggplot(data = pdata, aes(x = px, y = py)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  scale_x_discrete(label=xlabels) + 
  #ylim(c(-1.25,1.25)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) +
  theme(axis.text.x = element_text(face = "bold", size = 12)) +
  geom_text(size = 3, aes(label=round(py,2),y=0), stat= "identity", vjust = -1)

##visits per person
tlabel <- "15-Day Visits-per-Person Delta by Change in CC Score"
xlabel <- "Delta in CC score between time 1 and time 2,\n(net of delta for 'same' scoring customers)"
ylabel <- "Delta in post-15-day visits-per-person\nbetween time 1 and 2 (USD)"
pdata <- temp[ccdelgrp!="02-same"]
px <- temp[ccdelgrp!="02-same",ccdelgrp]
py <- temp[ccdelgrp!="02-same",vppdeltafromsame]

ggplot(data = pdata, aes(x = px, y = py)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  scale_x_discrete(label=xlabels) + 
  ylim(c(-.15,.05)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) +
  theme(axis.text.x = element_text(face = "bold", size = 12)) +
  geom_text(size = 3, aes(label=round(py,2),y=0), stat= "identity", vjust = -1)









#SPLIT INTO SPECIFIC MOVEMENTS - cleanliness
scust <- copy(scus)
#create subsetting variable for change in Cleanliness score
scust[CLEAN_1==7&CLEAN_2==6, clndelgrp := "08-7to6"]
scust[CLEAN_1==7&CLEAN_2==5, clndelgrp := "09-7to5"]
scust[CLEAN_1==7&CLEAN_2==4, clndelgrp := "10-7to4"]
scust[CLEAN_1==7&CLEAN_2==3, clndelgrp := "11-7to3"]
scust[CLEAN_1==7&CLEAN_2==2, clndelgrp := "12-7to2"]
scust[CLEAN_1==7&CLEAN_2==1, clndelgrp := "13-7to1"]
scust[CLEAN_1==6&CLEAN_2==5, clndelgrp := "14-6to5"]
scust[CLEAN_1==6&CLEAN_2==4, clndelgrp := "15-6to4"]
scust[CLEAN_1==6&CLEAN_2==3, clndelgrp := "16-6to3"]
scust[CLEAN_1==6&CLEAN_2==2, clndelgrp := "17-6to2"]
scust[CLEAN_1==6&CLEAN_2==1, clndelgrp := "18-6to1"]
scust[CLEAN_1==5&CLEAN_2==4, clndelgrp := "19-5to4"]
scust[CLEAN_1==5&CLEAN_2==3, clndelgrp := "20-5to3"]
scust[CLEAN_1==5&CLEAN_2==2, clndelgrp := "21-5to2"]
scust[CLEAN_1==5&CLEAN_2==1, clndelgrp := "22-5to1"]
scust[CLEAN_1<CLEAN_2, clndelgrp := "23-growth"]
scust[CLEAN_1==7&CLEAN_2==7, clndelgrp := "07-7_both"]
scust[CLEAN_1==6&CLEAN_2==6, clndelgrp := "06-6_both"]
scust[CLEAN_1==5&CLEAN_2==5, clndelgrp := "05-5_both"]
scust[CLEAN_1==4&CLEAN_2==4, clndelgrp := "04-4_both"]
scust[CLEAN_1==3&CLEAN_2==3, clndelgrp := "03-3_both"]
scust[CLEAN_1==2&CLEAN_2==2, clndelgrp := "02-2_both"]
scust[CLEAN_1==1&CLEAN_2==1, clndelgrp := "01-1_both"]
#assess differences in behavior across *groups* of people based on clndelgrp
temp <- scust[!is.na(clndelgrp), list(n = .N,
                                     VISITS_POST_15D_1 = sum(VISITS_POST_15D_1,na.rm=T),
                                     VISITS_POST_15D_2 = sum(VISITS_POST_15D_2,na.rm=T),
                                     SPEND_POST_15D_1 = sum(SPEND_POST_15D_1,na.rm=T),
                                     SPEND_POST_15D_2 = sum(SPEND_POST_15D_2,na.rm=T),
                                     avg_vis_post_1 = sum(VISITS_POST_15D_1,na.rm=T)/.N,
                                     avg_vis_post_2 = sum(VISITS_POST_15D_2,na.rm=T)/.N,
                                     avg_spend_post_1 = sum(SPEND_POST_15D_1,na.rm=T)/.N,
                                     avg_spend_post_2 = sum(SPEND_POST_15D_2,na.rm=T)/.N),
              by=c("clndelgrp")]
#calculate spend per visits
temp[, spv_post_1 := SPEND_POST_15D_1/VISITS_POST_15D_1]
temp[, spv_post_2 := SPEND_POST_15D_2/VISITS_POST_15D_2]
#calculate total spend and visit deltas
temp[, visdelta := VISITS_POST_15D_2 - VISITS_POST_15D_1]
temp[, spenddelta := SPEND_POST_15D_2 - SPEND_POST_15D_1]
#calculate spend per visit delta
temp[, spvdelta := spv_post_2 - spv_post_1]
#calculate spend per person delta
temp[, sppdelta := avg_spend_post_2 - avg_spend_post_1]
#calculate visits per person delta
temp[, vppdelta := avg_vis_post_2 - avg_vis_post_1]

##spend per person
tlabel <- "15-Day Spend-per-Person Delta by Change in Cleanliness Score"
xlabel <- "Delta in Cleanliness score between time 1 and time 2"
ylabel <- "Delta in post-15-day spending-per-person\nbetween time 1 and 2 (USD)"
pdata <- temp
px <- temp[,clndelgrp]
py <- temp[,sppdelta]
ggplot(data = pdata, aes(x = px, y = py)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  ylim(c(-1.25,1.25)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) +
  theme(axis.text.x = element_text(face = "bold", size = 7, angle=45)) +
  geom_text(size = 3, aes(label=round(py,2),y=0), stat= "identity", vjust = -1)



#SPLIT INTO SPECIFIC MOVEMENTS - worth
scust <- copy(scus)
#create subsetting variable for change in Cleanliness score
scust[WORTH_1==7&WORTH_2==6, wpdelgrp := "08-7to6"]
scust[WORTH_1==7&WORTH_2==5, wpdelgrp := "09-7to5"]
scust[WORTH_1==7&WORTH_2==4, wpdelgrp := "10-7to4"]
scust[WORTH_1==7&WORTH_2==3, wpdelgrp := "11-7to3"]
scust[WORTH_1==7&WORTH_2==2, wpdelgrp := "12-7to2"]
scust[WORTH_1==7&WORTH_2==1, wpdelgrp := "13-7to1"]
scust[WORTH_1==6&WORTH_2==5, wpdelgrp := "14-6to5"]
scust[WORTH_1==6&WORTH_2==4, wpdelgrp := "15-6to4"]
scust[WORTH_1==6&WORTH_2==3, wpdelgrp := "16-6to3"]
scust[WORTH_1==6&WORTH_2==2, wpdelgrp := "17-6to2"]
scust[WORTH_1==6&WORTH_2==1, wpdelgrp := "18-6to1"]
scust[WORTH_1==5&WORTH_2==4, wpdelgrp := "19-5to4"]
scust[WORTH_1==5&WORTH_2==3, wpdelgrp := "20-5to3"]
scust[WORTH_1==5&WORTH_2==2, wpdelgrp := "21-5to2"]
scust[WORTH_1==5&WORTH_2==1, wpdelgrp := "22-5to1"]
scust[WORTH_1<WORTH_2, wpdelgrp := "23-growth"]
scust[WORTH_1==7&WORTH_2==7, wpdelgrp := "07-7_both"]
scust[WORTH_1==6&WORTH_2==6, wpdelgrp := "06-6_both"]
scust[WORTH_1==5&WORTH_2==5, wpdelgrp := "05-5_both"]
scust[WORTH_1==4&WORTH_2==4, wpdelgrp := "04-4_both"]
scust[WORTH_1==3&WORTH_2==3, wpdelgrp := "03-3_both"]
scust[WORTH_1==2&WORTH_2==2, wpdelgrp := "02-2_both"]
scust[WORTH_1==1&WORTH_2==1, wpdelgrp := "01-1_both"]
#assess differences in behavior across *groups* of people based on wpdelgrp
temp <- scust[!is.na(wpdelgrp), list(n = .N,
                                      VISITS_POST_15D_1 = sum(VISITS_POST_15D_1,na.rm=T),
                                      VISITS_POST_15D_2 = sum(VISITS_POST_15D_2,na.rm=T),
                                      SPEND_POST_15D_1 = sum(SPEND_POST_15D_1,na.rm=T),
                                      SPEND_POST_15D_2 = sum(SPEND_POST_15D_2,na.rm=T),
                                      avg_vis_post_1 = sum(VISITS_POST_15D_1,na.rm=T)/.N,
                                      avg_vis_post_2 = sum(VISITS_POST_15D_2,na.rm=T)/.N,
                                      avg_spend_post_1 = sum(SPEND_POST_15D_1,na.rm=T)/.N,
                                      avg_spend_post_2 = sum(SPEND_POST_15D_2,na.rm=T)/.N),
              by=c("wpdelgrp")]
#calculate spend per visits
temp[, spv_post_1 := SPEND_POST_15D_1/VISITS_POST_15D_1]
temp[, spv_post_2 := SPEND_POST_15D_2/VISITS_POST_15D_2]
#calculate total spend and visit deltas
temp[, visdelta := VISITS_POST_15D_2 - VISITS_POST_15D_1]
temp[, spenddelta := SPEND_POST_15D_2 - SPEND_POST_15D_1]
#calculate spend per visit delta
temp[, spvdelta := spv_post_2 - spv_post_1]
#calculate spend per person delta
temp[, sppdelta := avg_spend_post_2 - avg_spend_post_1]
#calculate visits per person delta
temp[, vppdelta := avg_vis_post_2 - avg_vis_post_1]

##spend per person
tlabel <- "15-Day Spend-per-Person Delta by Change in Worth Score"
xlabel <- "Delta in Worth score between time 1 and time 2"
ylabel <- "Delta in post-15-day spending-per-person\nbetween time 1 and 2 (USD)"
pdata <- temp
px <- temp[,wpdelgrp]
py <- temp[,sppdelta]
ggplot(data = pdata, aes(x = px, y = py)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  ylim(c(-1.25,1.25)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) +
  theme(axis.text.x = element_text(face = "bold", size = 7, angle=45)) +
  geom_text(size = 3, aes(label=round(py,2),y=0), stat= "identity", vjust = -1)





#SPLIT INTO SPECIFIC MOVEMENTS - SPEED
scust <- copy(scus)
#create subsetting variable for change in Cleanliness score
scust[SPEED_1==7&SPEED_2==6, spddelgrp := "08-7to6"]
scust[SPEED_1==7&SPEED_2==5, spddelgrp := "09-7to5"]
scust[SPEED_1==7&SPEED_2==4, spddelgrp := "10-7to4"]
scust[SPEED_1==7&SPEED_2==3, spddelgrp := "11-7to3"]
scust[SPEED_1==7&SPEED_2==2, spddelgrp := "12-7to2"]
scust[SPEED_1==7&SPEED_2==1, spddelgrp := "13-7to1"]
scust[SPEED_1==6&SPEED_2==5, spddelgrp := "14-6to5"]
scust[SPEED_1==6&SPEED_2==4, spddelgrp := "15-6to4"]
scust[SPEED_1==6&SPEED_2==3, spddelgrp := "16-6to3"]
scust[SPEED_1==6&SPEED_2==2, spddelgrp := "17-6to2"]
scust[SPEED_1==6&SPEED_2==1, spddelgrp := "18-6to1"]
scust[SPEED_1==5&SPEED_2==4, spddelgrp := "19-5to4"]
scust[SPEED_1==5&SPEED_2==3, spddelgrp := "20-5to3"]
scust[SPEED_1==5&SPEED_2==2, spddelgrp := "21-5to2"]
scust[SPEED_1==5&SPEED_2==1, spddelgrp := "22-5to1"]
scust[SPEED_1<SPEED_2, spddelgrp := "23-growth"]
scust[SPEED_1==7&SPEED_2==7, spddelgrp := "07-7_both"]
scust[SPEED_1==6&SPEED_2==6, spddelgrp := "06-6_both"]
scust[SPEED_1==5&SPEED_2==5, spddelgrp := "05-5_both"]
scust[SPEED_1==4&SPEED_2==4, spddelgrp := "04-4_both"]
scust[SPEED_1==3&SPEED_2==3, spddelgrp := "03-3_both"]
scust[SPEED_1==2&SPEED_2==2, spddelgrp := "02-2_both"]
scust[SPEED_1==1&SPEED_2==1, spddelgrp := "01-1_both"]
#assess differences in behavior across *groups* of people based on spddelgrp
temp <- scust[!is.na(spddelgrp), list(n = .N,
                                     VISITS_POST_15D_1 = sum(VISITS_POST_15D_1,na.rm=T),
                                     VISITS_POST_15D_2 = sum(VISITS_POST_15D_2,na.rm=T),
                                     SPEND_POST_15D_1 = sum(SPEND_POST_15D_1,na.rm=T),
                                     SPEND_POST_15D_2 = sum(SPEND_POST_15D_2,na.rm=T),
                                     avg_vis_post_1 = sum(VISITS_POST_15D_1,na.rm=T)/.N,
                                     avg_vis_post_2 = sum(VISITS_POST_15D_2,na.rm=T)/.N,
                                     avg_spend_post_1 = sum(SPEND_POST_15D_1,na.rm=T)/.N,
                                     avg_spend_post_2 = sum(SPEND_POST_15D_2,na.rm=T)/.N),
              by=c("spddelgrp")]
#calculate spend per visits
temp[, spv_post_1 := SPEND_POST_15D_1/VISITS_POST_15D_1]
temp[, spv_post_2 := SPEND_POST_15D_2/VISITS_POST_15D_2]
#calculate total spend and visit deltas
temp[, visdelta := VISITS_POST_15D_2 - VISITS_POST_15D_1]
temp[, spenddelta := SPEND_POST_15D_2 - SPEND_POST_15D_1]
#calculate spend per visit delta
temp[, spvdelta := spv_post_2 - spv_post_1]
#calculate spend per person delta
temp[, sppdelta := avg_spend_post_2 - avg_spend_post_1]
#calculate visits per person delta
temp[, vppdelta := avg_vis_post_2 - avg_vis_post_1]

##spend per person
tlabel <- "15-Day Spend-per-Person Delta by Change in Speed Score"
xlabel <- "Delta in Speed score between time 1 and time 2"
ylabel <- "Delta in post-15-day spending-per-person\nbetween time 1 and 2 (USD)"
pdata <- temp
px <- temp[,spddelgrp]
py <- temp[,sppdelta]
ggplot(data = pdata, aes(x = px, y = py)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  ylim(c(-1.25,1.25)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) +
  theme(axis.text.x = element_text(face = "bold", size = 7, angle=45)) +
  geom_text(size = 3, aes(label=round(py,2),y=0), stat= "identity", vjust = -1)




#SPLIT INTO SPECIFIC MOVEMENTS - SPEED
scust <- copy(scus)
#create subsetting variable for change in Cleanliness score
scust[RETURN_1==7&RETURN_2==6, spddelgrp := "08-7to6"]
scust[RETURN_1==7&RETURN_2==5, spddelgrp := "09-7to5"]
scust[RETURN_1==7&RETURN_2==4, spddelgrp := "10-7to4"]
scust[RETURN_1==7&RETURN_2==3, spddelgrp := "11-7to3"]
scust[RETURN_1==7&RETURN_2==2, spddelgrp := "12-7to2"]
scust[RETURN_1==7&RETURN_2==1, spddelgrp := "13-7to1"]
scust[RETURN_1==6&RETURN_2==5, spddelgrp := "14-6to5"]
scust[RETURN_1==6&RETURN_2==4, spddelgrp := "15-6to4"]
scust[RETURN_1==6&RETURN_2==3, spddelgrp := "16-6to3"]
scust[RETURN_1==6&RETURN_2==2, spddelgrp := "17-6to2"]
scust[RETURN_1==6&RETURN_2==1, spddelgrp := "18-6to1"]
scust[RETURN_1==5&RETURN_2==4, spddelgrp := "19-5to4"]
scust[RETURN_1==5&RETURN_2==3, spddelgrp := "20-5to3"]
scust[RETURN_1==5&RETURN_2==2, spddelgrp := "21-5to2"]
scust[RETURN_1==5&RETURN_2==1, spddelgrp := "22-5to1"]
scust[RETURN_1<RETURN_2, spddelgrp := "23-growth"]
scust[RETURN_1==7&RETURN_2==7, spddelgrp := "07-7_both"]
scust[RETURN_1==6&RETURN_2==6, spddelgrp := "06-6_both"]
scust[RETURN_1==5&RETURN_2==5, spddelgrp := "05-5_both"]
scust[RETURN_1==4&RETURN_2==4, spddelgrp := "04-4_both"]
scust[RETURN_1==3&RETURN_2==3, spddelgrp := "03-3_both"]
scust[RETURN_1==2&RETURN_2==2, spddelgrp := "02-2_both"]
scust[RETURN_1==1&RETURN_2==1, spddelgrp := "01-1_both"]
#assess differences in behavior across *groups* of people based on spddelgrp
temp <- scust[!is.na(spddelgrp), list(n = .N,
                                      VISITS_POST_15D_1 = sum(VISITS_POST_15D_1,na.rm=T),
                                      VISITS_POST_15D_2 = sum(VISITS_POST_15D_2,na.rm=T),
                                      SPEND_POST_15D_1 = sum(SPEND_POST_15D_1,na.rm=T),
                                      SPEND_POST_15D_2 = sum(SPEND_POST_15D_2,na.rm=T),
                                      avg_vis_post_1 = sum(VISITS_POST_15D_1,na.rm=T)/.N,
                                      avg_vis_post_2 = sum(VISITS_POST_15D_2,na.rm=T)/.N,
                                      avg_spend_post_1 = sum(SPEND_POST_15D_1,na.rm=T)/.N,
                                      avg_spend_post_2 = sum(SPEND_POST_15D_2,na.rm=T)/.N),
              by=c("spddelgrp")]
#calculate spend per visits
temp[, spv_post_1 := SPEND_POST_15D_1/VISITS_POST_15D_1]
temp[, spv_post_2 := SPEND_POST_15D_2/VISITS_POST_15D_2]
#calculate total spend and visit deltas
temp[, visdelta := VISITS_POST_15D_2 - VISITS_POST_15D_1]
temp[, spenddelta := SPEND_POST_15D_2 - SPEND_POST_15D_1]
#calculate spend per visit delta
temp[, spvdelta := spv_post_2 - spv_post_1]
#calculate spend per person delta
temp[, rtndelta := avg_spend_post_2 - avg_spend_post_1]
#calculate visits per person delta
temp[, vppdelta := avg_vis_post_2 - avg_vis_post_1]

##spend per person
tlabel <- "15-Day Spend-per-Person Delta by Change in Likelihood to Return Score"
xlabel <- "Delta in Return score between time 1 and time 2"
ylabel <- "Delta in post-15-day spending-per-person\nbetween time 1 and 2 (USD)"
pdata <- temp
px <- temp[,spddelgrp]
py <- temp[,rtndelta]
ggplot(data = pdata, aes(x = px, y = py)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  ylim(c(-1.25,1.25)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) +
  theme(axis.text.x = element_text(face = "bold", size = 7, angle=45)) +
  geom_text(size = 3, aes(label=round(py,2),y=0), stat= "identity", vjust = -1)

# #assess differences in behavior by time 1 CC and time 2 CC
# temp2 <- scust[, list(VISITS_POST_15D_1 = sum(VISITS_POST_15D_1,na.rm=T),
#                       VISITS_POST_15D_2 = sum(VISITS_POST_15D_2,na.rm=T),
#                       SPEND_POST_15D_1 = sum(SPEND_POST_15D_1,na.rm=T),
#                       SPEND_POST_15D_2 = sum(SPEND_POST_15D_2,na.rm=T)),
#                by=c("CC_1","CC_2")]
# temp2[, visdelta := VISITS_POST_15D_2 - VISITS_POST_15D_1]
# temp2[, spenddelta := SPEND_POST_15D_2 - SPEND_POST_15D_1]
# 
# #ANALYSIS 2: LOOK AT VISIT AND SPEND PRE-TO-POST DELTAS
# 
# #assess differences in behavior across *groups* of people based on ccdelgrp
# temp <- scust[, list(VISITS_PRE_15D_1 = sum(VISITS_PRE_15D_1,na.rm=T),
#                      VISITS_PRE_15D_2 = sum(VISITS_PRE_15D_2,na.rm=T),
#                      VISITS_POST_15D_1 = sum(VISITS_POST_15D_1,na.rm=T),
#                      VISITS_POST_15D_2 = sum(VISITS_POST_15D_2,na.rm=T),
#                      SPEND_PRE_15D_1 = sum(SPEND_PRE_15D_1,na.rm=T),
#                      SPEND_PRE_15D_2 = sum(SPEND_PRE_15D_2,na.rm=T),
#                      SPEND_POST_15D_1 = sum(SPEND_POST_15D_1,na.rm=T),
#                      SPEND_POST_15D_2 = sum(SPEND_POST_15D_2,na.rm=T)),
#               by=c("ccdelgrp")]
# temp[, visdelta_1 := VISITS_POST_15D_1 - VISITS_PRE_15D_1]
# temp[, visdelta_2 := VISITS_POST_15D_2 - VISITS_PRE_15D_2]
# temp[, spenddelta_1 := SPEND_POST_15D_1 - SPEND_PRE_15D_1]
# temp[, spenddelta_2 := SPEND_POST_15D_2 - SPEND_PRE_15D_2]
# 
# #assess differences in behavior by time 1 CC and time 2 CC
# temp2 <- scust[, list(VISITS_PRE_15D_1 = sum(VISITS_PRE_15D_1,na.rm=T),
#                       VISITS_PRE_15D_2 = sum(VISITS_PRE_15D_2,na.rm=T),
#                       VISITS_POST_15D_1 = sum(VISITS_POST_15D_1,na.rm=T),
#                       VISITS_POST_15D_2 = sum(VISITS_POST_15D_2,na.rm=T),
#                       SPEND_PRE_15D_1 = sum(SPEND_PRE_15D_1,na.rm=T),
#                       SPEND_PRE_15D_2 = sum(SPEND_PRE_15D_2,na.rm=T),
#                       SPEND_POST_15D_1 = sum(SPEND_POST_15D_1,na.rm=T),
#                       SPEND_POST_15D_2 = sum(SPEND_POST_15D_2,na.rm=T)),
#                by=c("CC_1","CC_2")]
# temp2[, visdelta_1 := VISITS_POST_15D_1 - VISITS_PRE_15D_1]
# temp2[, visdelta_2 := VISITS_POST_15D_2 - VISITS_PRE_15D_2]
# temp2[, spenddelta_1 := SPEND_POST_15D_1 - SPEND_PRE_15D_1]
# temp2[, spenddelta_2 := SPEND_POST_15D_2 - SPEND_PRE_15D_2]
# 
# temp2[, visdelta := VISITS_POST_15D_2 - VISITS_POST_15D_1]
# temp2[, spenddelta := SPEND_POST_15D_2 - SPEND_POST_15D_1]
