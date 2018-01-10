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
scus <- read.csv("O:/CoOp/CoOp194_PROReportng&OM/Julie/SameCust_02_pre-post-surveypairs.csv")
setDT(scus)

# #subset to only same-store and AMs (N=801,175 to 193,342)
# length(unique(scus[TRANS_DTM_2new>="2017-10-01 PDT"&TRANS_DTM_2new<"2017-11-01 PDT",GUID_USER_ID]))
#scus <- scus[STORE_NUM_1==STORE_NUM_2&TRANS_HR_1>=4&TRANS_HR_1<=12&TRANS_HR_2>=4&TRANS_HR_2<=12]

#change date format
scus[, TRANS_DTM_1new := strptime(TRANS_DTM_1, "%Y-%m-%d %H:%M:%S")]
scus[, TRANS_DTM_2new := strptime(TRANS_DTM_2, "%Y-%m-%d %H:%M:%S")]

#date delta
scus[, date_delta := TRANS_DTM_1new - TRANS_DTM_2new]
mean(scus[,date_delta])

#create a copy
#scust <- copy(scust)

#convert 9's to NA
listofvars <- colnames(scus)[c(7:15,26:34)]
scus[, (listofvars) := lapply(.SD, function(x) ifelse(x==9,NA,x)), .SDcols=listofvars]

#remove outliers: people who spent over $1000 in times 1 or 2...
scus <- scus[SPEND_POST_15D_1<1000&SPEND_POST_15D_2<1000]
#remove outliers: people who visit over 125 times...
scus <- scus[VISITS_POST_15D_1<150&VISITS_POST_15D_2<150]
#calculate average spend per visit
scus[, spv_1 := SPEND_POST_15D_1/VISITS_POST_15D_1]
scus[, spv_2 := SPEND_POST_15D_2/VISITS_POST_15D_2]
#remove outliers: people who spend over $150 per visit...
scus <- scus[spv_1<150&spv_2<150]

#do some initial exploration
ggplot(scus,aes(x=SPEND_POST_15D_1,y=SPEND_POST_15D_2)) + geom_point()
ggplot(scus,aes(x=VISITS_POST_15D_1,y=VISITS_POST_15D_2)) + geom_point()
ggplot(scus,aes(x=SPEND_POST_15D_1/VISITS_POST_15D_1,y=SPEND_POST_15D_2/VISITS_POST_15D_2)) + geom_point()

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
p1 <- ggplot(data = temp[CC_1<=7], aes(x = factor(CC_1), y = spv_post_1)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + 
  #ggtitle(tlabel) +
  theme(axis.text.x = element_text(face = "bold", size = 12, vjust = 1.5)) 
# +
#   geom_text(size = 2.5, aes(label=paste0("n=",comma(n)),y=0), stat= "identity", vjust = 1.25) +
#   geom_text(size = 5, aes(label=round(spv_post_1,1),y=0), stat= "identity", vjust = -1.5)

#
tlabel <- "15-Day Visits-Per-Person (VPP) after time 1 by CC time 1"
xlabel <- "Time 1 CC score"
ylabel <- "15-Day VPP"
xlabels <- c(sort(unique(temp[CC_1<=7,CC_1])))
p2 <- ggplot(data = temp[CC_1<=7], aes(x = factor(CC_1), y = avg_vis_post_1)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + 
  #ggtitle(tlabel) +
  theme(axis.text.x = element_text(face = "bold", size = 12, vjust = 1.5)) 
# +
#   geom_text(size = 2.5, aes(label=paste0("n=",comma(n)),y=0), stat= "identity", vjust = 1.25) +
#   geom_text(size = 5, aes(label=round(avg_vis_post_1,1),y=0), stat= "identity", vjust = -1.5)

#
tlabel <- "15-Day Total Spend after time 1 by CC time 1"
xlabel <- "Time 1 CC score"
ylabel <- "15-Day Total Spend (USD)"
p3 <- ggplot(data = temp[CC_1<=7], aes(x = factor(CC_1), y = SPEND_POST_15D_1)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + 
  #ggtitle(tlabel) +
  scale_y_continuous(labels=comma) +
  theme(axis.text.x = element_text(face = "bold", size = 12, vjust = 1.5)) 
# +
#   geom_text(size = 2.5, aes(label=paste0("n=",comma(n)),y=0), stat= "identity", vjust = 1.25) +
#   geom_text(size = 3, aes(label=comma(round(SPEND_POST_15D_1,0)),y=0), stat= "identity", vjust = -1)

#
tlabel <- "15-Day Total Visits after time 1 by CC time 1"
xlabel <- "Time 1 CC score"
ylabel <- "15-Day Total Visits"
p4 <- ggplot(data = temp[CC_1<=7], aes(x = factor(CC_1), y = VISITS_POST_15D_1)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + 
  #ggtitle(tlabel) +
  scale_y_continuous(labels=comma) +
  theme(axis.text.x = element_text(face = "bold", size = 12, vjust = 1.5)) 
# +
#   geom_text(size = 2.5, aes(label=paste0("n=",comma(n)),y=0), stat= "identity", vjust = 1.25) +
#   geom_text(size = 3, aes(label=comma(round(VISITS_POST_15D_1,0)),y=0), stat= "identity", vjust = -1)

#
tlabel <- "15-Day Average Spend-per-Person (SPP) by CC time 1"
xlabel <- "Time 1 CC score"
ylabel <- "15-Day SPP (USD)"
p5 <- ggplot(data = temp[CC_1<=7], aes(x = factor(CC_1), y = avg_spend_post_1)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + 
  #ggtitle(tlabel) +
  scale_y_continuous(labels=comma) +
  theme(axis.text.x = element_text(face = "bold", size = 12, vjust = 1.5)) 
# +
#   geom_text(size = 2.5, aes(label=paste0("n=",comma(n)),y=0), stat= "identity", vjust = 1.25) +
#   geom_text(size = 3, aes(label=round(avg_spend_post_1,1),y=0), stat= "identity", vjust = -1)

#
tlabel <- "Transaction count by CC time 1"
xlabel <- "Time 1 CC score"
ylabel <- "N"
p6 <- ggplot(data = temp[CC_1<=7], aes(x = factor(CC_1), y = n)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + 
  #ggtitle(tlabel) +
  scale_y_continuous(labels=comma, breaks = scales::pretty_breaks(n = 6)) 
# +
#   theme(axis.text.x = element_text(face = "bold", size = 12, vjust = 1.5)) +
#   geom_text(size = 3, aes(label=comma(round(n,0)),y=0), angle=90, stat= "identity", hjust = -1)

#patchwork them together
(p1 | p2 | p3) / (p4 | p5 | p6)





###PART A

##subset to JUST those who dropped 1 point and those who stayed the same

#SPLIT INTO POINT CHANGE - CC
scust <- copy(scus)
#ANALYSIS 1: FOCUS ONLY ON POST-PERIODS
#create subsetting variable for change in CC score
scust[CC_1==CC_2, ccdelgrp := 0]
scust[CC_1-1>=CC_2, ccdelgrp := 1]
#t-tests
temp <- scust[!is.na(ccdelgrp)]
t.test(temp[ccdelgrp==0,(SPEND_POST_15D_2-SPEND_POST_15D_1)],temp[ccdelgrp==1,(SPEND_POST_15D_2-SPEND_POST_15D_1)],conf.level = 0.99)
#assess differences in behavior across *groups* of people based on ccdelgrp
temp <- temp[!is.na(ccdelgrp), list(n = .N,
                                    avg_spend_post_1 = sum(SPEND_POST_15D_1,na.rm=T)/.N,
                                    avg_spend_post_2 = sum(SPEND_POST_15D_2,na.rm=T)/.N,
                                    sd_spend_post_1 = sd(SPEND_POST_15D_1),
                                    sd_spend_post_2 = sd(SPEND_POST_15D_2)),
              by=c("ccdelgrp")]
#calculate spend per person delta
temp[, sppdelta := avg_spend_post_2 - avg_spend_post_1]
temp[, sppvp := (((n-1)*(sd_spend_post_1^2) + (n-1)*(sd_spend_post_2^2)))/((n-1)+(n-1))]
temp[, sppse := sqrt(sppvp*(1/n + 1/n))]

##spend per person
tlabel <- "15-Day Spend-per-Person Delta by Change in CC Score"
xlabel <- "Delta in CC score between time 1 and time 2"
ylabel <- "Delta in post-15-day spending-per-person\nbetween time 1 and 2 (USD)"
xlabels <- c("Same","1pt Lower")
pdata <- temp
px <- temp[,ccdelgrp]
py <- temp[,sppdelta]
nvar <- temp[,n]
se <- temp[,sppse]
ggplot(data = pdata, aes(x = factor(px), y = py)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  geom_errorbar(aes(x = factor(px), ymin=py-se, ymax=py+se), width=0.1, size=0.5, color="blue") +
  scale_x_discrete(label=xlabels) + 
  ylim(c(-0.5,1)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) +
  theme(axis.text.x = element_text(face = "bold", size = 12)) +
  geom_text(size = 4.5, aes(label=round(py,2),y=0), stat= "identity", vjust = -1) +
  geom_text(size = 2.5, aes(label=paste0("n=",comma(nvar)),y=0), stat= "identity", vjust = 1.5)



###PART B

##subset to JUST those who dropped from TB and those who remained in TB

#SPLIT INTO TB CHANGE - CC
scust <- copy(scus)
#ANALYSIS 1: FOCUS ONLY ON POST-PERIODS
#create subsetting variable for change in CC score
scust[CC_1==7&CC_2==7, ccdelgrp := 0]
scust[CC_1==7&CC_2<7, ccdelgrp := 1]
#t-tests
temp <- scust[!is.na(ccdelgrp)]
t.test(temp[ccdelgrp==0,(SPEND_POST_15D_2-SPEND_POST_15D_1)],temp[ccdelgrp==1,(SPEND_POST_15D_2-SPEND_POST_15D_1)],conf.level = 0.99)
#assess differences in behavior across *groups* of people based on ccdelgrp
temp <- temp[!is.na(ccdelgrp), list(n = .N,
            avg_spend_post_1 = sum(SPEND_POST_15D_1,na.rm=T)/.N,
            avg_spend_post_2 = sum(SPEND_POST_15D_2,na.rm=T)/.N,
            sd_spend_post_1 = sd(SPEND_POST_15D_1),
            sd_spend_post_2 = sd(SPEND_POST_15D_2)),
        by=c("ccdelgrp")]
#calculate spend per person delta
temp[, sppdelta := avg_spend_post_2 - avg_spend_post_1]
temp[, sppvp := (((n-1)*(sd_spend_post_1^2) + (n-1)*(sd_spend_post_2^2)))/((n-1)+(n-1))]
temp[, sppse := sqrt(sppvp*(1/n + 1/n))]

##spend per person
tlabel <- "15-Day Spend-per-Person Delta by Change in CC Score"
xlabel <- "Delta in CC score between time 1 and time 2"
ylabel <- "Delta in post-15-day spending-per-person\nbetween time 1 and 2 (USD)"
xlabels <- c("Remained in TB","Fell from TB")
pdata <- temp
px <- temp[,ccdelgrp]
py <- temp[,sppdelta]
se <- temp[,sppse]
nvar <- temp[,n]
ggplot(data = pdata, aes(x = factor(px), y = py)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  geom_errorbar(aes(x = factor(px), ymin=py-se, ymax=py+se), width=0.1, size=0.5, color="blue") +
  scale_x_discrete(label=xlabels) + 
  ylim(c(-0.5,1)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) +
  theme(axis.text.x = element_text(face = "bold", size = 12)) +
  geom_text(size = 4.5, aes(label=round(py,2),y=0), stat= "identity", vjust = -1) +
  geom_text(size = 2.5, aes(label=paste0("n=",comma(nvar)),y=0), stat= "identity", vjust = 1.5)




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
                                    avg_spend_post_1 = sum(SPEND_POST_15D_1,na.rm=T)/.N,
                                    avg_spend_post_2 = sum(SPEND_POST_15D_2,na.rm=T)/.N,
                                    sd_spend_post_1 = sd(SPEND_POST_15D_1),
                                    sd_spend_post_2 = sd(SPEND_POST_15D_2)),
             by=c("ccdelgrp")]
#calculate spend per person delta
temp[, sppdelta := avg_spend_post_2 - avg_spend_post_1]
temp[, sppvp := (((n-1)*(sd_spend_post_1^2) + (n-1)*(sd_spend_post_2^2)))/((n-1)+(n-1))]
temp[, sppse := sqrt(sppvp*(1/n + 1/n))]

#use "same" as control
temp[ccdelgrp!="07-same", sppdeltafromsame := sppdelta - temp[ccdelgrp=="07-same",sppdelta]]
temp[ccdelgrp!="07-same", sppsefromsame := sppse - temp[ccdelgrp=="07-same",sppse]]
temp <- setorder(temp,ccdelgrp)

##spend per person
tlabel <- "15-Day Spend-per-Person Delta by Change in CC Score"
xlabel <- "Delta in CC score between time 1 and time 2,\n(net of delta for 'same' scoring customers)"
ylabel <- "Delta in post-15-day spending-per-person\nbetween time 1 and 2 (USD)"
xlabels <- c("-6pts","-5pts","-4pts","-3pts","-2pts","-1pt",
             "+1pt","+2pts","+3pts","+4pts","+5pts","+6pts")
pdata <- temp[ccdelgrp!="07-same"]
px <- temp[ccdelgrp!="07-same",ccdelgrp]
py <- temp[ccdelgrp!="07-same",sppdeltafromsame]
se <- temp[ccdelgrp!="07-same",sppsefromsame]
nvar <- temp[ccdelgrp!="07-same",n]
ggplot(data = pdata, aes(x = px, y = py)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  geom_errorbar(aes(x = factor(px), ymin=py-se, ymax=py+se), width=0.1, size=0.5, color="blue") +
  scale_x_discrete(label=xlabels) + ylim(c(-2.5,3.25)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) +
  theme(axis.text.x = element_text(face = "bold", size = 12)) +
  geom_text(size = 3, aes(label=round(py,2),y=0), stat= "identity", vjust = -1.25) +
  geom_text(size = 2.5, aes(label=paste0("n=",comma(nvar)),y=0), stat= "identity", vjust = 2)


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
                                     avg_spend_post_1 = sum(SPEND_POST_15D_1,na.rm=T)/.N,
                                     avg_spend_post_2 = sum(SPEND_POST_15D_2,na.rm=T)/.N,
                                     sd_spend_post_1 = sd(SPEND_POST_15D_1),
                                     sd_spend_post_2 = sd(SPEND_POST_15D_2)),
              by=c("ccdelgrp")]
#calculate spend per person delta
temp[, sppdelta := avg_spend_post_2 - avg_spend_post_1]
temp[, sppvp := (((n-1)*(sd_spend_post_1^2) + (n-1)*(sd_spend_post_2^2)))/((n-1)+(n-1))]
temp[, sppse := sqrt(sppvp*(1/n + 1/n))]

##spend per person
tlabel <- "15-Day Spend-per-Person Delta by Change in CC Score"
xlabel <- "Delta in CC score between time 1 and time 2"
ylabel <- "Delta in post-15-day spending-per-person\nbetween time 1 and 2 (USD)"
pdata <- temp
px <- temp[,ccdelgrp]
py <- temp[,sppdelta]
se <- temp[,sppse]
nvar <- temp[,n]
ggplot(data = pdata, aes(x = px, y = py)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  geom_errorbar(aes(x = factor(px), ymin=py-se, ymax=py+se), width=0.1, size=0.5, color="blue") +
  ylim(c(-2.5,3.5)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) +
  theme(axis.text.x = element_text(face = "bold", size = 7, angle=45)) +
  geom_text(size = 3, aes(label=round(py,2),y=0), stat= "identity", vjust = -1.5) +
  geom_text(size = 1.5, aes(label=paste0("n=",comma(nvar)),y=0), stat= "identity", vjust = 2.25)



#SPLIT INTO HIGHER VS LOWER
scust <- copy(scus)
#scust <- scust[STORE_NUM_1==STORE_NUM_2&TRANS_HR_1>=4&TRANS_HR_1<=12&TRANS_HR_2>=4&TRANS_HR_2<=12]
#create subsetting variable for change in CC score
scust[CC_1==CC_2, ccdelgrp := "02-same"]
scust[CC_1>CC_2, ccdelgrp := "01-CC2_lower"]
scust[CC_1<CC_2, ccdelgrp := "03-CC2_higher"]
#t-tests
temp <- scust[!is.na(ccdelgrp)]
t.test(temp[ccdelgrp=="01-CC2_lower",(SPEND_POST_15D_2-SPEND_POST_15D_1)],temp[ccdelgrp=="03-CC2_higher",(SPEND_POST_15D_2-SPEND_POST_15D_1)],conf.level = 0.99)
#assess differences in behavior across *groups* of people based on ccdelgrp
temp <- scust[!is.na(ccdelgrp), list(n = .N,
                                     avg_spend_post_1 = sum(SPEND_POST_15D_1,na.rm=T)/.N,
                                     avg_spend_post_2 = sum(SPEND_POST_15D_2,na.rm=T)/.N,
                                     sd_spend_post_1 = sd(SPEND_POST_15D_1),
                                     sd_spend_post_2 = sd(SPEND_POST_15D_2)),
              by=c("ccdelgrp")]
#calculate spend per person delta
temp[, sppdelta := avg_spend_post_2 - avg_spend_post_1]
temp[, sppvp := (((n-1)*(sd_spend_post_1^2) + (n-1)*(sd_spend_post_2^2)))/((n-1)+(n-1))]
temp[, sppse := sqrt(sppvp*(1/n + 1/n))]

#use "same" as control
temp[ccdelgrp!="02-same", sppdeltafromsame := sppdelta - temp[ccdelgrp=="02-same",sppdelta]]
temp[ccdelgrp!="02-same", sppsefromsame := abs(sppse) - temp[ccdelgrp=="02-same",abs(sppse)]]
temp <- setorder(temp,ccdelgrp)

##spend per person
tlabel <- "15-Day Spend-per-Person Delta by Change in CC Score"
xlabel <- "Delta in CC score between time 1 and time 2,\n(net of delta for 'same' scoring customers)"
ylabel <- "Delta in post-15-day spending-per-person\nbetween time 1 and 2 (USD)"
xlabels <- c("Lower","Higher")
#data
pdata <- temp[ccdelgrp!="02-same"]
px <- temp[ccdelgrp!="02-same",ccdelgrp]
py <- temp[ccdelgrp!="02-same",sppdeltafromsame]
se <- temp[ccdelgrp!="02-same",sppsefromsame]
nvar <- temp[ccdelgrp!="02-same",n]
#plot
ggplot(data = pdata, aes(x = factor(px), y = py)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  geom_errorbar(aes(x = factor(px), ymin=py-abs(se), ymax=py+abs(se)), width=0.1, size=0.5, color="blue") +
  scale_x_discrete(label=xlabels) +
  ylim(c(-0.5,1)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) +
  theme(axis.text.x = element_text(face = "bold", size = 12)) +
  geom_text(size = 4.5, aes(label=round(py,2),y=0), stat= "identity", vjust = -1) +
  geom_text(size = 2.5, aes(label=paste0("n=",comma(nvar)),y=0), stat= "identity", vjust = 1.5)

# temp[, ymin := sppdelta-sppse]
# temp[, ymax := sppdelta+sppse]
