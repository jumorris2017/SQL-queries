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


#BASED ON WORTH TIME 1
temp <- scus[!is.na(WORTH_1), list(n = .N,
                                VISITS_POST_15D_1 = sum(VISITS_POST_15D_1,na.rm=T),
                                VISITS_POST_15D_2 = sum(VISITS_POST_15D_2,na.rm=T),
                                SPEND_POST_15D_1 = sum(SPEND_POST_15D_1,na.rm=T),
                                SPEND_POST_15D_2 = sum(SPEND_POST_15D_2,na.rm=T),
                                avg_vis_post_1 = sum(VISITS_POST_15D_1,na.rm=T)/.N,
                                avg_vis_post_2 = sum(VISITS_POST_15D_2,na.rm=T)/.N,
                                avg_spend_post_1 = sum(SPEND_POST_15D_1,na.rm=T)/.N,
                                avg_spend_post_2 = sum(SPEND_POST_15D_2,na.rm=T)/.N),
             by=c("WORTH_1")]
#calculate spend per visits
temp[, spv_post_1 := SPEND_POST_15D_1/VISITS_POST_15D_1]
temp[, spv_post_2 := SPEND_POST_15D_2/VISITS_POST_15D_2]
#
tlabel <- "15-Day Spend-Per-Visit (SPV) after time 1 by Worth time 1"
xlabel <- "Time 1 Worth score"
ylabel <- "15-Day SPV (USD)"
xlabels <- c(sort(unique(temp[WORTH_1<=7,WORTH_1])))
p1 <- ggplot(data = temp[WORTH_1<=7], aes(x = factor(WORTH_1), y = spv_post_1)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + 
  theme(axis.text.x = element_text(face = "bold", size = 12, vjust = 1.5)) 
#
tlabel <- "15-Day Visits-Per-Person (VPP) after time 1 by Worth time 1"
xlabel <- "Time 1 Worth score"
ylabel <- "15-Day VPP"
xlabels <- c(sort(unique(temp[WORTH_1<=7,WORTH_1])))
p2 <- ggplot(data = temp[WORTH_1<=7], aes(x = factor(WORTH_1), y = avg_vis_post_1)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + 
  theme(axis.text.x = element_text(face = "bold", size = 12, vjust = 1.5)) 
#
tlabel <- "15-Day Total Spend after time 1 by Worth time 1"
xlabel <- "Time 1 Worth score"
ylabel <- "15-Day Total Spend (USD)"
p3 <- ggplot(data = temp[WORTH_1<=7], aes(x = factor(WORTH_1), y = SPEND_POST_15D_1)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + 
  scale_y_continuous(labels=comma) +
  theme(axis.text.x = element_text(face = "bold", size = 12, vjust = 1.5)) 
#
tlabel <- "15-Day Total Visits after time 1 by Worth time 1"
xlabel <- "Time 1 Worth score"
ylabel <- "15-Day Total Visits"
p4 <- ggplot(data = temp[WORTH_1<=7], aes(x = factor(WORTH_1), y = VISITS_POST_15D_1)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + 
  scale_y_continuous(labels=comma) +
  theme(axis.text.x = element_text(face = "bold", size = 12, vjust = 1.5)) 
#
tlabel <- "15-Day Average Spend-per-Person (SPP) by Worth time 1"
xlabel <- "Time 1 Worth score"
ylabel <- "15-Day SPP (USD)"
p5 <- ggplot(data = temp[WORTH_1<=7], aes(x = factor(WORTH_1), y = avg_spend_post_1)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + 
  scale_y_continuous(labels=comma) +
  theme(axis.text.x = element_text(face = "bold", size = 12, vjust = 1.5)) 
#
tlabel <- "Transaction count by Worth time 1"
xlabel <- "Time 1 Worth score"
ylabel <- "N"
p6 <- ggplot(data = temp[WORTH_1<=7], aes(x = factor(WORTH_1), y = n)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + 
  scale_y_continuous(labels=comma, breaks = scales::pretty_breaks(n = 6)) 

#patchwork them together
(p1 | p2 | p3) / (p4 | p5 | p6)


#BASED ON SPEED TIME 1
temp <- scus[!is.na(SPEED_1), list(n = .N,
                                   VISITS_POST_15D_1 = sum(VISITS_POST_15D_1,na.rm=T),
                                   VISITS_POST_15D_2 = sum(VISITS_POST_15D_2,na.rm=T),
                                   SPEND_POST_15D_1 = sum(SPEND_POST_15D_1,na.rm=T),
                                   SPEND_POST_15D_2 = sum(SPEND_POST_15D_2,na.rm=T),
                                   avg_vis_post_1 = sum(VISITS_POST_15D_1,na.rm=T)/.N,
                                   avg_vis_post_2 = sum(VISITS_POST_15D_2,na.rm=T)/.N,
                                   avg_spend_post_1 = sum(SPEND_POST_15D_1,na.rm=T)/.N,
                                   avg_spend_post_2 = sum(SPEND_POST_15D_2,na.rm=T)/.N),
             by=c("SPEED_1")]
#calculate spend per visits
temp[, spv_post_1 := SPEND_POST_15D_1/VISITS_POST_15D_1]
temp[, spv_post_2 := SPEND_POST_15D_2/VISITS_POST_15D_2]
#
tlabel <- "15-Day Spend-Per-Visit (SPV) after time 1 by Speed time 1"
xlabel <- "Time 1 Speed score"
ylabel <- "15-Day SPV (USD)"
xlabels <- c(sort(unique(temp[SPEED_1<=7,SPEED_1])))
p1 <- ggplot(data = temp[SPEED_1<=7], aes(x = factor(SPEED_1), y = spv_post_1)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + 
  theme(axis.text.x = element_text(face = "bold", size = 12, vjust = 1.5)) 
#
tlabel <- "15-Day Visits-Per-Person (VPP) after time 1 by Speed time 1"
xlabel <- "Time 1 Speed score"
ylabel <- "15-Day VPP"
xlabels <- c(sort(unique(temp[SPEED_1<=7,SPEED_1])))
p2 <- ggplot(data = temp[SPEED_1<=7], aes(x = factor(SPEED_1), y = avg_vis_post_1)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + 
  theme(axis.text.x = element_text(face = "bold", size = 12, vjust = 1.5)) 
#
tlabel <- "15-Day Total Spend after time 1 by Speed time 1"
xlabel <- "Time 1 Speed score"
ylabel <- "15-Day Total Spend (USD)"
p3 <- ggplot(data = temp[SPEED_1<=7], aes(x = factor(SPEED_1), y = SPEND_POST_15D_1)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + 
  scale_y_continuous(labels=comma) +
  theme(axis.text.x = element_text(face = "bold", size = 12, vjust = 1.5)) 
#
tlabel <- "15-Day Total Visits after time 1 by Speed time 1"
xlabel <- "Time 1 Speed score"
ylabel <- "15-Day Total Visits"
p4 <- ggplot(data = temp[SPEED_1<=7], aes(x = factor(SPEED_1), y = VISITS_POST_15D_1)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + 
  scale_y_continuous(labels=comma) +
  theme(axis.text.x = element_text(face = "bold", size = 12, vjust = 1.5)) 
#
tlabel <- "15-Day Average Spend-per-Person (SPP) by Speed time 1"
xlabel <- "Time 1 Speed score"
ylabel <- "15-Day SPP (USD)"
p5 <- ggplot(data = temp[SPEED_1<=7], aes(x = factor(SPEED_1), y = avg_spend_post_1)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + 
  scale_y_continuous(labels=comma) +
  theme(axis.text.x = element_text(face = "bold", size = 12, vjust = 1.5)) 
#
tlabel <- "Transaction count by Speed time 1"
xlabel <- "Time 1 Speed score"
ylabel <- "N"
p6 <- ggplot(data = temp[SPEED_1<=7], aes(x = factor(SPEED_1), y = n)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + 
  scale_y_continuous(labels=comma, breaks = scales::pretty_breaks(n = 6)) 

#patchwork them together
(p1 | p2 | p3) / (p4 | p5 | p6)


#BASED ON ACCURACY TIME 1
temp <- scus[!is.na(ACCR_1), list(n = .N,
                                   VISITS_POST_15D_1 = sum(VISITS_POST_15D_1,na.rm=T),
                                   VISITS_POST_15D_2 = sum(VISITS_POST_15D_2,na.rm=T),
                                   SPEND_POST_15D_1 = sum(SPEND_POST_15D_1,na.rm=T),
                                   SPEND_POST_15D_2 = sum(SPEND_POST_15D_2,na.rm=T),
                                   avg_vis_post_1 = sum(VISITS_POST_15D_1,na.rm=T)/.N,
                                   avg_vis_post_2 = sum(VISITS_POST_15D_2,na.rm=T)/.N,
                                   avg_spend_post_1 = sum(SPEND_POST_15D_1,na.rm=T)/.N,
                                   avg_spend_post_2 = sum(SPEND_POST_15D_2,na.rm=T)/.N),
             by=c("ACCR_1")]
#calculate spend per visits
temp[, spv_post_1 := SPEND_POST_15D_1/VISITS_POST_15D_1]
temp[, spv_post_2 := SPEND_POST_15D_2/VISITS_POST_15D_2]
#
tlabel <- "15-Day Spend-Per-Visit (SPV) after time 1 by Accuracy time 1"
xlabel <- "Time 1 Accuracy score"
ylabel <- "15-Day SPV (USD)"
xlabels <- c(sort(unique(temp[ACCR_1<=7,ACCR_1])))
p1 <- ggplot(data = temp[ACCR_1<=7], aes(x = factor(ACCR_1), y = spv_post_1)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + 
  theme(axis.text.x = element_text(face = "bold", size = 12, vjust = 1.5)) 
#
tlabel <- "15-Day Visits-Per-Person (VPP) after time 1 by Accuracy time 1"
xlabel <- "Time 1 Accuracy score"
ylabel <- "15-Day VPP"
xlabels <- c(sort(unique(temp[ACCR_1<=7,ACCR_1])))
p2 <- ggplot(data = temp[ACCR_1<=7], aes(x = factor(ACCR_1), y = avg_vis_post_1)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + 
  theme(axis.text.x = element_text(face = "bold", size = 12, vjust = 1.5)) 
#
tlabel <- "15-Day Total Spend after time 1 by Accuracy time 1"
xlabel <- "Time 1 Accuracy score"
ylabel <- "15-Day Total Spend (USD)"
p3 <- ggplot(data = temp[ACCR_1<=7], aes(x = factor(ACCR_1), y = SPEND_POST_15D_1)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + 
  scale_y_continuous(labels=comma) +
  theme(axis.text.x = element_text(face = "bold", size = 12, vjust = 1.5)) 
#
tlabel <- "15-Day Total Visits after time 1 by Accuracy time 1"
xlabel <- "Time 1 Accuracy score"
ylabel <- "15-Day Total Visits"
p4 <- ggplot(data = temp[ACCR_1<=7], aes(x = factor(ACCR_1), y = VISITS_POST_15D_1)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + 
  scale_y_continuous(labels=comma) +
  theme(axis.text.x = element_text(face = "bold", size = 12, vjust = 1.5)) 
#
tlabel <- "15-Day Average Spend-per-Person (SPP) by Accuracy time 1"
xlabel <- "Time 1 Accuracy score"
ylabel <- "15-Day SPP (USD)"
p5 <- ggplot(data = temp[ACCR_1<=7], aes(x = factor(ACCR_1), y = avg_spend_post_1)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + 
  scale_y_continuous(labels=comma) +
  theme(axis.text.x = element_text(face = "bold", size = 12, vjust = 1.5)) 
#
tlabel <- "Transaction count by Accuracy time 1"
xlabel <- "Time 1 Accuracy score"
ylabel <- "N"
p6 <- ggplot(data = temp[ACCR_1<=7], aes(x = factor(ACCR_1), y = n)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + 
  scale_y_continuous(labels=comma, breaks = scales::pretty_breaks(n = 6)) 

#patchwork them together
(p1 | p2 | p3) / (p4 | p5 | p6)


#BASED ON CLEAN TIME 1
temp <- scus[!is.na(CLEAN_1), list(n = .N,
                                  VISITS_POST_15D_1 = sum(VISITS_POST_15D_1,na.rm=T),
                                  VISITS_POST_15D_2 = sum(VISITS_POST_15D_2,na.rm=T),
                                  SPEND_POST_15D_1 = sum(SPEND_POST_15D_1,na.rm=T),
                                  SPEND_POST_15D_2 = sum(SPEND_POST_15D_2,na.rm=T),
                                  avg_vis_post_1 = sum(VISITS_POST_15D_1,na.rm=T)/.N,
                                  avg_vis_post_2 = sum(VISITS_POST_15D_2,na.rm=T)/.N,
                                  avg_spend_post_1 = sum(SPEND_POST_15D_1,na.rm=T)/.N,
                                  avg_spend_post_2 = sum(SPEND_POST_15D_2,na.rm=T)/.N),
             by=c("CLEAN_1")]
#calculate spend per visits
temp[, spv_post_1 := SPEND_POST_15D_1/VISITS_POST_15D_1]
temp[, spv_post_2 := SPEND_POST_15D_2/VISITS_POST_15D_2]
#
tlabel <- "15-Day Spend-Per-Visit (SPV) after time 1 by Cleanliness time 1"
xlabel <- "Time 1 Cleanliness score"
ylabel <- "15-Day SPV (USD)"
xlabels <- c(sort(unique(temp[CLEAN_1<=7,CLEAN_1])))
p1 <- ggplot(data = temp[CLEAN_1<=7], aes(x = factor(CLEAN_1), y = spv_post_1)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + 
  theme(axis.text.x = element_text(face = "bold", size = 12, vjust = 1.5)) 
#
tlabel <- "15-Day Visits-Per-Person (VPP) after time 1 by Cleanliness time 1"
xlabel <- "Time 1 Cleanliness score"
ylabel <- "15-Day VPP"
xlabels <- c(sort(unique(temp[CLEAN_1<=7,CLEAN_1])))
p2 <- ggplot(data = temp[CLEAN_1<=7], aes(x = factor(CLEAN_1), y = avg_vis_post_1)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + 
  theme(axis.text.x = element_text(face = "bold", size = 12, vjust = 1.5)) 
#
tlabel <- "15-Day Total Spend after time 1 by Cleanliness time 1"
xlabel <- "Time 1 Cleanliness score"
ylabel <- "15-Day Total Spend (USD)"
p3 <- ggplot(data = temp[CLEAN_1<=7], aes(x = factor(CLEAN_1), y = SPEND_POST_15D_1)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + 
  scale_y_continuous(labels=comma) +
  theme(axis.text.x = element_text(face = "bold", size = 12, vjust = 1.5)) 
#
tlabel <- "15-Day Total Visits after time 1 by Cleanliness time 1"
xlabel <- "Time 1 Cleanliness score"
ylabel <- "15-Day Total Visits"
p4 <- ggplot(data = temp[CLEAN_1<=7], aes(x = factor(CLEAN_1), y = VISITS_POST_15D_1)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + 
  scale_y_continuous(labels=comma) +
  theme(axis.text.x = element_text(face = "bold", size = 12, vjust = 1.5)) 
#
tlabel <- "15-Day Average Spend-per-Person (SPP) by Cleanliness time 1"
xlabel <- "Time 1 Cleanliness score"
ylabel <- "15-Day SPP (USD)"
p5 <- ggplot(data = temp[CLEAN_1<=7], aes(x = factor(CLEAN_1), y = avg_spend_post_1)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + 
  scale_y_continuous(labels=comma) +
  theme(axis.text.x = element_text(face = "bold", size = 12, vjust = 1.5)) 
#
tlabel <- "Transaction count by Cleanliness time 1"
xlabel <- "Time 1 Cleanliness score"
ylabel <- "N"
p6 <- ggplot(data = temp[CLEAN_1<=7], aes(x = factor(CLEAN_1), y = n)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + 
  scale_y_continuous(labels=comma, breaks = scales::pretty_breaks(n = 6)) 

#patchwork them together
(p1 | p2 | p3) / (p4 | p5 | p6)


#BASED ON ABVBYD TIME 1
temp <- scus[!is.na(ABVBYD_1), list(n = .N,
                                   VISITS_POST_15D_1 = sum(VISITS_POST_15D_1,na.rm=T),
                                   VISITS_POST_15D_2 = sum(VISITS_POST_15D_2,na.rm=T),
                                   SPEND_POST_15D_1 = sum(SPEND_POST_15D_1,na.rm=T),
                                   SPEND_POST_15D_2 = sum(SPEND_POST_15D_2,na.rm=T),
                                   avg_vis_post_1 = sum(VISITS_POST_15D_1,na.rm=T)/.N,
                                   avg_vis_post_2 = sum(VISITS_POST_15D_2,na.rm=T)/.N,
                                   avg_spend_post_1 = sum(SPEND_POST_15D_1,na.rm=T)/.N,
                                   avg_spend_post_2 = sum(SPEND_POST_15D_2,na.rm=T)/.N),
             by=c("ABVBYD_1")]
#calculate spend per visits
temp[, spv_post_1 := SPEND_POST_15D_1/VISITS_POST_15D_1]
temp[, spv_post_2 := SPEND_POST_15D_2/VISITS_POST_15D_2]
#
tlabel <- "15-Day Spend-Per-Visit (SPV) after time 1 by Above & Beyond time 1"
xlabel <- "Time 1 Above & Beyond score"
ylabel <- "15-Day SPV (USD)"
xlabels <- c(sort(unique(temp[ABVBYD_1<=7,ABVBYD_1])))
p1 <- ggplot(data = temp[ABVBYD_1<=7], aes(x = factor(ABVBYD_1), y = spv_post_1)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + 
  theme(axis.text.x = element_text(face = "bold", size = 12, vjust = 1.5)) 
#
tlabel <- "15-Day Visits-Per-Person (VPP) after time 1 by Above & Beyond time 1"
xlabel <- "Time 1 Above & Beyond score"
ylabel <- "15-Day VPP"
xlabels <- c(sort(unique(temp[ABVBYD_1<=7,ABVBYD_1])))
p2 <- ggplot(data = temp[ABVBYD_1<=7], aes(x = factor(ABVBYD_1), y = avg_vis_post_1)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + 
  theme(axis.text.x = element_text(face = "bold", size = 12, vjust = 1.5)) 
#
tlabel <- "15-Day Total Spend after time 1 by Above & Beyond time 1"
xlabel <- "Time 1 Above & Beyond score"
ylabel <- "15-Day Total Spend (USD)"
p3 <- ggplot(data = temp[ABVBYD_1<=7], aes(x = factor(ABVBYD_1), y = SPEND_POST_15D_1)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + 
  scale_y_continuous(labels=comma) +
  theme(axis.text.x = element_text(face = "bold", size = 12, vjust = 1.5)) 
#
tlabel <- "15-Day Total Visits after time 1 by Above & Beyond time 1"
xlabel <- "Time 1 Above & Beyond score"
ylabel <- "15-Day Total Visits"
p4 <- ggplot(data = temp[ABVBYD_1<=7], aes(x = factor(ABVBYD_1), y = VISITS_POST_15D_1)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + 
  scale_y_continuous(labels=comma) +
  theme(axis.text.x = element_text(face = "bold", size = 12, vjust = 1.5)) 
#
tlabel <- "15-Day Average Spend-per-Person (SPP) by Above & Beyond time 1"
xlabel <- "Time 1 Above & Beyond score"
ylabel <- "15-Day SPP (USD)"
p5 <- ggplot(data = temp[ABVBYD_1<=7], aes(x = factor(ABVBYD_1), y = avg_spend_post_1)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + 
  scale_y_continuous(labels=comma) +
  theme(axis.text.x = element_text(face = "bold", size = 12, vjust = 1.5)) 
#
tlabel <- "Transaction count by Above & Beyond time 1"
xlabel <- "Time 1 Above & Beyond score"
ylabel <- "N"
p6 <- ggplot(data = temp[ABVBYD_1<=7], aes(x = factor(ABVBYD_1), y = n)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + 
  scale_y_continuous(labels=comma, breaks = scales::pretty_breaks(n = 6)) 

#patchwork them together
(p1 | p2 | p3) / (p4 | p5 | p6)


#BASED ON BEV TIME 1
temp <- scus[!is.na(BEV_1), list(n = .N,
                                    VISITS_POST_15D_1 = sum(VISITS_POST_15D_1,na.rm=T),
                                    VISITS_POST_15D_2 = sum(VISITS_POST_15D_2,na.rm=T),
                                    SPEND_POST_15D_1 = sum(SPEND_POST_15D_1,na.rm=T),
                                    SPEND_POST_15D_2 = sum(SPEND_POST_15D_2,na.rm=T),
                                    avg_vis_post_1 = sum(VISITS_POST_15D_1,na.rm=T)/.N,
                                    avg_vis_post_2 = sum(VISITS_POST_15D_2,na.rm=T)/.N,
                                    avg_spend_post_1 = sum(SPEND_POST_15D_1,na.rm=T)/.N,
                                    avg_spend_post_2 = sum(SPEND_POST_15D_2,na.rm=T)/.N),
             by=c("BEV_1")]
#calculate spend per visits
temp[, spv_post_1 := SPEND_POST_15D_1/VISITS_POST_15D_1]
temp[, spv_post_2 := SPEND_POST_15D_2/VISITS_POST_15D_2]
#
tlabel <- "15-Day Spend-Per-Visit (SPV) after time 1 by Beverage time 1"
xlabel <- "Time 1 Beverage score"
ylabel <- "15-Day SPV (USD)"
xlabels <- c(sort(unique(temp[BEV_1<=7,BEV_1])))
p1 <- ggplot(data = temp[BEV_1<=7], aes(x = factor(BEV_1), y = spv_post_1)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + 
  theme(axis.text.x = element_text(face = "bold", size = 12, vjust = 1.5)) 
#
tlabel <- "15-Day Visits-Per-Person (VPP) after time 1 by Beverage time 1"
xlabel <- "Time 1 Beverage score"
ylabel <- "15-Day VPP"
xlabels <- c(sort(unique(temp[BEV_1<=7,BEV_1])))
p2 <- ggplot(data = temp[BEV_1<=7], aes(x = factor(BEV_1), y = avg_vis_post_1)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + 
  theme(axis.text.x = element_text(face = "bold", size = 12, vjust = 1.5)) 
#
tlabel <- "15-Day Total Spend after time 1 by Beverage time 1"
xlabel <- "Time 1 Beverage score"
ylabel <- "15-Day Total Spend (USD)"
p3 <- ggplot(data = temp[BEV_1<=7], aes(x = factor(BEV_1), y = SPEND_POST_15D_1)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + 
  scale_y_continuous(labels=comma) +
  theme(axis.text.x = element_text(face = "bold", size = 12, vjust = 1.5)) 
#
tlabel <- "15-Day Total Visits after time 1 by Beverage time 1"
xlabel <- "Time 1 Beverage score"
ylabel <- "15-Day Total Visits"
p4 <- ggplot(data = temp[BEV_1<=7], aes(x = factor(BEV_1), y = VISITS_POST_15D_1)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + 
  scale_y_continuous(labels=comma) +
  theme(axis.text.x = element_text(face = "bold", size = 12, vjust = 1.5)) 
#
tlabel <- "15-Day Average Spend-per-Person (SPP) by Beverage time 1"
xlabel <- "Time 1 Beverage score"
ylabel <- "15-Day SPP (USD)"
p5 <- ggplot(data = temp[BEV_1<=7], aes(x = factor(BEV_1), y = avg_spend_post_1)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + 
  scale_y_continuous(labels=comma) +
  theme(axis.text.x = element_text(face = "bold", size = 12, vjust = 1.5)) 
#
tlabel <- "Transaction count by Beverage time 1"
xlabel <- "Time 1 Beverage score"
ylabel <- "N"
p6 <- ggplot(data = temp[BEV_1<=7], aes(x = factor(BEV_1), y = n)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + 
  scale_y_continuous(labels=comma, breaks = scales::pretty_breaks(n = 6)) 

#patchwork them together
(p1 | p2 | p3) / (p4 | p5 | p6)


#BASED ON FOOD TIME 1
temp <- scus[!is.na(FOOD_1), list(n = .N,
                                 VISITS_POST_15D_1 = sum(VISITS_POST_15D_1,na.rm=T),
                                 VISITS_POST_15D_2 = sum(VISITS_POST_15D_2,na.rm=T),
                                 SPEND_POST_15D_1 = sum(SPEND_POST_15D_1,na.rm=T),
                                 SPEND_POST_15D_2 = sum(SPEND_POST_15D_2,na.rm=T),
                                 avg_vis_post_1 = sum(VISITS_POST_15D_1,na.rm=T)/.N,
                                 avg_vis_post_2 = sum(VISITS_POST_15D_2,na.rm=T)/.N,
                                 avg_spend_post_1 = sum(SPEND_POST_15D_1,na.rm=T)/.N,
                                 avg_spend_post_2 = sum(SPEND_POST_15D_2,na.rm=T)/.N),
             by=c("FOOD_1")]
#calculate spend per visits
temp[, spv_post_1 := SPEND_POST_15D_1/VISITS_POST_15D_1]
temp[, spv_post_2 := SPEND_POST_15D_2/VISITS_POST_15D_2]
#
tlabel <- "15-Day Spend-Per-Visit (SPV) after time 1 by Food time 1"
xlabel <- "Time 1 Food score"
ylabel <- "15-Day SPV (USD)"
xlabels <- c(sort(unique(temp[FOOD_1<=7,FOOD_1])))
p1 <- ggplot(data = temp[FOOD_1<=7], aes(x = factor(FOOD_1), y = spv_post_1)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + 
  theme(axis.text.x = element_text(face = "bold", size = 12, vjust = 1.5)) 
#
tlabel <- "15-Day Visits-Per-Person (VPP) after time 1 by Food time 1"
xlabel <- "Time 1 Food score"
ylabel <- "15-Day VPP"
xlabels <- c(sort(unique(temp[FOOD_1<=7,FOOD_1])))
p2 <- ggplot(data = temp[FOOD_1<=7], aes(x = factor(FOOD_1), y = avg_vis_post_1)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + 
  theme(axis.text.x = element_text(face = "bold", size = 12, vjust = 1.5)) 
#
tlabel <- "15-Day Total Spend after time 1 by Food time 1"
xlabel <- "Time 1 Food score"
ylabel <- "15-Day Total Spend (USD)"
p3 <- ggplot(data = temp[FOOD_1<=7], aes(x = factor(FOOD_1), y = SPEND_POST_15D_1)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + 
  scale_y_continuous(labels=comma) +
  theme(axis.text.x = element_text(face = "bold", size = 12, vjust = 1.5)) 
#
tlabel <- "15-Day Total Visits after time 1 by Food time 1"
xlabel <- "Time 1 Food score"
ylabel <- "15-Day Total Visits"
p4 <- ggplot(data = temp[FOOD_1<=7], aes(x = factor(FOOD_1), y = VISITS_POST_15D_1)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + 
  scale_y_continuous(labels=comma) +
  theme(axis.text.x = element_text(face = "bold", size = 12, vjust = 1.5)) 
#
tlabel <- "15-Day Average Spend-per-Person (SPP) by Food time 1"
xlabel <- "Time 1 Food score"
ylabel <- "15-Day SPP (USD)"
p5 <- ggplot(data = temp[FOOD_1<=7], aes(x = factor(FOOD_1), y = avg_spend_post_1)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + 
  scale_y_continuous(labels=comma) +
  theme(axis.text.x = element_text(face = "bold", size = 12, vjust = 1.5)) 
#
tlabel <- "Transaction count by Food time 1"
xlabel <- "Time 1 Food score"
ylabel <- "N"
p6 <- ggplot(data = temp[FOOD_1<=7], aes(x = factor(FOOD_1), y = n)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + 
  scale_y_continuous(labels=comma, breaks = scales::pretty_breaks(n = 6)) 

#patchwork them together
(p1 | p2 | p3) / (p4 | p5 | p6)



#BASED ON RETURN TIME 1
temp <- scus[!is.na(RETURN_1), list(n = .N,
                                  VISITS_POST_15D_1 = sum(VISITS_POST_15D_1,na.rm=T),
                                  VISITS_POST_15D_2 = sum(VISITS_POST_15D_2,na.rm=T),
                                  SPEND_POST_15D_1 = sum(SPEND_POST_15D_1,na.rm=T),
                                  SPEND_POST_15D_2 = sum(SPEND_POST_15D_2,na.rm=T),
                                  avg_vis_post_1 = sum(VISITS_POST_15D_1,na.rm=T)/.N,
                                  avg_vis_post_2 = sum(VISITS_POST_15D_2,na.rm=T)/.N,
                                  avg_spend_post_1 = sum(SPEND_POST_15D_1,na.rm=T)/.N,
                                  avg_spend_post_2 = sum(SPEND_POST_15D_2,na.rm=T)/.N),
             by=c("RETURN_1")]
#calculate spend per visits
temp[, spv_post_1 := SPEND_POST_15D_1/VISITS_POST_15D_1]
temp[, spv_post_2 := SPEND_POST_15D_2/VISITS_POST_15D_2]
#
tlabel <- "15-Day Spend-Per-Visit (SPV) after time 1 by Intent to Return time 1"
xlabel <- "Time 1 Intent to Return score"
ylabel <- "15-Day SPV (USD)"
xlabels <- c(sort(unique(temp[RETURN_1<=5,RETURN_1])))
p1 <- ggplot(data = temp[RETURN_1<=5], aes(x = factor(RETURN_1), y = spv_post_1)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + 
  theme(axis.text.x = element_text(face = "bold", size = 12, vjust = 1.5)) 
#
tlabel <- "15-Day Visits-Per-Person (VPP) after time 1 by Intent to Return time 1"
xlabel <- "Time 1 Intent to Return score"
ylabel <- "15-Day VPP"
xlabels <- c(sort(unique(temp[RETURN_1<=5,RETURN_1])))
p2 <- ggplot(data = temp[RETURN_1<=5], aes(x = factor(RETURN_1), y = avg_vis_post_1)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + 
  theme(axis.text.x = element_text(face = "bold", size = 12, vjust = 1.5)) 
#
tlabel <- "15-Day Total Spend after time 1 by Intent to Return time 1"
xlabel <- "Time 1 Intent to Return score"
ylabel <- "15-Day Total Spend (USD)"
p3 <- ggplot(data = temp[RETURN_1<=5], aes(x = factor(RETURN_1), y = SPEND_POST_15D_1)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + 
  scale_y_continuous(labels=comma) +
  theme(axis.text.x = element_text(face = "bold", size = 12, vjust = 1.5)) 
#
tlabel <- "15-Day Total Visits after time 1 by Intent to Return time 1"
xlabel <- "Time 1 Intent to Return score"
ylabel <- "15-Day Total Visits"
p4 <- ggplot(data = temp[RETURN_1<=5], aes(x = factor(RETURN_1), y = VISITS_POST_15D_1)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + 
  scale_y_continuous(labels=comma) +
  theme(axis.text.x = element_text(face = "bold", size = 12, vjust = 1.5)) 
#
tlabel <- "15-Day Average Spend-per-Person (SPP) by Intent to Return time 1"
xlabel <- "Time 1 Intent to Return score"
ylabel <- "15-Day SPP (USD)"
p5 <- ggplot(data = temp[RETURN_1<=5], aes(x = factor(RETURN_1), y = avg_spend_post_1)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + 
  scale_y_continuous(labels=comma) +
  theme(axis.text.x = element_text(face = "bold", size = 12, vjust = 1.5)) 
#
tlabel <- "Transaction count by Intent to Return time 1"
xlabel <- "Time 1 Intent to Return score"
ylabel <- "N"
p6 <- ggplot(data = temp[RETURN_1<=5], aes(x = factor(RETURN_1), y = n)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  xlab(xlabel) + ylab(ylabel) + 
  scale_y_continuous(labels=comma, breaks = scales::pretty_breaks(n = 6)) 

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
                                     avg_spend_post_2 = sum(SPEND_POST_15D_2,na.rm=T)/.N),
              by=c("ccdelgrp")]
#calculate spend per person delta
temp[, sppdelta := avg_spend_post_2 - avg_spend_post_1]
##spend per person
tlabel <- "15-Day Spend-per-Person Delta by Change in CC Score"
xlabel <- "Delta in CC score between time 1 and time 2"
ylabel <- "Delta in post-15-day spending-per-person\nbetween time 1 and 2 (USD)"
xlabels <- c("Same","1pt Lower")
pdata <- temp
px <- temp[,ccdelgrp]
py <- temp[,sppdelta]
nvar <- temp[,n]
ggplot(data = pdata, aes(x = factor(px), y = py)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  scale_x_discrete(label=xlabels) + 
  ylim(c(-1,0.5)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) +
  theme(axis.text.x = element_text(face = "bold", size = 12)) +
  geom_text(size = 4.5, aes(label=round(py,2),y=0), stat= "identity", vjust = -1) +
  geom_text(size = 2.5, aes(label=paste0("n=",comma(nvar)),y=0), stat= "identity", vjust = 1.5)


#SPLIT INTO POINT CHANGE - WP
scust <- copy(scus)
#ANALYSIS 1: FOCUS ONLY ON POST-PERIODS
#create subsetting variable for change in WP score
scust[WORTH_1==WORTH_2, wpdelgrp := 0]
scust[WORTH_1-1>=WORTH_2, wpdelgrp := 1]
#t-tests
temp <- scust[!is.na(wpdelgrp)]
t.test(temp[wpdelgrp==0,(SPEND_POST_15D_2-SPEND_POST_15D_1)],temp[wpdelgrp==1,(SPEND_POST_15D_2-SPEND_POST_15D_1)],conf.level = 0.99)
#assess differences in behavior across *groups* of people based on wpdelgrp
temp <- temp[!is.na(wpdelgrp), list(n = .N,
                                     avg_spend_post_1 = sum(SPEND_POST_15D_1,na.rm=T)/.N,
                                     avg_spend_post_2 = sum(SPEND_POST_15D_2,na.rm=T)/.N),
              by=c("wpdelgrp")]
#calculate spend per person delta
temp[, sppdelta := avg_spend_post_2 - avg_spend_post_1]
##spend per person
tlabel <- "15-Day Spend-per-Person Delta by Change in Worth Score"
xlabel <- "Delta in Worth score between time 1 and time 2"
ylabel <- "Delta in post-15-day spending-per-person\nbetween time 1 and 2 (USD)"
xlabels <- c("Same","1pt Lower")
pdata <- temp
px <- temp[,wpdelgrp]
py <- temp[,sppdelta]
nvar <- temp[,n]
ggplot(data = pdata, aes(x = factor(px), y = py)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  scale_x_discrete(label=xlabels) + 
  ylim(c(-1,0.5)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) +
  theme(axis.text.x = element_text(face = "bold", size = 12)) +
  geom_text(size = 4.5, aes(label=round(py,2),y=0), stat= "identity", vjust = -1) +
  geom_text(size = 2.5, aes(label=paste0("n=",comma(nvar)),y=0), stat= "identity", vjust = 1.5)


#SPLIT INTO POINT CHANGE - Speed
scust <- copy(scus)
#ANALYSIS 1: FOCUS ONLY ON POST-PERIODS
#create subsetting variable for change in Speed score
scust[SPEED_1==SPEED_2, spddelgrp := 0]
scust[SPEED_1-1>=SPEED_2, spddelgrp := 1]
#t-tests
temp <- scust[!is.na(spddelgrp)]
t.test(temp[spddelgrp==0,(SPEND_POST_15D_2-SPEND_POST_15D_1)],temp[spddelgrp==1,(SPEND_POST_15D_2-SPEND_POST_15D_1)],conf.level = 0.99)
#assess differences in behavior across *groups* of people based on spddelgrp
temp <- temp[!is.na(spddelgrp), list(n = .N,
                                      avg_spend_post_1 = sum(SPEND_POST_15D_1,na.rm=T)/.N,
                                      avg_spend_post_2 = sum(SPEND_POST_15D_2,na.rm=T)/.N),
              by=c("spddelgrp")]
#calculate spend per person delta
temp[, sppdelta := avg_spend_post_2 - avg_spend_post_1]
##spend per person
tlabel <- "15-Day Spend-per-Person Delta by Change in Speed Score"
xlabel <- "Delta in Speed score between time 1 and time 2"
ylabel <- "Delta in post-15-day spending-per-person\nbetween time 1 and 2 (USD)"
xlabels <- c("Same","1pt Lower")
pdata <- temp
px <- temp[,spddelgrp]
py <- temp[,sppdelta]
nvar <- temp[,n]
ggplot(data = pdata, aes(x = factor(px), y = py)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  scale_x_discrete(label=xlabels) + 
  ylim(c(-1,0.5)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) +
  theme(axis.text.x = element_text(face = "bold", size = 12)) +
  geom_text(size = 4.5, aes(label=round(py,2),y=0), stat= "identity", vjust = -1) +
  geom_text(size = 2.5, aes(label=paste0("n=",comma(nvar)),y=0), stat= "identity", vjust = 1.5)


#SPLIT INTO POINT CHANGE - Accuracy
scust <- copy(scus)
#ANALYSIS 1: FOCUS ONLY ON POST-PERIODS
#create subsetting variable for change in Accuracy score
scust[ACCR_1==ACCR_2, accdelgrp := 0]
scust[ACCR_1-1>=ACCR_2, accdelgrp := 1]
#t-tests
temp <- scust[!is.na(accdelgrp)]
t.test(temp[accdelgrp==0,(SPEND_POST_15D_2-SPEND_POST_15D_1)],temp[accdelgrp==1,(SPEND_POST_15D_2-SPEND_POST_15D_1)],conf.level = 0.99)
#assess differences in behavior across *groups* of people based on accdelgrp
temp <- temp[!is.na(accdelgrp), list(n = .N,
                                      avg_spend_post_1 = sum(SPEND_POST_15D_1,na.rm=T)/.N,
                                      avg_spend_post_2 = sum(SPEND_POST_15D_2,na.rm=T)/.N),
              by=c("accdelgrp")]
#calculate spend per person delta
temp[, sppdelta := avg_spend_post_2 - avg_spend_post_1]
##spend per person
tlabel <- "15-Day Spend-per-Person Delta by Change in Accuracy Score"
xlabel <- "Delta in Accuracy score between time 1 and time 2"
ylabel <- "Delta in post-15-day spending-per-person\nbetween time 1 and 2 (USD)"
xlabels <- c("Same","1pt Lower")
pdata <- temp
px <- temp[,accdelgrp]
py <- temp[,sppdelta]
nvar <- temp[,n]
ggplot(data = pdata, aes(x = factor(px), y = py)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  scale_x_discrete(label=xlabels) + 
  ylim(c(-1,0.5)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) +
  theme(axis.text.x = element_text(face = "bold", size = 12)) +
  geom_text(size = 4.5, aes(label=round(py,2),y=0), stat= "identity", vjust = -1) +
  geom_text(size = 2.5, aes(label=paste0("n=",comma(nvar)),y=0), stat= "identity", vjust = 1.5)

#SPLIT INTO POINT CHANGE - Cleanliness
scust <- copy(scus)
#ANALYSIS 1: FOCUS ONLY ON POST-PERIODS
#create subsetting variable for change in Clean score
scust[CLEAN_1==CLEAN_2, clndelgrp := 0]
scust[CLEAN_1-1>=CLEAN_2, clndelgrp := 1]
#t-tests
temp <- scust[!is.na(clndelgrp)]
t.test(temp[clndelgrp==0,(SPEND_POST_15D_2-SPEND_POST_15D_1)],temp[clndelgrp==1,(SPEND_POST_15D_2-SPEND_POST_15D_1)],conf.level = 0.99)
#assess differences in behavior across *groups* of people based on clndelgrp
temp <- temp[!is.na(clndelgrp), list(n = .N,
                                     avg_spend_post_1 = sum(SPEND_POST_15D_1,na.rm=T)/.N,
                                     avg_spend_post_2 = sum(SPEND_POST_15D_2,na.rm=T)/.N),
             by=c("clndelgrp")]
#calculate spend per person delta
temp[, sppdelta := avg_spend_post_2 - avg_spend_post_1]
##spend per person
tlabel <- "15-Day Spend-per-Person Delta by Change in Clean Score"
xlabel <- "Delta in Clean score between time 1 and time 2"
ylabel <- "Delta in post-15-day spending-per-person\nbetween time 1 and 2 (USD)"
xlabels <- c("Same","1pt Lower")
pdata <- temp
px <- temp[,clndelgrp]
py <- temp[,sppdelta]
nvar <- temp[,n]
ggplot(data = pdata, aes(x = factor(px), y = py)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  scale_x_discrete(label=xlabels) + 
  ylim(c(-1,0.5)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) +
  theme(axis.text.x = element_text(face = "bold", size = 12)) +
  geom_text(size = 4.5, aes(label=round(py,2),y=0), stat= "identity", vjust = -1) +
  geom_text(size = 2.5, aes(label=paste0("n=",comma(nvar)),y=0), stat= "identity", vjust = 1.25)


#SPLIT INTO POINT CHANGE - Above & Beyond
scust <- copy(scus)
#ANALYSIS 1: FOCUS ONLY ON POST-PERIODS
#create subsetting variable for change in Above & Beyond score
scust[ABVBYD_1==ABVBYD_2, abvdelgrp := 0]
scust[ABVBYD_1-1>=ABVBYD_2, abvdelgrp := 1]
#t-tests
temp <- scust[!is.na(abvdelgrp)]
t.test(temp[abvdelgrp==0,(SPEND_POST_15D_2-SPEND_POST_15D_1)],temp[abvdelgrp==1,(SPEND_POST_15D_2-SPEND_POST_15D_1)],conf.level = 0.99)
#assess differences in behavior across *groups* of people based on abvdelgrp
temp <- temp[!is.na(abvdelgrp), list(n = .N,
                                     avg_spend_post_1 = sum(SPEND_POST_15D_1,na.rm=T)/.N,
                                     avg_spend_post_2 = sum(SPEND_POST_15D_2,na.rm=T)/.N),
             by=c("abvdelgrp")]
#calculate spend per person delta
temp[, sppdelta := avg_spend_post_2 - avg_spend_post_1]
##spend per person
tlabel <- "15-Day Spend-per-Person Delta by Change in Above & Beyond Score"
xlabel <- "Delta in Above & Beyond score between time 1 and time 2"
ylabel <- "Delta in post-15-day spending-per-person\nbetween time 1 and 2 (USD)"
xlabels <- c("Same","1pt Lower")
pdata <- temp
px <- temp[,abvdelgrp]
py <- temp[,sppdelta]
nvar <- temp[,n]
ggplot(data = pdata, aes(x = factor(px), y = py)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  scale_x_discrete(label=xlabels) + 
  ylim(c(-1,0.5)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) +
  theme(axis.text.x = element_text(face = "bold", size = 12)) +
  geom_text(size = 4.5, aes(label=round(py,2),y=0), stat= "identity", vjust = -1) +
  geom_text(size = 2.5, aes(label=paste0("n=",comma(nvar)),y=0), stat= "identity", vjust = 2)


#SPLIT INTO POINT CHANGE - Beverage
scust <- copy(scus)
#ANALYSIS 1: FOCUS ONLY ON POST-PERIODS
#create subsetting variable for change in Beverage score
scust[BEV_1==BEV_2, bevdelgrp := 0]
scust[BEV_1-1>=BEV_2, bevdelgrp := 1]
#t-tests
temp <- scust[!is.na(bevdelgrp)]
t.test(temp[bevdelgrp==0,(SPEND_POST_15D_2-SPEND_POST_15D_1)],temp[bevdelgrp==1,(SPEND_POST_15D_2-SPEND_POST_15D_1)],conf.level = 0.99)
#assess differences in behavior across *groups* of people based on bevdelgrp
temp <- temp[!is.na(bevdelgrp), list(n = .N,
                                     avg_spend_post_1 = sum(SPEND_POST_15D_1,na.rm=T)/.N,
                                     avg_spend_post_2 = sum(SPEND_POST_15D_2,na.rm=T)/.N),
             by=c("bevdelgrp")]
#calculate spend per person delta
temp[, sppdelta := avg_spend_post_2 - avg_spend_post_1]
##spend per person
tlabel <- "15-Day Spend-per-Person Delta by Change in Beverage Score"
xlabel <- "Delta in Beverage score between time 1 and time 2"
ylabel <- "Delta in post-15-day spending-per-person\nbetween time 1 and 2 (USD)"
xlabels <- c("Same","1pt Lower")
pdata <- temp
px <- temp[,bevdelgrp]
py <- temp[,sppdelta]
nvar <- temp[,n]
ggplot(data = pdata, aes(x = factor(px), y = py)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  scale_x_discrete(label=xlabels) + 
  ylim(c(-1,0.5)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) +
  theme(axis.text.x = element_text(face = "bold", size = 12)) +
  geom_text(size = 4.5, aes(label=round(py,2),y=0), stat= "identity", vjust = -1) +
  geom_text(size = 2.5, aes(label=paste0("n=",comma(nvar)),y=0), stat= "identity", vjust = 1.5)


#SPLIT INTO POINT CHANGE - Food
scust <- copy(scus)
#ANALYSIS 1: FOCUS ONLY ON POST-PERIODS
#create subsetting variable for change in Food score
scust[FOOD_1==FOOD_2, fooddelgrp := 0]
scust[FOOD_1-1>=FOOD_2, fooddelgrp := 1]
#t-tests
temp <- scust[!is.na(fooddelgrp)]
t.test(temp[fooddelgrp==0,(SPEND_POST_15D_2-SPEND_POST_15D_1)],temp[fooddelgrp==1,(SPEND_POST_15D_2-SPEND_POST_15D_1)],conf.level = 0.99)
#assess differences in behavior across *groups* of people based on fooddelgrp
temp <- temp[!is.na(fooddelgrp), list(n = .N,
                                     avg_spend_post_1 = sum(SPEND_POST_15D_1,na.rm=T)/.N,
                                     avg_spend_post_2 = sum(SPEND_POST_15D_2,na.rm=T)/.N),
             by=c("fooddelgrp")]
#calculate spend per person delta
temp[, sppdelta := avg_spend_post_2 - avg_spend_post_1]
##spend per person
tlabel <- "15-Day Spend-per-Person Delta by Change in Food Score"
xlabel <- "Delta in Food score between time 1 and time 2"
ylabel <- "Delta in post-15-day spending-per-person\nbetween time 1 and 2 (USD)"
xlabels <- c("Same","1pt Lower")
pdata <- temp
px <- temp[,fooddelgrp]
py <- temp[,sppdelta]
nvar <- temp[,n]
ggplot(data = pdata, aes(x = factor(px), y = py)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  scale_x_discrete(label=xlabels) + 
  ylim(c(-1,0.5)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) +
  theme(axis.text.x = element_text(face = "bold", size = 12)) +
  geom_text(size = 4.5, aes(label=round(py,2),y=0), stat= "identity", vjust = -1) +
  geom_text(size = 2.5, aes(label=paste0("n=",comma(nvar)),y=0), stat= "identity", vjust = 1.25)



#SPLIT INTO POINT CHANGE - Return
scust <- copy(scus)
#ANALYSIS 1: FOCUS ONLY ON POST-PERIODS
#create subsetting variable for change in Return score
scust[RETURN_1==RETURN_2, rtndelgrp := 0]
scust[RETURN_1-1>=RETURN_2, rtndelgrp := 1]
#t-tests
temp <- scust[!is.na(rtndelgrp)]
t.test(temp[rtndelgrp==0,(SPEND_POST_15D_2-SPEND_POST_15D_1)],temp[rtndelgrp==1,(SPEND_POST_15D_2-SPEND_POST_15D_1)],conf.level = 0.99)
#assess differences in behavior across *groups* of people based on rtndelgrp
temp <- temp[!is.na(rtndelgrp), list(n = .N,
                                     avg_spend_post_1 = sum(SPEND_POST_15D_1,na.rm=T)/.N,
                                     avg_spend_post_2 = sum(SPEND_POST_15D_2,na.rm=T)/.N),
             by=c("rtndelgrp")]
#calculate spend per person delta
temp[, sppdelta := avg_spend_post_2 - avg_spend_post_1]
##spend per person
tlabel <- "15-Day Spend-per-Person Delta by Change in Return Score"
xlabel <- "Delta in Return score between time 1 and time 2"
ylabel <- "Delta in post-15-day spending-per-person\nbetween time 1 and 2 (USD)"
xlabels <- c("Same","1pt Lower")
pdata <- temp
px <- temp[,rtndelgrp]
py <- temp[,sppdelta]
nvar <- temp[,n]
ggplot(data = pdata, aes(x = factor(px), y = py)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  scale_x_discrete(label=xlabels) + 
  ylim(c(-4,0.5)) +
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
                                    avg_spend_post_2 = sum(SPEND_POST_15D_2,na.rm=T)/.N),
             by=c("ccdelgrp")]
#calculate spend per person delta
temp[, sppdelta := avg_spend_post_2 - avg_spend_post_1]
##spend per person
tlabel <- "15-Day Spend-per-Person Delta by Change in CC Score"
xlabel <- "Delta in CC score between time 1 and time 2"
ylabel <- "Delta in post-15-day spending-per-person\nbetween time 1 and 2 (USD)"
xlabels <- c("Remained in TB","Fell from TB")
pdata <- temp
px <- temp[,ccdelgrp]
py <- temp[,sppdelta]
nvar <- temp[,n]
ggplot(data = pdata, aes(x = factor(px), y = py)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  scale_x_discrete(label=xlabels) + 
  ylim(c(-1,0.5)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) +
  theme(axis.text.x = element_text(face = "bold", size = 12)) +
  geom_text(size = 4.5, aes(label=round(py,2),y=0), stat= "identity", vjust = -1) +
  geom_text(size = 2.5, aes(label=paste0("n=",comma(nvar)),y=0), stat= "identity", vjust = 1.5)

#SPLIT INTO TB CHANGE - WP
scust <- copy(scus)
#ANALYSIS 1: FOCUS ONLY ON POST-PERIODS
#create subsetting variable for change in WP score
scust[WORTH_1==7&WORTH_2==7, wpdelgrp := 0]
scust[WORTH_1==7&WORTH_2<7, wpdelgrp := 1]
#t-tests
temp <- scust[!is.na(wpdelgrp)]
t.test(temp[wpdelgrp==0,(SPEND_POST_15D_2-SPEND_POST_15D_1)],temp[wpdelgrp==1,(SPEND_POST_15D_2-SPEND_POST_15D_1)],conf.level = 0.95)
#assess differences in behavior across *groups* of people based on wpdelgrp
temp <- temp[!is.na(wpdelgrp), list(n = .N,
                                    avg_spend_post_1 = sum(SPEND_POST_15D_1,na.rm=T)/.N,
                                    avg_spend_post_2 = sum(SPEND_POST_15D_2,na.rm=T)/.N),
             by=c("wpdelgrp")]
#calculate spend per person delta
temp[, sppdelta := avg_spend_post_2 - avg_spend_post_1]
##spend per person
tlabel <- "15-Day Spend-per-Person Delta by Change in Worth Score"
xlabel <- "Delta in Worth score between time 1 and time 2"
ylabel <- "Delta in post-15-day spending-per-person\nbetween time 1 and 2 (USD)"
xlabels <- c("Remained in TB","Fell from TB")
pdata <- temp
px <- temp[,wpdelgrp]
py <- temp[,sppdelta]
nvar <- temp[,n]
ggplot(data = pdata, aes(x = factor(px), y = py)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  scale_x_discrete(label=xlabels) + 
  ylim(c(-1,0.5)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) +
  theme(axis.text.x = element_text(face = "bold", size = 12)) +
  geom_text(size = 4.5, aes(label=round(py,2),y=0), stat= "identity", vjust = -1) +
  geom_text(size = 2.5, aes(label=paste0("n=",comma(nvar)),y=0), stat= "identity", vjust = 1.5)


#SPLIT INTO TB CHANGE - Cleanliness
scust <- copy(scus)
#ANALYSIS 1: FOCUS ONLY ON POST-PERIODS
#create subsetting variable for change in Clean score
scust[CLEAN_1==7&CLEAN_2==7, clndelgrp := 0]
scust[CLEAN_1==7&CLEAN_2<7, clndelgrp := 1]
#t-tests
temp <- scust[!is.na(clndelgrp)]
t.test(temp[clndelgrp==0,(SPEND_POST_15D_2-SPEND_POST_15D_1)],temp[clndelgrp==1,(SPEND_POST_15D_2-SPEND_POST_15D_1)],conf.level = 0.99)
#assess differences in behavior across *groups* of people based on clndelgrp
temp <- temp[!is.na(clndelgrp), list(n = .N,
                                     avg_spend_post_1 = sum(SPEND_POST_15D_1,na.rm=T)/.N,
                                     avg_spend_post_2 = sum(SPEND_POST_15D_2,na.rm=T)/.N),
             by=c("clndelgrp")]
#calculate spend per person delta
temp[, sppdelta := avg_spend_post_2 - avg_spend_post_1]
##spend per person
tlabel <- "15-Day Spend-per-Person Delta by Change in Clean Score"
xlabel <- "Delta in Clean score between time 1 and time 2"
ylabel <- "Delta in post-15-day spending-per-person\nbetween time 1 and 2 (USD)"
xlabels <- c("Remained in TB","Fell from TB")
pdata <- temp
px <- temp[,clndelgrp]
py <- temp[,sppdelta]
nvar <- temp[,n]
ggplot(data = pdata, aes(x = factor(px), y = py)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  scale_x_discrete(label=xlabels) + 
  ylim(c(-1,0.5)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) +
  theme(axis.text.x = element_text(face = "bold", size = 12)) +
  geom_text(size = 4.5, aes(label=round(py,2),y=0), stat= "identity", vjust = -1) +
  geom_text(size = 2.5, aes(label=paste0("n=",comma(nvar)),y=0), stat= "identity", vjust = 1.25)


#SPLIT INTO TB CHANGE - Speed
scust <- copy(scus)
#ANALYSIS 1: FOCUS ONLY ON POST-PERIODS
#create subsetting variable for change in Speed score
scust[SPEED_1==7&SPEED_2==7, spddelgrp := 0]
scust[SPEED_1==7&SPEED_2<7, spddelgrp := 1]
#t-tests
temp <- scust[!is.na(spddelgrp)]
t.test(temp[spddelgrp==0,(SPEND_POST_15D_2-SPEND_POST_15D_1)],temp[spddelgrp==1,(SPEND_POST_15D_2-SPEND_POST_15D_1)],conf.level = 0.99)
#assess differences in behavior across *groups* of people based on spddelgrp
temp <- temp[!is.na(spddelgrp), list(n = .N,
                                     avg_spend_post_1 = sum(SPEND_POST_15D_1,na.rm=T)/.N,
                                     avg_spend_post_2 = sum(SPEND_POST_15D_2,na.rm=T)/.N),
             by=c("spddelgrp")]
#calculate spend per person delta
temp[, sppdelta := avg_spend_post_2 - avg_spend_post_1]
##spend per person
tlabel <- "15-Day Spend-per-Person Delta by Change in Speed Score"
xlabel <- "Delta in Speed score between time 1 and time 2"
ylabel <- "Delta in post-15-day spending-per-person\nbetween time 1 and 2 (USD)"
xlabels <- c("Remained in TB","Fell from TB")
pdata <- temp
px <- temp[,spddelgrp]
py <- temp[,sppdelta]
nvar <- temp[,n]
ggplot(data = pdata, aes(x = factor(px), y = py)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  scale_x_discrete(label=xlabels) + 
  ylim(c(-1,0.5)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) +
  theme(axis.text.x = element_text(face = "bold", size = 12)) +
  geom_text(size = 4.5, aes(label=round(py,2),y=0), stat= "identity", vjust = -1) +
  geom_text(size = 2.5, aes(label=paste0("n=",comma(nvar)),y=0), stat= "identity", vjust = 1.5)


#SPLIT INTO TB CHANGE - Accuracy
scust <- copy(scus)
#ANALYSIS 1: FOCUS ONLY ON POST-PERIODS
#create subsetting variable for change in Accuracy score
scust[ACCR_1==7&ACCR_2==7, accdelgrp := 0]
scust[ACCR_1==7&ACCR_2<7, accdelgrp := 1]
#t-tests
temp <- scust[!is.na(accdelgrp)]
t.test(temp[accdelgrp==0,(SPEND_POST_15D_2-SPEND_POST_15D_1)],temp[accdelgrp==1,(SPEND_POST_15D_2-SPEND_POST_15D_1)],conf.level = 0.99)
#assess differences in behavior across *groups* of people based on accdelgrp
temp <- temp[!is.na(accdelgrp), list(n = .N,
                                     avg_spend_post_1 = sum(SPEND_POST_15D_1,na.rm=T)/.N,
                                     avg_spend_post_2 = sum(SPEND_POST_15D_2,na.rm=T)/.N),
             by=c("accdelgrp")]
#calculate spend per person delta
temp[, sppdelta := avg_spend_post_2 - avg_spend_post_1]
##spend per person
tlabel <- "15-Day Spend-per-Person Delta by Change in Accuracy Score"
xlabel <- "Delta in Accuracy score between time 1 and time 2"
ylabel <- "Delta in post-15-day spending-per-person\nbetween time 1 and 2 (USD)"
xlabels <- c("Remained in TB","Fell from TB")
pdata <- temp
px <- temp[,accdelgrp]
py <- temp[,sppdelta]
nvar <- temp[,n]
ggplot(data = pdata, aes(x = factor(px), y = py)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  scale_x_discrete(label=xlabels) + 
  ylim(c(-1,0.5)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) +
  theme(axis.text.x = element_text(face = "bold", size = 12)) +
  geom_text(size = 4.5, aes(label=round(py,2),y=0), stat= "identity", vjust = -1) +
  geom_text(size = 2.5, aes(label=paste0("n=",comma(nvar)),y=0), stat= "identity", vjust = 1.5)


#SPLIT INTO TB CHANGE - Return
scust <- copy(scus)
#ANALYSIS 1: FOCUS ONLY ON POST-PERIODS
#create subsetting variable for change in Return score
scust[RETURN_1==5&RETURN_2==5, rtndelgrp := 0]
scust[RETURN_1==5&RETURN_2<5, rtndelgrp := 1]
#t-tests
temp <- scust[!is.na(rtndelgrp)]
t.test(temp[rtndelgrp==0,(SPEND_POST_15D_2-SPEND_POST_15D_1)],temp[rtndelgrp==1,(SPEND_POST_15D_2-SPEND_POST_15D_1)],conf.level = 0.99)
#assess differences in behavior across *groups* of people based on rtndelgrp
temp <- temp[!is.na(rtndelgrp), list(n = .N,
                                     avg_spend_post_1 = sum(SPEND_POST_15D_1,na.rm=T)/.N,
                                     avg_spend_post_2 = sum(SPEND_POST_15D_2,na.rm=T)/.N),
             by=c("rtndelgrp")]
#calculate spend per person delta
temp[, sppdelta := avg_spend_post_2 - avg_spend_post_1]
##spend per person
tlabel <- "15-Day Spend-per-Person Delta by Change in Return Score"
xlabel <- "Delta in Return score between time 1 and time 2"
ylabel <- "Delta in post-15-day spending-per-person\nbetween time 1 and 2 (USD)"
xlabels <- c("Remained in TB","Fell from TB")
pdata <- temp
px <- temp[,rtndelgrp]
py <- temp[,sppdelta]
nvar <- temp[,n]
ggplot(data = pdata, aes(x = factor(px), y = py)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  scale_x_discrete(label=xlabels) + 
  ylim(c(-4.5,1)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) +
  theme(axis.text.x = element_text(face = "bold", size = 12)) +
  geom_text(size = 4.5, aes(label=round(py,2),y=0), stat= "identity", vjust = -1) +
  geom_text(size = 2.5, aes(label=paste0("n=",comma(nvar)),y=0), stat= "identity", vjust = 1.5)


#SPLIT INTO TB CHANGE - Above & Beyond
scust <- copy(scus)
#ANALYSIS 1: FOCUS ONLY ON POST-PERIODS
#create subsetting variable for change in Above & Beyond score
scust[ABVBYD_1==7&ABVBYD_2==7, abvdelgrp := 0]
scust[ABVBYD_1==7&ABVBYD_2<7, abvdelgrp := 1]
#t-tests
temp <- scust[!is.na(abvdelgrp)]
t.test(temp[abvdelgrp==0,(SPEND_POST_15D_2-SPEND_POST_15D_1)],temp[abvdelgrp==1,(SPEND_POST_15D_2-SPEND_POST_15D_1)],conf.level = 0.99)
#assess differences in behavior across *groups* of people based on abvdelgrp
temp <- temp[!is.na(abvdelgrp), list(n = .N,
                                     avg_spend_post_1 = sum(SPEND_POST_15D_1,na.rm=T)/.N,
                                     avg_spend_post_2 = sum(SPEND_POST_15D_2,na.rm=T)/.N),
             by=c("abvdelgrp")]
#calculate spend per person delta
temp[, sppdelta := avg_spend_post_2 - avg_spend_post_1]
##spend per person
tlabel <- "15-Day Spend-per-Person Delta by Change in Above & Beyond Score"
xlabel <- "Delta in Above & Beyond score between time 1 and time 2"
ylabel <- "Delta in post-15-day spending-per-person\nbetween time 1 and 2 (USD)"
xlabels <- c("Remained in TB","Fell from TB")
pdata <- temp
px <- temp[,abvdelgrp]
py <- temp[,sppdelta]
nvar <- temp[,n]
ggplot(data = pdata, aes(x = factor(px), y = py)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  scale_x_discrete(label=xlabels) + 
  ylim(c(-1,0.5)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) +
  theme(axis.text.x = element_text(face = "bold", size = 12)) +
  geom_text(size = 4.5, aes(label=round(py,2),y=0), stat= "identity", vjust = -1) +
  geom_text(size = 2.5, aes(label=paste0("n=",comma(nvar)),y=0), stat= "identity", vjust = 1.5)


#SPLIT INTO TB CHANGE - Beverage
scust <- copy(scus)
#ANALYSIS 1: FOCUS ONLY ON POST-PERIODS
#create subsetting variable for change in Beverage score
scust[BEV_1==7&BEV_2==7, bevdelgrp := 0]
scust[BEV_1==7&BEV_2<7, bevdelgrp := 1]
#t-tests
temp <- scust[!is.na(bevdelgrp)]
t.test(temp[bevdelgrp==0,(SPEND_POST_15D_2-SPEND_POST_15D_1)],temp[bevdelgrp==1,(SPEND_POST_15D_2-SPEND_POST_15D_1)],conf.level = 0.99)
#assess differences in behavior across *groups* of people based on bevdelgrp
temp <- temp[!is.na(bevdelgrp), list(n = .N,
                                     avg_spend_post_1 = sum(SPEND_POST_15D_1,na.rm=T)/.N,
                                     avg_spend_post_2 = sum(SPEND_POST_15D_2,na.rm=T)/.N),
             by=c("bevdelgrp")]
#calculate spend per person delta
temp[, sppdelta := avg_spend_post_2 - avg_spend_post_1]
##spend per person
tlabel <- "15-Day Spend-per-Person Delta by Change in Beverage Score"
xlabel <- "Delta in Beverage score between time 1 and time 2"
ylabel <- "Delta in post-15-day spending-per-person\nbetween time 1 and 2 (USD)"
xlabels <- c("Remained in TB","Fell from TB")
pdata <- temp
px <- temp[,bevdelgrp]
py <- temp[,sppdelta]
nvar <- temp[,n]
ggplot(data = pdata, aes(x = factor(px), y = py)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  scale_x_discrete(label=xlabels) + 
  ylim(c(-1,0.5)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) +
  theme(axis.text.x = element_text(face = "bold", size = 12)) +
  geom_text(size = 4.5, aes(label=round(py,2),y=0), stat= "identity", vjust = -1) +
  geom_text(size = 2.5, aes(label=paste0("n=",comma(nvar)),y=0), stat= "identity", vjust = 1.5)


#SPLIT INTO TB CHANGE - Food
scust <- copy(scus)
#ANALYSIS 1: FOCUS ONLY ON POST-PERIODS
#create subsetting variable for change in Food score
scust[FOOD_1==7&FOOD_2==7, fooddelgrp := 0]
scust[FOOD_1==7&FOOD_2<7, fooddelgrp := 1]
#t-tests
temp <- scust[!is.na(fooddelgrp)]
t.test(temp[fooddelgrp==0,(SPEND_POST_15D_2-SPEND_POST_15D_1)],temp[fooddelgrp==1,(SPEND_POST_15D_2-SPEND_POST_15D_1)],conf.level = 0.99)
#assess differences in behavior across *groups* of people based on fooddelgrp
temp <- temp[!is.na(fooddelgrp), list(n = .N,
                                      avg_spend_post_1 = sum(SPEND_POST_15D_1,na.rm=T)/.N,
                                      avg_spend_post_2 = sum(SPEND_POST_15D_2,na.rm=T)/.N),
             by=c("fooddelgrp")]
#calculate spend per person delta
temp[, sppdelta := avg_spend_post_2 - avg_spend_post_1]
##spend per person
tlabel <- "15-Day Spend-per-Person Delta by Change in Food Score"
xlabel <- "Delta in Food score between time 1 and time 2"
ylabel <- "Delta in post-15-day spending-per-person\nbetween time 1 and 2 (USD)"
xlabels <- c("Remained in TB","Fell from TB")
pdata <- temp
px <- temp[,fooddelgrp]
py <- temp[,sppdelta]
nvar <- temp[,n]
ggplot(data = pdata, aes(x = factor(px), y = py)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  scale_x_discrete(label=xlabels) + 
  ylim(c(-1,0.5)) +
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

# ##spend per visit
# tlabel <- "15-Day Spend-per-Visit Delta by Change in CC Score"
# xlabel <- "Delta in CC score between time 1 and time 2,\n(net of delta for 'same' scoring customers)"
# ylabel <- "Delta in post-15-day spending-per-visit\nbetween time 1 and 2 (USD)"
# xlabels <- c("-6pts","-5pts","-4pts","-3pts","-2pts","-1pt",
#              "+1pt","+2pts","+3pts","+4pts","+5pts","+6pts")
# pdata <- temp[ccdelgrp!="07-same"]
# px <- temp[ccdelgrp!="07-same",ccdelgrp]
# py <- temp[ccdelgrp!="07-same",svpdeltafromsame]
# 
# ggplot(data = pdata, aes(x = px, y = py)) +
#   geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
#   scale_x_discrete(label=xlabels) + ylim(c(-.05,.05)) +
#   xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) +
#   theme(axis.text.x = element_text(face = "bold", size = 12)) +
#   geom_text(size = 3, aes(label=round(py,2),y=0), stat= "identity", vjust = -1)

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

# ##visits per person
# tlabel <- "15-Day Visits-per-Person Delta by Change in CC Score"
# xlabel <- "Delta in CC score between time 1 and time 2,\n(net of delta for 'same' scoring customers)"
# ylabel <- "Delta in post-15-day visits-per-person\nbetween time 1 and 2 (USD)"
# xlabels <- c("-6pts","-5pts","-4pts","-3pts","-2pts","-1pt",
#              "+1pt","+2pts","+3pts","+4pts","+5pts","+6pts")
# pdata <- temp[ccdelgrp!="07-same"]
# px <- temp[ccdelgrp!="07-same",ccdelgrp]
# py <- temp[ccdelgrp!="07-same",vppdeltafromsame]
# 
# ggplot(data = pdata, aes(x = px, y = py)) +
#   geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
#   scale_x_discrete(label=xlabels) + 
#   #ylim(c(-1.25,1.25)) +
#   xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) +
#   theme(axis.text.x = element_text(face = "bold", size = 12)) +
#   geom_text(size = 3, aes(label=round(py,2),y=0), stat= "identity", vjust = -1)

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
#calculate spend per person delta
temp[, sppdelta := avg_spend_post_2 - avg_spend_post_1]
#use "same" as control
temp[ccdelgrp!="07-same", sppdeltafromsame := sppdelta - temp[ccdelgrp=="07-same",sppdelta]]
temp <- setorder(temp,ccdelgrp)
##spend per person
tlabel <- "15-Day Spend-per-Person Delta by Change in CC Score"
xlabel <- "Delta in CC score between time 1 and time 2,\n(net of delta for 'same' scoring customers)"
ylabel <- "Delta in post-15-day spending-per-person\nbetween time 1 and 2 (USD)"
xlabels <- c("-6pts","-5pts","-4pts","-3pts","-2pts","-1pt",
             "+1pt","+2pts","+3pts","+4pts","+5pts","+6pts")
temp[ccdelgrp!="07-same"]
px <- temp[ccdelgrp!="07-same",ccdelgrp]
py <- temp[ccdelgrp!="07-same",sppdeltafromsame]
nvar <- temp[ccdelgrp!="07-same",n]
ggplot(data = pdata, aes(x = px, y = py)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  scale_x_discrete(label=xlabels) + ylim(c(-1.25,1.25)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) +
  theme(axis.text.x = element_text(face = "bold", size = 12)) +
  geom_text(size = 3, aes(label=round(py,2),y=0), stat= "identity", vjust = -1) +
  geom_text(size = 2.5, aes(label=paste0("n=",comma(nvar)),y=0), stat= "identity", vjust = 1.5)

#SPLIT INTO POINT CHANGE
scust <- copy(scus)
#ANALYSIS 1: FOCUS ONLY ON POST-PERIODS
#create subsetting variable for change in score
scust[SPEED_1==SPEED_2, spdelgrp := "07-same"]
scust[SPEED_1+1==SPEED_2, spdelgrp := "08-SP2_hi_1pt"]
scust[SPEED_1+2==SPEED_2, spdelgrp := "09-SP2_hi_2pt"]
scust[SPEED_1+3==SPEED_2, spdelgrp := "10-SP2_hi_3pt"]
scust[SPEED_1+4==SPEED_2, spdelgrp := "11-SP2_hi_4pt"]
scust[SPEED_1+5==SPEED_2, spdelgrp := "12-SP2_hi_5pt"]
scust[SPEED_1+6==SPEED_2, spdelgrp := "13-SP2_hi_6pt"]
scust[SPEED_1-1==SPEED_2, spdelgrp := "06-SP2_lo_1pt"]
scust[SPEED_1-2==SPEED_2, spdelgrp := "05-SP2_lo_2pt"]
scust[SPEED_1-3==SPEED_2, spdelgrp := "04-SP2_lo_3pt"]
scust[SPEED_1-4==SPEED_2, spdelgrp := "03-SP2_lo_4pt"]
scust[SPEED_1-5==SPEED_2, spdelgrp := "02-SP2_lo_5pt"]
scust[SPEED_1-6==SPEED_2, spdelgrp := "01-SP2_lo_6pt"]
#assess differences in behavior across *groups* of people based on spdelgrp
temp <- scust[!is.na(spdelgrp), list(n = .N,
                                     VISITS_POST_15D_1 = sum(VISITS_POST_15D_1,na.rm=T),
                                     VISITS_POST_15D_2 = sum(VISITS_POST_15D_2,na.rm=T),
                                     SPEND_POST_15D_1 = sum(SPEND_POST_15D_1,na.rm=T),
                                     SPEND_POST_15D_2 = sum(SPEND_POST_15D_2,na.rm=T),
                                     avg_vis_post_1 = sum(VISITS_POST_15D_1,na.rm=T)/.N,
                                     avg_vis_post_2 = sum(VISITS_POST_15D_2,na.rm=T)/.N,
                                     avg_spend_post_1 = sum(SPEND_POST_15D_1,na.rm=T)/.N,
                                     avg_spend_post_2 = sum(SPEND_POST_15D_2,na.rm=T)/.N),
              by=c("spdelgrp")]
#calculate spend per person delta
temp[, sppdelta := avg_spend_post_2 - avg_spend_post_1]
#use "same" as control
temp[spdelgrp!="07-same", sppdeltafromsame := sppdelta - temp[spdelgrp=="07-same",sppdelta]]
temp <- setorder(temp,spdelgrp)
##spend per person
tlabel <- "15-Day Spend-per-Person Delta by Change in Speed Score"
xlabel <- "Delta in Speed score between time 1 and time 2,\n(net of delta for 'same' scoring customers)"
ylabel <- "Delta in post-15-day spending-per-person\nbetween time 1 and 2 (USD)"
xlabels <- c("-6pts","-5pts","-4pts","-3pts","-2pts","-1pt",
             "+1pt","+2pts","+3pts","+4pts","+5pts","+6pts")
temp[spdelgrp!="07-same"]
px <- temp[spdelgrp!="07-same",spdelgrp]
py <- temp[spdelgrp!="07-same",sppdeltafromsame]
nvar <- temp[spdelgrp!="07-same",n]
ggplot(data = pdata, aes(x = px, y = py)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  scale_x_discrete(label=xlabels) + ylim(c(-1.25,1.25)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) +
  theme(axis.text.x = element_text(face = "bold", size = 12)) +
  geom_text(size = 3, aes(label=round(py,2),y=0), stat= "identity", vjust = -1.5) +
  geom_text(size = 2.5, aes(label=paste0("n=",comma(nvar)),y=0), stat= "identity", vjust = 1.05)

#SPLIT INTO POINT CHANGE
scust <- copy(scus)
#ANALYSIS 1: FOCUS ONLY ON POST-PERIODS
#create subsetting variable for change in score
scust[ABVBYD_1==ABVBYD_2, abdelgrp := "07-same"]
scust[ABVBYD_1+1==ABVBYD_2, abdelgrp := "08-AB2_hi_1pt"]
scust[ABVBYD_1+2==ABVBYD_2, abdelgrp := "09-AB2_hi_2pt"]
scust[ABVBYD_1+3==ABVBYD_2, abdelgrp := "10-AB2_hi_3pt"]
scust[ABVBYD_1+4==ABVBYD_2, abdelgrp := "11-AB2_hi_4pt"]
scust[ABVBYD_1+5==ABVBYD_2, abdelgrp := "12-AB2_hi_5pt"]
scust[ABVBYD_1+6==ABVBYD_2, abdelgrp := "13-AB2_hi_6pt"]
scust[ABVBYD_1-1==ABVBYD_2, abdelgrp := "06-AB2_lo_1pt"]
scust[ABVBYD_1-2==ABVBYD_2, abdelgrp := "05-AB2_lo_2pt"]
scust[ABVBYD_1-3==ABVBYD_2, abdelgrp := "04-AB2_lo_3pt"]
scust[ABVBYD_1-4==ABVBYD_2, abdelgrp := "03-AB2_lo_4pt"]
scust[ABVBYD_1-5==ABVBYD_2, abdelgrp := "02-AB2_lo_5pt"]
scust[ABVBYD_1-6==ABVBYD_2, abdelgrp := "01-AB2_lo_6pt"]
#assess differences in behavior across *groups* of people based on abdelgrp
temp <- scust[!is.na(abdelgrp), list(n = .N,
                                     VISITS_POST_15D_1 = sum(VISITS_POST_15D_1,na.rm=T),
                                     VISITS_POST_15D_2 = sum(VISITS_POST_15D_2,na.rm=T),
                                     SPEND_POST_15D_1 = sum(SPEND_POST_15D_1,na.rm=T),
                                     SPEND_POST_15D_2 = sum(SPEND_POST_15D_2,na.rm=T),
                                     avg_vis_post_1 = sum(VISITS_POST_15D_1,na.rm=T)/.N,
                                     avg_vis_post_2 = sum(VISITS_POST_15D_2,na.rm=T)/.N,
                                     avg_spend_post_1 = sum(SPEND_POST_15D_1,na.rm=T)/.N,
                                     avg_spend_post_2 = sum(SPEND_POST_15D_2,na.rm=T)/.N),
              by=c("abdelgrp")]
#calculate spend per person delta
temp[, sppdelta := avg_spend_post_2 - avg_spend_post_1]
#use "same" as control
temp[abdelgrp!="07-same", sppdeltafromsame := sppdelta - temp[abdelgrp=="07-same",sppdelta]]
temp <- setorder(temp,abdelgrp)
##spend per person
tlabel <- "15-Day Spend-per-Person Delta by Change in Above & Beyond Score"
xlabel <- "Delta in Above & Beyond score between time 1 and time 2,\n(net of delta for 'same' scoring customers)"
ylabel <- "Delta in post-15-day spending-per-person\nbetween time 1 and 2 (USD)"
xlabels <- c("-6pts","-5pts","-4pts","-3pts","-2pts","-1pt",
             "+1pt","+2pts","+3pts","+4pts","+5pts","+6pts")
temp[abdelgrp!="07-same"]
px <- temp[abdelgrp!="07-same",abdelgrp]
py <- temp[abdelgrp!="07-same",sppdeltafromsame]
nvar <- temp[abdelgrp!="07-same",n]
ggplot(data = pdata, aes(x = px, y = py)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  scale_x_discrete(label=xlabels) + ylim(c(-1.75,1.25)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) +
  theme(axis.text.x = element_text(face = "bold", size = 12)) +
  geom_text(size = 3, aes(label=round(py,2),y=0), stat= "identity", vjust = -1.5) +
  geom_text(size = 2.5, aes(label=paste0("n=",comma(nvar)),y=0), stat= "identity", vjust = 1.5)


#SPLIT INTO POINT CHANGE
scust <- copy(scus)
#ANALYSIS 1: FOCUS ONLY ON POST-PERIODS
#create subsetting variable for change in score
scust[ACCR_1==ACCR_2, acdelgrp := "07-same"]
scust[ACCR_1+1==ACCR_2, acdelgrp := "08-AC2_hi_1pt"]
scust[ACCR_1+2==ACCR_2, acdelgrp := "09-AC2_hi_2pt"]
scust[ACCR_1+3==ACCR_2, acdelgrp := "10-AC2_hi_3pt"]
scust[ACCR_1+4==ACCR_2, acdelgrp := "11-AC2_hi_4pt"]
scust[ACCR_1+5==ACCR_2, acdelgrp := "12-AC2_hi_5pt"]
scust[ACCR_1+6==ACCR_2, acdelgrp := "13-AC2_hi_6pt"]
scust[ACCR_1-1==ACCR_2, acdelgrp := "06-AC2_lo_1pt"]
scust[ACCR_1-2==ACCR_2, acdelgrp := "05-AC2_lo_2pt"]
scust[ACCR_1-3==ACCR_2, acdelgrp := "04-AC2_lo_3pt"]
scust[ACCR_1-4==ACCR_2, acdelgrp := "03-AC2_lo_4pt"]
scust[ACCR_1-5==ACCR_2, acdelgrp := "02-AC2_lo_5pt"]
scust[ACCR_1-6==ACCR_2, acdelgrp := "01-AC2_lo_6pt"]
#assess differences in behavior across *groups* of people based on acdelgrp
temp <- scust[!is.na(acdelgrp), list(n = .N,
                                     VISITS_POST_15D_1 = sum(VISITS_POST_15D_1,na.rm=T),
                                     VISITS_POST_15D_2 = sum(VISITS_POST_15D_2,na.rm=T),
                                     SPEND_POST_15D_1 = sum(SPEND_POST_15D_1,na.rm=T),
                                     SPEND_POST_15D_2 = sum(SPEND_POST_15D_2,na.rm=T),
                                     avg_vis_post_1 = sum(VISITS_POST_15D_1,na.rm=T)/.N,
                                     avg_vis_post_2 = sum(VISITS_POST_15D_2,na.rm=T)/.N,
                                     avg_spend_post_1 = sum(SPEND_POST_15D_1,na.rm=T)/.N,
                                     avg_spend_post_2 = sum(SPEND_POST_15D_2,na.rm=T)/.N),
              by=c("acdelgrp")]
#calculate spend per person delta
temp[, sppdelta := avg_spend_post_2 - avg_spend_post_1]
#use "same" as control
temp[acdelgrp!="07-same", sppdeltafromsame := sppdelta - temp[acdelgrp=="07-same",sppdelta]]
temp <- setorder(temp,acdelgrp)
##spend per person
tlabel <- "15-Day Spend-per-Person Delta by Change in Accuracy Score"
xlabel <- "Delta in Accuracy score between time 1 and time 2,\n(net of delta for 'same' scoring customers)"
ylabel <- "Delta in post-15-day spending-per-person\nbetween time 1 and 2 (USD)"
xlabels <- c("-6pts","-5pts","-4pts","-3pts","-2pts","-1pt",
             "+1pt","+2pts","+3pts","+4pts","+5pts","+6pts")
temp[acdelgrp!="07-same"]
px <- temp[acdelgrp!="07-same",acdelgrp]
py <- temp[acdelgrp!="07-same",sppdeltafromsame]
nvar <- temp[acdelgrp!="07-same",n]
ggplot(data = pdata, aes(x = px, y = py)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  scale_x_discrete(label=xlabels) + ylim(c(-1.75,1.25)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) +
  theme(axis.text.x = element_text(face = "bold", size = 12)) +
  geom_text(size = 3, aes(label=round(py,2),y=0), stat= "identity", vjust = -1.65) +
  geom_text(size = 2.5, aes(label=paste0("n=",comma(nvar)),y=0), stat= "identity", vjust = 1.5)


#SPLIT INTO POINT CHANGE
scust <- copy(scus)
#ANALYSIS 1: FOCUS ONLY ON POST-PERIODS
#create subsetting variable for change in score
scust[BEV_1==BEV_2, bevdelgrp := "07-same"]
scust[BEV_1+1==BEV_2, bevdelgrp := "08-BV2_hi_1pt"]
scust[BEV_1+2==BEV_2, bevdelgrp := "09-BV2_hi_2pt"]
scust[BEV_1+3==BEV_2, bevdelgrp := "10-BV2_hi_3pt"]
scust[BEV_1+4==BEV_2, bevdelgrp := "11-BV2_hi_4pt"]
scust[BEV_1+5==BEV_2, bevdelgrp := "12-BV2_hi_5pt"]
scust[BEV_1+6==BEV_2, bevdelgrp := "13-BV2_hi_6pt"]
scust[BEV_1-1==BEV_2, bevdelgrp := "06-BV2_lo_1pt"]
scust[BEV_1-2==BEV_2, bevdelgrp := "05-BV2_lo_2pt"]
scust[BEV_1-3==BEV_2, bevdelgrp := "04-BV2_lo_3pt"]
scust[BEV_1-4==BEV_2, bevdelgrp := "03-BV2_lo_4pt"]
scust[BEV_1-5==BEV_2, bevdelgrp := "02-BV2_lo_5pt"]
scust[BEV_1-6==BEV_2, bevdelgrp := "01-BV2_lo_6pt"]
#assess differences in behavior across *groups* of people based on bevdelgrp
temp <- scust[!is.na(bevdelgrp), list(n = .N,
                                     VISITS_POST_15D_1 = sum(VISITS_POST_15D_1,na.rm=T),
                                     VISITS_POST_15D_2 = sum(VISITS_POST_15D_2,na.rm=T),
                                     SPEND_POST_15D_1 = sum(SPEND_POST_15D_1,na.rm=T),
                                     SPEND_POST_15D_2 = sum(SPEND_POST_15D_2,na.rm=T),
                                     avg_vis_post_1 = sum(VISITS_POST_15D_1,na.rm=T)/.N,
                                     avg_vis_post_2 = sum(VISITS_POST_15D_2,na.rm=T)/.N,
                                     avg_spend_post_1 = sum(SPEND_POST_15D_1,na.rm=T)/.N,
                                     avg_spend_post_2 = sum(SPEND_POST_15D_2,na.rm=T)/.N),
              by=c("bevdelgrp")]
#calculate spend per person delta
temp[, sppdelta := avg_spend_post_2 - avg_spend_post_1]
#use "same" as control
temp[bevdelgrp!="07-same", sppdeltafromsame := sppdelta - temp[bevdelgrp=="07-same",sppdelta]]
temp <- setorder(temp,bevdelgrp)
##spend per person
tlabel <- "15-Day Spend-per-Person Delta by Change in Beverage Score"
xlabel <- "Delta in Beverage score between time 1 and time 2,\n(net of delta for 'same' scoring customers)"
ylabel <- "Delta in post-15-day spending-per-person\nbetween time 1 and 2 (USD)"
xlabels <- c("-6pts","-5pts","-4pts","-3pts","-2pts","-1pt",
             "+1pt","+2pts","+3pts","+4pts","+5pts","+6pts")
temp[bevdelgrp!="07-same"]
px <- temp[bevdelgrp!="07-same",bevdelgrp]
py <- temp[bevdelgrp!="07-same",sppdeltafromsame]
nvar <- temp[bevdelgrp!="07-same",n]
ggplot(data = pdata, aes(x = px, y = py)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  scale_x_discrete(label=xlabels) + ylim(c(-1.75,1.25)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) +
  theme(axis.text.x = element_text(face = "bold", size = 12)) +
  geom_text(size = 3, aes(label=round(py,2),y=0), stat= "identity", vjust = -1.5) +
  geom_text(size = 2.5, aes(label=paste0("n=",comma(nvar)),y=0), stat= "identity", vjust = 1.5)

#SPLIT INTO POINT CHANGE
scust <- copy(scus)
#ANALYSIS 1: FOCUS ONLY ON POST-PERIODS
#create subsetting variable for change in score
scust[FOOD_1==FOOD_2, fooddelgrp := "07-same"]
scust[FOOD_1+1==FOOD_2, fooddelgrp := "08-FD2_hi_1pt"]
scust[FOOD_1+2==FOOD_2, fooddelgrp := "09-FD2_hi_2pt"]
scust[FOOD_1+3==FOOD_2, fooddelgrp := "10-FD2_hi_3pt"]
scust[FOOD_1+4==FOOD_2, fooddelgrp := "11-FD2_hi_4pt"]
scust[FOOD_1+5==FOOD_2, fooddelgrp := "12-FD2_hi_5pt"]
scust[FOOD_1+6==FOOD_2, fooddelgrp := "13-FD2_hi_6pt"]
scust[FOOD_1-1==FOOD_2, fooddelgrp := "06-FD2_lo_1pt"]
scust[FOOD_1-2==FOOD_2, fooddelgrp := "05-FD2_lo_2pt"]
scust[FOOD_1-3==FOOD_2, fooddelgrp := "04-FD2_lo_3pt"]
scust[FOOD_1-4==FOOD_2, fooddelgrp := "03-FD2_lo_4pt"]
scust[FOOD_1-5==FOOD_2, fooddelgrp := "02-FD2_lo_5pt"]
scust[FOOD_1-6==FOOD_2, fooddelgrp := "01-FD2_lo_6pt"]
#assess differences in behavior across *groups* of people based on fooddelgrp
temp <- scust[!is.na(fooddelgrp), list(n = .N,
                                      VISITS_POST_15D_1 = sum(VISITS_POST_15D_1,na.rm=T),
                                      VISITS_POST_15D_2 = sum(VISITS_POST_15D_2,na.rm=T),
                                      SPEND_POST_15D_1 = sum(SPEND_POST_15D_1,na.rm=T),
                                      SPEND_POST_15D_2 = sum(SPEND_POST_15D_2,na.rm=T),
                                      avg_vis_post_1 = sum(VISITS_POST_15D_1,na.rm=T)/.N,
                                      avg_vis_post_2 = sum(VISITS_POST_15D_2,na.rm=T)/.N,
                                      avg_spend_post_1 = sum(SPEND_POST_15D_1,na.rm=T)/.N,
                                      avg_spend_post_2 = sum(SPEND_POST_15D_2,na.rm=T)/.N),
              by=c("fooddelgrp")]
#calculate spend per person delta
temp[, sppdelta := avg_spend_post_2 - avg_spend_post_1]
#use "same" as control
temp[fooddelgrp!="07-same", sppdeltafromsame := sppdelta - temp[fooddelgrp=="07-same",sppdelta]]
temp <- setorder(temp,fooddelgrp)
##spend per person
tlabel <- "15-Day Spend-per-Person Delta by Change in Food Score"
xlabel <- "Delta in Food score between time 1 and time 2,\n(net of delta for 'same' scoring customers)"
ylabel <- "Delta in post-15-day spending-per-person\nbetween time 1 and 2 (USD)"
xlabels <- c("-6pts","-5pts","-4pts","-3pts","-2pts","-1pt",
             "+1pt","+2pts","+3pts","+4pts","+5pts","+6pts")
temp[fooddelgrp!="07-same"]
px <- temp[fooddelgrp!="07-same",fooddelgrp]
py <- temp[fooddelgrp!="07-same",sppdeltafromsame]
nvar <- temp[fooddelgrp!="07-same",n]
ggplot(data = pdata, aes(x = px, y = py)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  scale_x_discrete(label=xlabels) + ylim(c(-1.75,3)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) +
  theme(axis.text.x = element_text(face = "bold", size = 12)) +
  geom_text(size = 3, aes(label=round(py,2),y=0), stat= "identity", vjust = -1.5) +
  geom_text(size = 2.5, aes(label=paste0("n=",comma(nvar)),y=0), stat= "identity", vjust = 1.5)

#SPLIT INTO POINT CHANGE
scust <- copy(scus)
#ANALYSIS 1: FOCUS ONLY ON POST-PERIODS
#create subsetting variable for change in score
scust[CLEAN_1==CLEAN_2, clndelgrp := "07-same"]
scust[CLEAN_1+1==CLEAN_2, clndelgrp := "08-CN2_hi_1pt"]
scust[CLEAN_1+2==CLEAN_2, clndelgrp := "09-CN2_hi_2pt"]
scust[CLEAN_1+3==CLEAN_2, clndelgrp := "10-CN2_hi_3pt"]
scust[CLEAN_1+4==CLEAN_2, clndelgrp := "11-CN2_hi_4pt"]
scust[CLEAN_1+5==CLEAN_2, clndelgrp := "12-CN2_hi_5pt"]
scust[CLEAN_1+6==CLEAN_2, clndelgrp := "13-CN2_hi_6pt"]
scust[CLEAN_1-1==CLEAN_2, clndelgrp := "06-CN2_lo_1pt"]
scust[CLEAN_1-2==CLEAN_2, clndelgrp := "05-CN2_lo_2pt"]
scust[CLEAN_1-3==CLEAN_2, clndelgrp := "04-CN2_lo_3pt"]
scust[CLEAN_1-4==CLEAN_2, clndelgrp := "03-CN2_lo_4pt"]
scust[CLEAN_1-5==CLEAN_2, clndelgrp := "02-CN2_lo_5pt"]
scust[CLEAN_1-6==CLEAN_2, clndelgrp := "01-CN2_lo_6pt"]
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
#calculate spend per person delta
temp[, sppdelta := avg_spend_post_2 - avg_spend_post_1]
#use "same" as control
temp[clndelgrp!="07-same", sppdeltafromsame := sppdelta - temp[clndelgrp=="07-same",sppdelta]]
temp <- setorder(temp,clndelgrp)
##spend per person
tlabel <- "15-Day Spend-per-Person Delta by Change in Cleanliness Score"
xlabel <- "Delta in Cleanliness score between time 1 and time 2,\n(net of delta for 'same' scoring customers)"
ylabel <- "Delta in post-15-day spending-per-person\nbetween time 1 and 2 (USD)"
xlabels <- c("-6pts","-5pts","-4pts","-3pts","-2pts","-1pt",
             "+1pt","+2pts","+3pts","+4pts","+5pts","+6pts")
temp[clndelgrp!="07-same"]
px <- temp[clndelgrp!="07-same",clndelgrp]
py <- temp[clndelgrp!="07-same",sppdeltafromsame]
nvar <- temp[clndelgrp!="07-same",n]
ggplot(data = pdata, aes(x = px, y = py)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  scale_x_discrete(label=xlabels) + ylim(c(-1.5,1.25)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) +
  theme(axis.text.x = element_text(face = "bold", size = 12)) +
  geom_text(size = 3, aes(label=round(py,2),y=0), stat= "identity", vjust = -1.75) +
  geom_text(size = 2.5, aes(label=paste0("n=",comma(nvar)),y=0), stat= "identity", vjust = 1.5)

#SPLIT INTO POINT CHANGE
scust <- copy(scus)
#ANALYSIS 1: FOCUS ONLY ON POST-PERIODS
#create subsetting variable for change in score
scust[WORTH_1==WORTH_2, wpdelgrp := "07-same"]
scust[WORTH_1+1==WORTH_2, wpdelgrp := "08-WP2_hi_1pt"]
scust[WORTH_1+2==WORTH_2, wpdelgrp := "09-WP2_hi_2pt"]
scust[WORTH_1+3==WORTH_2, wpdelgrp := "10-WP2_hi_3pt"]
scust[WORTH_1+4==WORTH_2, wpdelgrp := "11-WP2_hi_4pt"]
scust[WORTH_1+5==WORTH_2, wpdelgrp := "12-WP2_hi_5pt"]
scust[WORTH_1+6==WORTH_2, wpdelgrp := "13-WP2_hi_6pt"]
scust[WORTH_1-1==WORTH_2, wpdelgrp := "06-WP2_lo_1pt"]
scust[WORTH_1-2==WORTH_2, wpdelgrp := "05-WP2_lo_2pt"]
scust[WORTH_1-3==WORTH_2, wpdelgrp := "04-WP2_lo_3pt"]
scust[WORTH_1-4==WORTH_2, wpdelgrp := "03-WP2_lo_4pt"]
scust[WORTH_1-5==WORTH_2, wpdelgrp := "02-WP2_lo_5pt"]
scust[WORTH_1-6==WORTH_2, wpdelgrp := "01-WP2_lo_6pt"]
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
#calculate spend per person delta
temp[, sppdelta := avg_spend_post_2 - avg_spend_post_1]
#use "same" as control
temp[wpdelgrp!="07-same", sppdeltafromsame := sppdelta - temp[wpdelgrp=="07-same",sppdelta]]
temp <- setorder(temp,wpdelgrp)
##spend per person
tlabel <- "15-Day Spend-per-Person Delta by Change in Worth Score"
xlabel <- "Delta in Worth score between time 1 and time 2,\n(net of delta for 'same' scoring customers)"
ylabel <- "Delta in post-15-day spending-per-person\nbetween time 1 and 2 (USD)"
xlabels <- c("-6pts","-5pts","-4pts","-3pts","-2pts","-1pt",
             "+1pt","+2pts","+3pts","+4pts","+5pts","+6pts")
temp[wpdelgrp!="07-same"]
px <- temp[wpdelgrp!="07-same",wpdelgrp]
py <- temp[wpdelgrp!="07-same",sppdeltafromsame]
nvar <- temp[wpdelgrp!="07-same",n]
ggplot(data = pdata, aes(x = px, y = py)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  scale_x_discrete(label=xlabels) + ylim(c(-1.25,1.25)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) +
  theme(axis.text.x = element_text(face = "bold", size = 12)) +
  geom_text(size = 3, aes(label=round(py,2),y=0), stat= "identity", vjust = -1.05) +
  geom_text(size = 2.5, aes(label=paste0("n=",comma(nvar)),y=0), stat= "identity", vjust = 1.25)

#SPLIT INTO POINT CHANGE
scust <- copy(scus)
#ANALYSIS 1: FOCUS ONLY ON POST-PERIODS
#create subsetting variable for change in score
scust[RETURN_1==RETURN_2, rtndelgrp := "07-same"]
scust[RETURN_1+1==RETURN_2, rtndelgrp := "08-RT2_hi_1pt"]
scust[RETURN_1+2==RETURN_2, rtndelgrp := "09-RT2_hi_2pt"]
scust[RETURN_1+3==RETURN_2, rtndelgrp := "10-RT2_hi_3pt"]
scust[RETURN_1+4==RETURN_2, rtndelgrp := "11-RT2_hi_4pt"]
scust[RETURN_1-1==RETURN_2, rtndelgrp := "06-RT2_lo_1pt"]
scust[RETURN_1-2==RETURN_2, rtndelgrp := "05-RT2_lo_2pt"]
scust[RETURN_1-3==RETURN_2, rtndelgrp := "04-RT2_lo_3pt"]
scust[RETURN_1-4==RETURN_2, rtndelgrp := "03-RT2_lo_4pt"]
#assess differences in behavior across *groups* of people based on rtndelgrp
temp <- scust[!is.na(rtndelgrp), list(n = .N,
                                     VISITS_POST_15D_1 = sum(VISITS_POST_15D_1,na.rm=T),
                                     VISITS_POST_15D_2 = sum(VISITS_POST_15D_2,na.rm=T),
                                     SPEND_POST_15D_1 = sum(SPEND_POST_15D_1,na.rm=T),
                                     SPEND_POST_15D_2 = sum(SPEND_POST_15D_2,na.rm=T),
                                     avg_vis_post_1 = sum(VISITS_POST_15D_1,na.rm=T)/.N,
                                     avg_vis_post_2 = sum(VISITS_POST_15D_2,na.rm=T)/.N,
                                     avg_spend_post_1 = sum(SPEND_POST_15D_1,na.rm=T)/.N,
                                     avg_spend_post_2 = sum(SPEND_POST_15D_2,na.rm=T)/.N),
              by=c("rtndelgrp")]
#calculate spend per person delta
temp[, sppdelta := avg_spend_post_2 - avg_spend_post_1]
#use "same" as control
temp[rtndelgrp!="07-same", sppdeltafromsame := sppdelta - temp[rtndelgrp=="07-same",sppdelta]]
temp <- setorder(temp,rtndelgrp)
##spend per person
tlabel <- "15-Day Spend-per-Person Delta by Change in Return Score"
xlabel <- "Delta in Return score between time 1 and time 2,\n(net of delta for 'same' scoring customers)"
ylabel <- "Delta in post-15-day spending-per-person\nbetween time 1 and 2 (USD)"
xlabels <- c("-4pts","-3pts","-2pts","-1pt",
             "+1pt","+2pts","+3pts","+4pts")
pdata <- temp[rtndelgrp!="07-same"]
px <- temp[rtndelgrp!="07-same",rtndelgrp]
py <- temp[rtndelgrp!="07-same",sppdeltafromsame]
nvar <- temp[rtndelgrp!="07-same",n]
ggplot(data = pdata, aes(x = px, y = py)) +
  geom_bar(stat="identity", width = 0.7, fill="lightgray", colour="black") + theme_bw() +
  scale_x_discrete(label=xlabels) + ylim(c(-4,6.75)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) +
  theme(axis.text.x = element_text(face = "bold", size = 12)) +
  geom_text(size = 3, aes(label=round(py,2),y=0), stat= "identity", vjust = -1.05) +
  geom_text(size = 2.5, aes(label=paste0("n=",comma(nvar)),y=0), stat= "identity", vjust = 1.25)




















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
