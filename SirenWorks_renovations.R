##making plots for Siren Works renovations
##weekly trends and YoY comparison
##with YoY deltas called out
##CE measures: CC, SO, Speed, Cleanliness

#load libraries
library(data.table)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(PerformanceAnalytics)
library(ggthemes)

#group by stores that *are* or *are not* using the new plays

#load data
#ce data from survox
swce <- fread("Q:/Departments/WMO/Marketing Research/New Q drive/Foundational/Customer Voice/2.0/Ad Hoc Q/2018_2_20_Siren Works Renovations/ce101_2017.csv")
swce[, dateymd := ymd(date1)]
swce[, datemonth := month(dateymd)]
swce[, datetime := ymd_hms(sbid_date)]
# swce[adhoc==1, testcase := 1];swce[adhoc==2, testcase := 0]

#reduce number of variables
swce <- swce[, c("guid","dateymd","datemonth","stid","q1",grep("q2",colnames(swce),value=T)),with=FALSE]

#store attributes
sta <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/store_attributes.csv")
setnames(sta,"STORE_NUM","stid")
sta <- sta[stid %in% swce[,stid]]
swce <- merge(swce,sta,by="stid")

#subset to US CO stores
swce_usco <- swce[OWNR_TYPE_CD=="CO"&CNTRY_CD_2_DGT_ISO=="US"]
#drop the one case with an out-of-range date
swce_usco <- swce_usco[datemonth>=3]

#pull in cust trans data
#XID to GUID mapping (guid = hashed guid, GUID_ID = real GUID)
ctrans <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CE_SirenWorks_custtrans.csv")
setnames(ctrans,"EXT_USER_ID","guid")
#keep only mapping rows which contain guids in the SirenWorks data
ctrans <- ctrans[guid %in% swce_usco[,guid]]
#subtract 3 from fiscal month because SirenWorks data uses calendar month
#ctrans[, datetime := ymd_hms(RSPNS_DT)]
ctrans[, datemonth := FSCL_PER_IN_YR_NUM-3]

#swing wide by month
ctrans <- dcast.data.table(ctrans, GUID_ID + guid ~ datemonth, value.var="TRANS")
colnames(ctrans)[3:7] <- paste("transmnth", colnames(ctrans)[3:7], sep = "_")
ctrans[is.na(ctrans)] <- 0

#merge
transtemp <- merge(swce_usco,ctrans,by=c("guid"))

#calculate 30-day, 60-day, and 90-day trans
#march survey-takers
transtemp[datemonth==3, visits30 := rowSums(.SD, na.rm = TRUE), .SDcols = c("transmnth_4")]
transtemp[datemonth==3, visits60 := rowSums(.SD, na.rm = TRUE), .SDcols = c("transmnth_4","transmnth_5")]
transtemp[datemonth==3, visits90 := rowSums(.SD, na.rm = TRUE), .SDcols = c("transmnth_4","transmnth_5","transmnth_6")]
#april survey-takers
transtemp[datemonth==4, visits30 := rowSums(.SD, na.rm = TRUE), .SDcols = c("transmnth_5")]
transtemp[datemonth==4, visits60 := rowSums(.SD, na.rm = TRUE), .SDcols = c("transmnth_5","transmnth_6")]
transtemp[datemonth==4, visits90 := rowSums(.SD, na.rm = TRUE), .SDcols = c("transmnth_5","transmnth_6","transmnth_7")]

###correlation matrices
#set up functions
transcormat <- transtemp[, c("q22_1","q22_2","q22_3",grep("visits",colnames(transtemp),value=T)),with=FALSE]
#flatten the table
transcormat <- flattenSquareMatrix(cor.prob(transcormat))

#regress ad hocs on visits
summary(lm(visits30 ~ q22_1 + q22_2 + q22_3, data=transtemp))
summary(lm(visits60 ~ q22_1 + q22_2 + q22_3, data=transtemp))
summary(lm(visits90 ~ q22_1 + q22_2 + q22_3, data=transtemp))
#TB
summary(lm(visits30 ~ q22_1_tb + q22_2_tb + q22_3_tb, data=transtemp))
summary(lm(visits60 ~ q22_1_tb + q22_2_tb + q22_3_tb, data=transtemp))
summary(lm(visits90 ~ q22_1_tb + q22_2_tb + q22_3_tb, data=transtemp))
#regress all CE ques on visits
summary(lm(visits30 ~ q22_1 + q22_2 + q22_3 +
             q1 + q2_1 + q2_2 + q2_3 + q2_4 + q2_5 + q2_6 + q2_7 + q2_8, data=transtemp))
summary(lm(visits60 ~ q22_1 + q22_2 + q22_3 +
             q1 + q2_1 + q2_2 + q2_3 + q2_4 + q2_5 + q2_6 + q2_7 + q2_8, data=transtemp))
summary(lm(visits90 ~ q22_1 + q22_2 + q22_3 +
             q1 + q2_1 + q2_2 + q2_3 + q2_4 + q2_5 + q2_6 + q2_7 + q2_8, data=transtemp))

#create ad hoc TBs
transtemp[q22_1==7, q22_1_tb := 1];transtemp[q22_1<7, q22_1_tb := 0]
transtemp[q22_2==7, q22_2_tb := 1];transtemp[q22_2<7, q22_2_tb := 0]
transtemp[q22_3==7, q22_3_tb := 1];transtemp[q22_3<7, q22_3_tb := 0]

#calculate average visits by TB and non-TB for ad hocs
q22_1vis <- transtemp[!is.na(q22_1_tb)] %>%
  group_by(q22_1_tb) %>%
  summarise(visits30 = round(mean(visits30),1),
            visits60 = round(mean(visits60),1),
            visits90 = round(mean(visits90),1))
setDT(q22_1vis)
q22_2vis <- transtemp[!is.na(q22_2_tb)] %>%
  group_by(q22_2_tb) %>%
  summarise(visits30 = round(mean(visits30),1),
            visits60 = round(mean(visits60),1),
            visits90 = round(mean(visits90),1))
setDT(q22_2vis)
q22_3vis <- transtemp[!is.na(q22_3_tb)] %>%
  group_by(q22_3_tb) %>%
  summarise(visits30 = round(mean(visits30),1),
            visits60 = round(mean(visits60),1),
            visits90 = round(mean(visits90),1))
setDT(q22_3vis)
#test differences
#22_1
t.test(transtemp[q22_1_tb==0, visits30],transtemp[q22_1_tb==1, visits30])
t.test(transtemp[q22_1_tb==0, visits60],transtemp[q22_1_tb==1, visits60])
t.test(transtemp[q22_1_tb==0, visits90],transtemp[q22_1_tb==1, visits90])
#22_2
t.test(transtemp[q22_2_tb==0, visits30],transtemp[q22_2_tb==1, visits30])
t.test(transtemp[q22_2_tb==0, visits60],transtemp[q22_2_tb==1, visits60])
t.test(transtemp[q22_2_tb==0, visits90],transtemp[q22_2_tb==1, visits90])
#22_3
t.test(transtemp[q22_3_tb==0, visits30],transtemp[q22_3_tb==1, visits30])
t.test(transtemp[q22_3_tb==0, visits60],transtemp[q22_3_tb==1, visits60])
t.test(transtemp[q22_3_tb==0, visits90],transtemp[q22_3_tb==1, visits90])

#initial analyses:
#1-correlations between adhocs and other CE questions
#2-N by store
temp <- swce[, list(nguid = length(unique(guid))), by="stid"]
mean(temp[,nguid])

#set labels
xlabel <- "Number of Surveys"
ylabel <- "Number of Stores"
tlabel <- "CE survey count per store"
sublabel <- "SirenWorks ad hoc questions"
caption <- "Store N = 13,997\nSurvey N = 715,591"
#plot itself
plot2 <- ggplot(temp,aes(nguid)) + 
  geom_histogram(binwidth=1,show.legend=FALSE,fill="lightgrey",col=I("black")) + 
  theme_economist() + scale_colour_brewer(palette = 1, name="", labels="") +
  #scale_x_continuous(limits=c(-35,35), breaks = scales::pretty_breaks(35)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption)
print(plot2)

#2-N by store -- US COMPANY-OPERATED
temp_usco <- swce_usco[, list(nguid = length(unique(guid))), by="stid"]
mean(temp_usco[,nguid])

#set labels
xlabel <- "Number of Surveys"
ylabel <- "Number of Stores"
tlabel <- "CE survey count per store"
sublabel <- "SirenWorks ad hoc questions"
caption <- "US Company-Operated\nStore N = 8,033\nSurvey N = 616,440"
#plot itself
plot2 <- ggplot(temp_usco,aes(nguid)) + 
  geom_histogram(binwidth=1,show.legend=FALSE,fill="lightgrey",col=I("black")) + 
  theme_economist() + scale_colour_brewer(palette = 1, name="", labels="") +
  scale_x_continuous(limits=c(0,max(temp_usco[,nguid])), breaks = scales::pretty_breaks(20)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption)
print(plot2)

###correlation matrices
#set up functions
swcecorrmat <- swce_usco[, c("q1",grep("q2",colnames(swce_usco),value=T)),with=FALSE]

## correlation matrix with p-values
cor.prob <- function (X, dfr = nrow(X) - 2) {
  R <- cor(X, use="pairwise.complete.obs")
  above <- row(R) < col(R)
  r2 <- R[above]^2
  Fstat <- r2 * dfr/(1 - r2)
  R[above] <- 1 - pf(Fstat, 1, dfr)
  R[row(R) == col(R)] <- NA
  R
}
## create function to dump the cor.prob output to a 4 column matrix
## with row/column indices, correlation, and p-value.
flattenSquareMatrix <- function(m) {
  if( (class(m) != "matrix") | (nrow(m) != ncol(m))) stop("Must be a square matrix.") 
  if(!identical(rownames(m), colnames(m))) stop("Row and column names must be equal.")
  ut <- upper.tri(m)
  data.frame(i = rownames(m)[row(m)[ut]],
             j = rownames(m)[col(m)[ut]],
             cor=t(m)[ut],
             p=m[ut])
}
#flatten the table
cm <- flattenSquareMatrix(cor.prob(swcecorrmat))
write.csv(cm,file="O:/CoOp/CoOp194_PROReportng&OM/Julie/CE_SirenWorks_corrmatrix_usco.csv")

#regression to get R^2
lm0 <- lm(q1 ~ q22_1 + q22_2 + q22_3, data=swce_usco)



##BY DAY PART


#load data
#ce data from survox
swce <- fread("Q:/Departments/WMO/Marketing Research/New Q drive/Foundational/Customer Voice/2.0/Ad Hoc Q/2018_2_20_Siren Works Renovations/ce101_2017.csv")
swce[, transdatetime := ymd_hms(txdt)]
swce[, transtime := hour(transdatetime)]

#make day parts
swce[transtime<7, daypart := 1]
swce[transtime>=7&transtime<11, daypart := 2]
swce[transtime>=11&transtime<14, daypart := 3]
swce[transtime>=14&transtime<17, daypart := 4]
swce[transtime>=17, daypart := 5]

#reduce number of variables
swce <- swce[, c("guid","daypart","stid","q1",grep("q2",colnames(swce),value=T)),with=FALSE]

#store attributes
sta <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/store_attributes.csv")
setnames(sta,"STORE_NUM","stid")
sta <- sta[stid %in% swce[,stid]]
swce <- merge(swce,sta,by="stid")

#subset to US CO stores - AM
swce_usco_AM <- swce[OWNR_TYPE_CD=="CO"&CNTRY_CD_2_DGT_ISO=="US"&daypart==2]

# #2-N by store -- US COMPANY-OPERATED
# temp_usco_AM <- swce_usco_AM[, list(nguid = length(unique(guid))), by="stid"]
# 
# #N stores
# length(unique(temp_usco_AM[,stid]))
# #N and Average surveys
# sum(temp_usco_AM[,nguid])
# mean(temp_usco_AM[,nguid])
# #N 50+ and 70+
# length(unique(temp_usco_AM[nguid>=50,stid]))
# length(unique(temp_usco_AM[nguid>=70,stid]))
# 
# #set labels
# xlabel <- "Number of Surveys"
# ylabel <- "Number of Stores"
# tlabel <- "CE survey count per store: AM (7-11am)"
# sublabel <- "SirenWorks ad hoc questions"
# caption <- "US Company-Operated\nStore N = 8,031\nSurvey N = 243,288"
# #plot itself
# plot2 <- ggplot(temp_usco_AM,aes(nguid)) + 
#   geom_histogram(binwidth=1,show.legend=FALSE,fill="lightgrey",col=I("black")) + 
#   theme_economist() + scale_colour_brewer(palette = 1, name="", labels="") +
#   scale_x_continuous(limits=c(0,max(temp_usco_AM[,nguid])), breaks = scales::pretty_breaks(20)) +
#   xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption)
# print(plot2)

###correlation matrices
#set up functions
swcecorrmat <- swce_usco_AM[, c("q1",grep("q2",colnames(swce_usco_AM),value=T)),with=FALSE]
#flatten the table
cm_usco_AM <- flattenSquareMatrix(cor.prob(swcecorrmat))
write.csv(cm_usco_AM,file="O:/CoOp/CoOp194_PROReportng&OM/Julie/CE_SirenWorks_corrmatrix_usco_AM.csv")

#regression to get R^2
summary(lm(q1 ~ q22_1 + q22_2 + q22_3, data=swce_usco_AM))
summary(lm(q2_1 ~ q22_1 + q22_2 + q22_3, data=swce_usco_AM))
summary(lm(q2_2 ~ q22_1 + q22_2 + q22_3, data=swce_usco_AM))
summary(lm(q2_3 ~ q22_1 + q22_2 + q22_3, data=swce_usco_AM))
summary(lm(q2_4 ~ q22_1 + q22_2 + q22_3, data=swce_usco_AM))
summary(lm(q2_5 ~ q22_1 + q22_2 + q22_3, data=swce_usco_AM))
summary(lm(q2_6 ~ q22_1 + q22_2 + q22_3, data=swce_usco_AM))
summary(lm(q2_7 ~ q22_1 + q22_2 + q22_3, data=swce_usco_AM))
summary(lm(q2_8 ~ q22_1 + q22_2 + q22_3, data=swce_usco_AM))






#aggregate by day part
swce_usco_dp <- swce[OWNR_TYPE_CD=="CO"&CNTRY_CD_2_DGT_ISO=="US"]
swce_usco_dp[q22_1==7, q22_1_tb := 1];swce_usco_dp[q22_1<7, q22_1_tb := 0]
swce_usco_dp[q22_2==7, q22_2_tb := 1];swce_usco_dp[q22_2<7, q22_2_tb := 0]
swce_usco_dp[q22_3==7, q22_3_tb := 1];swce_usco_dp[q22_3<7, q22_3_tb := 0]

#q22_1
#2-N by store -- US COMPANY-OPERATED
swce_usco_dp1 <- swce_usco_dp[, list(q22_1_tb = sum(q22_1_tb,na.rm=T),
                                    q22_1_resp = .N),
                             by="daypart"]
#calc tb
swce_usco_dp1[, q22_1_score := round(q22_1_tb/q22_1_resp,3)*100]

#set labels
tlabel <- "Q22_1 by Day Part"
sublabel <- "This store takes coffee more seriously than other Starbucks stores"
caption <- "US Company Operated Stores"
#labels
xlabels <- c("Early AM","AM","Midday","PM","Late PM")
ylabel <- "Top Box Score"
xlabel <- "Day Part"
#values
pdata3 <- swce_usco_dp1
px3 <- swce_usco_dp1[,daypart]
py3 <- swce_usco_dp1[,q22_1_score]
nvar3 <- swce_usco_dp3[,q22_1_resp]
#plot itself
plot3 <- ggplot(data=pdata3,aes(y=py3,x=as.factor(px3))) + 
  geom_bar(stat="identity", width = 0.7, position=position_dodge(), fill="lightblue", colour="black") +
  scale_fill_brewer(palette = 2, guide=FALSE) + theme_economist() +
  scale_x_discrete(name=xlabel,labels=xlabels) +
  xlab("") + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) +
  geom_text(size = 3.5, aes(label=py3,y=0), stat="identity", vjust = -2) +
  geom_text(size = 2.5, aes(label=paste0("n=",nvar3),y=0), stat= "identity", vjust = -1) 
print(plot3)

#q22_2
#2-N by store -- US COMPANY-OPERATED
swce_usco_dp2 <- swce_usco_dp[, list(q22_2_tb = sum(q22_2_tb,na.rm=T),
                                     q22_2_resp = .N),
                              by="daypart"]
#calc tb
swce_usco_dp2[, q22_2_score := round(q22_2_tb/q22_2_resp,3)*100]

#set labels
tlabel <- "Q22_2 by Day Part"
sublabel <- "My beverage was crafted by skilled baristas"
caption <- "US Company Operated Stores"
#labels
xlabels <- c("Early AM","AM","Midday","PM","Late PM")
ylabel <- "Top Box Score"
xlabel <- "Day Part"
#values
pdata3 <- swce_usco_dp2
px3 <- swce_usco_dp2[,daypart]
py3 <- swce_usco_dp2[,q22_2_score]
nvar3 <- swce_usco_dp2[,q22_2_resp]
#plot itself
plot3 <- ggplot(data=pdata3,aes(y=py3,x=as.factor(px3))) + 
  geom_bar(stat="identity", width = 0.7, position=position_dodge(), fill="lightblue", colour="black") +
  scale_fill_brewer(palette = 2, guide=FALSE) + theme_economist() +
  scale_x_discrete(name=xlabel,labels=xlabels) +
  xlab("") + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) +
  geom_text(size = 3.5, aes(label=py3,y=0), stat="identity", vjust = -2) +
  geom_text(size = 2.5, aes(label=paste0("n=",nvar3),y=0), stat= "identity", vjust = -1) 
print(plot3)


#q22_3
#2-N by store -- US COMPANY-OPERATED
swce_usco_dp3 <- swce_usco_dp[, list(q22_3_tb = sum(q22_3_tb,na.rm=T),
                                     q22_3_resp = .N),
                              by="daypart"]
#calc tb
swce_usco_dp3[, q22_3_score := round(q22_3_tb/q22_3_resp,3)*100]

#set labels
tlabel <- "Q22_3 by Day Part"
sublabel <- "I enjoyed being at this store"
caption <- "US Company Operated Stores"
#labels
xlabels <- c("Early AM","AM","Midday","PM","Late PM")
ylabel <- "Top Box Score"
xlabel <- "Day Part"
#values
pdata3 <- swce_usco_dp3
px3 <- swce_usco_dp3[,daypart]
py3 <- swce_usco_dp3[,q22_3_score]
nvar3 <- swce_usco_dp3[,q22_3_resp]
#plot itself
plot3 <- ggplot(data=pdata3,aes(y=py3,x=as.factor(px3))) + 
  geom_bar(stat="identity", width = 0.7, position=position_dodge(), fill="lightblue", colour="black") +
  scale_fill_brewer(palette = 2, guide=FALSE) + theme_economist() +
  scale_x_discrete(name=xlabel,labels=xlabels) +
  xlab("") + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) +
  geom_text(size = 3.5, aes(label=py3,y=0), stat="identity", vjust = -2) +
  geom_text(size = 2.5, aes(label=paste0("n=",nvar3),y=0), stat= "identity", vjust = -1) 
print(plot3)



#aggregate by day part
swce_usco_dp <- swce[OWNR_TYPE_CD=="CO"&CNTRY_CD_2_DGT_ISO=="US"]
swce_usco_dp[q2_1==7, q2_1_tb := 1];swce_usco_dp[q2_1<7, q2_1_tb := 0]
swce_usco_dp[q2_2==7, q2_2_tb := 1];swce_usco_dp[q2_2<7, q2_2_tb := 0]
swce_usco_dp[q2_3==7, q2_3_tb := 1];swce_usco_dp[q2_3<7, q2_3_tb := 0]
swce_usco_dp[q2_4==7, q2_4_tb := 1];swce_usco_dp[q2_4<7, q2_4_tb := 0]
swce_usco_dp[q2_5==7, q2_5_tb := 1];swce_usco_dp[q2_5<7, q2_5_tb := 0]
swce_usco_dp[q2_6==7, q2_6_tb := 1];swce_usco_dp[q2_6<7, q2_6_tb := 0]
swce_usco_dp[q2_7==7, q2_7_tb := 1];swce_usco_dp[q2_7<7, q2_7_tb := 0]
swce_usco_dp[q2_8==7, q2_8_tb := 1];swce_usco_dp[q2_8<7, q2_8_tb := 0]

#q2_2
#2-N by store -- US COMPANY-OPERATED
swce_usco_dp3 <- swce_usco_dp[, list(q2_2_tb = sum(q2_2_tb,na.rm=T),
                                     q2_2_resp = .N),
                              by="daypart"]
#calc tb
swce_usco_dp3[, q2_2_score := round(q2_2_tb/q2_2_resp,3)*100]

#set labels
tlabel <- "Customer Connection by Day Part"
sublabel <- "SirenWorks ad hoc questions"
caption <- "US Company Operated Stores"
#labels
xlabels <- c("Early AM","AM","Midday","PM","Late PM")
ylabel <- "Top Box Score"
xlabel <- "Day Part"
#values
pdata3 <- swce_usco_dp3
px3 <- swce_usco_dp3[,daypart]
py3 <- swce_usco_dp3[,q2_2_score]
nvar3 <- swce_usco_dp3[,q2_2_resp]
#plot itself
plot3 <- ggplot(data=pdata3,aes(y=py3,x=as.factor(px3))) + 
  geom_bar(stat="identity", width = 0.7, position=position_dodge(), fill="lightblue", colour="black") +
  scale_fill_brewer(palette = 2, guide=FALSE) + theme_economist() +
  scale_x_discrete(name=xlabel,labels=xlabels) +
  xlab("") + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) +
  geom_text(size = 3.5, aes(label=py3,y=0), stat="identity", vjust = -2) +
  geom_text(size = 2.5, aes(label=paste0("n=",nvar3),y=0), stat= "identity", vjust = -1) 
print(plot3)

# #N stores
# length(unique(temp_usco_PM[,stid]))
# #N and Average surveys
# sum(temp_usco_PM[,nguid])
# mean(temp_usco_PM[,nguid])
# #N 50+ and 70+
# length(unique(temp_usco_PM[nguid>=50,stid]))
# length(unique(temp_usco_PM[nguid>=70,stid]))
# 
# #set labels
# xlabel <- "Number of Surveys"
# ylabel <- "Number of Stores"
# tlabel <- "CE survey count per store: PM (2-5pm)"
# sublabel <- "SirenWorks ad hoc questions"
# caption <- "US Company-Operated\nStore N = 8,032\nSurvey N = 124,523"
# #plot itself
# plot2 <- ggplot(temp_usco_PM,aes(nguid)) + 
#   geom_histogram(binwidth=1,show.legend=FALSE,fill="lightgrey",col=I("black")) + 
#   theme_economist() + scale_colour_brewer(palette = 1, name="", labels="") +
#   scale_x_continuous(limits=c(0,max(temp_usco_PM[,nguid])), breaks = scales::pretty_breaks(20)) +
#   xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption)
# print(plot2)

###correlation matrices
#set up functions
swcecorrmat <- swce_usco_PM[, c("q1",grep("q2",colnames(swce_usco_PM),value=T)),with=FALSE]
#flatten the table
cm_usco_PM <- flattenSquareMatrix(cor.prob(swcecorrmat))
write.csv(cm_usco_PM,file="O:/CoOp/CoOp194_PROReportng&OM/Julie/CE_SirenWorks_corrmatrix_usco_PM.csv")

#regression to get R^2
summary(lm(q1 ~ q22_1 + q22_2 + q22_3, data=swce_usco_PM))
summary(lm(q2_1 ~ q22_1 + q22_2 + q22_3, data=swce_usco_PM))
summary(lm(q2_2 ~ q22_1 + q22_2 + q22_3, data=swce_usco_PM))
summary(lm(q2_3 ~ q22_1 + q22_2 + q22_3, data=swce_usco_PM))
summary(lm(q2_4 ~ q22_1 + q22_2 + q22_3, data=swce_usco_PM))
summary(lm(q2_5 ~ q22_1 + q22_2 + q22_3, data=swce_usco_PM))
summary(lm(q2_6 ~ q22_1 + q22_2 + q22_3, data=swce_usco_PM))
summary(lm(q2_7 ~ q22_1 + q22_2 + q22_3, data=swce_usco_PM))
summary(lm(q2_8 ~ q22_1 + q22_2 + q22_3, data=swce_usco_PM))







