## Training Hours Overspend Analysis -- Pulse & CC ##
## Request from Lisa 11/28/17 ##

#load libraries
library(data.table)
library(dplyr)

#load data
#training hours
####CSV flat file from macro-enabled .xlsb (project folder)
th <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/q1laborreport.csv")
#setnames
setnames(th,c("STORE_NUM","FISCAL_WEEK_NUMBER","QTD_ACTUAL_TRAINING"),
         c("store_num","fiscalweek","thours"))
#aggregate by fiscal week to get a sense of the number of additional hours
tempth <- th[, list(thours = round(sum(thours,na.rm=T),0)), by="fiscalweek"]

# ####holiday hours: CSV flat file from macro-enabled .xlsb (project folder)
# hol <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/q1laborreport_holiday.csv")
# #setnames
# setnames(hol,c("STORE_NUM","FSCL_WK_IN_YR_NUM","TOTAL_HRS"),
#          c("store_num","fiscalweek","holhours"))
# #aggregate by fiscal week to get a sense of the number of additional hours
# temphol <- hol[, list(holhours = round(sum(holhours,na.rm=T),0)), by="fiscalweek"]
# #subtract 1 week for holiday count, as they prep in advance
# hol[, fiscalweek := fiscalweek-1]
# #keep weeks 5 and 6 budget. agg together. split in half and parse between FW 4 & 5
# # hol <- hol[fiscalweek==5|fiscalweek==6]
# # hol <- hol[, list(holhours = sum(holhours,na.rm=T)), by="store_num"]
# # hol[, holhours := holhours/2]
# # hol <- rbind(hol,hol)
# # fwvec <- rep(4:5,each=length(unique(hol[,store_num])))
# # hol <- cbind(hol,fwvec)
# # setnames(hol,"fwvec","fiscalweek")


#CC data
cc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc_by_store_q1laborreport.csv")
setnames(cc,c("STORE_NUM","FSCL_WK_IN_YR_NUM","CAL_WK_IN_YR_NUM","CCTOTALRESP","CCTBCOUNT"),
         c("store_num","fiscalweek","calweek","totalresp_cc","tbcount_cc"))
cc[, c("store_num") := lapply(.SD, as.numeric), .SDcols=c("store_num")]

#pulse data
p <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/pulse_by_store_q1laborreport.csv")
setnames(p,c("PersonnelNumber","survWeek","STORE_NUM_ASSIGNED"),c("pn","calweek","store_num"))
p <- na.omit(p, cols="store_num")

#hire data
# h <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/hire-rehire_count_bystoreFY18.csv", colClasses=list(character=6))
# h[, store_num := as.numeric(StoreAbbrev)]
# h[, fiscalmonth := as.numeric(gsub('FP-2018-', '', Fiscal_Month))]
# h <- h[, c("store_num","fiscalmonth","tally"), with=F]
# h <- h[, list(tally = sum(tally,na.rm=T)), by=c("store_num","fiscalmonth")]
# h <- na.omit(h, cols="store_num")
hi <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/hiredate_by_storenumber.csv")
setnames(hi,c("STORE_NUM","FSCL_WK_IN_YR_NUM"),c("store_num","fiscalweek"))
#keep new baristas and promoted shifts
hi <- hi[(New_Job_Key==50000362&Move_Type==1)|(New_Job_Key==50000358&Move_Type==2)]
hi <- hi[, c("newhires","store_num","fiscalweek","New_Job_Key"), with=F]
#swing wide
hi <- dcast.data.table(hi, store_num + fiscalweek ~ New_Job_Key, value.var="newhires")
setnames(hi, c("50000362","50000358"),c("nh_bar","nh_shift"))
hi[is.na(hi)] <- 0

#headcount data
he <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/headcount_by_storenumber.csv")
#period 1
he1 <- he[FSCL_WK_IN_QTR_NUM==4]
he1 <- rbind(he1,he1,he1,he1)
fiscalweek <- rep(1:4, each=length(he1[,STORE_NUM]))
he1 <- cbind(he1,fiscalweek)
#period 2
he2 <- he[FSCL_WK_IN_QTR_NUM==8]
he2 <- rbind(he2,he2,he2,he2)
fiscalweek <- rep(5:8, each=length(he2[,STORE_NUM]))
he2 <- cbind(he2,fiscalweek)
#period 3
he3 <- he[FSCL_WK_IN_QTR_NUM==13]
he3 <- rbind(he3,he3,he3,he3,he3)
fiscalweek <- rep(9:13, each=length(he3[,STORE_NUM]))
he3 <- cbind(he3,fiscalweek)
#bind together
he <- rbind(he1,he2,he3)
he[, FSCL_WK_IN_QTR_NUM := NULL] #remove old week variable
setnames(he,"STORE_NUM","store_num") #rename for merging
#average by fiscal week and job code
he <- he[, list(AvgHeadcount = mean(AvgHeadcount,na.rm=T)), by=c("store_num","fiscalweek","Job_Key")]
#swing wide
he <- dcast.data.table(he, store_num + fiscalweek ~ Job_Key, value.var="AvgHeadcount")
setnames(he, c("50000362","50000358","50000117","50000118"),c("hcnt_bar","hcnt_shift","hcnt_sm","hcnt_asm"))
he[is.na(he)] <- 0

#organize pulse data
temp1 <- p %>% 
  group_by(calweek, Question_ID, store_num) %>%
  dplyr::summarise(totalresp = n())
setDT(temp1)
temp2 <- p %>% 
  filter(RespID==7) %>%
  group_by(calweek, Question_ID, store_num) %>%
  dplyr::summarise(tbcount = n())
setDT(temp2)
pf <- left_join(temp1,temp2,by=c("calweek","Question_ID","store_num"))
setDT(pf)
pf[is.na(pf[,tbcount]), tbcount := 0]
pf <- dcast.data.table(pf, calweek + store_num ~ Question_ID, value.var=c("tbcount","totalresp"))

#joins
#get rid of calendar week
pfcc <- merge(cc,pf, by=c("store_num","calweek"), all=FALSE)
pfcc[, calweek := NULL]
pfcc <- pfcc[, lapply(.SD,sum,na.rm=T), by=c("store_num","fiscalweek")]
#join together
full <- merge(th, pfcc, by=c("store_num","fiscalweek"), all=FALSE)
full <- merge(full, hi, by=c("store_num","fiscalweek"), all.x=T)
full <- merge(full, he, by=c("store_num","fiscalweek"), all.x=T)
# full <- merge(full, hol, by=c("store_num","fiscalweek"), all.x=T)
#aggregate
full <- full[, lapply(.SD,sum,na.rm=T), by=c("store_num","fiscalweek")]

# #create delta variable
# full <- setorder(full,store_num,fiscalweek)
# #this basic shift takes previous store value - *not* necessarily the previous week
# full[, thourslag := shift(thours, 1L, fill=NA, type="lag"), by="store_num"]

## 12/6/17 interjection ##


#generate variable for total hours per hourly worker (shift + barista)
full[, thoursphp := thours/(hcnt_shift+hcnt_bar)]

#compare week 8 to average of weeks 1-3
full[fiscalweek==8, fywk8 := 1]
full[fiscalweek>=1&fiscalweek<=3, fywk8 := 0]
full <- full[fywk8==0|fywk8==1]
full[, fiscalweek := NULL]

#aggregate
full <- full[, lapply(.SD,mean,na.rm=T), .SDcols=c("thours","hcnt_shift","hcnt_bar","thoursphp"), by=c("store_num","fywk8")]




















#aggregate up to get topline
fullwk <- full[, store_num := NULL]
fullwk <- fullwk[, lapply(.SD,sum,na.rm=T), by=c("fiscalweek")]
fullwk <- setorder(fullwk,fiscalweek)
fullwk[, thourslag := shift(thours, 1L, fill=NA, type="lag")]
fullwk[fiscalweek>1, tdelta := thours - thourslag]

#calculate average headcount by adding together headcount of different roles
fullwk[, AvgHeadcount := rowSums(.SD,na.rm=T), by=c("hcnt_bar","hcnt_shift","hcnt_sm","hcnt_asm")]

#calculate training hours
# fullwk[, newhirehalf := newhires/2]
# fullwk[, newhirehalflag := shift(newhirehalf, 1L, fill=NA, type="lag")]
# fullwk[, newhiresplit := ifelse(is.na(newhirehalflag),newhirehalf,newhirehalf+newhirehalflag)]
# fullwk[, newhirethours := newhiresplit * (19.5/2)]
fullwk[, newbaristathours := nh_bar * ((19.5 + 12.5 + 3.25))] #need to split into two weeks
fullwk[, proshiftthours := nh_shift * ((10.5 + 6.5))] #need to split into two weeks
fullwk[, safetytraining := AvgHeadcount * .25]
fullwk[, newsafethours := rowSums(.SD,na.rm=T), .SDcols=c("newbaristathours", "proshiftthours","safetytraining")]

# #create PPK measure (headcount*1 hour)
# fullwk[fiscalweek==4, ppkhours := AvgHeadcount*0.33]
# create PPK measure based on Tea's values 
fullwk[fiscalweek==4, ppkhours := AvgHeadcount*(1.25/2)]
fullwk[fiscalweek==5, ppkhours := AvgHeadcount*(0.75/2)]

#create updated total hour measure, which subtracts out holiday prep hours
# fullwk[, thoursnohol := thours-holhours]
fullwk[, thoursnoppk := ifelse(is.na(ppkhours),thours,thours-ppkhours)]
# #create updated total hour measure, which subtracts out half of holiday prep hours
# fullwk[, thourshalfhol := thours-(holhours/2)]
# #create updated total hour measure, with updated holiday hour measure
# fullwk[, thoursnohol_upd := thours-holhours]

#cc top box
fullwk[, ccscore := tbcount_cc / totalresp_cc]
fullwk[, Q1score := tbcount_Q1 / totalresp_Q1]
fullwk[, Q2_Ascore := tbcount_Q2_A / totalresp_Q2_A]
fullwk[, Q2_Bscore := tbcount_Q2_B / totalresp_Q2_B]
fullwk[, Q2_Cscore := tbcount_Q2_C / totalresp_Q2_C]
fullwk[, Q2_Dscore := tbcount_Q2_D / totalresp_Q2_D]
fullwk[, Q2_Escore := tbcount_Q2_E / totalresp_Q2_E]

#melt for plotting
m <- fullwk[, c("fiscalweek","thours",grep("score", names(fullwk), value=T)), with=F]
m <- melt(m, id=c("fiscalweek","thours"), 
          variable.name = "question", 
          value.name = "tbscore", 
          na.rm = T, #NA's removed from molten data when T
          variable.factor = F) #turns variable into factor

#plot trends
#set labels
xlabel <- "Fiscal Week (Q1 FY18)"
ylabel <- "Top Box Scores"
tlabel <- "Training Hours"
slabel <- "Customer Connection & Pulse Scores"
#set data and variables
pdata <- m
px <- m[, fiscalweek]
py <- m[, tbscore]
groupvar <- m[, question]
#manual legend labels
lname <- "Top Box Variable"
llabels <- c("Customer connection","Lived up to values","Connect with team","Supported","Feel proud",
             "Connect with customers","Reasonable demands") 
xbreaks <- 8
ybreaks <- 7
#line chart, factored by one variable
plot1a <- ggplot() +
  geom_line(data=pdata, aes(x=px, y=py, group=factor(groupvar), colour=factor(groupvar), linetype=factor(groupvar))) + 
  xlab(xlabel) + ylab(ylabel) + theme_bw() +
  scale_colour_discrete(name=lname, labels=llabels, guide=guide_legend(order=1)) +
  scale_linetype_manual(name=lname, labels=llabels, guide=guide_legend(order=1), values=c(5,1,1,1,1,1,1)) +
  scale_x_continuous(limits=c(pdata[,min(px)],pdata[,max(px)]), breaks = scales::pretty_breaks(n = xbreaks)) +
  labs(title = tlabel, subtitle = slabel)
print(plot1a)
plot1b <- ggplot() +
  geom_line(data=m[question=="ccscore"], aes(x=m[question=="ccscore",fiscalweek], y=m[question=="ccscore",thours])) +
  xlab(xlabel) + ylab(ylabel) + theme_bw() +
  scale_colour_discrete(name=lname, labels=llabels, guide=guide_legend(order=1)) +
  scale_y_continuous(limits=c(m[question=="ccscore",min(thours)*.75],m[question=="ccscore",max(thours)*1.15]),
                     breaks = scales::pretty_breaks(n = ybreaks), labels=comma, name="Training Hours") +
  labs(title = tlabel, subtitle = slabel)
print(plot1b)




##observation 1: big spike in week 4 training hours == upcoming holiday launch
##observation 2: precipitous drop-off following week 5

#melt for plotting
n <- fullwk[, c("fiscalweek","thoursnohol",grep("score", names(fullwk), value=T)), with=F]
n <- melt(n, id=c("fiscalweek","thoursnohol"), 
          variable.name = "question", 
          value.name = "tbscore", 
          na.rm = T, #NA's removed from molten data when T
          variable.factor = F) #turns variable into factor

#plot trends
#set labels
xlabel <- "Fiscal Week (Q1 FY18)"
ylabel <- "Top Box Scores"
tlabel <- "Training Hours - with Holiday Additional Hours Removed"
slabel <- "Customer Connection & Pulse Scores"
#set data and variables
pdata <- n
px <- n[, fiscalweek]
py <- n[, tbscore]
groupvar <- n[, question]
#manual legend labels
lname <- "Top Box Variable"
llabels <- c("Customer connection","Lived up to values","Connect with team","Supported","Feel proud",
             "Connect with customers","Reasonable demands") 
xbreaks <- 8
ybreaks <- 7
# #line chart, factored by one variable
# plot2 <- ggplot() +
#   geom_line(data=pdata, aes(x=px, y=py, group=factor(groupvar), colour=factor(groupvar), linetype=factor(groupvar))) + 
#   geom_line(data=n[question=="ccscore"], aes(x=n[question=="ccscore",fiscalweek], y=n[question=="ccscore",(thoursnohol+1155000)/5200000])) +
#   xlab(xlabel) + ylab(ylabel) + theme_bw() +
#   scale_colour_discrete(name=lname, labels=llabels, guide=guide_legend(order=1)) +
#   #scale_colour_discrete("") +
#   scale_linetype_manual(name=lname, labels=llabels, guide=guide_legend(order=1), values=c(5,1,1,1,1,1,1)) +
#   #guides(colour = guide_legend(override.aes = list(size = 7))) + 
#   scale_x_continuous(limits=c(pdata[,min(px)],pdata[,max(px)]), breaks = scales::pretty_breaks(n = xbreaks)) +
#   scale_y_continuous(limits=c(pdata[,min(py)*.75],pdata[,max(py)*1.15]),
#                      breaks = scales::pretty_breaks(n = ybreaks), labels=scales::percent, "TB Score",
#                      sec.axis=sec_axis(~.*5200000 - 1155000, 
#                                        breaks = scales::pretty_breaks(n = ybreaks), labels=comma, name="Training Hours")) +
#   labs(title = tlabel, subtitle = slabel)
# print(plot2)
plot2b <- ggplot() +
  geom_line(data=n[question=="ccscore"], aes(x=n[question=="ccscore",fiscalweek], y=n[question=="ccscore",thoursnohol])) +
  xlab(xlabel) + ylab(ylabel) + theme_bw() +
  scale_colour_discrete(name=lname, labels=llabels, guide=guide_legend(order=1)) +
  scale_y_continuous(limits=c(n[question=="ccscore",min(thoursnohol)*.75],n[question=="ccscore",max(thoursnohol)*1.15]),
                     breaks = scales::pretty_breaks(n = ybreaks), labels=comma, name="Training Hours") +
  labs(title = tlabel, subtitle = slabel)
print(plot2b)


#melt for plotting
o <- fullwk[, c("fiscalweek","thours","ppkhours","thoursnoppk","newsafethours",grep("score", names(fullwk), value=T)), with=F]
o <- melt(o, id=c("fiscalweek","thours","ppkhours","thoursnoppk","newsafethours"), 
          variable.name = "question", 
          value.name = "tbscore", 
          na.rm = T, #NA's removed from molten data when T
          variable.factor = F) #turns variable into factor

#plot trends
#set labels
xlabel <- "Fiscal Week (Q1 FY18)"
ylabel <- "Top Box Scores"
tlabel <- "Training hours, customer connection & pulse scores"
slabel <- "QTD actual (black line), actual minus holiday [JM calc] (red line), \nnew hire and safety training hours [JM calc] (blue line)"
#set data and variables
pdata <- o
px <- o[, fiscalweek]
py <- o[, tbscore]
groupvar <- o[, question]
#manual legend labels
lname <- "Top Box Variable"
llabels <- c("Customer connection","Lived up to values","Connect with team","Supported","Feel proud",
             "Connect with customers","Reasonable demands") 
xbreaks <- 8
ybreaks <- 7
#line chart, factored by one variable
plot3 <- ggplot() +
  geom_line(data=pdata, aes(x=px, y=py, group=factor(groupvar), colour=factor(groupvar), linetype=factor(groupvar))) +
  geom_line(data=o[question=="ccscore"], aes(x=o[question=="ccscore",fiscalweek], y=o[question=="ccscore",(thours+130000)/650000])) +
  geom_line(data=o[question=="ccscore"&fiscalweek>=3&fiscalweek<=6], aes(x=o[question=="ccscore"&fiscalweek>=3&fiscalweek<=6,fiscalweek], y=o[question=="ccscore"&fiscalweek>=3&fiscalweek<=6,(thoursnoppk+130000)/650000]),color='red') +
  geom_line(data=o[question=="ccscore"], aes(x=o[question=="ccscore",fiscalweek], y=o[question=="ccscore",(newsafethours+130000)/650000]),color='blue') +
  #geom_point(data=o[question=="ccscore"&ppkhours>0], aes(x=o[question=="ccscore"&ppkhours>0,fiscalweek], y=o[question=="ccscore"&ppkhours>0,(ppkhours+130000)/650000]),color='red') +
  xlab(xlabel) + ylab(ylabel) + theme_bw() +
  scale_colour_discrete(name=lname, labels=llabels, guide=guide_legend(order=1)) +
  scale_linetype_manual(name=lname, labels=llabels, guide=guide_legend(order=1), values=c(5,1,1,1,1,1,1)) +
  scale_x_continuous(limits=c(pdata[,min(px)],pdata[,max(px)]), breaks = scales::pretty_breaks(n = xbreaks)) +
  scale_y_continuous(limits=c(pdata[,min(py)*.65],pdata[,max(py)*1.25]),
                     breaks = scales::pretty_breaks(n = ybreaks), labels=scales::percent, "TB Score",
                     sec.axis=sec_axis(~.*650000 - 130000, 
                                       breaks = scales::pretty_breaks(n = ybreaks), labels=comma, name="Training Hours")) +
  labs(title = tlabel, subtitle = slabel)
print(plot3)

# #merge together
# tempthhol <- left_join(tempth,temphol,by="fiscalweek")
# setDT(tempthhol)
# tempthhol[, thoursnohol := ifelse(is.na(holhours),thours,thours-holhours)]
# temphol2 <- temphol[, fiscalweek := fiscalweek-1]
# tempthhol2 <- left_join(tempth,temphol2,by="fiscalweek")
# setDT(tempthhol2)
# tempthhol2[, thoursnohol := ifelse(is.na(holhours),thours,thours-holhours)]
