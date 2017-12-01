## Training Hours Overspend Analysis -- Pulse & CC ##
## Request from Lisa 11/28/17 ##

#load libraries
library(data.table)
library(plyr)

#load data
#training hours
####CSV flat file from macro-enabled .xlsb (project folder)
th <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/q1laborreport.csv")
#setnames
setnames(th,c("STORE_NUM","FISCAL_WEEK_NUMBER","QTD_ACTUAL_TRAINING"),
         c("store_num","fiscalweek","thours"))
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
hi <- hi[, c("newhires","store_num","fiscalweek"), with=F]

#headcount data
he <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/headcount_by_storenumber.csv")
he1 <- he[FSCL_WK_IN_QTR_NUM==4]
he1 <- rbind(he1,he1,he1,he1)
fiscalweek <- rep(1:4, each=length(he1[,STORE_NUM]))
he1 <- cbind(he1,fiscalweek)
he2 <- he[FSCL_WK_IN_QTR_NUM==8]
he2 <- rbind(he2,he2,he2,he2)
fiscalweek <- rep(5:8, each=length(he2[,STORE_NUM]))
he2 <- cbind(he2,fiscalweek)
he <- rbind(he1,he2)
he[, FSCL_WK_IN_QTR_NUM := NULL]
setnames(he,"STORE_NUM","store_num")


#organize pulse data
temp1 <- p %>% 
  group_by(calweek, Question_ID, store_num) %>%
  summarise(totalresp = n())
setDT(temp1)
temp2 <- p %>% 
  filter(RespID==7) %>%
  group_by(calweek, Question_ID, store_num) %>%
  summarise(tbcount = n())
setDT(temp2)
pf <- left_join(temp1,temp2,by=c("calweek","Question_ID","store_num"))
setDT(pf)
pf[is.na(pf[,tbcount]), tbcount := 0]
pf <- dcast.data.table(pf, calweek + store_num ~ Question_ID, value.var=c("tbcount","totalresp"))

#merge
full <- left_join(th, cc, by=c("store_num","fiscalweek"))
full <- left_join(full, pf, by=c("store_num","calweek"))
full <- left_join(full, hi, by=c("store_num","fiscalweek"))
full <- left_join(full, he, by=c("store_num","fiscalweek"))
setDT(full)
#get rid of calweek
full[, calweek := NULL]
full <- full[, lapply(.SD,sum,na.rm=T), by=c("store_num","fiscalweek")]

# #create delta variable
# full <- setorder(full,store_num,fiscalweek)
# #this basic shift takes previous store value - *not* necessarily the previous week
# full[, thourslag := shift(thours, 1L, fill=NA, type="lag"), by="store_num"]

#aggregate up to get topline
fullwk <- full[, store_num := NULL]
fullwk <- fullwk[, lapply(.SD,sum,na.rm=T), by=c("fiscalweek")]
fullwk <- setorder(fullwk,fiscalweek)
fullwk[, thourslag := shift(thours, 1L, fill=NA, type="lag")]
fullwk[fiscalweek>1, tdelta := thours - thourslag]

#calculate training hours
fullwk[, newhirehalf := newhires/2]
fullwk[, newhirehalflag := shift(newhirehalf, 1L, fill=NA, type="lag")]
fullwk[, newhiresplit := newhirehalf + newhirehalflag]
fullwk[, newhirethours := newhiresplit * (19.5/2)]

#create PPK measure (headcount*1 hour)
fullwk[fiscalweek==4, ppkhours := AvgHeadcount*0.33]

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
llabels <- c("CC","Lived up to values","Connect with team","Supported","Feel proud",
             "Connect with customers","Reasonable demands") 
xbreaks <- 8
ybreaks <- 7
#line chart, factored by one variable
plot1 <- ggplot() +
  geom_line(data=pdata, aes(x=px, y=py, group=factor(groupvar), colour=factor(groupvar))) + 
  geom_line(data=m[question=="ccscore"], aes(x=m[question=="ccscore",fiscalweek], y=m[question=="ccscore",(thours+550000)/2200000])) +
  xlab(xlabel) + ylab(ylabel) + theme_bw() +
  scale_colour_discrete(name=lname, labels=llabels, guide=guide_legend(order=1)) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  scale_x_continuous(limits=c(pdata[,min(px)],pdata[,max(px)]), breaks = scales::pretty_breaks(n = xbreaks)) +
  scale_y_continuous(limits=c(pdata[,min(py)*.85],pdata[,max(py)*1.1]),
                     breaks = scales::pretty_breaks(n = ybreaks), labels=scales::percent, "TB Score",
                     sec.axis=sec_axis(~.*2200000 - 550000, 
                     breaks = scales::pretty_breaks(n = ybreaks), labels=comma, name="Training Hours")) +
  labs(title = tlabel, subtitle = slabel)
print(plot1)

##observation 1: big spike in week 4 training hours == upcoming holiday launch
##observation 2: precipitous drop-off following week 5


