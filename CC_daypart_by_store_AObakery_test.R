



data_dir <- "O:/CoOp/CoOp194_PROReportng&OM/Julie"
aob <- fread(paste0(data_dir,"/AO_Bakery_CC_test.csv"))
aob <- aob[, .(TEST_FLAG, DAY_PART, TB_SCORE, TOTAL_RSPNS, FSCL_YR_NUM)]
aobw <- dcast.data.table(aob, TEST_FLAG + DAY_PART ~ FSCL_YR_NUM, value.var=c("TB_SCORE","TOTAL_RSPNS"))
setnames(aobw,c("TB_SCORE_2017","TB_SCORE_2018"),c("cc17","cc18"))

#update vars
aobw[, cc17 := cc17*100]
aobw[, cc18 := cc18*100]
#create delta
aobw[, ccyoy := round(cc18-cc17,1)]

#create pvar
aobw[, ptest := as.character(ccyoy)]
aobw[(DAY_PART>=2)&TEST_FLAG==0, ptest := paste0(ccyoy,"***")]


#plot 1: 
#set labels
xlabels <- c("Early AM\n(pre-7am)","AM\n(7-11am)","Midday\n(11am-2pm)","PM\n(2-5pm)","Late PM\n(post-5pm)")
ylabel <- "TB Score Delta"
tlabel <- "Customer Connection YoY Deltas by Day Part: AO Bakery Test"
sublabel <- "Fiscal Weeks 7, 9-12, 15-22, Fiscal Years 2017 and 2018"
caption <- "AO bakery test N = 154\nComparison group contains all US CO Stores not included in AO Bakery test set (N = 8,260)\nTests of significant YoY difference: *p<.01, **p<.05, ***p<.001"
#manual legend labels
lname <- "Test Group"
llabels <- c("US CO Stores (Non-Test)","Test Stores")
##CC Delta
pdata1a <- aobw
px1a <- aobw[,DAY_PART]
py1a <- aobw[,ccyoy]
deltavar <- aobw[,ptest]
groupvar1a <- aobw[,TEST_FLAG]
nvar <- aobw[,TOTAL_RSPNS_2018]
plot1a <- ggplot(data=pdata1a,aes(y=py1a,x=as.factor(px1a),fill=as.factor(groupvar1a))) + 
  geom_bar(stat="identity", width = 0.95, position=position_dodge(), colour="black") +
  scale_fill_brewer(palette = 1, name="", labels=llabels) + theme_economist() +
  scale_x_discrete(name="",labels=xlabels) + 
  scale_y_continuous(limits=c(-1.5,2)) +
  xlab("") + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) +
  geom_text(size = 3.5, aes(label=deltavar,y=1.5), stat="identity", position = position_dodge(0.95)) +
  geom_text(size = 2.25, aes(label=paste0("n=",nvar),y=-.1), stat="identity", position = position_dodge(0.95)) 
print(plot1a)



# #melt
# aobwm <- melt(aobw, id=c("TEST_FLAG","DAY_PART"))
# aobwm[variable=="cc18", yoy := aobwm[variable=="ccyoy", value]]
# aobwm <- aobwm[variable=="cc17"|variable=="cc18"]
# 
# #plot 2: 
# #set labels
# xlabels <- c("Early AM","AM","Midday","PM","Late PM")
# ylabel <- "TB Score"
# tlabel <- "Customer Connection by Day Part: AO Bakery Test"
# sublabel <- "Fiscal Weeks 7, 9-12, 15-22"
# caption <- "AO Bakery test N = 154\nComparison group contains all US CO Stores not included in AO Bakery test set"
# #manual legend labels
# lname <- "Test Group"
# llabels <- c("US CO Stores (non-test)",
#              "Test Stores")
# lname2 <- ""
# llabels2 <- c("2017","2018")
# #values
# pdata1a <- aobwm
# px1a <- aobwm[,DAY_PART]
# py1a <- aobwm[,value]
# groupvar1a <- aobwm[,TEST_FLAG]
# colourvar <- aobw[,variable]
# #plot itself
# plot1a <- ggplot(data=pdata1a,aes(y=py1a,x=as.factor(px1a),colour=factor(colourvar), fill=factor(groupvar1a), group=interaction(groupvar1a, colourvar))) + 
#   geom_bar(stat="identity", width = 0.95, position=position_dodge(), colour="black") +
#   scale_fill_brewer(palette = 1, name=lname, labels=llabels) + theme_economist() +
#   scale_x_discrete(name="",labels=xlabels) + 
#   scale_colour_discrete(name=lname2, labels=llabels2, guide=guide_legend(order=1)) +
#   guides(colour = guide_legend(override.aes = list(size = 5))) + 
#   scale_y_continuous(limits=c(0,45)) +
#   xlab("") + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) +
#   geom_text(size = 3.5, aes(label=py1a,y=42), stat="identity", position = position_dodge(0.95)) 
# print(plot1a)


#plot 2: 
#set labels
xlabels <- c("Early AM\n(pre-7am)","AM\n(7-11am)","Midday\n(11am-2pm)","PM\n(2-5pm)","Late PM\n(post-5pm)")
ylabel <- "TB Score"
tlabel <- "Customer Connection by Day Part, 2018: AO Bakery Test"
sublabel <- "Fiscal Weeks 7, 9-12, 15-22, Fiscal Year 2018"
caption <- "AO Bakery test N = 154\nComparison group contains all US CO Stores not included in AO Bakery test set"
#manual legend labels
lname <- "Test Group"
llabels <- c("US CO Stores (Non-Test)","Test Stores")
#values
##CC Delta
pdata1a <- aobw
px1a <- aobw[,DAY_PART]
py1a <- aobw[,cc18]
groupvar1a <- aobw[,TEST_FLAG]
plot1a <- ggplot(data=pdata1a,aes(y=py1a,x=as.factor(px1a),fill=as.factor(groupvar1a))) + 
  geom_bar(stat="identity", width = 0.95, position=position_dodge(), colour="black") +
  scale_fill_brewer(palette = 1, name="", labels=llabels) + theme_economist() +
  scale_x_discrete(name="",labels=xlabels) + 
  scale_y_continuous(limits=c(0,45)) +
  xlab("") + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) +
  geom_text(size = 3.5, aes(label=py1a,y=5), stat="identity", position = position_dodge(0.95)) 
print(plot1a)




data_dir <- "O:/CoOp/CoOp194_PROReportng&OM/Julie"
aob <- fread(paste0(data_dir,"/AO_Bakery_CC_statsigtest.csv"))
aob <- aob[, .(STORE_NUM, TEST_FLAG, DAY_PART, TOTAL_TB, TOTAL_RSPNS, FSCL_YR_NUM)]
aob <- aob[, list(TB_SCORE = sum(TOTAL_TB,na.rm=T)/sum(TOTAL_RSPNS,na.rm=T)),
           by=c("STORE_NUM","TEST_FLAG","DAY_PART","FSCL_YR_NUM")]
aobw <- dcast.data.table(aob, STORE_NUM + TEST_FLAG + DAY_PART ~ FSCL_YR_NUM, value.var="TB_SCORE")
setnames(aobw,c("2017","2018"),c("cc17","cc18"))
#create delta
aobw[, ccyoy := cc18-cc17]

t.test(aobw[DAY_PART==1&TEST_FLAG==0, cc17],aobw[DAY_PART==1&TEST_FLAG==0, cc18])#NS
t.test(aobw[DAY_PART==2&TEST_FLAG==0, cc17],aobw[DAY_PART==2&TEST_FLAG==0, cc18])#***
t.test(aobw[DAY_PART==3&TEST_FLAG==0, cc17],aobw[DAY_PART==3&TEST_FLAG==0, cc18])#***
t.test(aobw[DAY_PART==4&TEST_FLAG==0, cc17],aobw[DAY_PART==4&TEST_FLAG==0, cc18])#***
t.test(aobw[DAY_PART==5&TEST_FLAG==0, cc17],aobw[DAY_PART==5&TEST_FLAG==0, cc18])#***

t.test(aobw[DAY_PART==1&TEST_FLAG==1, cc17],aobw[DAY_PART==1&TEST_FLAG==1, cc18])#NS
t.test(aobw[DAY_PART==2&TEST_FLAG==1, cc17],aobw[DAY_PART==2&TEST_FLAG==1, cc18])#NS
t.test(aobw[DAY_PART==3&TEST_FLAG==1, cc17],aobw[DAY_PART==3&TEST_FLAG==1, cc18])#NS
t.test(aobw[DAY_PART==4&TEST_FLAG==1, cc17],aobw[DAY_PART==4&TEST_FLAG==1, cc18])#NS
t.test(aobw[DAY_PART==5&TEST_FLAG==1, cc17],aobw[DAY_PART==5&TEST_FLAG==1, cc18])#NS
