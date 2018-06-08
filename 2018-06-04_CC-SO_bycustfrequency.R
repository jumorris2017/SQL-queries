##SR/Non-SR Analysis using Brand Equity Study##
##December FY 18 data##

#load libraries
library(data.table)
library(tidyverse)
library(ggthemes)

#bring in our data
ce <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/2018-06-04-cc-so_bycustfrequency.csv")

#keep only december fy18
ce <- ce[FSCL_YR_NUM==2018&FSCL_PER_IN_YR_NUM==7]

#create frequency bins (1: 1-5, 2: 6-10, 3: 11-15, 4: 16+)
ce[TRANS<=5, vis_bin := 1]
ce[TRANS>=6&TRANS<=10, vis_bin := 2]
ce[TRANS>=11&TRANS<=15, vis_bin := 3]
ce[TRANS>=16&TRANS<=20, vis_bin := 4]
ce[TRANS>=21&TRANS<=25, vis_bin := 5]
ce[TRANS>=26, vis_bin := 6]

#aggregate by vis_bin and question
ce <- ce[, list(USER_COUNT = sum(USER_COUNT,na.rm=T),
                TB_COUNT = sum(TB_COUNT,na.rm=T),
                RSPSN_COUNT = sum(RSPSN_COUNT,na.rm=T),
                tbscore = sum(TB_COUNT,na.rm=T)/sum(RSPSN_COUNT,na.rm=T)),
         by=c("vis_bin","QSTN_ID")]
#drop worth question
ce <- ce[QSTN_ID!='Q2_8']
#make cc vs so variable
ce[QSTN_ID!='Q2_2', variable := 1];ce[QSTN_ID=='Q2_2', variable := 0]

#average of averages for so score
ceagg <- ce[, list(n = max(USER_COUNT,na.rm=T),
                   tbscore = round(mean(tbscore,na.rm=T),3)*100),
            by=c("vis_bin","variable")]

#plot 1
#set labels
# xlabel <- "Proven SR"
xlabels <- c("Customer Connection","Store Operations")
ylabel <- "Top Box Score"
tlabel <- "Customer Experience by Visitation Frequency"
sublabel <- "Customer Connection & Store Operations"
caption <- "Customer Experience Survey and SR Visitation, April FY18"
#manual legend labels
lname <- "30-Day Visitation"
llabels <- c("1-5", "6-10", "11-15", "16-20", "21-25", "26+")
#values
pdata1a <- ceagg
px1a <- ceagg[,variable]
py1a <- ceagg[,tbscore]
groupvar1a <- ceagg[,vis_bin]
#nvar1a <- ceagg[,n]
#plot itself
plot1a <- ggplot(data=pdata1a,aes(y=py1a,x=as.factor(px1a),fill=as.factor(groupvar1a))) + 
  geom_bar(stat="identity", width = 0.95, position=position_dodge(), colour="black") +
  scale_fill_brewer(palette = 1, name=lname, labels=llabels) + theme_economist_white(gray_bg = FALSE) +
  scale_x_discrete(name="",labels=xlabels) +
  scale_y_continuous(limits=c(0,80)) +
  xlab("") + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) +
  geom_text(size = 3.5, aes(label=py1a,y=0), stat="identity", vjust = -1, position = position_dodge(0.95)) 
print(plot1a)


#breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75),labels=c(0,10,20,30,40,50,60,70)
