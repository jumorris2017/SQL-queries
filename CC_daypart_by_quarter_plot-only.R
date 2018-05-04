#makes a plot of AM/PM Speed and CC monthly scores
#for Brittney; Strat Plan

#load libraries
library(data.table)
library(ggplot2)
library(ggthemes)

#load data
data_dir <- "O:/CoOp/CoOp194_PROReportng&OM/Julie"
ce <- fread(paste0(data_dir,"/cc_daypart_fy18q2_forjasmine.csv"))
ce <- fread(paste0(data_dir,"/cc_daypart_fy18P7early_foralison.csv"))
ce <- fread(paste0(data_dir,"/cc_daypart_fy18P6_foralison.csv"))

#agg by fiscal year
ce <- ce[, list(TOTAL_TB = sum(TOTAL_TB),
                TOTAL_RSPNS = sum(TOTAL_RSPNS),
                TB_SCORE = round(sum(TOTAL_TB)/sum(TOTAL_RSPNS),3)*100),
         by=c("DAY_PART")]

#plot 2
ylabel <- "TB Score (%)"
tlabel <- "Customer Connection by Day Part"
sublabel <- "Customer Experience Survey, April-to-Date FY18 (Data as of May 3, 2018)"
caption <- "US Company-Operated Stores"
xlabels <- c("Early AM\nMidnight - 7am","AM\n7am - 11am","Midday\n11am - 2pm","PM\n2pm - 4pm","Late PM\n4pm - Close")
#values
pdata1a <- ce
px1a <- ce[,DAY_PART]
py1a <- ce[,TB_SCORE]
#plot itself
plot1a <- ggplot(data=pdata1a,aes(y=py1a,x=as.factor(px1a))) + 
  geom_bar(stat="identity", width = 0.95, position=position_dodge(), fill="lightblue", colour="black") +
  theme_economist_white(gray_bg=FALSE) +
  scale_x_discrete(name="",labels=xlabels) +
  scale_y_continuous(limits=c(0,max(py1a)*1.25)) +
  xlab("") + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) +
  geom_text(size = 4, aes(label=py1a,y=0), stat="identity", vjust = -2, position = position_dodge(0.95))
print(plot1a)
