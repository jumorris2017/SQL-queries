##SR/Non-SR Analysis using Brand Equity Study##
##December FY 18 data##

#load libraries
library(data.table)
library(tidyverse)
library(ggthemes)

#load data
c1 <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/canada-all_cc-so.csv")
c1[, area := "Canada"]
c2 <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/canada-vancouver_cc-so.csv")
c2[, area := "Vancouver"]
#rbind
l <- list(c1,c2)
can <- rbindlist(l,use.names=T,fill=T)
#melt
can <- melt(can, id.vars=c("FSCL_PER_IN_YR_NUM","FSCL_YR_NUM","area"))

#reduce to the past year
can <- can[(FSCL_YR_NUM==2018&FSCL_PER_IN_YR_NUM<=5)|(FSCL_YR_NUM==2017&FSCL_PER_IN_YR_NUM>5)]

#make year-month variable for plotting
can[, fyfp := paste0(FSCL_YR_NUM,".",str_pad(can[,FSCL_PER_IN_YR_NUM],2,pad="0"))]

#make a plotting height label
can[variable=="SO_SCORE"&area=="Canada", value_y := value+2.5]
can[variable=="SO_SCORE"&area=="Vancouver", value_y := value-2.5]
can[variable=="CC_SCORE"&area=="Canada", value_y := value+2.5]
can[variable=="CC_SCORE"&area=="Vancouver", value_y := value-2.5]

#set labels
xlabels <- c("Mar 17", "Apr 17", "May 17", "June 17", "July 17", "Aug 17", 
             "Sep 17", "Oct 17", "Nov 17", "Dec 17", "Jan 18", "Feb 18")
ylabel <- "TB Score"
tlabel <- "Vancouver, Canada Customer Experience"
sublabel <- "Company-Operated Stores, March 2017 - February 2018"
caption <- "Canada Store N = 1,750\nVancouver Store N = 122"
#manual legend labels
lname1 <- "Area"
llabels1 <- c("Canada","Vancouver")
lname2 <- "Metric"
llabels2 <- c("Customer Connection","Store Operations")
#values
pdata <- can
px <- can[,fyfp]
py <- can[,value]
groupvar <- can[,variable]
colourvar <- can[,area]
#plot itself
plot2 <- ggplot(data=pdata, aes(x=px, y=py, colour=factor(colourvar), group=interaction(groupvar, colourvar))) + 
  geom_point(size=1) + geom_line(size=1) +
  xlab("") + ylab(ylabel) + 
  scale_x_discrete(labels=xlabels) +
  scale_colour_discrete(name="", labels=llabels1, guide=guide_legend(order=1)) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  scale_y_continuous(limits=c(0,70)) + theme_economist_white(gray_bg = FALSE) +
  ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) +
  annotate(size=5, geom="text", x=1, y=63, label= "Store Operations",hjust = 0) +
  annotate(size=5, geom="text", x=1, y=38, label= "Customer Connection",hjust = 0) +
  geom_text(size = 4, aes(label=py,y=value_y), stat="identity")
print(plot2)
