#worth perceptions for Ale 5/29/18

#load libraries
library(data.table)
library(ggplot2)
library(ggthemes)

#Request #1: worth perceptions pre- and post- price change by store

#load data
wp <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/worthperceptions_ontario-toronto.csv")
wp <- wp[FSCL_YR_NUM>=2017]

#set labels
xlabel <- "Fiscal Week"
ylabel <- "WP Top Box Score"
tlabel <- "Worth Perception Scores in Ontario & Toronto"
sublabel <- "*Ontario & Toronto stores with price increases"
caption <- "Price changes in test stores FW5 FY18\nTest store N = 425 stores in Ontario and Toronto"
#set data and variables
pdata <- wp
px <- wp[, FSCL_WK_IN_YR_NUM]
py <- wp[, PRICE_SCORE]
colourvar <- wp[, PRICE_INCREASE]
groupvar <- wp[,FSCL_YR_NUM]
#manual legend labels
lname2 <- "Fiscal Year"
llabels2 <- c("2017","2018") 
lname1 <- "Areas"
llabels1 <- c("Rest of Canada","Stores with Price Increases")
#line chart, factored by one variable
#plot itself
plot2 <- ggplot(data=pdata, aes(x=px, y=py, linetype=factor(groupvar), colour=factor(colourvar), group=interaction(groupvar, colourvar))) + 
  geom_point(size=1) + geom_line(size=1) +
  xlab(xlabel) + ylab(ylabel) + 
  scale_colour_discrete(name="", labels=llabels1, guide=guide_legend(order=1)) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  scale_linetype_discrete(name=lname2, labels=llabels2, guide=guide_legend(order=1)) +
  geom_vline(aes(xintercept = 5)) + annotate(geom = "text", x=5, y=0.29, hjust=0, vjust=-0.5, label = "Price Increase FY18", angle=90, size=3, fontface = "italic") +
  scale_x_continuous(limits=c(1,52)) +
  scale_y_continuous(limits=c(.2,.37)) + theme_economist_white(gray_bg = FALSE) +
  ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) 
  #annotate(size=5, geom="text", x=6, y=.22, label= "Canada - No Price Increase",hjust = 0) +
  #annotate(size=5, geom="text", x=6, y=.32, label= "Ontario/Toronto - Price Increase",hjust = 0) 
print(plot2)

#delta
#reload data
wp <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/worthperceptions_ontario-toronto.csv")
wp <- wp[FSCL_YR_NUM>=2016]

#calculate delta
wp2 <- dcast.data.table(wp, FSCL_WK_IN_YR_NUM + PRICE_INCREASE ~ FSCL_YR_NUM, value.var=c("PRICE_SCORE"))
setnames(wp2,c("2016","2017","2018"),c("f2016","f2017","f2018"))

#calc YOY
wp2[, yoy17 := f2017-f2016]
wp2[FSCL_WK_IN_YR_NUM<35, yoy18 := f2018-f2017]

#melt for plot
wp2 <- wp2[, .(FSCL_WK_IN_YR_NUM,PRICE_INCREASE,yoy17,yoy18)]
wp2 <- melt(wp2, id=c("FSCL_WK_IN_YR_NUM","PRICE_INCREASE"))

#set labels
xlabel <- "Fiscal Week"
ylabel <- "YOY Change in WP Top Box Score"
tlabel <- "YOY Change in Worth Perception Scores in Ontario & Toronto"
sublabel <- "*Ontario & Toronto stores with price increases"
caption <- "Price changes in test stores FW5 FY18\nTest store N = 425 stores in Ontario and Toronto"
#set data and variables
pdata <- wp2
px <- wp2[, FSCL_WK_IN_YR_NUM]
py <- wp2[, value]
colourvar <- wp2[, PRICE_INCREASE]
groupvar <- wp2[,variable]
#manual legend labels
lname2 <- "Fiscal Year"
llabels2 <- c("2017","2018") 
lname1 <- "Areas"
llabels1 <- c("Rest of Canada","Stores with Price Increases")
#line chart, factored by one variable
#plot itself
plot2 <- ggplot(data=pdata, aes(x=px, y=py, linetype=factor(groupvar), colour=factor(colourvar), group=interaction(groupvar, colourvar))) + 
  geom_point(size=1) + geom_line(size=1) +
  xlab(xlabel) + ylab(ylabel) + 
  scale_colour_discrete(name="", labels=llabels1, guide=guide_legend(order=1)) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  scale_linetype_discrete(name=lname2, labels=llabels2, guide=guide_legend(order=1)) +
  geom_vline(aes(xintercept = 5)) +  annotate(geom = "text", x=5, y=-0.08, hjust=0, vjust=-0.5, label = "Price Increase FY18", angle=90, size=3, fontface = "italic") +
  scale_x_continuous(limits=c(1,52)) +
  scale_y_continuous(limits=c(-.08,.05)) + theme_economist_white(gray_bg = FALSE) +
  ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption) 
#annotate(size=5, geom="text", x=6, y=.22, label= "Canada - No Price Increase",hjust = 0) +
#annotate(size=5, geom="text", x=6, y=.32, label= "Ontario/Toronto - Price Increase",hjust = 0) 
print(plot2)







