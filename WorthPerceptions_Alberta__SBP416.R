##worth perceptions for Alberta

#load libraries
library(data.table)
library(xlsx)
library(ggplot2)

#load data
wp4 <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/worthperceptions_alberta.csv")

#write file
#write.xlsx(totalag,file="O:/CoOp/CoOp194_PROReportng&OM/Julie/worth_perception_pricezones_112017.xlsx")

#keep only 2016 & 2017 data
wp4 <- wp4[CAL_YR_NUM>=2016]
#get rid of december 2017
wp4 <- wp4[!(CAL_MNTH_IN_YR_NUM==12&CAL_YR_NUM==2017)]

#create month variable for plotting
wp4[, CAL_MNTH_IN_YR_NUM := str_pad(wp4[,CAL_MNTH_IN_YR_NUM], 2, pad = "0")]
wp4[, mnth := paste0(CAL_MNTH_IN_YR_NUM,"-",CAL_MNTH_ABBR_NM)]

#Alberta
#set labels
xlabel <- "Month"
ylabel <- "WP Top Box Score"
tlabel <- "Alberta: Worth Perceptions"
slabel <- "2016-2017"
#set data and variables
pdata <- wp4[AREA_AGG=="AB"]
px <- wp4[AREA_AGG=="AB", mnth]
py <- wp4[AREA_AGG=="AB", WP_TB_SCORE]
groupvar <- wp4[AREA_AGG=="AB", CAL_YR_NUM]
#manual legend labels
lname <- "Calendar Year"
llabels <- c("2016","2017") 
#line chart, factored by one variable
plot1 <- ggplot() +
  geom_line(data=pdata, aes(x=px, y=py, group=factor(groupvar), colour=factor(groupvar))) + 
  xlab(xlabel) + ylab(ylabel) + theme_bw() +
  theme(axis.text.x=element_text(angle=25, hjust=1)) +
  scale_colour_discrete(name=lname, labels=llabels, guide=guide_legend(order=1)) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  scale_y_continuous(limits=c(0,40)) +
  labs(title = tlabel, subtitle = slabel)
print(plot1)

#THREE AREAS
#set labels
wp4 <- wp4[AREA_AGG!="US"]
xlabel <- "Month"
ylabel <- "WP Top Box Score"
tlabel <- "Alberta and rest of Canada: Worth Perceptions"
slabel <- "2017"
#set data and variables
pdata <- wp4[CAL_YR_NUM==2017]
px <- wp4[CAL_YR_NUM==2017, mnth]
py <- wp4[CAL_YR_NUM==2017, WP_TB_SCORE]
groupvar <- wp4[CAL_YR_NUM==2017, AREA_AGG]
#manual legend labels
lname <- "Country/Area"
llabels <- c("Alberta","Rest of Canada") 
#line chart, factored by one variable
plot1 <- ggplot() +
  geom_line(data=pdata, aes(x=px, y=py, group=factor(groupvar), colour=factor(groupvar))) + 
  xlab(xlabel) + ylab(ylabel) + theme_bw() +
  theme(axis.text.x=element_text(angle=25, hjust=1)) +
  scale_colour_discrete(name=lname, labels=llabels, guide=guide_legend(order=1)) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  scale_y_continuous(limits=c(0,40)) +
  labs(title = tlabel, subtitle = slabel)
print(plot1)
