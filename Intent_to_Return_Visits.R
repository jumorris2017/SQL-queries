#Testing intent to return CE ratings against actual return visitation behavior

#load libraries
library(data.table)
library(ggplot2)
library(ggthemes)

#load data
data_dir <- "O:/CoOp/CoOp194_PROReportng&OM/Julie"
ir60 <- fread(paste0(data_dir,"/intent_to_return_visits_60days.csv"))
ir30 <- fread(paste0(data_dir,"/intent_to_return_visits_30days.csv"))
ir30 <- ir30[, .(GUID_USER_ID,SPEND_POST30D,VISITS_POST30D)]
ir7 <- fread(paste0(data_dir,"/intent_to_return_visits_7days.csv"))
ir7 <- ir7[, .(GUID_USER_ID,SPEND_POST7D,VISITS_POST7D)]
ir1 <- fread(paste0(data_dir,"/intent_to_return_visits_1day.csv"))
ir1 <- ir1[, .(GUID_USER_ID,SPEND_POST1D,VISITS_POST1D)]
#TB versus TB2
irtb <- fread(paste0(data_dir,"/intent_to_return_TBvTB2.csv"))
#Sales and TSDs
irs <- fread(paste0(data_dir,"/intent_to_return_sales.csv"))

#merge together
ir <- Reduce(function(x, y) {merge(x, y, by=c("GUID_USER_ID"), all = TRUE)}, list(ir60,ir30,ir7,ir1))

#create top box var
ir[RETURN==5, returnTB := 1]; ir[RETURN<=4&RETURN>=1, returnTB := 0]

#create top 2 box var
ir[RETURN>=4, returnTB2 := 1]; ir[RETURN<=3&RETURN>=1, returnTB2 := 0]

#create top 3 box var
ir[RETURN>=3, returnTB3 := 1]; ir[RETURN<=2&RETURN>=1, returnTB3 := 0]

#create indicator for actual returns in 30 days
ir[VISITS_POST30D>=1, returnin30days := 1]; ir[VISITS_POST30D==0, returnin30days := 0]
#create indicator for actual returns in 7 days
ir[VISITS_POST7D>=1, returnin7days := 1]; ir[VISITS_POST7D==0, returnin7days := 0]
#create indicator for actual returns in 1 day
ir[VISITS_POST1D>=1, returnin1day := 1]; ir[VISITS_POST1D==0, returnin1day := 0]

#remove outliers... (71 people visited more than 200x in 60 days)
ir <- ir[VISITS_POST60D<=200]

#t.tests
#60 days: number of visits
t.test(ir[returnTB==0, VISITS_POST60D],ir[returnTB==1, VISITS_POST60D])
t.test(ir[returnTB2==0, VISITS_POST60D],ir[returnTB2==1, VISITS_POST60D])
t.test(ir[returnTB3==0, VISITS_POST60D],ir[returnTB3==1, VISITS_POST60D])
#7 days: number of visits
t.test(ir[returnTB==0, VISITS_POST7D],ir[returnTB==1, VISITS_POST7D])
t.test(ir[returnTB2==0, VISITS_POST7D],ir[returnTB2==1, VISITS_POST7D])
t.test(ir[returnTB3==0, VISITS_POST7D],ir[returnTB3==1, VISITS_POST7D])
#30 days: binary visitation
t.test(ir[returnTB3==0, returnin30days],ir[returnTB3==1, returnin30days])
#7 days: binary visitation
t.test(ir[returnTB2==0, returnin7days],ir[returnTB2==1, returnin7days])
#1 day: binary visitation
t.test(ir[returnTB==0, returnin1day],ir[returnTB==1, returnin1day])

#melt data
irtb <- irtb[,.(FSCL_YR_NUM,FSCL_WK_IN_YR_NUM,TB_SCORE,TB2_SCORE)]
irtb <- melt(irtb, id=c("FSCL_YR_NUM","FSCL_WK_IN_YR_NUM"))

#create an x-variable
irtb[, fyfw := paste0(FSCL_YR_NUM,".",str_pad(irtb[,FSCL_WK_IN_YR_NUM],2,pad="0"))]

#set up line chart
pdata <- irtb
px <- irtb[, fyfw]
py <- irtb[, value]
groupvar <- irtb[, variable]
#set labels
xlabel <- "Time"
ylabel <- "Score (%)"
tlabel <- "Intent to Return"
#manual legend labels
lname <- "Metric"
llabels <- c("Top Box (5 out of 5)","Top 2 Box (4+ out of 5)") 

#line chart
plot2 <- ggplot() +
  geom_line(data=pdata, aes(x=factor(px), y=py, group=factor(groupvar), colour=factor(groupvar))) + 
  xlab(xlabel) + ylab(ylabel) + theme_economist() +
  scale_colour_discrete(name=lname, labels=llabels, guide=guide_legend(order=1)) +
  scale_x_discrete(breaks = px[seq(1, length(px), by = 4)]) +
  guides(colour = guide_legend(override.aes = list(size = 7))) + 
  theme(axis.text.x = element_text(size = 7, angle = 90, hjust = 1)) +
  ggtitle(tlabel)
print(plot2)