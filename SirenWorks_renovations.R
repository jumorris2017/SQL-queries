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
tlabel <- "CE survey count per store - US Company-Operated"
sublabel <- "SirenWorks ad hoc questions"
caption <- "Store N = 8,033\nSurvey N = 616,440"
#plot itself
plot2 <- ggplot(temp_usco,aes(nguid)) + 
  geom_histogram(binwidth=1,show.legend=FALSE,fill="lightgrey",col=I("black")) + 
  theme_economist() + scale_colour_brewer(palette = 1, name="", labels="") +
  scale_x_continuous(limits=c(0,max(temp_usco[,nguid])), breaks = scales::pretty_breaks(20)) +
  xlab(xlabel) + ylab(ylabel) + ggtitle(tlabel) + labs(subtitle=sublabel,caption=caption)
print(plot2)

###correlation matrices
#set up functions
swcecorrmat <- temp_usco[, c("q1",grep("q2",colnames(temp_usco),value=T)),with=FALSE]

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


#pull in cust trans data
ctrans <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CE_SirenWorks_custtrans.csv")
setnames(ctrans,c("GUID_ID"),c("guid"))
ctrans[, datetime := ymd_hms(RSPNS_DT)]
ctrans[, datemonth := FSCL_PER_IN_YR_NUM-3]

#swing wide by month
ctrans <- dcast.data.table(ctrans, guid + datetime + STORE_NUM ~ datemonth, value.var="TRANS")
colnames(ctrans)[4:7] <- paste("transmnth", colnames(ctrans)[2:5], sep = "_")

#merge
temp <- merge(swce,ctrans,by=c("guid"))


