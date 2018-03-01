##Links CE and Pulse data to MFS stores based on converstion dates
##Some stores converted before our CE survey began
##CE data should be restricted to post-August 2015 for accuracy
##Want 4 months pre- and 4 months post-
##So, earliest conversion dates we can take are Dec 2015

#load libraries
library(data.table)
library(ggplot2)
library(lubridate)

#load data
#stores and converstion dates
mfsst <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/MFS_Roster_2_12_2018.csv")
#ce scores
mfsce <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CE_by_store_MFS_2014-2018.csv")
#pulse scores

#create 4-month pre- and post- bands
mfsst[, CONVDATE := mdy(CONVDATE)]
# mfsst[, convmonth := month(CONVDATE)]
# mfsst[, convyear := year(CONVDATE)]
mfsst[, convpre := CONVDATE %m-% months(4)]
mfsst[, convpost := CONVDATE %m+% months(4)]


#agg CE scores for pre- and post- periods


