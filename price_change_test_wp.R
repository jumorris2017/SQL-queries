##Price Change Test. Assessing WP for first purchase post-price-increase##
##From Lisa 1/2/18##

#load libraries
library(data.table)
library(ggplot2)

#load data
#list of stores in Boston test
bos <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/price_change_test_wp_boston_stores.csv")

#price change launched FW 41 FY 16
#pull data in for the first 30 days following the FW 41 FY 16 launch (first survey post launch)
#get list of customers
#pull their most recent before that launch
