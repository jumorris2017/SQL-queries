##Price Change Test. Assessing WP for first purchase post-price-increase##
##From Lisa 1/2/18##

#load libraries
library(data.table)
library(ggplot2)

##STEP 1: pull STORE_NUMS where the price change went into effect
#list of stores in Boston test
bos <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/price_change_test_wp_boston_stores.csv")

##STEP 2: pull GUIDs for customers who visited those STORE_NUMS (2 weeks prior FY16FW41) and (2 weeks post FY16FW41)
###and purchased the beverage of choice (americano tall)
#keep one WP score from pre- and one WP score from post-launch for each GUID
##last from pre, and first from post

##STEP 3: calculate average and TB % for WP pre- and post-launches
