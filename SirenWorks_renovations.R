##making plots for Siren Works renovations
##weekly trends and YoY comparison
##with YoY deltas called out
##CE measures: CC, SO, Speed, Cleanliness

#load libraries
library(data.table)
library(ggplot2)
library(tidyverse)

#group by stores that *are* or *are not* using the new plays

#load data
dp <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CE_deploymentplays.csv")
strlist <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/APT_Test_and_Control_site_mapping.csv")

#group by test/control stores for renovations
###this is fake for setting up code
dp[STORE_NUM %in% strlist[,stores_test], controlstr := 0]; dp[STORE_NUM %in% strlist[,stores_control], controlstr := 1]
dp <- na.omit(dp,cols="controlstr")
