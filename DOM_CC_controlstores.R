##CC by DOM adoption - case and control stores##

#load libraries
library(data.table)
library(xlsx)
library(ggplot2)

#load data
dom <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/DOM_CC_controlstores.csv")
mopcc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC_by_store_cafe_only.csv")
mopcc[, STORE_NUM := as.numeric(STORE_NUM)]