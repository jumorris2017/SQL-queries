##Same customer, item-level, CE and behavior pre-and post- scores##
##Goal: Following customers 15 days after Purchase 1, and 15 days after Purchase 2##
####Assess if behavior changes *after* different CE raings##
####e.g., If at Purchase 1, they rate a 7, do they come back more frequenly,
####than if at Purchase 2, they rate a 6. 

#load libraries
library(data.table)
library(xlsx)
library(ggplot2)

#load data
scus <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/SameCust_02_pre-post-surveypairs.csv")