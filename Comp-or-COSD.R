#COSD/COMP and CE Scores

#load libraries
library(data.table)

#read data
ce <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc-so_bystoreperformance_Canada_FY18Q2.csv")
ce <- ce[, .(STORE_NUM,Q2_2_TB_SCORE)]
comp <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/Comps_by_store_ca_q2fy18.csv")

#make consistent
ce[, STORE_NUM := as.numeric(STORE_NUM)]
ce[, (grep("TB_SCORE",colnames(ce),value=T)) := lapply(.SD,as.numeric), .SDcols=grep("TB_SCORE",colnames(ce),value=T)]
setnames(comp,"STORE_NUMBER","STORE_NUM")

#merge by store number
full <- left_join(ce,comp,by=c("STORE_NUM"))
setDT(full)

# #create list of variables for correlatoin matrix
# listofvars <- grep("TB_SCORE",colnames(full),value=T)
# 
# #correlation matrix
# cormat <- round(cor(full[, (c("SALESCOMP",listofvars)), with=F],method="spearman",use="pairwise.complete.obs"),3)
# cormat <- as.data.table(cormat)
# cormat <- cbind(c("SALESCOMP",listofvars),cormat)
# cormat <- setorder(cormat,-SALESCOMP)