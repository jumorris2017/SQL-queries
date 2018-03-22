#load libraries
library(data.table)

#load data
data_dir <- "Q:/Departments/WMO/Marketing Research/New Q drive/Partner Insights/Partner Perspectives/Research/Partner Experience Study/13_Wave5_NonRetail (Mar 18)/Hierarchy"
orgdt1 <- fread(paste0(data_dir,"/PDW_function_org_bypartner_l1.csv"))
orgdt2 <- fread(paste0(data_dir,"/PDW_function_org_bypartner_l2.csv"))
orgdt3 <- fread(paste0(data_dir,"/PDW_function_org_bypartner_func.csv"))
paneldt <- fread(paste0(data_dir,"/Panelist ID Feb 26 2018.csv"))
setnames(paneldt,"PartnerID","SAP_PRTNR_ID")

#create a panelist flag
paneldt[, panelist := 1]

#merge together
orgdt1 <- left_join(orgdt1,paneldt,by="SAP_PRTNR_ID")
setDT(orgdt1)
orgdt2 <- left_join(orgdt2,paneldt,by="SAP_PRTNR_ID")
setDT(orgdt2)
orgdt3 <- left_join(orgdt3,paneldt,by="SAP_PRTNR_ID")
setDT(orgdt3)

#make 0 for NA's (not in panel ID list)
orgdt1[is.na(orgdt1[,panelist]), panelist := 0]
orgdt2[is.na(orgdt2[,panelist]), panelist := 0]
orgdt3[is.na(orgdt3[,panelist]), panelist := 0]

#calculate headcounts and panelist counts
#LEADER 1
orgdt_hc1 <- orgdt1[!is.na(LEADER1), list(partnerN = .N, 
                         panelistN = sum(panelist,na.rm=T),
                         panelistpct = round(sum(panelist,na.rm=T)/.N,3)*100), by=c("LEADER1")]
orgdt_hc1 <- setorder(orgdt_hc1,LEADER1)
#LEADER 2
orgdt_hc2 <- orgdt2[!is.na(LEADER2), list(partnerN = .N, 
                         panelistN = sum(panelist,na.rm=T),
                         panelistpct = round(sum(panelist,na.rm=T)/.N,3)*100), by=c("LEADER2")]
orgdt_hc2 <- setorder(orgdt_hc2,LEADER2)
#FUNCTIONAL
orgdt_hc3 <- orgdt3[!is.na(FUNCTIONAL), list(partnerN = .N, 
                         panelistN = sum(panelist,na.rm=T),
                         panelistpct = round(sum(panelist,na.rm=T)/.N,3)*100), by=c("FUNCTIONAL")]
orgdt_hc3 <- setorder(orgdt_hc3,FUNCTIONAL)

#rbindlist
l = list(orgdt_hc1, orgdt_hc2, orgdt_hc3)
full <- rbindlist(l, use.names=TRUE, fill=TRUE) #If "use.names" = T items will be bound by matching column names. If "fill" = T fills missing columns with NAs. By default FALSE. 
setcolorder(full,c("partnerN","panelistN","panelistpct","LEADER1","LEADER2","FUNCTIONAL"))

#WRITE.CSV
write.csv(full,file=paste0(data_dir,"/hl_functional_panelist_counts.csv"))
