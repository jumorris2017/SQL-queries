##request 1: worth perceptions for Lisa request 11/15/17
##request 2: CC and SO correlations for Lisa request 11/15/17

#load libraries
library(data.table)
library(xlsx)


#Request #2: cc and so by store
#read .csv
ccso <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CC-StoreOps_sept-oct.csv")
#calculate top box score for store ops
ccso[, so_resp_total := rowSums(.SD,na.rm=T), .SDcols=names(ccso)[c(2,4:8)]]
ccso[, so_tb_cnt := rowSums(.SD,na.rm=T), .SDcols=names(ccso)[c(9,11:15)]]
ccso[, so_tb_score := so_tb_cnt/so_resp_total]
setnames(ccso, c("Q2_2_RESPONSE_TOTAL","Q2_2_TB_CNT","Q2_2_TB_SCORE"),
         c("cc_resp_total","cc_tb_cnt","cc_tb_score"))
#reduce the dataset
ccso_1 <- ccso[, c("STORE_NUM","cc_tb_score","so_tb_score"),with=FALSE]
#correlation
ccso_1[, (names(ccso_1)[2:3]) := lapply(.SD, as.numeric), .SDcols=names(ccso_1)[2:3]]
cor(ccso_1[,cc_tb_score],ccso_1[,so_tb_score])
#write file
write.xlsx(ccso_1, file="O:/CoOp/CoOp194_PROReportng&OM/Julie/CC-StoreOps_sept-oct.xlsx")

#so quartiles
##tenure
##split into categories for clustering
prob = c(0.25, .5, 0.75, 1)
temp <- ccso %>% summarise( 
  so25 = quantile(so_tb_score, probs = prob[1], na.rm = T), 
  so50 = quantile(so_tb_score, probs = prob[2], na.rm = T),
  so75 = quantile(so_tb_score, probs = prob[3], na.rm = T), 
  so100 = quantile(so_tb_score, probs = prob[4], na.rm = T)
)
setDT(temp)
#recode tenure based on quartiles
ccso[so_tb_score <= temp[,so25], quartile := 1]
ccso[so_tb_score > temp[,so25] & so_tb_score <= temp[,so50], quartile := 2]
ccso[so_tb_score > temp[,so50] & so_tb_score <= temp[,so75], quartile := 3]
ccso[so_tb_score > temp[,so75] & so_tb_score <= temp[,so100], quartile := 4]
#calculate top box score for cc, by so quartile
ccso <- ccso[, list(cc_resp_total=sum(cc_resp_total,na.rm=T),
                    cc_tb_cnt=sum(cc_tb_cnt,na.rm=T)),
             by="quartile"]
ccso[, cc_tb_score := cc_tb_cnt/cc_resp_total]
#order by quartile
ccso <- setorder(ccso,quartile)
ccso <- cbind(ccso,t(temp))
setnames(ccso,"V1","so_q_value")
#reduce the dataset
#write file
write.xlsx(ccso, file="O:/CoOp/CoOp194_PROReportng&OM/Julie/CC-StoreOps_sept-oct_quartiles.xlsx")


#plot of SO quartiles with average CC top box score for each

#set up unique elements
DT <- copy(ccso)
maintitle <- "Customer Connection Top Box Score by Store Operations Quartile"
ylabel <- "Store Operations Top Box Score"
xvar <- DT[,quartile]
yvar <- DT[,so_q_value]
yvarcount <- DT[,cc_tb_score]
pdata <- DT
#plot
ggplot(data = pdata, aes(x = xvar, y = yvar, fill = "#ADD8E6")) +
  geom_bar(stat="identity", width = 0.7) + theme_bw() + 
  ggtitle(maintitle) + guides(fill=FALSE) +
  geom_text(size = 5, aes(label=paste0("CC = ",round(yvarcount,2)),y=0), stat= "identity", vjust = -.5) +
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8), 
        plot.title = element_text(size = 10, face = "bold")) + 
  labs(x = NULL, y = ylabel) 











