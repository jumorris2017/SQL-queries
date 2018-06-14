##R code for SQL query 
##CC by comps for Canada stores
##Request from Lisa 11/22/17

##load libraries
library(data.table)


##slide #5 - CC, SO, Q1, and WP by store performance
cc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/cc-so_bystoreperformance_LS_FY18Q2.csv")
cc1 <- copy(cc)
#remove store number variable for topline
cc1[, STORE_NUM := NULL]
#sum - to get one row for the quarter
cc1 <- cc1[, lapply(.SD, sum, na.rm=TRUE)]
#will return
cc1[, RETURN_TB_SCORE := Q1_TB_CNT/Q1_RESPONSE_TOTAL]
#customer connection
cc1[, CC_TB_SCORE := Q2_2_TB_CNT/Q2_2_RESPONSE_TOTAL]
#worth perceptions
cc1[, WP_TB_SCORE := Q2_8_TB_CNT/Q2_8_RESPONSE_TOTAL]
#SO sub categories
cc1[, Q2_1_TB_SCORE := Q2_1_TB_CNT/Q2_1_RESPONSE_TOTAL]
cc1[, Q2_3_TB_SCORE := Q2_3_TB_CNT/Q2_3_RESPONSE_TOTAL]
cc1[, Q2_4_TB_SCORE := Q2_4_TB_CNT/Q2_4_RESPONSE_TOTAL]
cc1[, Q2_5_TB_SCORE := Q2_5_TB_CNT/Q2_5_RESPONSE_TOTAL]
cc1[, Q2_6_TB_SCORE := Q2_6_TB_CNT/Q2_6_RESPONSE_TOTAL]
cc1[, Q2_7_TB_SCORE := Q2_7_TB_CNT/Q2_7_RESPONSE_TOTAL]
#average the SO scores
cc1[, SO_TB_SCORE := rowMeans(.SD, na.rm = TRUE),
    .SDcols = c("Q2_1_TB_SCORE", "Q2_3_TB_SCORE", "Q2_4_TB_SCORE",
                "Q2_5_TB_SCORE", "Q2_6_TB_SCORE", "Q2_7_TB_SCORE")]
#keep only variables we need
cc1 <- cc1[, (grep("TB_SCORE",colnames(cc1),value=T)), with=FALSE]
cc1[, Q1_TB_SCORE := NULL]
cc1 <- cc1[, lapply(.SD,function(x) round(x,2)*100)]

#quantile stores
cc2 <- copy(cc)
#will return
cc2[, RETURN_TB_SCORE := Q1_TB_CNT/Q1_RESPONSE_TOTAL]
#customer connection
cc2[, CC_TB_SCORE := Q2_2_TB_CNT/Q2_2_RESPONSE_TOTAL]
#worth perceptions
cc2[, WP_TB_SCORE := Q2_8_TB_CNT/Q2_8_RESPONSE_TOTAL]
#SO sub categories
cc2[, Q2_1_TB_SCORE := Q2_1_TB_CNT/Q2_1_RESPONSE_TOTAL]
cc2[, Q2_3_TB_SCORE := Q2_3_TB_CNT/Q2_3_RESPONSE_TOTAL]
cc2[, Q2_4_TB_SCORE := Q2_4_TB_CNT/Q2_4_RESPONSE_TOTAL]
cc2[, Q2_5_TB_SCORE := Q2_5_TB_CNT/Q2_5_RESPONSE_TOTAL]
cc2[, Q2_6_TB_SCORE := Q2_6_TB_CNT/Q2_6_RESPONSE_TOTAL]
cc2[, Q2_7_TB_SCORE := Q2_7_TB_CNT/Q2_7_RESPONSE_TOTAL]
#average the SO scores
cc2[, SO_TB_SCORE := rowMeans(.SD, na.rm = TRUE),
    .SDcols = c("Q2_1_TB_SCORE", "Q2_3_TB_SCORE", "Q2_4_TB_SCORE",
                "Q2_5_TB_SCORE", "Q2_6_TB_SCORE", "Q2_7_TB_SCORE")]
#
ccquant <- cc2[, list(RETURN_10 = quantile(RETURN_TB_SCORE,.1,na.rm=T),
                      RETURN_90 = quantile(RETURN_TB_SCORE,.9,na.rm=T),
                    CC_10 = quantile(CC_TB_SCORE,.1,na.rm=T),
                    CC_90 = quantile(CC_TB_SCORE,.9,na.rm=T),
                    SO_10 = quantile(SO_TB_SCORE,.1,na.rm=T),
                    SO_90 = quantile(SO_TB_SCORE,.9,na.rm=T),
                    WP_10 = quantile(WP_TB_SCORE,.1,na.rm=T),
                    WP_90 = quantile(WP_TB_SCORE,.9,na.rm=T),
                    Q2_1_10 = quantile(Q2_1_TB_SCORE,.1,na.rm=T),
                    Q2_1_90 = quantile(Q2_1_TB_SCORE,.9,na.rm=T),
                    Q2_3_10 = quantile(Q2_3_TB_SCORE,.1,na.rm=T),
                    Q2_3_90 = quantile(Q2_3_TB_SCORE,.9,na.rm=T),
                    Q2_4_10 = quantile(Q2_4_TB_SCORE,.1,na.rm=T),
                    Q2_4_90 = quantile(Q2_4_TB_SCORE,.9,na.rm=T),
                    Q2_5_10 = quantile(Q2_5_TB_SCORE,.1,na.rm=T),
                    Q2_5_90 = quantile(Q2_5_TB_SCORE,.9,na.rm=T),
                    Q2_6_10 = quantile(Q2_6_TB_SCORE,.1,na.rm=T),
                    Q2_6_90 = quantile(Q2_6_TB_SCORE,.9,na.rm=T),
                    Q2_7_10 = quantile(Q2_7_TB_SCORE,.1,na.rm=T),
                    Q2_7_90 = quantile(Q2_7_TB_SCORE,.9,na.rm=T))]
ccquant <- ccquant[, lapply(.SD,function(x) round(x,2)*100)]

