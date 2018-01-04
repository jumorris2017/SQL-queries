##CE by Channel
##12/21/17

#load libraries
library(data.table)
library(ggplot2)
library(lavaan)
library(flipRegression)


#load data
ch <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CE_bychannelandGUI.csv")
tc <- fread("O:/CoOp/CoOp194_PROReportng&OM/Julie/CE_bychannelandGUI_transcount.csv")
setnames(tc,"GUID_ID","GUID_USER_ID")
#only keep GUIDs in the CE data
guids <- unique(ch[,GUID_USER_ID])
tc <- tc[GUID_USER_ID %in% guids]

#convert 9's to NA
ch[, RSPNS_ID := ifelse(RSPNS_ID==9,NA,RSPNS_ID)]

#reshape from long to wide
ch <- dcast.data.table(ch, GUID_USER_ID + RSPNS_DT + MOBILE_ORD_PAY_IND + MOBILE_IND + ORD_MTHD_CD ~ QSTN_ID, value.var="RSPNS_ID")
#left join
ch <- merge(ch,tc,by="GUID_USER_ID",all.x=T)
ch[is.na(ch[,FY17Q4_TRAN]), FY17Q4_TRAN := 1]

#drop food question
#ch[,Q2_6 := NULL]

#recode variables as binary TB
listofvars <- grep("Q",colnames(ch),value=T)
#ch[, (listofvars[1]) := lapply(.SD, function(x) ifelse(x==5,1,0)), .SDcols=listofvars[1]]
#ch[, (listofvars[-1]) := lapply(.SD, function(x) ifelse(x==7,1,0)), .SDcols=listofvars[-1]]

#drop rows with NAs
ch <- na.omit(ch)

#create two binary variables from order method

#correlation matrix for ranking
matall <- round(cor(ch[, (listofvars), with=F],method="spearman",use="pairwise.complete.obs"),3)
matall <- as.data.table(matall)
matall <- cbind(listofvars,matall)
#matall <- setorder(matall,-FY17Q4_TRAN)

#correlation matrix for ranking
matmop <- round(cor(ch[ORD_MTHD_CD=='MOP', (listofvars), with=F],method="spearman",use="pairwise.complete.obs"),3)
matmop <- as.data.table(matmop)
matmop <- cbind(listofvars,matmop)
matmop <- setorder(matmop,-FY17Q4_TRAN)

#correlation matrix for ranking
matcafe <- round(cor(ch[ORD_MTHD_CD=='CAFE', (listofvars), with=F],method="spearman",use="pairwise.complete.obs"),3)
matcafe <- as.data.table(matcafe)
matcafe <- cbind(listofvars,matcafe)
matcafe <- setorder(matcafe,-FY17Q4_TRAN)

#correlation matrix for ranking
matotw <- round(cor(ch[ORD_MTHD_CD=='OTW', (listofvars), with=F],method="spearman",use="pairwise.complete.obs"),3)
matotw <- as.data.table(matotw)
matotw <- cbind(listofvars,matotw)
matotw <- setorder(matotw,-FY17Q4_TRAN)

#RWA
#all channels
rwa1 <- Regression(FY17Q4_TRAN ~ Q2_1 + Q2_2 + Q2_3 + Q2_4 + Q2_5 + Q2_7, data=ch,
                   output = "Relative Importance Analysis", importance.absolute = T)
slices <- rwa1$relative.importance$importance
lbls <- c("Speed","Customer Connection","Above and Beyond","Accuracy","Beverage Taste","Cleanliness")
lbls <- paste(lbls, round(slices,0)) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, cex=0.75,
    #col=terrain.colors(length(lbls)),
    main="CE Survey Drivers of Transaction Count\nAll Channels",
    cex.main=1)

#cafe
rwa2 <- Regression(FY17Q4_TRAN ~ Q2_1 + Q2_2 + Q2_3 + Q2_4 + Q2_5 + Q2_7, data=ch[ORD_MTHD_CD=='CAFE'],
                   output = "Relative Importance Analysis", importance.absolute = T)
slices <- rwa2$relative.importance$importance
lbls <- c("Speed","Customer Connection","Above and Beyond","Accuracy","Beverage Taste","Cleanliness")
lbls <- paste(lbls, round(slices,0)) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, cex=0.75,
    #col=terrain.colors(length(lbls)),
    main="CE Survey Drivers of Transaction Count\nCafe",
    cex.main=1)

#mop
rwa3 <- Regression(FY17Q4_TRAN ~ Q2_1 + Q2_2 + Q2_3 + Q2_4 + Q2_5 + Q2_7, data=ch[ORD_MTHD_CD=='MOP'],
                   output = "Relative Importance Analysis", importance.absolute = T)
slices <- rwa3$relative.importance$importance
lbls <- c("Speed","Customer Connection","Above and Beyond","Accuracy","Beverage Taste","Cleanliness")
lbls <- paste(lbls, round(slices,0)) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, cex=0.75,
    #col=terrain.colors(length(lbls)),
    main="CE Survey Drivers of Transaction Count\nMOP",
    cex.main=1)

#otw
rwa4 <- Regression(FY17Q4_TRAN ~ Q2_1 + Q2_2 + Q2_3 + Q2_4 + Q2_5 + Q2_7, data=ch[ORD_MTHD_CD=='OTW'],
                   output = "Relative Importance Analysis", importance.absolute = T)
slices <- rwa4$relative.importance$importance
lbls <- c("Speed","Customer Connection","Above and Beyond","Accuracy","Beverage Taste","Cleanliness")
lbls <- paste(lbls, round(slices,0)) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, cex=0.75,
    #col=terrain.colors(length(lbls)),
    main="CE Survey Drivers of Transaction Count\nOTW",
    cex.main=1)






###INTENT TO RETURN FROM CE SURVEY

#recode variables as binary TB
listofvars <- grep("Q",colnames(ch),value=T)
#ch[, (listofvars[1]) := lapply(.SD, function(x) ifelse(x==5,1,0)), .SDcols=listofvars[1]]
#ch[, (listofvars[-1]) := lapply(.SD, function(x) ifelse(x==7,1,0)), .SDcols=listofvars[-1]]

#drop rows with NAs
ch <- na.omit(ch)

#create two binary variables from order method

#correlation matrix for ranking
matall <- round(cor(ch[, (listofvars), with=F],method="spearman",use="pairwise.complete.obs"),3)
matall <- as.data.table(matall)
matall <- cbind(listofvars,matall)
matall <- setorder(matall,-Q1)

#correlation matrix for ranking
matmop <- round(cor(ch[ORD_MTHD_CD=='MOP', (listofvars), with=F],method="spearman",use="pairwise.complete.obs"),3)
matmop <- as.data.table(matmop)
matmop <- cbind(listofvars,matmop)
matmop <- setorder(matmop,-Q1)

#correlation matrix for ranking
matcafe <- round(cor(ch[ORD_MTHD_CD=='CAFE', (listofvars), with=F],method="spearman",use="pairwise.complete.obs"),3)
matcafe <- as.data.table(matcafe)
matcafe <- cbind(listofvars,matcafe)
matcafe <- setorder(matcafe,-Q1)

#correlation matrix for ranking
matotw <- round(cor(ch[ORD_MTHD_CD=='OTW', (listofvars), with=F],method="spearman",use="pairwise.complete.obs"),3)
matotw <- as.data.table(matotw)
matotw <- cbind(listofvars,matotw)
matotw <- setorder(matotw,-Q1)

#RWA
#all channels
rwa1 <- Regression(Q1 ~ Q2_1 + Q2_2 + Q2_3 + Q2_4 + Q2_5 + Q2_7, data=ch,
                   output = "Relative Importance Analysis", importance.absolute = T,
                   type = "Poisson")
slices <- rwa1$relative.importance$importance
lbls <- c("Speed","Customer Connection","Above and Beyond","Accuracy","Beverage Taste","Cleanliness")
lbls <- paste(lbls, round(slices,0)) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, cex=0.75,
    #col=terrain.colors(length(lbls)),
    main="CE Survey Drivers of Intent to Return (1-5)\nAll Channels",
    cex.main=1)

#cafe
rwa2 <- Regression(Q1 ~ Q2_1 + Q2_2 + Q2_3 + Q2_4 + Q2_5 + Q2_7, data=ch[ORD_MTHD_CD=='CAFE'],
                   output = "Relative Importance Analysis", importance.absolute = T,
                   type = "Poisson")
slices <- rwa2$relative.importance$importance
lbls <- c("Speed","Customer Connection","Above and Beyond","Accuracy","Beverage Taste","Cleanliness")
lbls <- paste(lbls, round(slices,0)) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, cex=0.75,
    #col=terrain.colors(length(lbls)),
    main="CE Survey Drivers of Intent to Return (1-5)\nCafe",
    cex.main=1)

#mop
rwa3 <- Regression(Q1 ~ Q2_1 + Q2_2 + Q2_3 + Q2_4 + Q2_5 + Q2_7, data=ch[ORD_MTHD_CD=='MOP'],
                   output = "Relative Importance Analysis", importance.absolute = T,
                   type = "Poisson")
slices <- rwa3$relative.importance$importance
lbls <- c("Speed","Customer Connection","Above and Beyond","Accuracy","Beverage Taste","Cleanliness")
lbls <- paste(lbls, round(slices,0)) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, cex=0.75,
    #col=terrain.colors(length(lbls)),
    main="CE Survey Drivers of Intent to Return (1-5)\nMOP",
    cex.main=1)

#otw
rwa4 <- Regression(Q1 ~ Q2_1 + Q2_2 + Q2_3 + Q2_4 + Q2_5 + Q2_7, data=ch[ORD_MTHD_CD=='OTW'],
                   output = "Relative Importance Analysis", importance.absolute = T,
                   type = "Poisson")
slices <- rwa4$relative.importance$importance
lbls <- c("Speed","Customer Connection","Above and Beyond","Accuracy","Beverage Taste","Cleanliness")
lbls <- paste(lbls, round(slices,0)) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, cex=0.75,
    #col=terrain.colors(length(lbls)),
    main="CE Survey Drivers of Intent to Return (1-5)\nOTW",
    cex.main=1)


#correlations
round(cor(ch[, (c("Q2_4","Q2_1","Q2_7","Q2_5")), with=F],method="spearman",use="pairwise.complete.obs"),3)
# round(cor(ch[ORD_MTHD_CD=='MOP', (listofvars), with=F],method="spearman",use="pairwise.complete.obs"),3)
# round(cor(ch[ORD_MTHD_CD=='CAFE', (listofvars), with=F],method="spearman",use="pairwise.complete.obs"),3)
# round(cor(ch[ORD_MTHD_CD=='OTW', (listofvars), with=F],method="spearman",use="pairwise.complete.obs"),3)

# ch[,c("Q2_1","Q2_2","Q2_3","Q2_4","Q2_5","Q2_7","Q2_8")] <-
#   lapply(ch[,c("Q2_1","Q2_2","Q2_3","Q2_4","Q2_5","Q2_7","Q2_8")], ordered)
# specify the model
mymodel <- '#regressions
            Q1 ~ essentials + Q2_3 + Q2_2
            essentials ~ Q2_4 + Q2_1 + Q2_7 + Q2_5
            #latent variable definitions
            essentials  =~ Q2_2 + Q2_1 + Q2_7 + Q2_5
            '
# fit the model (using a logit model for my binary outcome)
fit <- cfa(mymodel, data=ch, ordered=c("Q2_1","Q2_2","Q2_4","Q2_5","Q2_7"))
# display summary output
summary(fit, fit.measures=TRUE)

#bivariate logistic regression models
m1 <- glm(Q1 ~ Q2_1,data=ch[ORD_MTHD_CD=='MOP'],family="binomial")
preds <- predict(m1,data=ch[ORD_MTHD_CD=='MOP'],type="response", se.fit=TRUE)
predf <- preds$fit # predicted
lower <- preds$fit - (1.96*preds$se.fit) # lower bounds
upper <- preds$fit + (1.96*preds$se.fit) # upper bounds
plot(ch[ORD_MTHD_CD=='MOP',Q2_1],ch[ORD_MTHD_CD=='MOP',Q1])
lines(predf)
# lines(lower,lty=2)
# lines(upper,lty=2)

m2 <- glm(Q1 ~ Q2_1,data=ch[ORD_MTHD_CD=='CAFE'],family="binomial")
preds <- predict(m2,data=ch[ORD_MTHD_CD=='CAFE'],type="response", se.fit=TRUE)
predf <- preds$fit # predicted
lower <- preds$fit - (1.96*preds$se.fit) # lower bounds
upper <- preds$fit + (1.96*preds$se.fit) # upper bounds
plot(ch[ORD_MTHD_CD=='CAFE',Q2_1],ch[ORD_MTHD_CD=='CAFE',Q1])
lines(predf)

m3 <- glm(Q1 ~ Q2_1,data=ch[ORD_MTHD_CD=='OTW'],family="binomial")
preds <- predict(m3,data=ch[ORD_MTHD_CD=='OTW'],type="response", se.fit=TRUE)
predf <- preds$fit # predicted
lower <- preds$fit - (1.96*preds$se.fit) # lower bounds
upper <- preds$fit + (1.96*preds$se.fit) # upper bounds
plot(ch[ORD_MTHD_CD=='OTW',Q2_1],ch[ORD_MTHD_CD=='OTW',Q1])
lines(predf)

m4 <- glm(Q1 ~ Q2_2+Q2_3+Q2_5,data=ch[ORD_MTHD_CD=='MOP'],family=binomial)
preds <- predict(m4,data=ch[ORD_MTHD_CD=='OTW'],type="response", se.fit=TRUE)
predf <- preds$fit # predicted
lower <- preds$fit - (1.96*preds$se.fit) # lower bounds
upper <- preds$fit + (1.96*preds$se.fit) # upper bounds
plot(ch[ORD_MTHD_CD=='OTW',Q2_1],ch[ORD_MTHD_CD=='OTW',Q1])
lines(predf)




# #split by channel
# chmop <- ch[ORD_MTHD_CD=='MOP']
# chcafe <- ch[ORD_MTHD_CD=='CAFE']
# chotw <- ch[ORD_MTHD_CD=='OTW']
# 
# #look at correlation matrices
# matmop <- chmop[, (listofvars), with=F]
# round(cor(matmop,method="pearson",use="pairwise.complete.obs"),3)
# matcafe <- chcafe[, (listofvars), with=F]
# round(cor(matcafe,method="pearson",use="pairwise.complete.obs"),3)
# matotw <- chotw[, (listofvars), with=F]
# round(cor(matotw,method="pearson",use="pairwise.complete.obs"),3)
# 
# #Set up logistic regression: Intent to Return (0/1)
# #logistic regression model
# mmop <- glm(Q1 ~ Q2_2 + Q2_8, data = chmop, family = binomial)
# mcafe <- glm(Q1 ~ Q2_2 + Q2_8, data = chcafe, family = binomial)
# motw <- glm(Q1 ~ Q2_2 + Q2_8, data = chotw, family = binomial)