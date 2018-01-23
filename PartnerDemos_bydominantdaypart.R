##Analysis of Partner Demographics by Dominant Day-Part Worked
##Author: Julie Morris

##load libraries
library(foreign)
library(data.table)
library(ggplot2)
library(tidyverse)

##read in SPSS dataset (as data.frame) and set as data.table
db <- read.spss("//starbucks/amer/portal/Departments/WMO/Marketing Research/New Q drive/Partner Insights/Partner Perspectives/Research/Partner Life Survey/Data/Partner Life_FinalData_CLEAN_1.sav", to.data.frame=TRUE)
setDT(db)
setnames(db,"PanelistIdQuestion","RID")

#keep only baristas
db <- db[Store_Role=="Barista"]

#merge in daypart data
dp <- read.csv()
db <- left_join(db,dp,by=c("PartnerID"))
setDT(db)

##outliers
#drop values where Sbux hours worked is 0 or >60 (same as Q1_7_Starbucks_Hours_Worked_Remove_Outliers)
db[Q1_7_Starbucks_Hours_Worked>0 & Q1_7_Starbucks_Hours_Worked<61, sbux_hours_worked := Q1_7_Starbucks_Hours_Worked]

##start using the SAP question. only for hoursly; all managers *should* be 40 
##(not what is reported; most managers report >40 hours)
db[role==3, SAP_Avg_Hrs_per_Wk_8_weeks := 40]

###recode binary variables as 0/1
#more than one job (yes=1;no=0)
db[Q1_1_Recode_Flag_MT1_Job=="No", more_than_one_job := 0]
db[Q1_1_Recode_Flag_MT1_Job=="Yes", more_than_one_job := 1]

##marital status: married or living with partner
db[Q8_1_Marital_Status=="Living with a partner", marital_married_part := 1]
db[Q8_1_Marital_Status=="Married/remarried", marital_married_part := 1]
db[Q8_1_Marital_Status=="Single, never been married", marital_married_part := 0]
db[Q8_1_Marital_Status=="Prefer not to answer", marital_married_part := NA]
db[Q8_1_Marital_Status=="Separated", marital_married_part := 0]
db[Q8_1_Marital_Status=="Divorced or widowed", marital_married_part := 0]

###student status
db[Q8_6_Student_Flag=="Not Student", student := 0]
db[Q8_6_Student_Flag=="Student", student := 1]

#consistent work schedules (very consistent = 1; else = 0)
db[Q2_1_Schedule_Consistency=="Very consistent", sched_very_consistent := 1]
db[Q2_1_Schedule_Consistency=="Somewhat consistent", sched_very_consistent := 0]
db[Q2_1_Schedule_Consistency=="Neutral", sched_very_consistent := 0]
db[Q2_1_Schedule_Consistency=="Somewhat inconsistent", sched_very_consistent := 0]
db[Q2_1_Schedule_Consistency=="Very inconsistent", sched_very_consistent := 0]

#revalue, for labels
db[Q8_5_Health_Coverage=="Affordable Care Act (also referred to as Obamacare or the Health Insurance Marketplace)", Q8_5_Health_Coverage := "Affordable Care Act"]
db[Q8_5_Health_Coverage=="Through Starbucks", health_sbux := 1]
db[Q8_5_Health_Coverage=="Affordable Care Act", health_sbux := 0]
db[Q8_5_Health_Coverage=="Through my parents", health_sbux := 0]
db[Q8_5_Health_Coverage=="Through my spouse", health_sbux := 0]
db[Q8_5_Health_Coverage=="Other (e.g., school, military)", health_sbux := 0]
db[Q8_5_Health_Coverage=="I don't currently have health insurance", health_sbux := 0]
db[Q8_5_Health_Coverage=="Prefer not to answer", health_sbux := NA]
###highest education level
##high school
db[Q8_7_Education_Level=="Some college or Associate degree", educ_highschool := 1]
db[Q8_7_Education_Level=="Graduated college/Bachelor's degree", educ_highschool := 1]
db[Q8_7_Education_Level=="Some Graduate school", educ_highschool := 1]
db[Q8_7_Education_Level=="Advanced degree (Master's, Ph.D.)", educ_highschool := 1]
db[Q8_7_Education_Level=="High school graduate or GED", educ_highschool := 1]
db[Q8_7_Education_Level=="Some high school", educ_highschool := 0]
db[Q8_7_Education_Level=="Trade/technical school degree", educ_highschool := 1]
db[Q8_7_Education_Level=="Other", educ_highschool := NA]
db[Q8_7_Education_Level=="Prefer not to answer", educ_highschool := NA]
##bachelor's degree
db[Q8_7_Education_Level=="Some college or Associate degree", educ_bach := 0]
db[Q8_7_Education_Level=="Graduated college/Bachelor's degree", educ_bach := 1]
db[Q8_7_Education_Level=="Some Graduate school", educ_bach := 1]
db[Q8_7_Education_Level=="Advanced degree (Master's, Ph.D.)", educ_bach := 1]
db[Q8_7_Education_Level=="High school graduate or GED", educ_bach := 0]
db[Q8_7_Education_Level=="Some high school", educ_bach := 0]
db[Q8_7_Education_Level=="Trade/technical school degree", educ_bach := 0]
db[Q8_7_Education_Level=="Other", educ_bach := NA]
db[Q8_7_Education_Level=="Prefer not to answer", educ_bach := NA]

#frequency table: binary & continuous variables
tempbin <- db %>%
  group_by(Store_Role) %>%
  summarize(more_than_one_job = mean(more_than_one_job,na.rm=T),
            marital_married_part = mean(marital_married_part,na.rm=T),
            Kids_Flag = mean(Kids_Flag,na.rm=T),
            student = mean(student,na.rm=T),
            primary_income = mean(Q8_4_Primary_Income_Flag,na.rm=T),
            sched_very_consistent = mean(sched_very_consistent,na.rm=T),
            health_sbux = mean(health_sbux,na.rm=T),
            educ_highschool = mean(educ_highschool,na.rm=T),
            educ_bach = mean(educ_bach,na.rm=T),
            hrs_min_selfrpt = round(min(sbux_hours_worked,na.rm=T),1),
            hrs_max_selfrpt = round(max(sbux_hours_worked,na.rm=T),1),
            hrs_avg_selfrpt = round(mean(sbux_hours_worked,na.rm=T),1),
            hrs_med_selfrpt = round(median(sbux_hours_worked,na.rm=T),1),
            hrs_min_SAP = round(min(SAP_Avg_Hrs_per_Wk_8_weeks,na.rm=T),1),
            hrs_max_SAP = round(max(SAP_Avg_Hrs_per_Wk_8_weeks,na.rm=T),1),
            hrs_avg_SAP = round(mean(SAP_Avg_Hrs_per_Wk_8_weeks,na.rm=T),1),
            hrs_med_SAP = round(median(SAP_Avg_Hrs_per_Wk_8_weeks,na.rm=T),1))
setDT(tempbin)

#frequency table: education
temped <- db %>%
  group_by(Store_Role,Q8_7_Education_Level) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))
setDT(temped)

#frequency table: tenure
tempten <- db %>%
  group_by(Store_Role,Tenure_Rollup) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))
setDT(tempten)

