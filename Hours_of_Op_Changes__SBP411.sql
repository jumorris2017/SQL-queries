/*Future time information*/
/*Export as "Hours_of_Op_Changes_future.csv"*/
SELECT * FROM (
SELECT DISTINCT
   STORE_NUMBER, 
   DAY_OF_WEEK_NUMBER AS dayofweek, 
   --HRS_TYPE_CD, 
   --HOLIDAY_CD, 
   --BUSINESS_OPEN_TIME AS timeopen, 
   BUSINESS_CLOSE_TIME AS timeclose, 
   --STAFF_START_TIME, STAFF_END_TIME, 
   --OPEN_STAFF_COUNT AS staffopen, 
   --CLOSE_STAFF_COUNT AS staffclose, 
   OPEN_24_HR_IND, 
   BUSINESS_OPEN_CD, 
   EFFECTIVE_DATE AS effdate, 
   --EXPIRATION_DATE AS expdate, 
   REC_STAT_CD,
   row_number() over(partition by STORE_NUMBER, DAY_OF_WEEK_NUMBER order by EFFECTIVE_DATE desc) SEQ
FROM APPBUS.SDT_LOC_MSTR_HRS_VW
WHERE HOLIDAY_CD IS NULL
--AND effective_date >= '13-NOV-17' AND effective_date <= '17-DEC-17'
AND effective_date >= '14-NOV-16' AND effective_date <= '18-DEC-16'
AND HRS_TYPE_CD = 'P' AND effective_date is NOT NULL
ORDER BY STORE_NUMBER, SEQ
)
WHERE SEQ = 1


/*Historical time information*/
/*Export as "Hours_of_Op_Changes_past.csv"*/
SELECT * FROM (
SELECT DISTINCT
   STORE_NUMBER,
   DAY_OF_WEEK_NUMBER AS dayofweek, 
   BUSINESS_CLOSE_TIME AS timeclose, 
   OPEN_24_HR_IND, 
   BUSINESS_OPEN_CD, 
   EFFECTIVE_DATE AS effdate, 
   REC_STAT_CD,
   row_number() over(partition by STORE_NUMBER, DAY_OF_WEEK_NUMBER order by EFFECTIVE_DATE desc) SEQ
FROM APPBUS.SDT_LOC_MSTR_HRS_VW
WHERE HOLIDAY_CD IS NULL
--AND effective_date < '13-NOV-17'
AND effective_date < '14-NOV-16'
AND HRS_TYPE_CD = 'P' AND effective_date is NOT NULL
ORDER BY STORE_NUMBER, SEQ
)
WHERE SEQ = 1

/*COSD*/
/*export as "Hours_of_Op_Changes_FY17Q4comp.csv"*/
SELECT ca.FSCL_YR_NUM
  ,f.SALE_HOUR 
  ,f.STORE_NUMBER
  ,SUM(f.NET_DISCOUNTED_SALES_AMT) AS "SALESAMT"
  ,ca.DAY_IN_CAL_WK_NUM AS "DAYOFWEEK"
FROM APPBUS.AFT_POS_INTL_HDR_VW f
    INNER JOIN APPDWH.ADT_CAL ca
        ON f.BUSINESS_DATE = ca.CAL_DT
WHERE (ca.FSCL_YR_NUM = 2017 OR ca.FSCL_YR_NUM = 2016) AND ca.FSCL_QTR_IN_YR_NUM = 4
GROUP BY 
  ca.FSCL_YR_NUM
  ,f.SALE_HOUR
  ,f.STORE_NUMBER
  ,ca.DAY_IN_CAL_WK_NUM
;


