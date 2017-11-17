/* query 1 */

SELECT
      ce.STORE_NUM
      ,org.DIV_ORG_LVL_ID
      ,c.CAL_MNTH_IN_YR_NUM
      ,c.CAL_YR_NUM
      ,d.DRIVE_THRU_IND

  -- Total valid response counts by question
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_2' THEN ce.QSTN_ID END) AS Q2_2_Response_Total

  -- Total top box responses
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_2' THEN 1 ELSE 0 END) AS Q2_2_TB_Cnt

/*
  -- Compute top box scores for each question
  ,CASE WHEN COUNT(CASE WHEN ce.QSTN_ID = 'Q2_2' THEN ce.QSTN_ID END) = 0 THEN NULL
   ELSE TO_CHAR(SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_2' THEN 1 ELSE 0 END)
    / COUNT(CASE WHEN ce.QSTN_ID = 'Q2_2' THEN ce.QSTN_ID END),'0.0000') END AS Q2_2_TB_Score

  -- Compute average scores for each question
  ,TO_CHAR(AVG(CASE WHEN ce.QSTN_ID = 'Q2_2' THEN ce.RSPNS_ID END),'0.00') AS CC_Avg_Score
*/
FROM APPDWH.AFT_CV_SRVY_RSPNS ce

  INNER JOIN APPDWH.ADT_CAL c
    ON TRUNC(ce.TRANS_DTM) = c.CAL_DT
      AND c.FSCL_YR_NUM > 2015
      
  INNER JOIN APPDWH.ADT_STORE d
    ON ce.STORE_NUM = d.STORE_NUM
    
  INNER JOIN APPDWH.DDM_RETAIL_ORG_STORE_DIST org
    ON ce.STORE_NUM = org.STORE_NUM
    AND org.DIV_ORG_LVL_ID IN (2)  -- Canada stores
      --AND org.DIV_ORG_LVL_ID IN (3,6,106)  -- U.S. company stores only (including reserve bar), but excluding New Concepts and Roastery
      --AND org.DIV_ORG_LVL_ID IN (3,6,105,106,800)  -- U.S. company stores only, including New Concepts and Roastery

WHERE ce.RSPNS_ID <> '9'  -- rspns_id = 9 for unanswered questions
  AND ce.QSTN_ID NOT IN ('Q1','Q11') -- these questions are not in Customer Connection or Store Operations scores

GROUP BY
    ce.STORE_NUM
    ,org.DIV_ORG_LVL_ID
    ,c.CAL_MNTH_IN_YR_NUM
    ,c.CAL_YR_NUM
    ,d.DRIVE_THRU_IND
;


/* query 2 */
SELECT
      m.STORE_NUMBER
      ,to_char(m.BUSINESS_DATE,'MM') AS date_month
      ,to_char(m.BUSINESS_DATE,'YYYY') AS date_year

  -- Sum customer occassions
  ,SUM(m.CUST_TRANS_CNT) AS cust_trans_cnt
  ,SUM(m.ACTIVE_STORE_DAY_CNT) AS active_store_day_cnt
  -- COSD
  ,CASE WHEN SUM(m.ACTIVE_STORE_DAY_CNT) = 0 THEN 0 ELSE ROUND(SUM(m.CUST_TRANS_CNT) / SUM(m.ACTIVE_STORE_DAY_CNT),2) END AS cosd

FROM APPBUS.DFT_INTL_STORE_DAY_VW m

  INNER JOIN APPDWH.DDM_RETAIL_ORG_STORE_DIST org
    ON m.STORE_NUMBER = org.STORE_NUM
    AND org.DIV_ORG_LVL_ID IN (2)  -- Canada stores

WHERE m.BUSINESS_DATE >= '01-JAN-16'

GROUP BY
    m.STORE_NUMBER
    ,to_char(m.BUSINESS_DATE,'MM')
    ,to_char(m.BUSINESS_DATE,'YYYY') 
;



