
WITH sq1 AS (
SELECT
    ce.STORE_NUM
    ,ce.GUID_USER_ID
    ,tr.TRANSACTION_NUMBER
    ,it.ITEM_NUMBER
    ,c.CAL_DT AS cal_date
    ,c.FSCL_WK_IN_YR_NUM
    ,c.FSCL_YR_NUM

  -- Total valid response counts by question
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_8' THEN ce.QSTN_ID END) AS WP_Response_Total
  
  -- Total top box responses
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_8' THEN 1 ELSE 0 END) AS WP_TB_Cnt
  
  -- Compute average scores for each question
  ,TO_CHAR(AVG(CASE WHEN ce.QSTN_ID = 'Q2_8' THEN ce.RSPNS_ID END),'0.00') AS WP_Avg_Score
  
  --keep first transaction only
  ,row_number() over(partition by ce.GUID_USER_ID order by c.CAL_DT ASC) SEQ

FROM APPDWH.AFT_CV_SRVY_RSPNS ce

  INNER JOIN APPDWH.ADT_CAL c
    ON TRUNC(ce.TRANS_DTM) = c.CAL_DT
      --AND TRUNC(ce.TRANS_DTM) >= '01-JAN-17'
    AND c.FSCL_WK_IN_YR_NUM > 41 AND c.FSCL_WK_IN_YR_NUM < 43
    AND c.FSCL_YR_NUM = 2016
      
  INNER JOIN APPDWH.DDM_RETAIL_ORG_STORE_DIST org
    ON ce.STORE_NUM = org.STORE_NUM
      AND org.DIV_ORG_LVL_ID IN (3,6,106)  -- U.S. company stores only (including reserve bar), but excluding New Concepts and Roastery
  INNER JOIN APPBUS.USR_ATTRIBUTES_STORE cb
    ON ce.STORE_NUM = cb.STORE_NUMBER
      AND cb.CBSA_CODE = 14460 -- Boston area
      AND ce.STORE_NUM = 807 -- For Testing
      
  INNER JOIN APPBUS.AFT_POS_INTL_LINE_ITEM_VW tr
    ON TRUNC(ce.TRANS_DTM) = tr.BUSINESS_DATE
    AND ce.TRANS_ID = tr.TRANSACTION_NUMBER
  INNER JOIN APPBUS.USR_ATTRIBUTES_PRODUCT it 
    ON tr.ITEM_NUMBER = it.ITEM_NUMBER

WHERE ce.RSPNS_ID <> '9'  -- rspns_id = 9 for unanswered questions
  AND ce.QSTN_ID NOT IN ('Q1','Q11') -- these questions are not in Customer Connection or Store Operations scores
  AND it.ITEM_DESCRIPTION = 'AMERICANO TALL'

GROUP BY
    ce.STORE_NUM
    ,ce.GUID_USER_ID
    ,tr.TRANSACTION_NUMBER
    ,it.ITEM_NUMBER
    ,c.CAL_DT
    ,c.FSCL_WK_IN_YR_NUM
    ,c.FSCL_YR_NUM
) 
SELECT * FROM sq1
WHERE sq1.SEQ = 1





SELECT * FROM APPBUS.USR_ATTRIBUTES_STORE cb
where cb.CBSA_CODE = 14460 -- Boston area

SELECT * FROM APPBUS.USR_ATTRIBUTES_PRODUCT it
where it.ITEM_DESCRIPTION = 'AMERICANO TALL'