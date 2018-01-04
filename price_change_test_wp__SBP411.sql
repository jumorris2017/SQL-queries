
SELECT
    ce.STORE_NUM
    ,tr.TRANSACTION_NUMBER
    ,it.ITEM_NUMBER
    ,c.CAL_DT AS cal_date
    ,c.FSCL_PER_IN_YR_NUM
    ,c.FSCL_YR_NUM

  -- Total valid response counts by question
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_8' THEN ce.QSTN_ID END) AS Q2_8_Response_Total
  
  -- Total top box responses
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_8' THEN 1 ELSE 0 END) AS Q2_8_TB_Cnt

FROM APPDWH.AFT_CV_SRVY_RSPNS ce

  INNER JOIN APPDWH.ADT_CAL c
    ON TRUNC(ce.TRANS_DTM) = c.CAL_DT
      AND TRUNC(ce.TRANS_DTM) >= '01-JAN-17'
    --AND c.FSCL_PER_IN_YR_NUM = 3
    --AND (c.FSCL_YR_NUM = 2018)
      
  INNER JOIN APPDWH.DDM_RETAIL_ORG_STORE_DIST org
    ON ce.STORE_NUM = org.STORE_NUM
      AND org.DIV_ORG_LVL_ID IN (3,6,106)  -- U.S. company stores only (including reserve bar), but excluding New Concepts and Roastery
  INNER JOIN APPBUS.USR_ATTRIBUTES_STORE cb
    ON ce.STORE_NUM = cb.STORE_NUMBER
      AND cb.CBSA_CODE = 14460 -- Boston area
      
  INNER JOIN APPBUS.AFT_POS_INTL_LINE_ITEM_VW tr
    ON TRUNC(ce.TRANS_DTM) = tr.BUSINESS_DATE
    AND ce.TRANS_ID = tr.TRANSACTION_NUMBER
  INNER JOIN APPBUS.USR_ATTRIBUTES_PRODUCT it 
    ON tr.ITEM_NUMBER = it.ITEM_NUMBER

WHERE ce.RSPNS_ID <> '9'  -- rspns_id = 9 for unanswered questions
  AND ce.QSTN_ID NOT IN ('Q1','Q11') -- these questions are not in Customer Connection or Store Operations scores
  AND it.ITEM_DESCRIPTION = 'AMERICANO GRANDE'

GROUP BY
    ce.STORE_NUM
    ,tr.TRANSACTION_NUMBER
    ,it.ITEM_NUMBER
    ,c.CAL_DT
    ,c.FSCL_PER_IN_YR_NUM
    ,c.FSCL_YR_NUM
;


