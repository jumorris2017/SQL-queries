
SELECT DISTINCT
  sr.STORE_NUM
  ,ca.FSCL_YR_NUM
  ,ca.FSCL_WK_IN_YR_NUM
  --,tt.MOBILE_ORD_PAY_IND --MOBILE ORDER INDICATOR
  ,SUM(CASE  WHEN sr.RSPNS_ID = '7' THEN COALESCE(w.WEIGHT_RT,1) ELSE 0 END) AS TB_COUNT
  ,SUM(COALESCE(w.WEIGHT_RT,1)) AS RSPNS_COUNT
  
  --,ROUND((SUM(CASE  WHEN sr.RSPNS_ID = '5' AND sr.QSTN_ID = 'Q1' THEN COALESCE(w.WEIGHT_RT,1) WHEN sr.RSPNS_ID = '7' THEN COALESCE(w.WEIGHT_RT,1) ELSE 0 END)) / SUM(COALESCE(w.WEIGHT_RT,1)),4) AS TB_SCORE
  
FROM APPOTHER.AFT_CV_SRVY_RSPNS sr

JOIN APPCA.D_CAL ca
  ON ca.CAL_DT = TRUNC(sr.TRANS_DTM)
  
JOIN APPCA.D_STORE_VERS st
  ON sr.STORE_NUM = st.STORE_NUM
    AND st.CURRENT_FLG = 'Y'
    AND st.OWNR_TYPE_CD = 'CO'

LEFT JOIN APPOTHER.CUST_INS_WEIGHTS w
  ON sr.CASE_ID = w.CASE_ID

/* Create universal transaction ID from survey table and use that to join to POS table
/  Limit to U.S. and Canada and transactions after the survey began to improve performance */
JOIN APPCA.F_POS_LINE_ITEM pi
  ON TO_CHAR(sr.TRANS_DTM, 'YYYYMMDD') || TRIM(TO_CHAR(sr.STORE_NUM, '000000'))
      || TRIM(TO_CHAR(SUBSTR(sr.RGSTR_NUM, -1, 2),'00')) || sr.TRANS_ID = pi.TRANS_ID
    AND pi.CNTRY_CD = 'US'
    --AND pi.BUS_DT >= '01-OCT-17' AND pi.BUS_DT < '01-DEC-17'
  
JOIN APPCA.D_POS_LINE_ITEM_TRANS_TYPE tt
  ON pi.POS_LINE_ITEM_TRANS_TYPE_KEY = tt.POS_LINE_ITEM_TRANS_TYPE_KEY
  WHERE tt.ORD_MTHD_CD IN ('MOP')
  AND sr.QSTN_ID IN ('Q2_1')
  AND sr.RSPNS_ID <> '9'
      AND ca.FSCL_YR_NUM = 2018
      AND ca.FSCL_WK_IN_YR_NUM IN (14,15,16,17)
      
GROUP BY
  sr.STORE_NUM
  ,ca.FSCL_YR_NUM
  ,ca.FSCL_WK_IN_YR_NUM







