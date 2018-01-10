/* CE by channel, at the GUID level -- CAW */ 

SELECT DISTINCT
   --sr.CASE_ID
  --sr.GUID_USER_ID
  --,sr.RSPNS_DT
  --,sr.STORE_NUM
  --,sr.TRANS_DTM
  sr.QSTN_ID
  --,sr.RSPNS_ID
  
  ,ca.FSCL_YR_NUM
  ,ca.FSCL_QTR_IN_YR_NUM
  
  --,pi.TRANS_END_TM_KEY
  
  ,tt.MOBILE_ORD_PAY_IND --MOBILE ORDER INDICATOR
  ,tt.MOBILE_IND
  ,tt.ORD_MTHD_CD
FROM APPOTHER.AFT_CV_SRVY_RSPNS sr

JOIN APPCA.D_CAL ca
  ON ca.CAL_DT = TRUNC(sr.TRANS_DTM)
  
JOIN APPCA.D_STORE_VERS st
  ON sr.STORE_NUM = st.STORE_NUM
    AND st.CURRENT_FLG = 'Y'
    AND st.OWNR_TYPE_CD = 'CO'

/* Create universal transaction ID from survey table and use that to join to POS table
/  Limit to U.S. and Canada and transactions after the survey began to improve performance */
JOIN APPCA.F_POS_LINE_ITEM pi
  ON TO_CHAR(sr.TRANS_DTM, 'YYYYMMDD') || TRIM(TO_CHAR(sr.STORE_NUM, '000000'))
      || TRIM(TO_CHAR(SUBSTR(sr.RGSTR_NUM, -1, 2),'00')) || sr.TRANS_ID = pi.TRANS_ID
    AND pi.CNTRY_CD = 'US'
    --AND pi.BUS_DT >= '01-OCT-17' AND pi.BUS_DT < '01-DEC-17'
  
JOIN APPCA.D_POS_LINE_ITEM_TRANS_TYPE tt
  ON pi.POS_LINE_ITEM_TRANS_TYPE_KEY = tt.POS_LINE_ITEM_TRANS_TYPE_KEY
  WHERE tt.ORD_MTHD_CD IN ('CAFE','MOP','OTW')
  AND sr.QSTN_ID IN ('Q1', 'Q2_1','Q2_2','Q2_3','Q2_4','Q2_5','Q2_6','Q2_7','Q2_8')
  AND sr.RSPNS_ID <> '9'
  AND sr.STORE_NUM = 5798
      AND (ca.FSCL_YR_NUM = 2018 AND ca.FSCL_QTR_IN_YR_NUM = 1) 
      --OR (ca.FSCL_YR_NUM = 2017 AND ca.FSCL_QTR_IN_YR_NUM = 4) 
