/* export as "CC_Peak-NonPeak.csv" */
/* export as "CC_DT-CAFE.csv" */

/*peak non-peak*/

WITH SQ AS
(SELECT DISTINCT
  tt.ORD_MTHD_CD
  ,ca.FSCL_YR_NUM
  ,ca.FSCL_QTR_IN_YR_NUM
  ,TO_CHAR(sr.TRANS_DTM, 'HH24') AS TRANS_HOUR
  ,(CASE WHEN (TO_CHAR(sr.TRANS_DTM, 'HH24') >=7 AND TO_CHAR(sr.TRANS_DTM, 'HH24') < 10) THEN 'PEAK' 
    WHEN (TO_CHAR(sr.TRANS_DTM, 'HH24') <7 OR TO_CHAR(sr.TRANS_DTM, 'HH24') >= 10) THEN 'NON-PEAK'
    ELSE 'NA' END) AS TIME_OF_DAY
  ,SUM(CASE  WHEN sr.RSPNS_ID = '5' AND sr.QSTN_ID = 'Q1' THEN COALESCE(w.WEIGHT_RT,1) WHEN sr.RSPNS_ID = '7' THEN COALESCE(w.WEIGHT_RT,1) ELSE 0 END) AS TB_COUNT
  ,SUM(COALESCE(w.WEIGHT_RT,1)) AS RSPNS_COUNT
  --,ROUND((SUM(CASE  WHEN sr.RSPNS_ID = '5' AND sr.QSTN_ID = 'Q1' THEN COALESCE(w.WEIGHT_RT,1) WHEN sr.RSPNS_ID = '7' THEN COALESCE(w.WEIGHT_RT,1) ELSE 0 END)) / SUM(COALESCE(w.WEIGHT_RT,1)),4) AS PEAK_CC_TB_SCORE
  
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
  AND sr.QSTN_ID IN ('Q2_2')
  AND sr.RSPNS_ID <> '9'
  AND ((ca.FSCL_YR_NUM = 2018 AND ca.FSCL_QTR_IN_YR_NUM IN (1,2)) OR (ca.FSCL_YR_NUM = 2017))
  --AND (TO_CHAR(sr.TRANS_DTM, 'HH24') >=7 AND TO_CHAR(sr.TRANS_DTM, 'HH24') < 10) -- PEAK ONLY
  --AND (TO_CHAR(sr.TRANS_DTM, 'HH24') <7 OR TO_CHAR(sr.TRANS_DTM, 'HH24') > 10) -- non-PEAK ONLY

GROUP BY
  tt.ORD_MTHD_CD
  ,ca.FSCL_YR_NUM
  ,ca.FSCL_QTR_IN_YR_NUM
  ,TO_CHAR(sr.TRANS_DTM, 'HH24')
ORDER BY
  tt.ORD_MTHD_CD
  ,ca.FSCL_YR_NUM
  ,ca.FSCL_QTR_IN_YR_NUM)
  SELECT 
  SQ.ORD_MTHD_CD
  ,SQ.FSCL_YR_NUM
  ,SQ.FSCL_QTR_IN_YR_NUM
  ,SQ.TIME_OF_DAY
  ,ROUND((SUM(SQ.TB_COUNT) / SUM(SQ.RSPNS_COUNT)),4) AS CC_TB_SCORE
  FROM SQ
  GROUP BY 
  SQ.ORD_MTHD_CD
  ,SQ.FSCL_YR_NUM
  ,SQ.FSCL_QTR_IN_YR_NUM
  ,SQ.TIME_OF_DAY




/*dt vs cafe - MOP transactions*/

SELECT DISTINCT
  ca.FSCL_YR_NUM
  ,ca.FSCL_QTR_IN_YR_NUM
  ,st.DRIVE_THRU_IND
  ,ROUND((SUM(CASE  WHEN sr.RSPNS_ID = '5' AND sr.QSTN_ID = 'Q1' THEN COALESCE(w.WEIGHT_RT,1) WHEN sr.RSPNS_ID = '7' THEN COALESCE(w.WEIGHT_RT,1) ELSE 0 END)) / SUM(COALESCE(w.WEIGHT_RT,1)),4) AS CC_TB_SCORE
  
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
  AND sr.QSTN_ID IN ('Q2_2')
  AND sr.RSPNS_ID <> '9'
  AND ((ca.FSCL_YR_NUM = 2018 AND ca.FSCL_QTR_IN_YR_NUM IN (1,2)) OR ca.FSCL_YR_NUM = 2017)

GROUP BY
  ca.FSCL_YR_NUM
  ,ca.FSCL_QTR_IN_YR_NUM
  ,st.DRIVE_THRU_IND
ORDER BY
  st.DRIVE_THRU_IND
  ,ca.FSCL_YR_NUM
  ,ca.FSCL_QTR_IN_YR_NUM


