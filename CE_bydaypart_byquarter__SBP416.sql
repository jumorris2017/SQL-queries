/*CAW*/
WITH sq AS
(SELECT DISTINCT
  TO_CHAR(sr.TRANS_DTM, 'HH24') AS HOUR
    ,(CASE WHEN TO_CHAR(sr.TRANS_DTM, 'HH24') < 7 THEN 1 -- early am
        WHEN TO_CHAR(sr.TRANS_DTM, 'HH24') >=7 AND TO_CHAR(sr.TRANS_DTM, 'HH24') < 11 THEN 2 -- am
        WHEN TO_CHAR(sr.TRANS_DTM, 'HH24') >=11 AND TO_CHAR(sr.TRANS_DTM, 'HH24') < 14 THEN 3 -- midday
        WHEN TO_CHAR(sr.TRANS_DTM, 'HH24') >=14 AND TO_CHAR(sr.TRANS_DTM, 'HH24') < 16 THEN 4 -- pm
        WHEN TO_CHAR(sr.TRANS_DTM, 'HH24') >=16 THEN 5 -- late pm
        ELSE 0 
        END) AS DAY_PART
  --,sr.STORE_NUM
  ,sr.QSTN_ID
  ,SUM(CASE  WHEN sr.RSPNS_ID = '5' AND sr.QSTN_ID = 'Q1' THEN COALESCE(w.WEIGHT_RT,1) /* coalesce returns non-null weight value, OR if null, returns 1 */
    WHEN sr.RSPNS_ID = '7' THEN COALESCE(w.WEIGHT_RT,1) ELSE 0 END) AS TOTAL_TB
  ,SUM(COALESCE(w.WEIGHT_RT,1)) AS TOTAL_RSPNS
  ,ca.FSCL_YR_NUM
  --,ca.FSCL_QTR_IN_YR_NUM
  ,ca.FSCL_QTR_IN_YR_NUM

FROM APPOTHER.AFT_CV_SRVY_RSPNS sr

JOIN APPCA.D_CAL ca
  ON ca.CAL_DT = TRUNC(sr.TRANS_DTM)
  
JOIN APPCA.D_STORE_VERS st
  ON sr.STORE_NUM = st.STORE_NUM
    AND st.CURRENT_FLG = 'Y'
    AND st.OWNR_TYPE_CD = 'CO'
    AND st.CNTRY_CD_2_DGT_ISO = 'US'

LEFT JOIN APPOTHER.CUST_INS_WEIGHTS w
  ON sr.CASE_ID = w.CASE_ID

  WHERE sr.QSTN_ID IN ('Q2_2')
  AND sr.RSPNS_ID <> '9'
  --AND sr.STORE_NUM = 349
  --AND ((ca.FSCL_YR_NUM = 2018 AND ca.FSCL_QTR_IN_YR_NUM = 1) OR (ca.FSCL_YR_NUM = 2017 AND ca.FSCL_QTR_IN_YR_NUM = 4)) 
  --AND (ca.FSCL_YR_NUM = 2018 AND ca.FSCL_QTR_IN_YR_NUM = 1) 
  AND ca.FSCL_YR_NUM = 2018 AND ca.FSCL_QTR_IN_YR_NUM = 2

GROUP BY
  TO_CHAR(sr.TRANS_DTM, 'HH24') 
  --,sr.STORE_NUM
  ,sr.QSTN_ID
  ,ca.FSCL_YR_NUM
  --,ca.FSCL_QTR_IN_YR_NUM
  ,ca.FSCL_QTR_IN_YR_NUM
)
SELECT 
--sq.STORE_NUM
sq.QSTN_ID
,sq.DAY_PART
,SUM(sq.TOTAL_TB) AS TOTAL_TB
,SUM(sq.TOTAL_RSPNS) AS TOTAL_RSPNS
,ROUND(SUM(sq.TOTAL_TB)/SUM(sq.TOTAL_RSPNS),3) AS TB_SCORE
,sq.FSCL_YR_NUM
--,sq.FSCL_QTR_IN_YR_NUM
,sq.FSCL_QTR_IN_YR_NUM
FROM sq
GROUP BY 
--sq.STORE_NUM
sq.QSTN_ID
,sq.DAY_PART
,sq.FSCL_YR_NUM
--,sq.FSCL_QTR_IN_YR_NUM
,sq.FSCL_QTR_IN_YR_NUM
ORDER BY sq.FSCL_YR_NUM
--,sq.FSCL_QTR_IN_YR_NUM
,sq.FSCL_QTR_IN_YR_NUM
,sq.DAY_PART
