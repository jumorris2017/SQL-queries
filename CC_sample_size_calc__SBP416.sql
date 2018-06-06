
/*query for CC score and survey counts by daypart for a specific store */

WITH sq AS
(SELECT DISTINCT
  TO_CHAR(sr.TRANS_DTM, 'HH24') AS HOUR
    ,(CASE WHEN TO_CHAR(sr.TRANS_DTM, 'HH24') < 14 THEN 1 -- early am
        WHEN TO_CHAR(sr.TRANS_DTM, 'HH24') >=14 THEN 2 -- late pm
        ELSE 0 
        END) AS DAY_PART
  ,sr.STORE_NUM
  ,sr.QSTN_ID
  ,SUM(COALESCE(w.WEIGHT_RT,1)) AS TOTAL_RSPNS
  ,SUM(CASE  WHEN sr.RSPNS_ID = '5' AND sr.QSTN_ID = 'Q1' THEN COALESCE(w.WEIGHT_RT,1) /* coalesce returns non-null weight value, OR if null, returns 1 */
    WHEN sr.RSPNS_ID = '7' THEN COALESCE(w.WEIGHT_RT,1) ELSE 0 END) AS TOTAL_TB
  ,SUM(CASE  WHEN sr.RSPNS_ID = '5' AND sr.QSTN_ID = 'Q1' THEN COALESCE(w.WEIGHT_RT,1) /* coalesce returns non-null weight value, OR if null, returns 1 */
    WHEN sr.RSPNS_ID = '7' THEN COALESCE(w.WEIGHT_RT,1) ELSE 0 END) / SUM(COALESCE(w.WEIGHT_RT,1)) AS TB_SCORE
  ,ca.FSCL_YR_NUM

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
  AND (ca.FSCL_YR_NUM = 2018 AND ca.FSCL_PER_IN_YR_NUM >= 6 AND ca.FSCL_PER_IN_YR_NUM <= 7) 

GROUP BY
  TO_CHAR(sr.TRANS_DTM, 'HH24') 
  ,sr.STORE_NUM
  ,sr.QSTN_ID
  ,ca.FSCL_YR_NUM
), SQ2 AS (
SELECT 
sq.QSTN_ID
,sq.DAY_PART
,sq.STORE_NUM
,sq.FSCL_YR_NUM
,SUM(sq.TOTAL_TB) AS TOTAL_TB
,SUM(sq.TOTAL_RSPNS) AS TOTAL_RSPNS
,ROUND(SUM(sq.TOTAL_TB)/SUM(sq.TOTAL_RSPNS),3) AS TB_SCORE
FROM sq 
GROUP BY sq.QSTN_ID
,sq.FSCL_YR_NUM
,sq.DAY_PART
,sq.STORE_NUM) 
SELECT 
SQ2.QSTN_ID
,SQ2.DAY_PART
,SQ2.FSCL_YR_NUM
,COUNT(DISTINCT SQ2.STORE_NUM) AS STORE_N
,SUM(SQ2.TOTAL_RSPNS) AS TOTAL_RSPNS
,ROUND(SUM(SQ2.TOTAL_RSPNS) / COUNT(DISTINCT SQ2.STORE_NUM),1) AS AVG_RSPNS_PER_STORE
,ROUND(AVG(SQ2.TB_SCORE),3) AS MEAN_STORE_TB
,ROUND(STDDEV_SAMP(SQ2.TB_SCORE),3) AS STDDEV_STORE_TB
FROM SQ2
GROUP BY SQ2.QSTN_ID
,SQ2.DAY_PART
,SQ2.FSCL_YR_NUM
ORDER BY SQ2.DAY_PART