/*query for CC score and survey counts by daypart by store */
/*CAW*/
WITH sq AS
(SELECT DISTINCT
  TO_CHAR(sr.TRANS_DTM, 'HH24') AS HOUR
    ,(CASE WHEN TO_CHAR(sr.TRANS_DTM, 'HH24') < 11 THEN '1_early_am_am' 
        WHEN TO_CHAR(sr.TRANS_DTM, 'HH24') >=11 AND TO_CHAR(sr.TRANS_DTM, 'HH24') < 14 THEN '2_midday' -- midday
        WHEN TO_CHAR(sr.TRANS_DTM, 'HH24') >=14 AND TO_CHAR(sr.TRANS_DTM, 'HH24') < 16 THEN '3_pm' -- pm
        WHEN TO_CHAR(sr.TRANS_DTM, 'HH24') >=16 THEN '4_late_pm' -- late pm
        ELSE 'NA' 
        END) AS DAY_PART
  ,sr.QSTN_ID
  ,sr.STORE_NUM
  ,SUM(CASE  WHEN sr.RSPNS_ID = '5' AND sr.QSTN_ID = 'Q1' THEN COALESCE(w.WEIGHT_RT,1) /* coalesce returns non-null weight value, OR if null, returns 1 */
    WHEN sr.RSPNS_ID = '7' THEN COALESCE(w.WEIGHT_RT,1) ELSE 0 END) AS TOTAL_TB
  ,SUM(COALESCE(w.WEIGHT_RT,1)) AS TOTAL_RSPNS
  ,ca.FSCL_YR_NUM
  --,ca.FSCL_QTR_IN_YR_NUM

FROM APPOTHER.AFT_CV_SRVY_RSPNS sr

JOIN APPCA.D_CAL ca
  ON ca.CAL_DT = TRUNC(sr.TRANS_DTM)
  
JOIN APPCA.D_STORE_VERS st
  ON sr.STORE_NUM = st.STORE_NUM
    AND st.CURRENT_FLG = 'Y'
    AND st.OWNR_TYPE_CD = 'CO'
    AND st.CNTRY_CD_2_DGT_ISO = 'CA'
    
LEFT JOIN APPOTHER.CUST_INS_WEIGHTS w
  ON sr.CASE_ID = w.CASE_ID

  WHERE sr.QSTN_ID IN ('Q2_2')
  AND sr.RSPNS_ID <> '9'
  AND ca.FSCL_YR_NUM IN (2017,2018) 
  AND ca.FSCL_QTR_IN_YR_NUM IN (2,3)

GROUP BY
  TO_CHAR(sr.TRANS_DTM, 'HH24') 
  ,sr.STORE_NUM
  ,sr.QSTN_ID
  ,ca.FSCL_YR_NUM
  --,ca.FSCL_QTR_IN_YR_NUM
), SQ2 AS (
SELECT 
sq.QSTN_ID
,sq.DAY_PART
,sq.STORE_NUM
,SUM(sq.TOTAL_TB) AS TOTAL_TB
,SUM(sq.TOTAL_RSPNS) AS TOTAL_RSPNS
,ROUND(SUM(sq.TOTAL_TB)/SUM(sq.TOTAL_RSPNS),3) AS CC_SCORE
,sq.FSCL_YR_NUM
--,sq.FSCL_QTR_IN_YR_NUM

FROM sq

WHERE sq.DAY_PART IN ('3_pm') AND sq.TOTAL_RSPNS >= 40

GROUP BY 
sq.QSTN_ID
,sq.DAY_PART
,sq.STORE_NUM
,sq.FSCL_YR_NUM
--,sq.FSCL_QTR_IN_YR_NUM

ORDER BY sq.FSCL_YR_NUM
--,sq.FSCL_QTR_IN_YR_NUM
,sq.DAY_PART
,sq.STORE_NUM), SQ3 AS(
SELECT
    SQ2.FSCL_YR_NUM
    ,SQ2.STORE_NUM
    ,SQ2.CC_SCORE AS CC_PM_2018
    ,lag(SQ2.CC_SCORE) over(order by SQ2.STORE_NUM, SQ2.FSCL_YR_NUM) AS CC_PM_2017
    ,SQ2.CC_SCORE - lag(SQ2.CC_SCORE) over(order by SQ2.STORE_NUM, SQ2.FSCL_YR_NUM) AS CC_PM_DELTA
FROM SQ2

GROUP BY 
    SQ2.FSCL_YR_NUM
    ,SQ2.CC_SCORE
    ,SQ2.STORE_NUM
ORDER BY
    SQ2.STORE_NUM
    ,SQ2.FSCL_YR_NUM DESC)
SELECT
    TO_NUMBER(SQ3.STORE_NUM) AS STORE_NUM
    --,SQ3.FSCL_YR_NUM
    ,SQ3.CC_PM_2018*100 AS CC_PM_2018
    ,SQ3.CC_PM_2017*100 AS CC_PM_2017
    ,SQ3.CC_PM_DELTA*100 AS CC_PM_DELTA
    ,stv.STORE_NM, stv.DRIVE_THRU_TYPE_CD, stv.STORE_OPEN_DT, stv.CITY_NM, stv.POSTAL_CD 
    ,stv.AREA_NUM, stv.AREA_DESCR, stv.DIST_NUM, stv.DIST_DESCR, stv.RGN_NUM, stv.RGN_DESCR

FROM SQ3

LEFT JOIN (SELECT STORE_NUM, STORE_NM, DRIVE_THRU_TYPE_CD, STORE_OPEN_DT, CITY_NM, POSTAL_CD, AREA_NUM, AREA_DESCR, DIST_NUM, DIST_DESCR, RGN_NUM, RGN_DESCR
    FROM APPCA.D_STORE_VERS 
    WHERE CURRENT_FLG = 'Y'
    AND CNTRY_CD_2_DGT_ISO IN ('CA')
    AND OWNR_TYPE_CD = 'CO') stv
  ON SQ3.STORE_NUM =  stv.STORE_NUM

WHERE SQ3.FSCL_YR_NUM = 2018

ORDER BY
    TO_NUMBER(SQ3.STORE_NUM) 