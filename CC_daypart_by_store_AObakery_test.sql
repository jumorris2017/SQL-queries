/*query for CC score and survey counts by daypart by store */
/*CAW*/
WITH sq AS
(SELECT DISTINCT
  TO_CHAR(sr.TRANS_DTM, 'HH24') AS HOUR
    ,(CASE WHEN TO_CHAR(sr.TRANS_DTM, 'HH24') < 7 THEN 1 -- early am
        WHEN TO_CHAR(sr.TRANS_DTM, 'HH24') >=7 AND TO_CHAR(sr.TRANS_DTM, 'HH24') < 11 THEN 2 -- am
        WHEN TO_CHAR(sr.TRANS_DTM, 'HH24') >=11 AND TO_CHAR(sr.TRANS_DTM, 'HH24') < 14 THEN 3 -- midday
        WHEN TO_CHAR(sr.TRANS_DTM, 'HH24') >=14 AND TO_CHAR(sr.TRANS_DTM, 'HH24') < 17 THEN 4 -- pm
        WHEN TO_CHAR(sr.TRANS_DTM, 'HH24') >=17 THEN 5 -- late pm
        ELSE 0 
        END) AS DAY_PART
  ,sr.STORE_NUM
  ,(CASE WHEN sr.STORE_NUM IN (9749,2211,6481,10711,2592,5233,10156,18565,9324,6479,9526,10339,5281,9762,2228,2578,11602,
  13227,615,8851,2330,21728,2672,570,13463,21634,9217,6469,11337,24962,5710,640,5711,3470,9712,
  276,21371,262,9479,9490,10546,25535,10602,18163,9266,19815,21750,8951,2648,6764,2705,3484,2843,
  5751,5970,8911,2220,17601,295,9652,9711,5941,13782,21296,10953,8729,10364,7926,20688,25515,17602,
  13624,22822,14542,10942,5993,8142,19864,10446,2366,632,11880,19892,10547,8912,20107,5728,10511,5406,22421,23000,2696,
  8959,9605,263,8660,5855,21376,22725,9558,23160,2855,10743,2733,11178,514,23870,5203,10544,2325,13972,5975,11453,
  2469,5780,604,2440,11352,17564,9595,16188,3471,2341,13637,8722,288,625,2241,274,2318,5545,24961,2529,
  5778,9353,2452,9815,2450,24525,5863,20689,10348,9263,8950,6789,5242,8611,672,9260,10077,5849,5361,5418,3456) THEN 1 ELSE 0
  END) AS TEST_FLAG
  ,sr.QSTN_ID
  ,SUM(CASE  WHEN sr.RSPNS_ID = '5' AND sr.QSTN_ID = 'Q1' THEN COALESCE(w.WEIGHT_RT,1) /* coalesce returns non-null weight value, OR if null, returns 1 */
    WHEN sr.RSPNS_ID = '7' THEN COALESCE(w.WEIGHT_RT,1) ELSE 0 END) AS TOTAL_TB
  ,SUM(COALESCE(w.WEIGHT_RT,1)) AS TOTAL_RSPNS
  ,ca.FSCL_YR_NUM
  --,ca.FSCL_WK_IN_YR_NUM

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
  AND ca.FSCL_YR_NUM IN (2017,2018)
  AND ca.FSCL_WK_IN_YR_NUM IN (7,9,10,11,12,15,16,17,18,19,20,21,22)

GROUP BY
  TO_CHAR(sr.TRANS_DTM, 'HH24') 
  ,sr.STORE_NUM
  ,sr.QSTN_ID
  ,ca.FSCL_YR_NUM
  --,ca.FSCL_WK_IN_YR_NUM
)
SELECT 
--sq.STORE_NUM
sq.QSTN_ID
,sq.TEST_FLAG
,sq.DAY_PART
,SUM(sq.TOTAL_TB) AS TOTAL_TB
,SUM(sq.TOTAL_RSPNS) AS TOTAL_RSPNS
,ROUND(SUM(sq.TOTAL_TB)/SUM(sq.TOTAL_RSPNS),3) AS TB_SCORE
,sq.FSCL_YR_NUM
--,sq.FSCL_WK_IN_YR_NUM
FROM sq
GROUP BY 
--sq.STORE_NUM
sq.QSTN_ID
,sq.TEST_FLAG
,sq.DAY_PART
,sq.FSCL_YR_NUM
--,sq.FSCL_WK_IN_YR_NUM
ORDER BY sq.FSCL_YR_NUM
--,sq.FSCL_WK_IN_YR_NUM
,sq.DAY_PART






