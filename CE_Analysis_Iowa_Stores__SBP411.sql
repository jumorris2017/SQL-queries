--TOPLINE CE SCORES

WITH SQ AS
(SELECT
   c.FSCL_YR_NUM
  ,c.FSCL_QTR_IN_YR_NUM
  ,ce.STORE_NUM

  -- Compute top box scores for each question
  ,CASE WHEN COUNT(CASE WHEN ce.QSTN_ID = 'Q2_2' THEN ce.QSTN_ID END) = 0 THEN NULL
   ELSE TO_CHAR(SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_2' THEN 1 ELSE 0 END)
    / COUNT(CASE WHEN ce.QSTN_ID = 'Q2_2' THEN ce.QSTN_ID END),'0.0000') END
   AS CC
  ,CASE WHEN COUNT(CASE WHEN ce.QSTN_ID = 'Q2_1' THEN ce.QSTN_ID END) = 0 THEN NULL
   ELSE TO_CHAR(SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_1' THEN 1 ELSE 0 END)
    / COUNT(CASE WHEN ce.QSTN_ID = 'Q2_1' THEN ce.QSTN_ID END),'0.0000') END
   AS SPEED
  ,CASE WHEN COUNT(CASE WHEN ce.QSTN_ID = 'Q2_3' THEN ce.QSTN_ID END) = 0 THEN NULL
   ELSE TO_CHAR(SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_3' THEN 1 ELSE 0 END)
    / COUNT(CASE WHEN ce.QSTN_ID = 'Q2_3' THEN ce.QSTN_ID END),'0.0000') END
   AS ABVBEYND
  ,CASE WHEN COUNT(CASE WHEN ce.QSTN_ID = 'Q2_4' THEN ce.QSTN_ID END) = 0 THEN NULL
   ELSE TO_CHAR(SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_4' THEN 1 ELSE 0 END)
    / COUNT(CASE WHEN ce.QSTN_ID = 'Q2_4' THEN ce.QSTN_ID END),'0.0000') END
   AS ACCURACY
  ,CASE WHEN COUNT(CASE WHEN ce.QSTN_ID = 'Q2_5' THEN ce.QSTN_ID END) = 0 THEN NULL
   ELSE TO_CHAR(SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_5' THEN 1 ELSE 0 END)
    / COUNT(CASE WHEN ce.QSTN_ID = 'Q2_5' THEN ce.QSTN_ID END),'0.0000') END
   AS BEVTASTE
  ,CASE WHEN COUNT(CASE WHEN ce.QSTN_ID = 'Q2_6' THEN ce.QSTN_ID END) = 0 THEN NULL
   ELSE TO_CHAR(SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_6' THEN 1 ELSE 0 END)
    / COUNT(CASE WHEN ce.QSTN_ID = 'Q2_6' THEN ce.QSTN_ID END),'0.0000') END
   AS FOODTASTE
  ,CASE WHEN COUNT(CASE WHEN ce.QSTN_ID = 'Q2_7' THEN ce.QSTN_ID END) = 0 THEN NULL
   ELSE TO_CHAR(SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_7' THEN 1 ELSE 0 END)
    / COUNT(CASE WHEN ce.QSTN_ID = 'Q2_7' THEN ce.QSTN_ID END),'0.0000') END
   AS CLEAN

FROM APPDWH.AFT_CV_SRVY_RSPNS ce

  INNER JOIN APPDWH.ADT_CAL c
    ON TRUNC(ce.TRANS_DTM) = c.CAL_DT
      AND c.FSCL_YR_NUM IN (2017,2018)
      AND c.FSCL_QTR_IN_YR_NUM IN (2)
      
  INNER JOIN APPDWH.ADT_STORE org
      ON ce.STORE_NUM = org.STORE_NUM
         AND org.OWNR_TYPE_CD IN ('CO')
         AND org.CNTRY_CD IN ('US')

WHERE ce.RSPNS_ID <> '9'  -- rspns_id = 9 for unanswered questions
  AND ce.STORE_NUM IN (51248,50409,51408,9353,11047,26717,26814,2855,11789,10363,10364,9475,21750,9558,9266,
  2680,10556,2672,8950,10953,10446,7926,11352,8951,23870)

GROUP BY
   c.FSCL_YR_NUM
  ,c.FSCL_QTR_IN_YR_NUM
  ,ce.STORE_NUM), SQ2 AS(
SELECT     
    SQ.FSCL_QTR_IN_YR_NUM
    ,SQ.FSCL_YR_NUM
    ,SQ.STORE_NUM
    ,SQ.CC
    ,SQ.SPEED
    ,SQ.ABVBEYND
    ,SQ.ACCURACY
    ,SQ.BEVTASTE
    ,SQ.FOODTASTE   
    ,SQ.CLEAN
    ,ROUND((AVG(SQ.SPEED)+AVG(SQ.ABVBEYND)+AVG(SQ.ACCURACY)+AVG(SQ.BEVTASTE)+AVG(SQ.FOODTASTE)+AVG(SQ.CLEAN))/6,4) AS STOREOPS
FROM SQ
GROUP BY
    SQ.FSCL_YR_NUM
    ,SQ.FSCL_QTR_IN_YR_NUM
    ,SQ.CC
    ,SQ.SPEED
    ,SQ.ABVBEYND
    ,SQ.ACCURACY
    ,SQ.BEVTASTE
    ,SQ.FOODTASTE   
    ,SQ.CLEAN
    ,SQ.STORE_NUM
Order BY
    SQ.STORE_NUM
    ,SQ.FSCL_YR_NUM DESC), SQ3 AS(
SELECT
    SQ2.FSCL_YR_NUM
    ,SQ2.STORE_NUM
    ,SQ2.FSCL_QTR_IN_YR_NUM
    ,SQ2.CC
    ,SQ2.CC - lag(SQ2.CC) over(order by SQ2.STORE_NUM, SQ2.FSCL_YR_NUM) AS CC_YOY
    ,SQ2.STOREOPS
    ,SQ2.STOREOPS - lag(SQ2.STOREOPS) over(order by SQ2.STORE_NUM, SQ2.FSCL_YR_NUM) AS STOREOPS_YOY
    ,SQ2.SPEED
    ,SQ2.SPEED - lag(SQ2.SPEED) over(order by SQ2.STORE_NUM, SQ2.FSCL_YR_NUM) AS SPEED_YOY
    ,SQ2.ABVBEYND
    ,SQ2.ABVBEYND - lag(SQ2.ABVBEYND) over(order by SQ2.STORE_NUM, SQ2.FSCL_YR_NUM) AS ABVBEYND_YOY
    ,SQ2.ACCURACY
    ,SQ2.ACCURACY - lag(SQ2.ACCURACY) over(order by SQ2.STORE_NUM, SQ2.FSCL_YR_NUM) AS ACCURACY_YOY
    ,SQ2.BEVTASTE
    ,SQ2.BEVTASTE - lag(SQ2.BEVTASTE) over(order by SQ2.STORE_NUM, SQ2.FSCL_YR_NUM) AS BEVTASTE_YOY
    ,SQ2.FOODTASTE
    ,SQ2.FOODTASTE - lag(SQ2.FOODTASTE) over(order by SQ2.STORE_NUM, SQ2.FSCL_YR_NUM) AS FOODTASTE_YOY
    ,SQ2.CLEAN
    ,SQ2.CLEAN - lag(SQ2.CLEAN) over(order by SQ2.STORE_NUM, SQ2.FSCL_YR_NUM) AS CLEAN_YOY

FROM SQ2
GROUP BY 
    SQ2.FSCL_YR_NUM
    ,SQ2.STORE_NUM
    ,SQ2.FSCL_QTR_IN_YR_NUM
    ,SQ2.CC
    ,SQ2.STOREOPS
    ,SQ2.SPEED
    ,SQ2.ABVBEYND
    ,SQ2.ACCURACY
    ,SQ2.BEVTASTE
    ,SQ2.FOODTASTE   
    ,SQ2.CLEAN   
    
ORDER BY
    SQ2.STORE_NUM
    ,SQ2.FSCL_YR_NUM DESC)
SELECT
    SQ3.STORE_NUM
    ,SQ3.CC
    ,SQ3.CC_YOY
    ,SQ3.STOREOPS
    ,SQ3.STOREOPS_YOY
    ,SQ3.SPEED    
    ,SQ3.SPEED_YOY
    ,SQ3.ABVBEYND
    ,SQ3.ABVBEYND_YOY
    ,SQ3.ACCURACY
    ,SQ3.ACCURACY_YOY
    ,SQ3.BEVTASTE
    ,SQ3.BEVTASTE_YOY
    ,SQ3.FOODTASTE   
    ,SQ3.FOODTASTE_YOY  
    ,SQ3.CLEAN 
    ,SQ3.CLEAN_YOY 
FROM SQ3
    WHERE SQ3.FSCL_YR_NUM = 2018
    
    
    
--PEAK CE SCORES

WITH SQ AS
(SELECT
   c.FSCL_YR_NUM
  ,c.FSCL_QTR_IN_YR_NUM
  ,ce.STORE_NUM

  -- Compute top box scores for each question
  ,CASE WHEN COUNT(CASE WHEN ce.QSTN_ID = 'Q2_2' THEN ce.QSTN_ID END) = 0 THEN NULL
   ELSE TO_CHAR(SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_2' THEN 1 ELSE 0 END)
    / COUNT(CASE WHEN ce.QSTN_ID = 'Q2_2' THEN ce.QSTN_ID END),'0.0000') END
   AS PEAKCC
  ,CASE WHEN COUNT(CASE WHEN ce.QSTN_ID = 'Q2_1' THEN ce.QSTN_ID END) = 0 THEN NULL
   ELSE TO_CHAR(SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_1' THEN 1 ELSE 0 END)
    / COUNT(CASE WHEN ce.QSTN_ID = 'Q2_1' THEN ce.QSTN_ID END),'0.0000') END
   AS PEAKSPEED
  ,CASE WHEN COUNT(CASE WHEN ce.QSTN_ID = 'Q2_3' THEN ce.QSTN_ID END) = 0 THEN NULL
   ELSE TO_CHAR(SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_3' THEN 1 ELSE 0 END)
    / COUNT(CASE WHEN ce.QSTN_ID = 'Q2_3' THEN ce.QSTN_ID END),'0.0000') END
   AS PEAKABVBEYND
  ,CASE WHEN COUNT(CASE WHEN ce.QSTN_ID = 'Q2_4' THEN ce.QSTN_ID END) = 0 THEN NULL
   ELSE TO_CHAR(SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_4' THEN 1 ELSE 0 END)
    / COUNT(CASE WHEN ce.QSTN_ID = 'Q2_4' THEN ce.QSTN_ID END),'0.0000') END
   AS PEAKACCURACY
  ,CASE WHEN COUNT(CASE WHEN ce.QSTN_ID = 'Q2_5' THEN ce.QSTN_ID END) = 0 THEN NULL
   ELSE TO_CHAR(SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_5' THEN 1 ELSE 0 END)
    / COUNT(CASE WHEN ce.QSTN_ID = 'Q2_5' THEN ce.QSTN_ID END),'0.0000') END
   AS PEAKBEVTASTE
  ,CASE WHEN COUNT(CASE WHEN ce.QSTN_ID = 'Q2_6' THEN ce.QSTN_ID END) = 0 THEN NULL
   ELSE TO_CHAR(SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_6' THEN 1 ELSE 0 END)
    / COUNT(CASE WHEN ce.QSTN_ID = 'Q2_6' THEN ce.QSTN_ID END),'0.0000') END
   AS PEAKFOODTASTE
  ,CASE WHEN COUNT(CASE WHEN ce.QSTN_ID = 'Q2_7' THEN ce.QSTN_ID END) = 0 THEN NULL
   ELSE TO_CHAR(SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_7' THEN 1 ELSE 0 END)
    / COUNT(CASE WHEN ce.QSTN_ID = 'Q2_7' THEN ce.QSTN_ID END),'0.0000') END
   AS PEAKCLEAN

FROM APPDWH.AFT_CV_SRVY_RSPNS ce

  INNER JOIN APPDWH.ADT_CAL c
    ON TRUNC(ce.TRANS_DTM) = c.CAL_DT
      AND c.FSCL_YR_NUM IN (2017,2018)
      AND c.FSCL_QTR_IN_YR_NUM IN (2)
      
  INNER JOIN APPDWH.ADT_STORE org
      ON ce.STORE_NUM = org.STORE_NUM
         AND org.OWNR_TYPE_CD IN ('CO')
         AND org.CNTRY_CD IN ('US')

WHERE ce.RSPNS_ID <> '9'  -- rspns_id = 9 for unanswered questions
  AND ce.STORE_NUM IN (51248,50409,51408,9353,11047,26717,26814,2855,11789,10363,10364,9475,21750,9558,9266,
  2680,10556,2672,8950,10953,10446,7926,11352,8951,23870)
  AND (TO_CHAR(ce.TRANS_DTM, 'HH24') >=7 AND TO_CHAR(ce.TRANS_DTM, 'HH24') < 11)

GROUP BY
   c.FSCL_YR_NUM
  ,c.FSCL_QTR_IN_YR_NUM
  ,ce.STORE_NUM), SQ2 AS(
SELECT     
    SQ.FSCL_QTR_IN_YR_NUM
    ,SQ.FSCL_YR_NUM
    ,SQ.STORE_NUM
    ,SQ.PEAKCC
    ,SQ.PEAKSPEED
    ,SQ.PEAKABVBEYND
    ,SQ.PEAKACCURACY
    ,SQ.PEAKBEVTASTE
    ,SQ.PEAKFOODTASTE   
    ,SQ.PEAKCLEAN
    ,ROUND((AVG(SQ.PEAKSPEED)+AVG(SQ.PEAKABVBEYND)+AVG(SQ.PEAKACCURACY)+AVG(SQ.PEAKBEVTASTE)+AVG(SQ.PEAKFOODTASTE)+AVG(SQ.PEAKCLEAN))/6,4) AS STOREOPS
FROM SQ
GROUP BY
    SQ.FSCL_YR_NUM
    ,SQ.FSCL_QTR_IN_YR_NUM
    ,SQ.PEAKCC
    ,SQ.PEAKSPEED
    ,SQ.PEAKABVBEYND
    ,SQ.PEAKACCURACY
    ,SQ.PEAKBEVTASTE
    ,SQ.PEAKFOODTASTE   
    ,SQ.PEAKCLEAN
    ,SQ.STORE_NUM
Order BY
    SQ.STORE_NUM
    ,SQ.FSCL_YR_NUM DESC), SQ3 AS(
SELECT
    SQ2.FSCL_YR_NUM
    ,SQ2.STORE_NUM
    ,SQ2.FSCL_QTR_IN_YR_NUM
    ,SQ2.PEAKCC
    ,SQ2.PEAKCC - lag(SQ2.PEAKCC) over(order by SQ2.STORE_NUM, SQ2.FSCL_YR_NUM) AS PEAKCC_YOY
    ,SQ2.STOREOPS
    ,SQ2.STOREOPS - lag(SQ2.STOREOPS) over(order by SQ2.STORE_NUM, SQ2.FSCL_YR_NUM) AS STOREOPS_YOY
    ,SQ2.PEAKSPEED
    ,SQ2.PEAKSPEED - lag(SQ2.PEAKSPEED) over(order by SQ2.STORE_NUM, SQ2.FSCL_YR_NUM) AS PEAKSPEED_YOY
    ,SQ2.PEAKABVBEYND
    ,SQ2.PEAKABVBEYND - lag(SQ2.PEAKABVBEYND) over(order by SQ2.STORE_NUM, SQ2.FSCL_YR_NUM) AS PEAKABVBEYND_YOY
    ,SQ2.PEAKACCURACY
    ,SQ2.PEAKACCURACY - lag(SQ2.PEAKACCURACY) over(order by SQ2.STORE_NUM, SQ2.FSCL_YR_NUM) AS PEAKACCURACY_YOY
    ,SQ2.PEAKBEVTASTE
    ,SQ2.PEAKBEVTASTE - lag(SQ2.PEAKBEVTASTE) over(order by SQ2.STORE_NUM, SQ2.FSCL_YR_NUM) AS PEAKBEVTASTE_YOY
    ,SQ2.PEAKFOODTASTE
    ,SQ2.PEAKFOODTASTE - lag(SQ2.PEAKFOODTASTE) over(order by SQ2.STORE_NUM, SQ2.FSCL_YR_NUM) AS PEAKFOODTASTE_YOY
    ,SQ2.PEAKCLEAN
    ,SQ2.PEAKCLEAN - lag(SQ2.PEAKCLEAN) over(order by SQ2.STORE_NUM, SQ2.FSCL_YR_NUM) AS PEAKCLEAN_YOY

FROM SQ2
GROUP BY 
    SQ2.FSCL_YR_NUM
    ,SQ2.STORE_NUM
    ,SQ2.FSCL_QTR_IN_YR_NUM
    ,SQ2.PEAKCC
    ,SQ2.STOREOPS
    ,SQ2.PEAKSPEED
    ,SQ2.PEAKABVBEYND
    ,SQ2.PEAKACCURACY
    ,SQ2.PEAKBEVTASTE
    ,SQ2.PEAKFOODTASTE   
    ,SQ2.PEAKCLEAN   
    
ORDER BY
    SQ2.STORE_NUM
    ,SQ2.FSCL_YR_NUM DESC)
SELECT
    SQ3.STORE_NUM
    ,SQ3.PEAKCC
    ,SQ3.PEAKCC_YOY
    ,SQ3.STOREOPS
    ,SQ3.STOREOPS_YOY
    ,SQ3.PEAKSPEED    
    ,SQ3.PEAKSPEED_YOY
    ,SQ3.PEAKABVBEYND
    ,SQ3.PEAKABVBEYND_YOY
    ,SQ3.PEAKACCURACY
    ,SQ3.PEAKACCURACY_YOY
    ,SQ3.PEAKBEVTASTE
    ,SQ3.PEAKBEVTASTE_YOY
    ,SQ3.PEAKFOODTASTE   
    ,SQ3.PEAKFOODTASTE_YOY  
    ,SQ3.PEAKCLEAN 
    ,SQ3.PEAKCLEAN_YOY 
FROM SQ3
    WHERE SQ3.FSCL_YR_NUM = 2018
    
    
    
    
/* CE by channel -- CAW */ 

SELECT DISTINCT
  sr.STORE_NUM
  ,sr.QSTN_ID
  ,ca.FSCL_YR_NUM
  ,ca.FSCL_QTR_IN_YR_NUM
  ,tt.ORD_MTHD_CD
  ,SUM(CASE  WHEN sr.RSPNS_ID = '5' AND sr.QSTN_ID = 'Q1' THEN COALESCE(w.WEIGHT_RT,1) /* coalesce returns non-null weight value, OR if null, returns 1 */
    WHEN sr.RSPNS_ID = '7' THEN COALESCE(w.WEIGHT_RT,1) ELSE 0 END) AS TOTAL_TB
  ,SUM(COALESCE(w.WEIGHT_RT,1)) AS TOTAL_RSPNS
  ,ROUND((SUM(CASE  WHEN sr.RSPNS_ID = '5' AND sr.QSTN_ID = 'Q1' THEN COALESCE(w.WEIGHT_RT,1) WHEN sr.RSPNS_ID = '7' THEN COALESCE(w.WEIGHT_RT,1) ELSE 0 END)) / SUM(COALESCE(w.WEIGHT_RT,1)),4) AS TB_SCORE
  
FROM APPOTHER.AFT_CV_SRVY_RSPNS sr

JOIN APPCA.D_CAL ca
  ON ca.CAL_DT = TRUNC(sr.TRANS_DTM)
  
JOIN APPCA.D_STORE_VERS st
  ON sr.STORE_NUM = st.STORE_NUM
    AND st.CURRENT_FLG = 'Y'
    AND st.OWNR_TYPE_CD IN ('CO')
    AND st.CNTRY_CD_2_DGT_ISO IN ('US')
    AND sr.STORE_NUM IN (51248,50409,51408,9353,11047,26717,26814,2855,11789,10363,10364,9475,21750,9558,9266,
  2680,10556,2672,8950,10953,10446,7926,11352,8951,23870)

LEFT JOIN APPOTHER.CUST_INS_WEIGHTS w
  ON sr.CASE_ID = w.CASE_ID

/* Create universal transaction ID from survey table and use that to join to POS table
/  Limit to U.S. and Canada and transactions after the survey began to improve performance */
JOIN APPCA.F_POS_LINE_ITEM pi
  ON TO_CHAR(sr.TRANS_DTM, 'YYYYMMDD') || TRIM(TO_CHAR(sr.STORE_NUM, '000000'))
      || TRIM(TO_CHAR(SUBSTR(sr.RGSTR_NUM, -1, 2),'00')) || sr.TRANS_ID = pi.TRANS_ID

  
JOIN APPCA.D_POS_LINE_ITEM_TRANS_TYPE tt
  ON pi.POS_LINE_ITEM_TRANS_TYPE_KEY = tt.POS_LINE_ITEM_TRANS_TYPE_KEY
  WHERE tt.ORD_MTHD_CD IN ('CAFE','MOP','OTW')
  AND sr.QSTN_ID IN ('Q2_1','Q2_2','Q2_3','Q2_4','Q2_5','Q2_6','Q2_7')
  AND sr.RSPNS_ID <> '9'
  AND ca.FSCL_YR_NUM = 2018
  AND ca.FSCL_QTR_IN_YR_NUM = 2
      
GROUP BY
  sr.STORE_NUM
  ,sr.QSTN_ID
  ,ca.FSCL_YR_NUM
  ,ca.FSCL_QTR_IN_YR_NUM
  ,tt.ORD_MTHD_CD

ORDER BY
  tt.ORD_MTHD_CD
  ,sr.QSTN_ID
  
  
  
--Channel mix by store
--416

SELECT
   ss.STORE_NUM
  ,ss.DRIVE_THRU_IND
  ,ca.FSCL_YR_NUM
  ,ca.FSCL_QTR_IN_YR_NUM
  ,COUNT(DISTINCT tr.TRANS_ID) AS TTL_TRANS_CNT
  --,COUNT(DISTINCT CASE WHEN ty.ORD_MTHD_CD = 'MOP' THEN tr.TRANS_ID END) AS MOP_TRANS_CNT
  --,COUNT(DISTINCT CASE WHEN ty.ORD_MTHD_CD = 'OTW' THEN tr.TRANS_ID END) AS OTW_TRANS_CNT
  ,ROUND(COUNT(DISTINCT CASE WHEN tr.TRANS_END_TM_KEY BETWEEN 70000 AND 95959 AND ty.ORD_MTHD_CD = 'MOP' THEN tr.TRANS_ID END) / COUNT(DISTINCT tr.TRANS_ID),4)*100 AS MOP_PCT
  ,ROUND(COUNT(DISTINCT CASE WHEN tr.TRANS_END_TM_KEY BETWEEN 70000 AND 95959 AND ty.ORD_MTHD_CD = 'MOP' THEN tr.TRANS_ID END) / COUNT(DISTINCT CASE WHEN tr.TRANS_END_TM_KEY BETWEEN 70000 AND 95959 THEN tr.TRANS_ID END),4)*100 AS PEAK_MOP_PCT
  ,ROUND(COUNT(DISTINCT CASE WHEN tr.TRANS_END_TM_KEY BETWEEN 70000 AND 95959 AND ty.ORD_MTHD_CD = 'OTW' THEN tr.TRANS_ID END) / COUNT(DISTINCT tr.TRANS_ID),4)*100 AS OTW_PCT
  ,ROUND(COUNT(DISTINCT CASE WHEN tr.TRANS_END_TM_KEY BETWEEN 70000 AND 95959 AND ty.ORD_MTHD_CD = 'OTW' THEN tr.TRANS_ID END) / COUNT(DISTINCT CASE WHEN tr.TRANS_END_TM_KEY BETWEEN 70000 AND 95959 THEN tr.TRANS_ID END),4)*100 AS PEAK_OTW_PCT

FROM APPCA.F_POS_HDR tr

INNER JOIN D_POS_HDR_TRANS_TYPE ty
  ON tr.POS_HDR_TRANS_TYPE_KEY = ty.POS_HDR_TRANS_TYPE_KEY
  
INNER JOIN APPCA.D_STORE_VERS ss
  ON ss.STORE_VERS_KEY = tr.STORE_VERS_KEY
  
INNER JOIN APPCA.D_CAL ca
  ON tr.BUS_DT = ca.CAL_DT

WHERE tr.CNTRY_CD = 'US'
  AND ss.OWNR_TYPE_CD = 'CO'
  AND ca.FSCL_YR_NUM = 2018 AND ca.FSCL_QTR_IN_YR_NUM = 2
  AND ss.STORE_NUM IN (51248,50409,51408,9353,11047,26717,26814,2855,11789,10363,10364,9475,21750,9558,9266,
  2680,10556,2672,8950,10953,10446,7926,11352,8951,23870)

GROUP BY
   ss.STORE_NUM
  ,ca.FSCL_YR_NUM
  ,ca.FSCL_QTR_IN_YR_NUM
  ,ss.DRIVE_THRU_IND
  
  
  
--PARTNER TENURE AND STORE

WITH prtnr AS 
(SELECT DISTINCT
      SAP_PRTNR_ID
      ,ORIG_HIRE_DT
      ,MOST_RECENT_HIRE_DT
      FROM D_PRTNR_VERS
      WHERE EMP_STAT_CD = 'Active'
 ) SELECT 
      prtnr.SAP_PRTNR_ID AS PRTNR_NUM
      ,prtnr.ORIG_HIRE_DT
      ,prtnr.MOST_RECENT_HIRE_DT
      ,gls.STORE_NUM
      ,gls.JOB_ID

FROM prtnr

INNER JOIN (
  SELECT * FROM (
    SELECT PRTNR_NUM, STORE_NUM, JOB_ID,
    ROW_NUMBER() OVER (PARTITION BY PRTNR_NUM ORDER BY END_DTM DESC) AS mostrec
    FROM APPDWH.AFT_GLS_PRTNR_TMCARD@SBP411  
    WHERE CNTRY_CD = 'US'
      AND BUS_DT BETWEEN '01-JAN-18' AND '01-APR-18'
      AND JOB_ID IN (50000362,50000358,50000117)
      AND STORE_NUM IN (51248,50409,51408,9353,11047,26717,26814,2855,11789,10363,10364,9475,21750,9558,9266,
  2680,10556,2672,8950,10953,10446,7926,11352,8951,23870)
    ) WHERE mostrec = 1
  ) gls 
  
ON prtnr.SAP_PRTNR_ID = gls.PRTNR_NUM