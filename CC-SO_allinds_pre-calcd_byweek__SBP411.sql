WITH SQ AS
(SELECT
    ce.STORE_NUM
    ,c.FSCL_YR_NUM
  -- Compute top box scores for each question
    ,CASE WHEN COUNT(CASE WHEN ce.QSTN_ID = 'Q2_2' THEN ce.QSTN_ID END) = 0 THEN NULL
   ELSE TO_CHAR(SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_2' THEN 1 ELSE 0 END)
    / COUNT(CASE WHEN ce.QSTN_ID = 'Q2_2' THEN ce.QSTN_ID END),'0.00000') END
   AS Q2_2_TB_Score
  ,CASE WHEN COUNT(CASE WHEN ce.QSTN_ID = 'Q2_1' THEN ce.QSTN_ID END) = 0 THEN NULL
   ELSE TO_CHAR(SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_1' THEN 1 ELSE 0 END)
    / COUNT(CASE WHEN ce.QSTN_ID = 'Q2_1' THEN ce.QSTN_ID END),'0.000') END
   AS Q2_1_TB_Score
  ,CASE WHEN COUNT(CASE WHEN ce.QSTN_ID = 'Q2_3' THEN ce.QSTN_ID END) = 0 THEN NULL
   ELSE TO_CHAR(SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_3' THEN 1 ELSE 0 END)
    / COUNT(CASE WHEN ce.QSTN_ID = 'Q2_3' THEN ce.QSTN_ID END),'0.000') END
   AS Q2_3_TB_Score
  ,CASE WHEN COUNT(CASE WHEN ce.QSTN_ID = 'Q2_4' THEN ce.QSTN_ID END) = 0 THEN NULL
   ELSE TO_CHAR(SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_4' THEN 1 ELSE 0 END)
    / COUNT(CASE WHEN ce.QSTN_ID = 'Q2_4' THEN ce.QSTN_ID END),'0.000') END
   AS Q2_4_TB_Score
  ,CASE WHEN COUNT(CASE WHEN ce.QSTN_ID = 'Q2_5' THEN ce.QSTN_ID END) = 0 THEN NULL
   ELSE TO_CHAR(SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_5' THEN 1 ELSE 0 END)
    / COUNT(CASE WHEN ce.QSTN_ID = 'Q2_5' THEN ce.QSTN_ID END),'0.000') END
   AS Q2_5_TB_Score
  ,CASE WHEN COUNT(CASE WHEN ce.QSTN_ID = 'Q2_6' THEN ce.QSTN_ID END) = 0 THEN NULL
   ELSE TO_CHAR(SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_6' THEN 1 ELSE 0 END)
    / COUNT(CASE WHEN ce.QSTN_ID = 'Q2_6' THEN ce.QSTN_ID END),'0.000') END
   AS Q2_6_TB_Score
  ,CASE WHEN COUNT(CASE WHEN ce.QSTN_ID = 'Q2_7' THEN ce.QSTN_ID END) = 0 THEN NULL
   ELSE TO_CHAR(SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_7' THEN 1 ELSE 0 END)
    / COUNT(CASE WHEN ce.QSTN_ID = 'Q2_7' THEN ce.QSTN_ID END),'0.000') END
   AS Q2_7_TB_Score

FROM APPDWH.AFT_CV_SRVY_RSPNS ce
  
  INNER JOIN APPDWH.ADT_CAL c
    ON TRUNC(ce.TRANS_DTM) = c.CAL_DT
    AND c.FSCL_WK_IN_YR_NUM IN (30,31,32,33)
    AND c.FSCL_YR_NUM IN (2017,2018)
    
  INNER JOIN APPDWH.ADT_STORE org
      ON ce.STORE_NUM = org.STORE_NUM
         AND org.OWNR_TYPE_CD IN ('CO')
         AND org.CNTRY_CD IN ('US')
    
WHERE ce.RSPNS_ID <> '9'  -- rspns_id = 9 for unanswered questions
  AND ce.QSTN_ID NOT IN ('Q1','Q11') -- these questions are not in Customer Connection or Store Operations scores

GROUP BY
    ce.STORE_NUM,
    c.FSCL_YR_NUM
), SQ2 AS(
SELECT     
    SQ.STORE_NUM
    ,SQ.FSCL_YR_NUM
    ,ROUND(SQ.Q2_2_TB_Score,4) AS CE_TB_CustConn
    ,ROUND(SQ.Q2_1_TB_Score,4) AS CE_TB_Speed
    ,ROUND(SQ.Q2_3_TB_Score,4) AS CE_TB_AbovBey
    ,ROUND(SQ.Q2_4_TB_Score,4) AS CE_TB_Accuracy
    ,ROUND(SQ.Q2_5_TB_Score,4) AS CE_TB_BevTaste
    ,ROUND(SQ.Q2_6_TB_Score,4) AS CE_TB_FoodTaste
    ,ROUND(SQ.Q2_7_TB_Score,4) AS CE_TB_Clean
    ,ROUND((AVG(SQ.Q2_1_TB_Score)+AVG(SQ.Q2_3_TB_Score)+AVG(SQ.Q2_4_TB_Score)+AVG(SQ.Q2_5_TB_Score)+AVG(SQ.Q2_6_TB_Score)+AVG(SQ.Q2_7_TB_Score))/6,4) AS CE_TB_StoreOps
FROM SQ
GROUP BY
    SQ.STORE_NUM
    ,SQ.FSCL_YR_NUM
    ,SQ.Q2_2_TB_Score
    ,SQ.Q2_1_TB_Score
    ,SQ.Q2_3_TB_Score
    ,SQ.Q2_4_TB_Score
    ,SQ.Q2_5_TB_Score
    ,SQ.Q2_6_TB_Score
    ,SQ.Q2_7_TB_Score
Order BY
    SQ.FSCL_YR_NUM ASC
    ,SQ.STORE_NUM DESC), SQ3 AS(
SELECT
    SQ2.FSCL_YR_NUM
    ,SQ2.STORE_NUM
    ,SQ2.CE_TB_CustConn
    ,SQ2.CE_TB_CustConn - lag(SQ2.CE_TB_CustConn) over(order by SQ2.STORE_NUM, SQ2.FSCL_YR_NUM) AS CE_TB_CustConn_YOY_DELTA
    ,SQ2.CE_TB_Speed
    ,SQ2.CE_TB_Speed - lag(SQ2.CE_TB_Speed) over(order by SQ2.STORE_NUM, SQ2.FSCL_YR_NUM) AS CE_TB_Speed_YOY_DELTA
    ,SQ2.CE_TB_AbovBey
    ,SQ2.CE_TB_AbovBey - lag(SQ2.CE_TB_AbovBey) over(order by SQ2.STORE_NUM, SQ2.FSCL_YR_NUM) AS CE_TB_AbovBey_YOY_DELTA
    ,SQ2.CE_TB_Accuracy
    ,SQ2.CE_TB_Accuracy - lag(SQ2.CE_TB_Accuracy) over(order by SQ2.STORE_NUM, SQ2.FSCL_YR_NUM) AS CE_TB_Accuracy_YOY_DELTA
    ,SQ2.CE_TB_BevTaste
    ,SQ2.CE_TB_BevTaste - lag(SQ2.CE_TB_BevTaste) over(order by SQ2.STORE_NUM, SQ2.FSCL_YR_NUM) AS CE_TB_BevTaste_YOY_DELTA
    ,SQ2.CE_TB_FoodTaste
    ,SQ2.CE_TB_FoodTaste - lag(SQ2.CE_TB_FoodTaste) over(order by SQ2.STORE_NUM, SQ2.FSCL_YR_NUM) AS CE_TB_FoodTaste_YOY_DELTA
    ,SQ2.CE_TB_Clean
    ,SQ2.CE_TB_Clean - lag(SQ2.CE_TB_Clean) over(order by SQ2.STORE_NUM, SQ2.FSCL_YR_NUM) AS CE_TB_Clean_YOY_DELTA
    ,SQ2.CE_TB_StoreOps
    ,SQ2.CE_TB_StoreOps - lag(SQ2.CE_TB_StoreOps) over(order by SQ2.STORE_NUM, SQ2.FSCL_YR_NUM) AS CE_TB_StoreOps_YOY_DELTA
FROM SQ2
GROUP BY 
    SQ2.FSCL_YR_NUM
    ,SQ2.STORE_NUM
    ,SQ2.CE_TB_CustConn
    ,SQ2.CE_TB_Speed
    ,SQ2.CE_TB_AbovBey
    ,SQ2.CE_TB_Accuracy
    ,SQ2.CE_TB_BevTaste
    ,SQ2.CE_TB_FoodTaste
    ,SQ2.CE_TB_Clean
    ,SQ2.CE_TB_StoreOps
ORDER BY
    SQ2.FSCL_YR_NUM DESC
    ,SQ2.STORE_NUM DESC)
SELECT
    SQ3.STORE_NUM
    ,SQ3.CE_TB_CustConn
    ,SQ3.CE_TB_Speed
    ,SQ3.CE_TB_AbovBey
    ,SQ3.CE_TB_Accuracy
    ,SQ3.CE_TB_BevTaste
    ,SQ3.CE_TB_FoodTaste
    ,SQ3.CE_TB_Clean
    ,SQ3.CE_TB_StoreOps
    ,SQ3.CE_TB_CustConn_YOY_DELTA
    ,SQ3.CE_TB_Speed_YOY_DELTA
    ,SQ3.CE_TB_AbovBey_YOY_DELTA
    ,SQ3.CE_TB_Accuracy_YOY_DELTA
    ,SQ3.CE_TB_BevTaste_YOY_DELTA
    ,SQ3.CE_TB_FoodTaste_YOY_DELTA
    ,SQ3.CE_TB_Clean_YOY_DELTA
    ,SQ3.CE_TB_StoreOps_YOY_DELTA
FROM SQ3
    WHERE SQ3.FSCL_YR_NUM = 2018

