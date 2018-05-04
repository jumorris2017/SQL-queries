WITH SQ AS
(SELECT
    c.FSCL_PER_IN_YR_NUM
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
    AND (c.FSCL_PER_IN_YR_NUM = 7 OR c.FSCL_PER_IN_YR_NUM = 6)
    AND (c.FSCL_YR_NUM = 2018 OR c.FSCL_YR_NUM = 2017)   
    
  INNER JOIN APPDWH.ADT_STORE org
      ON ce.STORE_NUM = org.STORE_NUM
         AND org.OWNR_TYPE_CD IN ('CO') -- DO WE WANT LICENSED??
         AND org.CNTRY_CD IN ('US')
         
WHERE ce.RSPNS_ID <> '9'  -- rspns_id = 9 for unanswered questions
  AND ce.QSTN_ID NOT IN ('Q1','Q11') -- these questions are not in Customer Connection or Store Operations scores


GROUP BY
    c.FSCL_PER_IN_YR_NUM
    ,c.FSCL_YR_NUM
Order BY
    c.FSCL_YR_NUM
    ,c.FSCL_PER_IN_YR_NUM
), SQ2 AS(
SELECT     
    SQ.FSCL_PER_IN_YR_NUM
    ,SQ.FSCL_YR_NUM
    ,ROUND(SQ.Q2_2_TB_Score,3)*100 AS CC_SCORE
    ,ROUND((AVG(SQ.Q2_1_TB_Score)+AVG(SQ.Q2_3_TB_Score)+AVG(SQ.Q2_4_TB_Score)+AVG(SQ.Q2_5_TB_Score)+AVG(SQ.Q2_6_TB_Score)+AVG(SQ.Q2_7_TB_Score))/6,3)*100 AS SO_SCORE
    ,ROW_NUMBER() OVER (PARTITION BY SQ.FSCL_PER_IN_YR_NUM ORDER BY SQ.FSCL_YR_NUM, SQ.FSCL_PER_IN_YR_NUM) AS PERIOD
FROM SQ
GROUP BY
    SQ.FSCL_YR_NUM
    ,SQ.FSCL_PER_IN_YR_NUM
    ,SQ.Q2_2_TB_Score
Order BY
    SQ.FSCL_YR_NUM DESC
    ,SQ.FSCL_PER_IN_YR_NUM DESC), SQ3 AS(
SELECT
    SQ2.FSCL_YR_NUM
    ,SQ2.FSCL_PER_IN_YR_NUM
    ,SQ2.PERIOD
    ,SQ2.CC_SCORE
    ,SQ2.CC_SCORE - lag(SQ2.CC_SCORE) over(order by SQ2.FSCL_PER_IN_YR_NUM, SQ2.FSCL_YR_NUM) AS CC_YOY_DELTA
    ,SQ2.SO_SCORE
    ,SQ2.SO_SCORE - lag(SQ2.SO_SCORE) over(order by SQ2.FSCL_PER_IN_YR_NUM, SQ2.FSCL_YR_NUM) AS SO_YOY_DELTA
FROM SQ2
GROUP BY 
    SQ2.FSCL_YR_NUM
    ,SQ2.FSCL_PER_IN_YR_NUM
    ,SQ2.PERIOD
    ,SQ2.CC_SCORE
    ,SQ2.SO_SCORE
ORDER BY
    SQ2.FSCL_YR_NUM DESC
    ,SQ2.FSCL_PER_IN_YR_NUM DESC)
SELECT
    SQ3.FSCL_YR_NUM
    ,SQ3.FSCL_PER_IN_YR_NUM
    ,SQ3.CC_SCORE
    ,SQ3.SO_SCORE
    ,SQ3.CC_YOY_DELTA
    ,SQ3.SO_YOY_DELTA
FROM SQ3
    WHERE SQ3.PERIOD = 2