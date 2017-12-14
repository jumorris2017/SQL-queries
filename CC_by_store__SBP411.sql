SELECT
      ce.STORE_NUM

  -- Total valid response counts by question
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_2' THEN ce.QSTN_ID END) AS Q2_2_Response_Total

  -- Total top box responses
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_2' THEN 1 ELSE 0 END) AS Q2_2_TB_Cnt

  -- Compute top box scores for each question
  ,CASE WHEN COUNT(CASE WHEN ce.QSTN_ID = 'Q2_2' THEN ce.QSTN_ID END) = 0 THEN NULL
   ELSE TO_CHAR(SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_2' THEN 1 ELSE 0 END)
    / COUNT(CASE WHEN ce.QSTN_ID = 'Q2_2' THEN ce.QSTN_ID END),'0.0000') END AS Q2_2_TB_Score

  -- Compute average scores for each question
  ,TO_CHAR(AVG(CASE WHEN ce.QSTN_ID = 'Q2_2' THEN ce.RSPNS_ID END),'0.00') AS CC_Avg_Score

FROM APPDWH.AFT_CV_SRVY_RSPNS ce

  INNER JOIN APPDWH.ADT_CAL c
    ON TRUNC(ce.TRANS_DTM) = c.CAL_DT
      AND TRUNC(ce.TRANS_DTM) BETWEEN '01-SEP-17' AND '31-OCT-17'
      
  INNER JOIN APPDWH.DDM_RETAIL_ORG_STORE_DIST org
    ON ce.STORE_NUM = org.STORE_NUM
      AND org.DIV_ORG_LVL_ID IN (3,6,106)  -- U.S. company stores only (including reserve bar), but excluding New Concepts and Roastery
      --AND org.DIV_ORG_LVL_ID IN (3,6,105,106,800)  -- U.S. company stores only, including New Concepts and Roastery

WHERE ce.RSPNS_ID <> '9'  -- rspns_id = 9 for unanswered questions
  AND ce.QSTN_ID NOT IN ('Q1','Q11') -- these questions are not in Customer Connection or Store Operations scores

GROUP BY
    ce.STORE_NUM
;




SELECT
      TRUNC(ce.TRANS_DTM) as trans_date

  -- Total valid response counts by question
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_2' THEN ce.QSTN_ID END) AS Response_Total

FROM APPDWH.AFT_CV_SRVY_RSPNS ce

  INNER JOIN APPDWH.ADT_CAL c
    ON TRUNC(ce.TRANS_DTM) = c.CAL_DT
      AND c.FSCL_YR_NUM = 2017 
      AND c.FSCL_QTR_IN_YR_NUM = 4 -- ADDED FOR PT3
      
  INNER JOIN APPDWH.DDM_RETAIL_ORG_STORE_DIST org
    ON ce.STORE_NUM = org.STORE_NUM
      AND org.DIV_ORG_LVL_ID IN (2)  -- U.S. company stores only (including reserve bar), but excluding New Concepts and Roastery

WHERE ce.RSPNS_ID <> '9'  -- rspns_id = 9 for unanswered questions
  AND ce.QSTN_ID NOT IN ('Q1','Q11') -- these questions are not in Customer Connection or Store Operations scores

GROUP BY
    TRUNC(ce.TRANS_DTM)
ORDER BY
    TRUNC(ce.TRANS_DTM)
;




SELECT
  -- Total valid response counts by question
  AVG(CASE WHEN ce.QSTN_ID = 'Q2_2' THEN ce.QSTN_ID END),0) AS Response_Total

FROM APPDWH.AFT_CV_SRVY_RSPNS ce

  INNER JOIN APPDWH.ADT_CAL c
    ON TRUNC(ce.TRANS_DTM) = c.CAL_DT
      AND c.FSCL_YR_NUM = 2017 
      AND c.FSCL_QTR_IN_YR_NUM = 4 -- ADDED FOR PT3
      
  INNER JOIN APPDWH.DDM_RETAIL_ORG_STORE_DIST org
    ON ce.STORE_NUM = org.STORE_NUM
      AND org.DIV_ORG_LVL_ID IN (2)  -- U.S. company stores only (including reserve bar), but excluding New Concepts and Roastery

WHERE ce.RSPNS_ID <> '9'  -- rspns_id = 9 for unanswered questions
  AND ce.QSTN_ID NOT IN ('Q1','Q11') -- these questions are not in Customer Connection or Store Operations scores
;