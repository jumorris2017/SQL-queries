SELECT
   c.FSCL_YR_NUM
  ,ce.STORE_NUM
  ,org.AREA_ORG_LVL_VERS_SID
  ,TRUNC(ce.TRANS_DTM) AS trans_date

  -- Total valid response counts by question
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_8' THEN ce.QSTN_ID END) AS Q2_8_Response_Total
  
  -- Total top box responses
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_8' THEN 1 ELSE 0 END) AS Q2_8_TB_Cnt
  
  -- Compute top box scores for each question
  ,CASE WHEN COUNT(CASE WHEN ce.QSTN_ID = 'Q2_8' THEN ce.QSTN_ID END) = 0 THEN NULL
   ELSE TO_CHAR(SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_8' THEN 1 ELSE 0 END)
    / COUNT(CASE WHEN ce.QSTN_ID = 'Q2_8' THEN ce.QSTN_ID END),'0.0000') END
   AS Q2_8_TB_Score

  -- Compute average scores for each question
  ,TO_CHAR(AVG(CASE WHEN ce.QSTN_ID = 'Q2_8' THEN ce.RSPNS_ID END),'0.00') AS Q2_8_Avg_Score

FROM APPDWH.AFT_CV_SRVY_RSPNS ce

  INNER JOIN APPDWH.ADT_CAL c
    ON TRUNC(ce.TRANS_DTM) = c.CAL_DT
      
  INNER JOIN APPDWH.DDM_RETAIL_ORG_STORE_DIST org
    ON ce.STORE_NUM = org.STORE_NUM
      AND org.DIV_ORG_LVL_ID IN (3,6,106)  -- U.S. company stores only (including reserve bar), but excluding New Concepts and Roastery
      --AND org.DIV_ORG_LVL_ID IN (3,6,105,106,800)  -- U.S. company stores only, including New Concepts and Roastery

WHERE ce.RSPNS_ID <> '9'  -- rspns_id = 9 for unanswered questions
  AND ce.QSTN_ID NOT IN ('Q1','Q11') -- these questions are not in Customer Connection or Store Operations scores
  --AND ce.QSTN_ID = 'Q2_2' --CC only
  --AND ce.STORE_NUM IN (101,102)  -- for testing
    AND (c.FSCL_YR_NUM = 2017 OR c.FSCL_YR_NUM = 2018)
    AND (TRUNC(ce.TRANS_DTM) BETWEEN '04-NOV-16' AND '18-NOV-16' OR TRUNC(ce.TRANS_DTM) BETWEEN '04-NOV-17' AND '18-NOV-17')
    AND c.CAL_WK_IN_YR_NUM IN (45,46,47)
    --AND (org.AREA_ORG_LVL_VERS_SID = 10 OR org.AREA_ORG_LVL_VERS_SID = 74) --10 is SEA and (I believe) 74 is NYC

GROUP BY
   c.FSCL_YR_NUM
  ,TRUNC(ce.TRANS_DTM)
  ,ce.STORE_NUM
  ,org.AREA_ORG_LVL_VERS_SID
ORDER BY
   ce.STORE_NUM
   ,c.FSCL_YR_NUM
   ,TRUNC(ce.TRANS_DTM)
;

