/* worth perceptions by store FY17 Q4 */
/* export as "worthperceptions_QSRproximinity_FY17Q4.csv" */

    SELECT
   c.FSCL_YR_NUM
  ,c.FSCL_QTR_IN_YR_NUM
  ,ce.STORE_NUM

  -- Total valid response counts by question
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_8' THEN ce.QSTN_ID END) AS wp_resp_count
  
  -- Total top box responses
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_8' THEN 1 ELSE 0 END) AS wp_tb_count

  -- Compute top box scores for each question
  ,CASE WHEN COUNT(CASE WHEN ce.QSTN_ID = 'Q2_8' THEN ce.QSTN_ID END) = 0 THEN NULL
   ELSE TO_CHAR(SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_8' THEN 1 ELSE 0 END)
    / COUNT(CASE WHEN ce.QSTN_ID = 'Q2_8' THEN ce.QSTN_ID END),'0.0000') END
   AS wp_tb_score

FROM APPDWH.AFT_CV_SRVY_RSPNS ce

  INNER JOIN APPDWH.ADT_CAL c
    ON TRUNC(ce.TRANS_DTM) = c.CAL_DT
      
  INNER JOIN APPDWH.DDM_RETAIL_ORG_STORE_DIST org
    ON ce.STORE_NUM = org.STORE_NUM
      AND org.DIV_ORG_LVL_ID IN (3,6,106)  -- U.S. company stores only (including reserve bar), but excluding New Concepts and Roastery
      --AND org.AREA_ORG_LVL_VERS_SID = 10 -- ADDED FOR V2 (JUST SEATTLE)
      --AND org.RGN_ORG_LVL_ID IN (7,8) -- NY Metro & Northeast -- ADDED FOR V3 (NEW YORK)

WHERE ce.RSPNS_ID <> '9'  -- rspns_id = 9 for unanswered questions
  AND ce.QSTN_ID NOT IN ('Q1','Q11') -- these questions are not in Customer Connection or Store Operations scores
    AND c.FSCL_YR_NUM = 2017
    AND c.FSCL_QTR_IN_YR_NUM = 4
    -- AND (c.FSCL_YR_NUM = 2017 OR c.FSCL_YR_NUM = 2018)
    -- AND (TRUNC(ce.TRANS_DTM) BETWEEN '04-NOV-16' AND '18-NOV-16' OR TRUNC(ce.TRANS_DTM) BETWEEN '04-NOV-17' AND '18-NOV-17')

GROUP BY
   c.FSCL_YR_NUM
  ,c.FSCL_QTR_IN_YR_NUM
  --,TRUNC(ce.TRANS_DTM)
  ,ce.STORE_NUM
;

