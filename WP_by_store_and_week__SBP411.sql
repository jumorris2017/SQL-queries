SELECT
      ce.STORE_NUM
      ,c.FSCL_YR_NUM
      ,c.FSCL_WK_IN_YR_NUM

  -- Total valid response counts by question
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_8' THEN ce.QSTN_ID END) AS wp_resp

  -- Total top box responses
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_8' THEN 1 ELSE 0 END) AS wp_tb

  -- Compute top box scores for each question
  --,CASE WHEN COUNT(CASE WHEN ce.QSTN_ID = 'Q2_8' THEN ce.QSTN_ID END) = 0 THEN NULL
  -- ELSE TO_CHAR(SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_8' THEN 1 ELSE 0 END)
  --  / COUNT(CASE WHEN ce.QSTN_ID = 'Q2_8' THEN ce.QSTN_ID END),'0.0000') END AS wp_tb_score


FROM APPDWH.AFT_CV_SRVY_RSPNS ce

  INNER JOIN APPDWH.ADT_CAL c
    ON TRUNC(ce.TRANS_DTM) = c.CAL_DT
      --AND TRUNC(ce.TRANS_DTM) BETWEEN '01-SEP-17' AND '31-OCT-17'
      
  INNER JOIN APPDWH.ADT_STORE org
      ON ce.STORE_NUM = org.STORE_NUM
         AND org.OWNR_TYPE_CD IN ('CO')
         AND org.CNTRY_CD IN ('US','CA')
            
      AND (c.FSCL_YR_NUM IN (2016,2017) OR (c.FSCL_YR_NUM IN (2018) AND c.FSCL_WK_IN_YR_NUM BETWEEN 1 AND 41))
      --AND c.FSCL_WK_IN_YR_NUM BETWEEN 26 AND 41

WHERE ce.RSPNS_ID <> '9'  -- rspns_id = 9 for unanswered questions
  AND ce.QSTN_ID NOT IN ('Q1','Q11') -- these questions are not in Customer Connection or Store Operations scores

GROUP BY
      ce.STORE_NUM
      ,c.FSCL_YR_NUM
      ,c.FSCL_WK_IN_YR_NUM

ORDER BY
      ce.STORE_NUM
      ,c.FSCL_YR_NUM
      ,c.FSCL_WK_IN_YR_NUM

