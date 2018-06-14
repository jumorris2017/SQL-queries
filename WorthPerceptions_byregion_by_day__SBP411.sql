SELECT
   c.FSCL_YR_NUM
  --,c.FSCL_WK_IN_YR_NUM
  ,org.RGN_DESCR
  --,c.DAY_NM
  ,TRUNC(ce.TRANS_DTM) AS TRANS_DATE

  -- Total valid response counts by question
  --,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_8' THEN ce.QSTN_ID END) AS Q2_8_Response_Total
  
  -- Total top box responses
  --,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_8' THEN 1 ELSE 0 END) AS Q2_8_TB_Cnt
  
  -- Compute top box scores for each question
  ,CASE WHEN COUNT(CASE WHEN ce.QSTN_ID = 'Q2_8' THEN ce.QSTN_ID END) = 0 THEN NULL
   ELSE TO_CHAR(SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_8' THEN 1 ELSE 0 END)
    / COUNT(CASE WHEN ce.QSTN_ID = 'Q2_8' THEN ce.QSTN_ID END),'0.0000') END
   AS WP_TB_Score

FROM APPDWH.AFT_CV_SRVY_RSPNS ce

  INNER JOIN APPDWH.ADT_CAL c
    ON TRUNC(ce.TRANS_DTM) = c.CAL_DT
      AND c.FSCL_YR_NUM IN (2017,2018)
      AND c.FSCL_QTR_IN_YR_NUM = 3
      
  INNER JOIN APPDWH.ADT_STORE org
      ON ce.STORE_NUM = org.STORE_NUM
         AND org.OWNR_TYPE_CD IN ('CO')
         AND org.CNTRY_CD IN ('US')

WHERE ce.RSPNS_ID <> '9'  -- rspns_id = 9 for unanswered questions
  AND ce.QSTN_ID NOT IN ('Q1','Q11') -- these questions are not in Customer Connection or Store Operations scores
  
GROUP BY
   c.FSCL_YR_NUM
  ,org.RGN_DESCR
  --,c.FSCL_WK_IN_YR_NUM
  --,c.DAY_NM
  ,TRUNC(ce.TRANS_DTM)
ORDER BY
c.FSCL_YR_NUM
,org.RGN_DESCR
,TRUNC(ce.TRANS_DTM)

