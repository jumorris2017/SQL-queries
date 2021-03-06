/* recreating Factbook Slide #11 */

/* part 1 -- CC by store */
/* export as "Comps_by_store_US_pt1.csv" */

SELECT
      ce.STORE_NUM
      --,org.DIV_ORG_LVL_ID
      --,c.FSCL_PER_IN_YR_NUM
      ,c.FSCL_YR_NUM
      --,d.DRIVE_THRU_IND
      ,c.FSCL_QTR_IN_YR_NUM
      --,c.FSCL_WK_IN_YR_NUM

  -- Total valid response counts by question
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_2' THEN ce.QSTN_ID END) AS Q2_2_Response_Total

  -- Total top box responses
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_2' THEN 1 ELSE 0 END) AS Q2_2_TB_Cnt

FROM APPDWH.AFT_CV_SRVY_RSPNS ce

  INNER JOIN APPDWH.ADT_CAL c
    ON TRUNC(ce.TRANS_DTM) = c.CAL_DT
      AND c.FSCL_YR_NUM = 2018
      AND c.FSCL_QTR_IN_YR_NUM = 1
      --AND TRUNC(ce.TRANS_DTM) BETWEEN '01-SEP-17' AND '30-NOV-17'
      
  INNER JOIN APPDWH.ADT_STORE d
    ON ce.STORE_NUM = d.STORE_NUM
    
  INNER JOIN APPDWH.DDM_RETAIL_ORG_STORE_DIST org
    ON ce.STORE_NUM = org.STORE_NUM
    AND org.DIV_ORG_LVL_ID IN (3,6,106)  -- U.S. stores

WHERE ce.RSPNS_ID <> '9'  -- rspns_id = 9 for unanswered questions
  AND ce.QSTN_ID NOT IN ('Q1','Q11') -- these questions are not in Customer Connection or Store Operations scores

GROUP BY
    ce.STORE_NUM
    --,org.DIV_ORG_LVL_ID
    --,c.FSCL_PER_IN_YR_NUM
    ,c.FSCL_YR_NUM
    --,d.DRIVE_THRU_IND
    ,c.FSCL_QTR_IN_YR_NUM
    --,c.FSCL_WK_IN_YR_NUM
;

/* part 2 -- quartlery comps */
/* export as "Comps_by_store_US_pt2.csv" */
 SELECT ca.FSCL_YR_NUM
        , f.STORE_NUMBER
        , SUM(f.NET_DISCOUNTED_SALES_AMT) "QuarterlySales"
        , SUM(f.NET_DISCOUNTED_SALES_LY_AMT) "LYQuarterlySales"
        --, ROUND((SUM(f.NET_DISCOUNTED_SALES_AMT) - SUM(f.NET_DISCOUNTED_SALES_LY_AMT)) / SUM(f.NET_DISCOUNTED_SALES_LY_AMT),4) AS salescomp
        , SUM(f.CUST_TRANS_CNT) "CustTrans"
        , SUM(f.ACTIVE_STORE_DAY_CNT) "day_count"
        , ca.FSCL_QTR_IN_YR_NUM
    FROM APPBUS.DFT_INTL_STORE_DAY_VW f      
        INNER JOIN APPDWH.DDM_RETAIL_ORG_STORE_DIST org
            ON f.STORE_NUMBER = org.STORE_NUM
            AND org.DIV_ORG_LVL_ID IN (3,6,106)  -- US CO stores
        INNER JOIN APPDWH.ADT_CAL ca
            ON f.BUSINESS_DATE = ca.CAL_DT
            AND ca.FSCL_YR_NUM = 2018
            AND ca.FSCL_QTR_IN_YR_NUM = 1
            --AND f.BUSINESS_DATE BETWEEN '01-SEP-17' AND '30-NOV-17'
            --AND ca.FSCL_WK_IN_YR_NUM BETWEEN 41 AND 52
        INNER JOIN APPBUS.AFT_STORE_COMP_PER_VW comp
            ON f.STORE_NUMBER = comp.STORE_NUMBER
            AND ca.FSCL_PER_BEG_DT = comp.FISCAL_PERIOD_BEGIN_DATE
            AND COMP_CODE = 'Y'
    GROUP BY f.STORE_NUMBER, ca.FSCL_YR_NUM, ca.FSCL_QTR_IN_YR_NUM
 ;



/* recreating Factbook Slide #20 */
/* CC & Store Ops by hour */

SELECT
  TO_CHAR(ce.TRANS_DTM, 'HH24') AS TRANS_HR

  -- Compute top box scores for each question
  ,CASE WHEN COUNT(CASE WHEN ce.QSTN_ID = 'Q2_1' THEN ce.QSTN_ID END) = 0 THEN NULL
   ELSE TO_CHAR(SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_1' THEN 1 ELSE 0 END)
    / COUNT(CASE WHEN ce.QSTN_ID = 'Q2_1' THEN ce.QSTN_ID END),'0.0000') END
   AS Q2_1_TB_Score
  ,CASE WHEN COUNT(CASE WHEN ce.QSTN_ID = 'Q2_2' THEN ce.QSTN_ID END) = 0 THEN NULL
   ELSE TO_CHAR(SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_2' THEN 1 ELSE 0 END)
    / COUNT(CASE WHEN ce.QSTN_ID = 'Q2_2' THEN ce.QSTN_ID END),'0.0000') END
   AS CC_TB_Score
  ,CASE WHEN COUNT(CASE WHEN ce.QSTN_ID = 'Q2_3' THEN ce.QSTN_ID END) = 0 THEN NULL
   ELSE TO_CHAR(SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_3' THEN 1 ELSE 0 END)
    / COUNT(CASE WHEN ce.QSTN_ID = 'Q2_3' THEN ce.QSTN_ID END),'0.0000') END
   AS Q2_3_TB_Score
  ,CASE WHEN COUNT(CASE WHEN ce.QSTN_ID = 'Q2_4' THEN ce.QSTN_ID END) = 0 THEN NULL
   ELSE TO_CHAR(SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_4' THEN 1 ELSE 0 END)
    / COUNT(CASE WHEN ce.QSTN_ID = 'Q2_4' THEN ce.QSTN_ID END),'0.0000') END
   AS Q2_4_TB_Score
  ,CASE WHEN COUNT(CASE WHEN ce.QSTN_ID = 'Q2_5' THEN ce.QSTN_ID END) = 0 THEN NULL
   ELSE TO_CHAR(SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_5' THEN 1 ELSE 0 END)
    / COUNT(CASE WHEN ce.QSTN_ID = 'Q2_5' THEN ce.QSTN_ID END),'0.0000') END
   AS Q2_5_TB_Score
  ,CASE WHEN COUNT(CASE WHEN ce.QSTN_ID = 'Q2_6' THEN ce.QSTN_ID END) = 0 THEN NULL
   ELSE TO_CHAR(SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_6' THEN 1 ELSE 0 END)
    / COUNT(CASE WHEN ce.QSTN_ID = 'Q2_6' THEN ce.QSTN_ID END),'0.0000') END
   AS Q2_6_TB_Score
  ,CASE WHEN COUNT(CASE WHEN ce.QSTN_ID = 'Q2_7' THEN ce.QSTN_ID END) = 0 THEN NULL
   ELSE TO_CHAR(SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_7' THEN 1 ELSE 0 END)
    / COUNT(CASE WHEN ce.QSTN_ID = 'Q2_7' THEN ce.QSTN_ID END),'0.0000') END
   AS Q2_7_TB_Score
 
FROM APPDWH.AFT_CV_SRVY_RSPNS ce

  INNER JOIN APPDWH.ADT_CAL c
    ON TRUNC(ce.TRANS_DTM) = c.CAL_DT
      AND c.FSCL_YR_NUM = 2017
      AND c.FSCL_QTR_IN_YR_NUM = 4
      
  INNER JOIN APPDWH.DDM_RETAIL_ORG_STORE_DIST org
    ON ce.STORE_NUM = org.STORE_NUM
      AND org.DIV_ORG_LVL_ID IN (3,6,106)  -- U.S. company stores only (including reserve bar), but excluding New Concepts and Roastery

WHERE ce.RSPNS_ID <> '9'  -- rspns_id = 9 for unanswered questions
  AND ce.QSTN_ID NOT IN ('Q1','Q11') -- these questions are not in Customer Connection or Store Operations scores
  
GROUP BY
  TO_CHAR(ce.TRANS_DTM, 'HH24') 
;


