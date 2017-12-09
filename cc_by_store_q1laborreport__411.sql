SELECT
      ce.STORE_NUM
    ,c.FSCL_WK_IN_YR_NUM
    ,c.CAL_WK_IN_YR_NUM
    
  -- Total valid response counts by question
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_2' THEN ce.QSTN_ID END) AS cctotalresp

  -- Total top box responses
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_2' THEN 1 ELSE 0 END) AS cctbcount
    /*  
  -- Compute top box scores for each question
  ,CASE WHEN COUNT(CASE WHEN ce.QSTN_ID = 'Q2_2' THEN ce.QSTN_ID END) = 0 THEN NULL
   ELSE TO_CHAR(SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_2' THEN 1 ELSE 0 END)
    / COUNT(CASE WHEN ce.QSTN_ID = 'Q2_2' THEN ce.QSTN_ID END),'0.0000') END AS cctbscore
    */
FROM APPDWH.AFT_CV_SRVY_RSPNS ce

  INNER JOIN APPDWH.ADT_CAL c
    ON TRUNC(ce.TRANS_DTM) = c.CAL_DT
      --AND TRUNC(ce.TRANS_DTM) BETWEEN '01-SEP-17' AND '31-OCT-17'
    AND c.FSCL_YR_NUM = 2018
    AND c.FSCL_QTR_IN_YR_NUM = 1
      
  INNER JOIN APPDWH.DDM_RETAIL_ORG_STORE_DIST org
    ON ce.STORE_NUM = org.STORE_NUM
      AND org.DIV_ORG_LVL_ID IN (3,6,106)  -- U.S. company stores only (including reserve bar), but excluding New Concepts and Roastery

WHERE ce.RSPNS_ID <> '9'  -- rspns_id = 9 for unanswered questions
  AND ce.QSTN_ID NOT IN ('Q1','Q11') -- these questions are not in Customer Connection or Store Operations scores

GROUP BY
    ce.STORE_NUM
    ,c.FSCL_WK_IN_YR_NUM
    ,c.CAL_WK_IN_YR_NUM
;


/* part 2 -- weekly comps */
 SELECT --f.FISCAL_YEAR_NUMBER
         f.STORE_NUMBER
        , SUM(f.NET_DISCOUNTED_SALES_AMT) "MonthlySales"
        , SUM(f.NET_DISCOUNTED_SALES_LY_AMT) "LYMonthlySales"
        --, ROUND((SUM(f.NET_DISCOUNTED_SALES_AMT) - SUM(f.NET_DISCOUNTED_SALES_LY_AMT)) / SUM(f.NET_DISCOUNTED_SALES_LY_AMT),4) AS salescomp
        , ca.FSCL_WK_IN_YR_NUM
    FROM APPBUS.DFT_INTL_STORE_DAY_VW f      
        INNER JOIN APPDWH.DDM_RETAIL_ORG_STORE_DIST org
            ON f.STORE_NUMBER = org.STORE_NUM
            AND org.DIV_ORG_LVL_ID IN (3,6,106)  -- US CO stores
        INNER JOIN APPDWH.ADT_CAL ca
            ON f.BUSINESS_DATE = ca.CAL_DT
            AND ca.FSCL_YR_NUM = 2018
            AND ca.FSCL_WK_IN_YR_NUM IN (1,2,3,8)
        INNER JOIN APPBUS.AFT_STORE_COMP_PER_VW comp
            ON f.STORE_NUMBER = comp.STORE_NUMBER
            AND COMP_CODE = 'Y'
    GROUP BY f.STORE_NUMBER, ca.FSCL_WK_IN_YR_NUM
 ;


