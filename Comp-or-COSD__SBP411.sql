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
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q1' THEN ce.QSTN_ID END) AS Q1_Response_Total
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_1' THEN ce.QSTN_ID END) AS Q2_1_Response_Total
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_2' THEN ce.QSTN_ID END) AS Q2_2_Response_Total
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_3' THEN ce.QSTN_ID END) AS Q2_3_Response_Total
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_4' THEN ce.QSTN_ID END) AS Q2_4_Response_Total
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_5' THEN ce.QSTN_ID END) AS Q2_5_Response_Total
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_6' THEN ce.QSTN_ID END) AS Q2_6_Response_Total
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_7' THEN ce.QSTN_ID END) AS Q2_7_Response_Total
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_8' THEN ce.QSTN_ID END) AS Q2_8_Response_Total
  
  -- Total top box responses
  ,SUM(CASE WHEN ce.RSPNS_ID = '5' AND ce.QSTN_ID = 'Q1' THEN 1 ELSE 0 END) AS Q1_TB_Cnt
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_1' THEN 1 ELSE 0 END) AS Q2_1_TB_Cnt
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_2' THEN 1 ELSE 0 END) AS Q2_2_TB_Cnt
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_3' THEN 1 ELSE 0 END) AS Q2_3_TB_Cnt
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_4' THEN 1 ELSE 0 END) AS Q2_4_TB_Cnt
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_5' THEN 1 ELSE 0 END) AS Q2_5_TB_Cnt
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_6' THEN 1 ELSE 0 END) AS Q2_6_TB_Cnt
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_7' THEN 1 ELSE 0 END) AS Q2_7_TB_Cnt
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_8' THEN 1 ELSE 0 END) AS Q2_8_TB_Cnt
  
    -- Compute top box scores for each question
  ,CASE WHEN COUNT(CASE WHEN ce.QSTN_ID = 'Q1' THEN ce.QSTN_ID END) = 0 THEN NULL
   ELSE TO_CHAR(SUM(CASE WHEN ce.RSPNS_ID = '5' AND ce.QSTN_ID = 'Q1' THEN 1 ELSE 0 END)
    / COUNT(CASE WHEN ce.QSTN_ID = 'Q1' THEN ce.QSTN_ID END),'0.0000') END
   AS Q1_TB_Score
  ,CASE WHEN COUNT(CASE WHEN ce.QSTN_ID = 'Q2_1' THEN ce.QSTN_ID END) = 0 THEN NULL
   ELSE TO_CHAR(SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_1' THEN 1 ELSE 0 END)
    / COUNT(CASE WHEN ce.QSTN_ID = 'Q2_1' THEN ce.QSTN_ID END),'0.0000') END
   AS Q2_1_TB_Score
  ,CASE WHEN COUNT(CASE WHEN ce.QSTN_ID = 'Q2_2' THEN ce.QSTN_ID END) = 0 THEN NULL
   ELSE TO_CHAR(SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_2' THEN 1 ELSE 0 END)
    / COUNT(CASE WHEN ce.QSTN_ID = 'Q2_2' THEN ce.QSTN_ID END),'0.0000') END
   AS Q2_2_TB_Score
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
  ,CASE WHEN COUNT(CASE WHEN ce.QSTN_ID = 'Q2_8' THEN ce.QSTN_ID END) = 0 THEN NULL
   ELSE TO_CHAR(SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_8' THEN 1 ELSE 0 END)
    / COUNT(CASE WHEN ce.QSTN_ID = 'Q2_8' THEN ce.QSTN_ID END),'0.0000') END
   AS Q2_8_TB_Score

FROM APPDWH.AFT_CV_SRVY_RSPNS ce

  INNER JOIN APPDWH.ADT_CAL c
    ON TRUNC(ce.TRANS_DTM) = c.CAL_DT
      AND c.FSCL_YR_NUM = 2017
      AND c.FSCL_QTR_IN_YR_NUM = 4
      --AND TRUNC(ce.TRANS_DTM) BETWEEN '01-SEP-17' AND '30-NOV-17'
      
  INNER JOIN APPDWH.ADT_STORE d
    ON ce.STORE_NUM = d.STORE_NUM
    
  INNER JOIN APPDWH.DDM_RETAIL_ORG_STORE_DIST org
    ON ce.STORE_NUM = org.STORE_NUM
    AND org.DIV_ORG_LVL_ID IN (3,6,106)  -- U.S. stores

WHERE ce.RSPNS_ID <> '9'  -- rspns_id = 9 for unanswered questions
  --AND ce.QSTN_ID NOT IN ('Q1','Q11') -- these questions are not in Customer Connection or Store Operations scores

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
        --, SUM(f.NET_DISCOUNTED_SALES_AMT) "QuarterlySales"
        --, SUM(f.NET_DISCOUNTED_SALES_LY_AMT) "LYQuarterlySales"
        --, ROUND((SUM(f.NET_DISCOUNTED_SALES_AMT) - SUM(f.NET_DISCOUNTED_SALES_LY_AMT)) / SUM(f.NET_DISCOUNTED_SALES_LY_AMT),4) AS salescomp
        , SUM(f.CUST_TRANS_CNT) "CustTrans"
        , SUM(f.ACTIVE_STORE_DAY_CNT) "day_count"
        --, ca.FSCL_WK_IN_YR_NUM
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



/*customer transaction count by hour (and daypart indicator)*/
/*export as "cc-so_bystore-daypart_FY18Q1-TSDhourly.csv"*/ -- for comp
/*export as "cc-so_bystore-daypart_FY17Q1-TSDhourly.csv"*/ -- for comp
/*export as "cc-so_bystore-daypart_FY17Q1-FY18Q1-TSDhourly.csv"*/ -- for COSD trend
/* for Brooke/Kelly 1/8/17 */
/* use with "CE_bydaypart_byquarter.R */
SELECT ca.FSCL_YR_NUM
  ,ca.FSCL_QTR_IN_YR_NUM
  ,f.SALE_HOUR 
  ,f.STORE_NUMBER
  ,SUM(f.CUTOMER_TRANSACTION_COUNT) AS "TRANS"
      ,(CASE WHEN f.SALE_HOUR  < 110000 THEN 1 -- am
        WHEN f.SALE_HOUR  >=110000 AND f.SALE_HOUR  < 140000 THEN 2 -- midday
        WHEN f.SALE_HOUR  >=140000 AND f.SALE_HOUR  < 160000 THEN 3 -- pm
        WHEN f.SALE_HOUR  >=160000 THEN 4 -- evening
        ELSE 0 
        END) AS DAY_PART
FROM APPBUS.AFT_POS_INTL_HDR_VW f
    INNER JOIN APPDWH.ADT_CAL ca
        ON f.BUSINESS_DATE = ca.CAL_DT
  AND ((ca.FSCL_YR_NUM = 2018 AND ca.FSCL_QTR_IN_YR_NUM = 1) OR ca.FSCL_YR_NUM = 2017)
 AND f.COUNTRY_CODE = 'US'
GROUP BY 
  ca.FSCL_YR_NUM
  ,ca.FSCL_QTR_IN_YR_NUM
  ,f.SALE_HOUR
  ,f.STORE_NUMBER


/*active store day count*/
/*export as "cc-so_bystore-daypart_FY18Q1-TSDdaycount.csv"*/ -- for comp
/*export as "cc-so_bystore-daypart_FY17Q1-TSDdaycount.csv"*/ -- for comp
/*export as "cc-so_bystore-daypart_FY17Q1-FY18Q1-TSDdaycount.csv"*/ -- for COSD trend
/* for Brooke/Kelly 1/8/17 */
/* use with "CE_bydaypart_byquarter.R */
SELECT ca.FSCL_YR_NUM
  ,ca.FSCL_QTR_IN_YR_NUM
  ,t.STORE_NUMBER
  ,COUNT(t.ACTIVE_STORE_DAY_CNT) "day_count"
FROM APPBUS.DFT_INTL_STORE_DAY_VW t  
    INNER JOIN APPDWH.ADT_CAL ca
        ON t.BUSINESS_DATE = ca.CAL_DT
  AND ((ca.FSCL_YR_NUM = 2018 AND ca.FSCL_QTR_IN_YR_NUM = 1) OR ca.FSCL_YR_NUM = 2017)
 AND t.COUNTRY = 'US'
GROUP BY 
  ca.FSCL_YR_NUM
  ,ca.FSCL_QTR_IN_YR_NUM
  ,t.STORE_NUMBER
