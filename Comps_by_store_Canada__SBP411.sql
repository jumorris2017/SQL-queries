/* recreating Factbook Slide #11 */

/* part 1 -- CC by store, month, and drive-thru status */
/* export as "Comps_by_store_Canada_pt1.csv" */

SELECT
      ce.STORE_NUM
      ,org.DIV_ORG_LVL_ID
      --,c.FSCL_PER_IN_YR_NUM
      ,c.FSCL_YR_NUM
      ,d.DRIVE_THRU_IND
      ,c.FSCL_QTR_IN_YR_NUM
      ,c.FSCL_WK_IN_YR_NUM

  -- Total valid response counts by question
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_2' THEN ce.QSTN_ID END) AS Q2_2_Response_Total

  -- Total top box responses
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_2' THEN 1 ELSE 0 END) AS Q2_2_TB_Cnt

FROM APPDWH.AFT_CV_SRVY_RSPNS ce

  INNER JOIN APPDWH.ADT_CAL c
    ON TRUNC(ce.TRANS_DTM) = c.CAL_DT
      AND c.FSCL_YR_NUM > 2016
      
  INNER JOIN APPDWH.ADT_STORE d
    ON ce.STORE_NUM = d.STORE_NUM
    
  INNER JOIN APPDWH.DDM_RETAIL_ORG_STORE_DIST org
    ON ce.STORE_NUM = org.STORE_NUM
    AND org.DIV_ORG_LVL_ID IN (2)  -- Canada stores

WHERE ce.RSPNS_ID <> '9'  -- rspns_id = 9 for unanswered questions
  AND ce.QSTN_ID NOT IN ('Q1','Q11') -- these questions are not in Customer Connection or Store Operations scores

GROUP BY
    ce.STORE_NUM
    ,org.DIV_ORG_LVL_ID
    --,c.FSCL_PER_IN_YR_NUM
    ,c.FSCL_YR_NUM
    ,d.DRIVE_THRU_IND
    ,c.FSCL_QTR_IN_YR_NUM
    ,c.FSCL_WK_IN_YR_NUM
;

/* part 2 -- monthly comps for Canada */
/* export as "Comps_by_store_Canada_pt2.csv" */
SELECT f.FISCAL_YEAR_NUMBER
        , f.STORE_NUMBER
        , SUM(f.SALES_AMT) "MonthlySales"
        , SUM(f.TRANS_CNT) "MonthlyTrans"
        , SUM(f.SALES_LY_AMT) "LYMonthlySales"
        , SUM(f.TRANS_LY_CNT) "LYMonthlyTrans"
        , comp.COMP_CODE
        , org.DIV_ORG_LVL_ID
        , f.FISCAL_WEEK_NUMBER
    FROM APPBUS.USR_CO_STORE_WEEK_HOUR_RPT f      
        INNER JOIN APPDWH.DDM_RETAIL_ORG_STORE_DIST org
            ON f.STORE_NUMBER = org.STORE_NUM
            AND org.DIV_ORG_LVL_ID IN (2)  -- Canada stores
        INNER JOIN APPBUS.AFT_STORE_COMP_PER_VW comp
            ON f.STORE_NUMBER = comp.STORE_NUMBER
            AND COMP_CODE = 'Y'
    WHERE f.FISCAL_YEAR_NUMBER > 2016
    GROUP BY f.STORE_NUMBER, f.FISCAL_YEAR_NUMBER, comp.COMP_CODE, org.DIV_ORG_LVL_ID, f.FISCAL_WEEK_NUMBER
 ;


/* recreating Factbook Slide #18 */

/* part 1  -- Canada stores, CC & Store Ops by day of week */

SELECT
  c.DAY_ABBR_NM

  -- Total valid response counts by question
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_1' THEN ce.QSTN_ID END) AS Q2_1_Response_Total
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_2' THEN ce.QSTN_ID END) AS Q2_2_Response_Total
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_3' THEN ce.QSTN_ID END) AS Q2_3_Response_Total
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_4' THEN ce.QSTN_ID END) AS Q2_4_Response_Total
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_5' THEN ce.QSTN_ID END) AS Q2_5_Response_Total
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_6' THEN ce.QSTN_ID END) AS Q2_6_Response_Total
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_7' THEN ce.QSTN_ID END) AS Q2_7_Response_Total
  
  -- Total top box responses
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_1' THEN 1 ELSE 0 END) AS Q2_1_TB_Cnt
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_2' THEN 1 ELSE 0 END) AS Q2_2_TB_Cnt
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_3' THEN 1 ELSE 0 END) AS Q2_3_TB_Cnt
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_4' THEN 1 ELSE 0 END) AS Q2_4_TB_Cnt
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_5' THEN 1 ELSE 0 END) AS Q2_5_TB_Cnt
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_6' THEN 1 ELSE 0 END) AS Q2_6_TB_Cnt
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_7' THEN 1 ELSE 0 END) AS Q2_7_TB_Cnt
  
  -- Compute top box scores for each question
  ,CASE WHEN COUNT(CASE WHEN ce.QSTN_ID = 'Q2_2' THEN ce.QSTN_ID END) = 0 THEN NULL
   ELSE TO_CHAR(SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_2' THEN 1 ELSE 0 END)
    / COUNT(CASE WHEN ce.QSTN_ID = 'Q2_2' THEN ce.QSTN_ID END),'0.0000') END
   AS Q2_2_TB_Score
  
FROM APPDWH.AFT_CV_SRVY_RSPNS ce

  INNER JOIN APPDWH.ADT_CAL c
    ON TRUNC(ce.TRANS_DTM) = c.CAL_DT
      AND c.FSCL_YR_NUM = 2017
      AND c.FSCL_QTR_IN_YR_NUM = 4
      
  INNER JOIN APPDWH.DDM_RETAIL_ORG_STORE_DIST org
    ON ce.STORE_NUM = org.STORE_NUM
      AND org.DIV_ORG_LVL_ID IN (2)  -- U.S. company stores only (including reserve bar), but excluding New Concepts and Roastery

WHERE ce.RSPNS_ID <> '9'  -- rspns_id = 9 for unanswered questions
  AND ce.QSTN_ID NOT IN ('Q1','Q11') -- these questions are not in Customer Connection or Store Operations scores
  
GROUP BY
  c.DAY_ABBR_NM
;


/* part 2  -- CAW -- Canada stores, total quantity per order & beverage quantity per order, by store number & month */

SELECT
  r.FSCL_YR_NUM
  ,cal.FSCL_QTR_IN_YR_NUM
  ,cal.DAY_ABBR_NM
  ,ROUND(SUM(CASE WHEN r.IS_MOD = 0 THEN r.NDS_QTY END) / COUNT(DISTINCT r.TRANS_ID),2) AVG_TRANS_PER_ORDER
  ,ROUND(SUM(CASE WHEN r.IS_MOD = 0 AND r.IS_BEV = 1 THEN r.NDS_QTY END) / COUNT(DISTINCT r.TRANS_ID),2) AVG_BEV_PER_ORDER

FROM APPCA.F_MBR_POS_LINE_ITEM r

INNER JOIN D_CAL cal
  ON r.BUS_DT = cal.CAL_DT
    AND cal.FSCL_YR_NUM = 2017
    AND cal.FSCL_QTR_IN_YR_NUM = 4

INNER JOIN APPCA.D_STORE_VERS st
  ON st.STORE_NUM = r.STORE_NUM
    AND st.CURRENT_FLG = 'Y'
    AND st.CNTRY_CD_2_DGT_ISO = 'CA'

GROUP BY
  r.FSCL_YR_NUM
  ,cal.FSCL_QTR_IN_YR_NUM
  ,cal.DAY_ABBR_NM
;



/* recreating Factbook Slide #5 */

/* Canada stores, CC & Store Ops by day of week */

SELECT
  TO_CHAR(ce.TRANS_DTM, 'HH24') AS TRANS_HR

  -- Total valid response counts by question
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_1' THEN ce.QSTN_ID END) AS Q2_1_Response_Total
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_2' THEN ce.QSTN_ID END) AS Q2_2_Response_Total
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_3' THEN ce.QSTN_ID END) AS Q2_3_Response_Total
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_4' THEN ce.QSTN_ID END) AS Q2_4_Response_Total
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_5' THEN ce.QSTN_ID END) AS Q2_5_Response_Total
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_6' THEN ce.QSTN_ID END) AS Q2_6_Response_Total
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_7' THEN ce.QSTN_ID END) AS Q2_7_Response_Total
  
  -- Total top box responses
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_1' THEN 1 ELSE 0 END) AS Q2_1_TB_Cnt
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_2' THEN 1 ELSE 0 END) AS Q2_2_TB_Cnt
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_3' THEN 1 ELSE 0 END) AS Q2_3_TB_Cnt
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_4' THEN 1 ELSE 0 END) AS Q2_4_TB_Cnt
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_5' THEN 1 ELSE 0 END) AS Q2_5_TB_Cnt
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_6' THEN 1 ELSE 0 END) AS Q2_6_TB_Cnt
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_7' THEN 1 ELSE 0 END) AS Q2_7_TB_Cnt
  
  -- Compute top box scores for each question
  ,CASE WHEN COUNT(CASE WHEN ce.QSTN_ID = 'Q2_2' THEN ce.QSTN_ID END) = 0 THEN NULL
   ELSE TO_CHAR(SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_2' THEN 1 ELSE 0 END)
    / COUNT(CASE WHEN ce.QSTN_ID = 'Q2_2' THEN ce.QSTN_ID END),'0.0000') END
   AS Q2_2_TB_Score
  
FROM APPDWH.AFT_CV_SRVY_RSPNS ce

  INNER JOIN APPDWH.ADT_CAL c
    ON TRUNC(ce.TRANS_DTM) = c.CAL_DT
      AND c.FSCL_YR_NUM = 2017
      AND c.FSCL_QTR_IN_YR_NUM = 4
      
  INNER JOIN APPDWH.DDM_RETAIL_ORG_STORE_DIST org
    ON ce.STORE_NUM = org.STORE_NUM
      AND org.DIV_ORG_LVL_ID IN (2)  -- U.S. company stores only (including reserve bar), but excluding New Concepts and Roastery

WHERE ce.RSPNS_ID <> '9'  -- rspns_id = 9 for unanswered questions
  AND ce.QSTN_ID NOT IN ('Q1','Q11') -- these questions are not in Customer Connection or Store Operations scores
  
GROUP BY
  TO_CHAR(ce.TRANS_DTM, 'HH24') 
;



/* recreating Factbook Slide #20 */

/* Canada stores, CC & Store Ops by store number */

SELECT
  ce.STORE_NUM

  -- Total valid response counts by question
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_1' THEN ce.QSTN_ID END) AS Q2_1_Response_Total
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_2' THEN ce.QSTN_ID END) AS Q2_2_Response_Total
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_3' THEN ce.QSTN_ID END) AS Q2_3_Response_Total
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_4' THEN ce.QSTN_ID END) AS Q2_4_Response_Total
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_5' THEN ce.QSTN_ID END) AS Q2_5_Response_Total
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_6' THEN ce.QSTN_ID END) AS Q2_6_Response_Total
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_7' THEN ce.QSTN_ID END) AS Q2_7_Response_Total
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_7' THEN ce.QSTN_ID END) AS Q2_8_Response_Total
  
  -- Total top box responses
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_1' THEN 1 ELSE 0 END) AS Q2_1_TB_Cnt
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_2' THEN 1 ELSE 0 END) AS Q2_2_TB_Cnt
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_3' THEN 1 ELSE 0 END) AS Q2_3_TB_Cnt
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_4' THEN 1 ELSE 0 END) AS Q2_4_TB_Cnt
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_5' THEN 1 ELSE 0 END) AS Q2_5_TB_Cnt
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_6' THEN 1 ELSE 0 END) AS Q2_6_TB_Cnt
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_7' THEN 1 ELSE 0 END) AS Q2_7_TB_Cnt
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_7' THEN 1 ELSE 0 END) AS Q2_8_TB_Cnt
  
  -- Compute top box scores for each question
  ,CASE WHEN COUNT(CASE WHEN ce.QSTN_ID = 'Q2_2' THEN ce.QSTN_ID END) = 0 THEN NULL
   ELSE TO_CHAR(SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_2' THEN 1 ELSE 0 END)
    / COUNT(CASE WHEN ce.QSTN_ID = 'Q2_2' THEN ce.QSTN_ID END),'0.0000') END
   AS Q2_2_TB_Score
    ,CASE WHEN COUNT(CASE WHEN ce.QSTN_ID = 'Q2_8' THEN ce.QSTN_ID END) = 0 THEN NULL
   ELSE TO_CHAR(SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_8' THEN 1 ELSE 0 END)
    / COUNT(CASE WHEN ce.QSTN_ID = 'Q2_8' THEN ce.QSTN_ID END),'0.0000') END
   AS Q2_8_TB_Score
  
FROM APPDWH.AFT_CV_SRVY_RSPNS ce

  INNER JOIN APPDWH.ADT_CAL c
    ON TRUNC(ce.TRANS_DTM) = c.CAL_DT
      AND c.FSCL_YR_NUM = 2017
      AND c.FSCL_QTR_IN_YR_NUM = 4
      
  INNER JOIN APPDWH.DDM_RETAIL_ORG_STORE_DIST org
    ON ce.STORE_NUM = org.STORE_NUM
      AND org.DIV_ORG_LVL_ID IN (2)  -- U.S. company stores only (including reserve bar), but excluding New Concepts and Roastery

WHERE ce.RSPNS_ID <> '9'  -- rspns_id = 9 for unanswered questions
  AND ce.QSTN_ID NOT IN ('Q1','Q11') -- these questions are not in Customer Connection or Store Operations scores
  
GROUP BY
  ce.STORE_NUM
;



/* recreating Factbook Slide #5 */

/* Canada stores, CC & Store Ops, Q1 & WP by store number */

SELECT
  ce.STORE_NUM

  -- Total valid response counts by question
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q1' THEN ce.QSTN_ID END) AS Q1_Response_Total
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_1' THEN ce.QSTN_ID END) AS Q2_1_Response_Total
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_2' THEN ce.QSTN_ID END) AS Q2_2_Response_Total
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_3' THEN ce.QSTN_ID END) AS Q2_3_Response_Total
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_4' THEN ce.QSTN_ID END) AS Q2_4_Response_Total
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_5' THEN ce.QSTN_ID END) AS Q2_5_Response_Total
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_6' THEN ce.QSTN_ID END) AS Q2_6_Response_Total
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_7' THEN ce.QSTN_ID END) AS Q2_7_Response_Total
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_7' THEN ce.QSTN_ID END) AS Q2_8_Response_Total
  
  -- Total top box responses
  ,SUM(CASE WHEN ce.RSPNS_ID = '5' AND ce.QSTN_ID = 'Q1' THEN 1 ELSE 0 END) AS Q1_TDTMRW_Cnt
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_1' THEN 1 ELSE 0 END) AS Q2_1_TB_Cnt
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_2' THEN 1 ELSE 0 END) AS Q2_2_TB_Cnt
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_3' THEN 1 ELSE 0 END) AS Q2_3_TB_Cnt
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_4' THEN 1 ELSE 0 END) AS Q2_4_TB_Cnt
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_5' THEN 1 ELSE 0 END) AS Q2_5_TB_Cnt
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_6' THEN 1 ELSE 0 END) AS Q2_6_TB_Cnt
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_7' THEN 1 ELSE 0 END) AS Q2_7_TB_Cnt
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_7' THEN 1 ELSE 0 END) AS Q2_8_TB_Cnt
  
FROM APPDWH.AFT_CV_SRVY_RSPNS ce

  INNER JOIN APPDWH.ADT_CAL c
    ON TRUNC(ce.TRANS_DTM) = c.CAL_DT
      AND c.FSCL_YR_NUM = 2017
      AND c.FSCL_QTR_IN_YR_NUM = 4
      
  INNER JOIN APPDWH.DDM_RETAIL_ORG_STORE_DIST org
    ON ce.STORE_NUM = org.STORE_NUM
      AND org.DIV_ORG_LVL_ID IN (2)  -- U.S. company stores only (including reserve bar), but excluding New Concepts and Roastery

WHERE ce.RSPNS_ID <> '9'  -- rspns_id = 9 for unanswered questions

GROUP BY
  ce.STORE_NUM
;


/* recreating Factbook Slide #8 */

/* TAKE 1! EDW -- Canada stores, CC & Store Ops by calendar month */

SELECT
  c.CAL_YR_NUM
  ,c.CAL_MNTH_IN_YR_NUM
  --,c.FSCL_PER_IN_YR_NUM

    -- Compute top box scores for each question
  ,CASE WHEN COUNT(CASE WHEN ce.QSTN_ID = 'Q2_2' THEN ce.QSTN_ID END) = 0 THEN NULL
   ELSE TO_CHAR(SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_2' THEN 1 ELSE 0 END)
    / COUNT(CASE WHEN ce.QSTN_ID = 'Q2_2' THEN ce.QSTN_ID END),'0.0000') END
   AS CC_TB_Score
  ,CASE WHEN COUNT(CASE WHEN (ce.QSTN_ID = 'Q2_1' OR ce.QSTN_ID = 'Q2_3' OR ce.QSTN_ID = 'Q2_4' OR ce.QSTN_ID = 'Q2_5' OR ce.QSTN_ID = 'Q2_6' OR ce.QSTN_ID = 'Q2_7') THEN ce.QSTN_ID END) = 0 THEN NULL
   ELSE TO_CHAR(SUM(CASE WHEN ce.RSPNS_ID = '7' AND (ce.QSTN_ID = 'Q2_1' OR ce.QSTN_ID = 'Q2_3' OR ce.QSTN_ID = 'Q2_4' OR ce.QSTN_ID = 'Q2_5' OR ce.QSTN_ID = 'Q2_6' OR ce.QSTN_ID = 'Q2_7') THEN 1 ELSE 0 END)
    / COUNT(CASE WHEN (ce.QSTN_ID = 'Q2_1' OR ce.QSTN_ID = 'Q2_3' OR ce.QSTN_ID = 'Q2_4' OR ce.QSTN_ID = 'Q2_5' OR ce.QSTN_ID = 'Q2_6' OR ce.QSTN_ID = 'Q2_7') THEN ce.QSTN_ID END),'0.0000') END 
    AS SO_TB_Score
  
FROM APPDWH.AFT_CV_SRVY_RSPNS ce

  INNER JOIN APPDWH.ADT_CAL c
    ON TRUNC(ce.TRANS_DTM) = c.CAL_DT
      AND c.CAL_YR_NUM > 2014
      --AND c.CAL_MNTH_IN_YR_NUM = 4
      
  INNER JOIN APPDWH.DDM_RETAIL_ORG_STORE_DIST org
    ON ce.STORE_NUM = org.STORE_NUM
      AND org.DIV_ORG_LVL_ID IN (2)  -- Canada

WHERE ce.RSPNS_ID <> '9'  -- rspns_id = 9 for unanswered questions
  AND ce.QSTN_ID NOT IN ('Q1','Q11') -- these questions are not in Customer Connection or Store Operations scores
  
GROUP BY
  c.CAL_YR_NUM
  ,c.CAL_MNTH_IN_YR_NUM
  --,c.FSCL_PER_IN_YR_NUM
ORDER BY
  c.CAL_YR_NUM
  ,c.CAL_MNTH_IN_YR_NUM
  --,c.FSCL_PER_IN_YR_NUM
;

/* TAKE 2! CAW! EDW WAS BEING SLOW! */

SELECT
  ca.CAL_YR_NUM
  ,ca.CAL_MNTH_IN_YR_NUM
  ,sr.QSTN_ID
  
  ,SUM(CASE
    WHEN RSPNS_ID = '5' AND QSTN_ID = 'Q1' THEN COALESCE(w.WEIGHT_RT,1) 
    WHEN RSPNS_ID = '7' THEN COALESCE(w.WEIGHT_RT,1)
    ELSE 0
   END) AS TOTAL_TB
  ,SUM(COALESCE(w.WEIGHT_RT,1)) AS TOTAL_RSPNS

FROM APPOTHER.AFT_CV_SRVY_RSPNS sr

LEFT JOIN APPOTHER.CUST_INS_WEIGHTS w
  ON sr.CASE_ID = w.CASE_ID
  
INNER JOIN APPCA.D_CAL ca
  ON TRUNC(sr.TRANS_DTM) = ca.CAL_DT
  
INNER JOIN APPCA.D_STORE_VERS st
  ON st.STORE_NUM = sr.STORE_NUM
    AND st.CURRENT_FLG = 'Y'
  
WHERE sr.TRANS_DTM >= '01-JAN-15'  -- offical launch date of CO store program
  AND sr.QSTN_ID IN ('Q2_1','Q2_2','Q2_3','Q2_4','Q2_5','Q2_6','Q2_7')
  AND sr.RSPNS_ID <> '9'
  AND st.CNTRY_CD_2_DGT_ISO = 'CA'
  AND st.OWNR_TYPE_CD = 'CO'
  --AND sr.STORE_NUM = 101  -- for testing

GROUP BY
  ca.CAL_YR_NUM
  ,ca.CAL_MNTH_IN_YR_NUM
  ,sr.QSTN_ID
;

SELECT * FROM APPDWH
;