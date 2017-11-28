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

/* recreating Factbook Slide #20 */

/* Canada stores, CC & Store Ops by hour */

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
      AND org.DIV_ORG_LVL_ID IN (2)  -- U.S. company stores only (including reserve bar), but excluding New Concepts and Roastery

WHERE ce.RSPNS_ID <> '9'  -- rspns_id = 9 for unanswered questions
  AND ce.QSTN_ID NOT IN ('Q1','Q11') -- these questions are not in Customer Connection or Store Operations scores
  
GROUP BY
  TO_CHAR(ce.TRANS_DTM, 'HH24') 
;



/* recreating Factbook Slide #5 */

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

/* Canada stores, CC & Store Ops, Q1 & WP by store performance */

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

/* TAKE 1! EDW -- Canada stores, CC & Store Ops by month */

SELECT
  c.FSCL_YR_NUM
  ,c.FSCL_PER_IN_YR_NUM

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
  
FROM APPDWH.AFT_CV_SRVY_RSPNS ce

  INNER JOIN APPDWH.ADT_CAL c
    ON TRUNC(ce.TRANS_DTM) = c.CAL_DT
      AND c.FSCL_YR_NUM > 2014
      
  INNER JOIN APPDWH.DDM_RETAIL_ORG_STORE_DIST org
    ON ce.STORE_NUM = org.STORE_NUM
      AND org.DIV_ORG_LVL_ID IN (2)  -- Canada

WHERE ce.RSPNS_ID <> '9'  -- rspns_id = 9 for unanswered questions
  AND ce.QSTN_ID NOT IN ('Q1','Q11') -- these questions are not in Customer Connection or Store Operations scores
  
GROUP BY
  c.FSCL_YR_NUM
  ,c.FSCL_PER_IN_YR_NUM
ORDER BY
  c.FSCL_YR_NUM
  ,c.FSCL_PER_IN_YR_NUM
;

/* TAKE 2! CAW! -- Canada stores, CC & Store Ops by month */

SELECT
  ca.FSCL_YR_NUM
  ,ca.FSCL_PER_IN_YR_NUM
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
  
WHERE sr.TRANS_DTM >= '01-AUG-14'  
  AND sr.QSTN_ID IN ('Q2_1','Q2_2','Q2_3','Q2_4','Q2_5','Q2_6','Q2_7')
  AND sr.RSPNS_ID <> '9'
  AND st.CNTRY_CD_2_DGT_ISO = 'CA'
  AND st.OWNR_TYPE_CD = 'CO'

GROUP BY
  ca.FSCL_YR_NUM
  ,ca.FSCL_PER_IN_YR_NUM
  ,sr.QSTN_ID
;

/* For slide #28 - CC & SO by store volume (number of transactions) */

/* query 1 -- CC and SO by store */

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
  
  -- Total top box responses
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_1' THEN 1 ELSE 0 END) AS Q2_1_TB_Cnt
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_2' THEN 1 ELSE 0 END) AS Q2_2_TB_Cnt
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_3' THEN 1 ELSE 0 END) AS Q2_3_TB_Cnt
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_4' THEN 1 ELSE 0 END) AS Q2_4_TB_Cnt
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_5' THEN 1 ELSE 0 END) AS Q2_5_TB_Cnt
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_6' THEN 1 ELSE 0 END) AS Q2_6_TB_Cnt
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_7' THEN 1 ELSE 0 END) AS Q2_7_TB_Cnt

FROM APPDWH.AFT_CV_SRVY_RSPNS ce

  INNER JOIN APPDWH.ADT_CAL c
    ON TRUNC(ce.TRANS_DTM) = c.CAL_DT
      AND c.FSCL_YR_NUM = 2017
      AND c.FSCL_QTR_IN_YR_NUM = 4 -- ADDED FOR PT3
      
  INNER JOIN APPDWH.ADT_STORE d
    ON ce.STORE_NUM = d.STORE_NUM
    
  INNER JOIN APPDWH.DDM_RETAIL_ORG_STORE_DIST org
    ON ce.STORE_NUM = org.STORE_NUM
    AND org.DIV_ORG_LVL_ID IN (2)  -- Canada stores

WHERE ce.RSPNS_ID <> '9'  -- rspns_id = 9 for unanswered questions
  AND ce.QSTN_ID NOT IN ('Q1','Q11') -- these questions are not in Customer Connection or Store Operations scores

GROUP BY
    ce.STORE_NUM
;

/* query 2  -- Canada stores, customer trans count, active store day count, COSD by store number & month */
SELECT
      m.STORE_NUMBER
      --,to_char(m.BUSINESS_DATE,'MM') AS date_month
      --,to_char(m.BUSINESS_DATE,'YYYY') AS date_year

  -- Sum customer occassions
  --,SUM(m.CUST_TRANS_CNT) AS cust_trans_cnt
  --,SUM(m.ACTIVE_STORE_DAY_CNT) AS active_store_day_cnt
  -- COSD
  ,CASE WHEN SUM(m.ACTIVE_STORE_DAY_CNT) = 0 THEN 0 ELSE ROUND(SUM(m.CUST_TRANS_CNT) / SUM(m.ACTIVE_STORE_DAY_CNT),2) END AS cosd

FROM APPBUS.DFT_INTL_STORE_DAY_VW m

  INNER JOIN APPDWH.DDM_RETAIL_ORG_STORE_DIST org
    ON m.STORE_NUMBER = org.STORE_NUM
    AND org.DIV_ORG_LVL_ID IN (2)  -- Canada stores

WHERE m.BUSINESS_DATE BETWEEN '10-JUL-17' AND '01-OCT-17'

GROUP BY
    m.STORE_NUMBER
    --,to_char(m.BUSINESS_DATE,'MM')
    --,to_char(m.BUSINESS_DATE,'YYYY') 
;


/* slide #31 -- CE by channel -- CAW */ 

SELECT
   FSCL_YR_NUM
  ,FSCL_QTR_IN_YR_NUM
  ,QSTN_ID
  ,MOBILE_ORD_PAY_IND
  ,ORD_MTHD_CD
  ,SUM(CASE WHEN RSPNS_ID = '7' THEN 1 ELSE 0 END) AS TB_COUNT
  ,COUNT(*) AS RSPNS_COUNT
  ,SUM(CASE WHEN TRANS_END_TM_KEY BETWEEN 70000 AND 95959 AND RSPNS_ID = '7' THEN 1 ELSE 0 END) AS PEAK_TB_COUNT
  ,SUM(CASE WHEN TRANS_END_TM_KEY BETWEEN 70000 AND 95959 THEN 1 ELSE 0 END) AS PEAK_RSPNS_COUNT
FROM (
SELECT DISTINCT
   sr.CASE_ID
  ,sr.GUID_USER_ID
  ,sr.RSPNS_DT
  ,sr.STORE_NUM
  ,sr.TRANS_DTM
  ,sr.QSTN_ID
  ,sr.RSPNS_ID
  
  ,ca.FSCL_YR_NUM
  ,ca.FSCL_QTR_IN_YR_NUM
  
  ,pi.TRANS_END_TM_KEY
  
  ,tt.MOBILE_ORD_PAY_IND
  ,tt.MOBILE_IND
  ,tt.ORD_MTHD_CD
FROM APPOTHER.AFT_CV_SRVY_RSPNS sr

JOIN APPCA.D_CAL ca
  ON ca.CAL_DT = TRUNC(sr.TRANS_DTM)
  
JOIN APPCA.D_STORE_VERS st
  ON sr.STORE_NUM = st.STORE_NUM
    AND st.CURRENT_FLG = 'Y'
    AND st.OWNR_TYPE_CD = 'CO'

/* Create universal transaction ID from survey table and use that to join to POS table
/  Limit to U.S. and Canada and transactions after the survey began to improve performance */
JOIN APPCA.F_POS_LINE_ITEM pi
  ON TO_CHAR(sr.TRANS_DTM, 'YYYYMMDD') || TRIM(TO_CHAR(sr.STORE_NUM, '000000'))
      || TRIM(TO_CHAR(SUBSTR(sr.RGSTR_NUM, -1, 2),'00')) || sr.TRANS_ID = pi.TRANS_ID
    AND pi.CNTRY_CD = 'CA'
    AND pi.BUS_DT >= '10-JUL-17'
  
JOIN APPCA.D_POS_LINE_ITEM_TRANS_TYPE tt
  ON pi.POS_LINE_ITEM_TRANS_TYPE_KEY = tt.POS_LINE_ITEM_TRANS_TYPE_KEY

WHERE ca.FSCL_YR_NUM = 2017
  AND ca.FSCL_QTR_IN_YR_NUM = 4
  AND sr.QSTN_ID IN ('Q2_1','Q2_2','Q2_3','Q2_4','Q2_5','Q2_6','Q2_7','Q2_8')
  AND sr.RSPNS_ID <> '9'
  --AND sr.STORE_NUM = 5798
)

GROUP BY
   FSCL_YR_NUM
  ,FSCL_QTR_IN_YR_NUM
  ,QSTN_ID
  ,MOBILE_ORD_PAY_IND
  ,ORD_MTHD_CD
;


/* Slide #32 -- CE by company-owned versus licensed -- CAW*/

SELECT
  ca.FSCL_YR_NUM
  ,ca.FSCL_QTR_IN_YR_NUM
  ,sr.QSTN_ID
  ,st.OWNR_TYPE_CD
  
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
  AND ca.FSCL_YR_NUM = 2017
  AND ca.FSCL_QTR_IN_YR_NUM = 4
  
INNER JOIN APPCA.D_STORE_VERS st
  ON st.STORE_NUM = sr.STORE_NUM
    AND st.CURRENT_FLG = 'Y'
  
WHERE sr.QSTN_ID IN ('Q2_1','Q2_2','Q2_3','Q2_4','Q2_5','Q2_6','Q2_7')
  --AND sr.TRANS_DTM >= '10-JUL-17'  
  AND sr.RSPNS_ID <> '9'
  AND st.CNTRY_CD_2_DGT_ISO = 'CA'

GROUP BY
  ca.FSCL_YR_NUM
  ,ca.FSCL_QTR_IN_YR_NUM
  ,sr.QSTN_ID
  ,st.OWNR_TYPE_CD
;


/* Slide #15 CC & SO by % home store customers */

SELECT
   T3.STORE_NUM
  ,T3.FSCL_YR_NUM
  ,T3.FSCL_QTR_IN_YR_NUM
  ,T3.FSCL_PER_CNT_NUM
  ,SUM(CASE WHEN T2.HOME_STORE = T3.STORE_NUM THEN 1 
       ELSE NULL 
   END) AS HS_CUST_COUNT
  ,COUNT(DISTINCT T2.GUID_ID) AS ALL_CUST_COUNT
FROM(
    --LIST OF ALL ACTIVE STORES AND GUIDS WHO VISITED THOSE STORES DURING THE GIVEN TIME PERIOD
    SELECT
       STR.STORE_NUM
      ,ca.FSCL_YR_NUM
      ,ca.FSCL_QTR_IN_YR_NUM
      ,ca.FSCL_PER_CNT_NUM
      ,POSH2.GUID_ID
    FROM APPCA.F_POS_HDR POSH2
    LEFT OUTER JOIN APPCA.D_STORE_VERS STR
      ON POSH2.STORE_VERS_KEY = STR.STORE_VERS_KEY
    INNER JOIN APPCA.D_CAL ca
      ON POSH2.BUS_DT = ca.CAL_DT
    WHERE STR.OWNR_TYPE_CD = 'CO'
      AND STR.CNTRY_CD_2_DGT_ISO = 'CA'
      AND POSH2.GROSS_REV_LCL_AMT > 0
       AND ca.FSCL_YR_NUM = 2017
       AND ca.FSCL_QTR_IN_YR_NUM = 4
    
    GROUP BY
       STR.STORE_NUM
      ,ca.FSCL_YR_NUM
      ,ca.FSCL_QTR_IN_YR_NUM
      ,ca.FSCL_PER_CNT_NUM
      ,POSH2.GUID_ID
    ) T3
  --JOINING LIST OF GUIDS WHO WERE ACTIVE, THEIR HOMESTORE, TOTAL TRANSACTIONS AND SPEND AT ALL STORES
  INNER JOIN (
    SELECT
       T1.GUID_ID
      ,T1.FSCL_YR_NUM
      ,T1.FSCL_QTR_IN_YR_NUM
      ,T1.FSCL_PER_CNT_NUM
      ,SUM(CASE WHEN T1.RNK_HS = 1 AND T1.N_TRANS>=3 
         THEN T1.STORE_NUM 
         ELSE NULL 
       END) AS HOME_STORE
      ,SUM(CASE WHEN T1.RNK_HS = 1 AND T1.N_TRANS>=3 
         THEN T1.N_TRANS 
         ELSE NULL 
       END) AS HOME_STORE_VISITS
      ,SUM(CASE WHEN T1.RNK_HS = 1 AND T1.N_TRANS>=3 
         THEN T1.TOT_SPEND 
         ELSE NULL 
       END) AS HOME_STORE_SPEND
      ,SUM(T1.N_TRANS) AS ALL_STORES_VISITS
      ,SUM(T1.TOT_SPEND) AS ALL_STORES_SPEND
    FROM(
      --INNER QUERY - ALL GUIDS, STORES THEY VISITED, # TRANSACTIONS AT THOSE STORES AND TOTAL SPEND AT THOSE STORES
      SELECT
         POSH.GUID_ID
        ,STR.STORE_NUM
        ,ca.FSCL_YR_NUM
        ,ca.FSCL_QTR_IN_YR_NUM
        ,ca.FSCL_PER_CNT_NUM
        ,COUNT(DISTINCT POSH.TRANS_ID) AS N_TRANS
        ,SUM(POSH.GROSS_REV_LCL_AMT) AS TOT_SPEND
        --this ranks each store that the guid has visited by the number of transactions and spend
        ,RANK() OVER(PARTITION BY POSH.GUID_ID, ca.FSCL_PER_CNT_NUM 
                  ORDER BY -COUNT(DISTINCT POSH.TRANS_ID), 
                           -SUM(POSH.GROSS_REV_LCL_AMT)
                  ) 
         AS RNK_HS
      FROM APPCA.F_POS_HDR POSH
      LEFT OUTER JOIN APPCA.D_STORE_VERS STR
        ON POSH.STORE_VERS_KEY = STR.STORE_VERS_KEY
      INNER JOIN APPCA.D_CAL ca
        ON POSH.BUS_DT = ca.CAL_DT
      WHERE STR.OWNR_TYPE_CD = 'CO'
        AND STR.CNTRY_CD_2_DGT_ISO = 'CA'
        AND POSH.GROSS_REV_LCL_AMT > 0
        AND ca.FSCL_YR_NUM = 2017
        AND ca.FSCL_QTR_IN_YR_NUM = 4
        
      GROUP BY
         POSH.GUID_ID
        ,STR.STORE_NUM
        ,ca.FSCL_YR_NUM
        ,ca.FSCL_QTR_IN_YR_NUM
        ,ca.FSCL_PER_CNT_NUM
    ) T1 --THIS IS THE LIST OF GUIDS AND THEIR HOMESTORE,SPEND ETC
    GROUP BY
       T1.GUID_ID
      ,T1.FSCL_YR_NUM
      ,T1.FSCL_QTR_IN_YR_NUM
      ,T1.FSCL_PER_CNT_NUM
  ) T2 --THIS IS THE LIST OF ALL STORES AND THEIR CUSTOMERS
ON T2.GUID_ID = T3.GUID_ID
  AND T2.FSCL_PER_CNT_NUM = T3.FSCL_PER_CNT_NUM

WHERE T2.GUID_ID IS NOT NULL 
  AND T3.GUID_ID IS NOT NULL 
  AND T2.GUID_ID !='0' 
  AND T3.GUID_ID !='0' 

GROUP BY
   T3.STORE_NUM
  ,T3.FSCL_YR_NUM
  ,T3.FSCL_QTR_IN_YR_NUM
  ,T3.FSCL_PER_CNT_NUM
;

