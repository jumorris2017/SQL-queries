--SELECT TEST_STORE_NUM, DOM_INSTALL_DT, CONTROL_STORE_NUM 
--FROM DEPT_CUST_INS.TN_DOM_TEST_STORES
/* export as "DOM_CC_teststores.csv" */

SELECT
   TEST_STORE_NUM
  ,DOM_INSTALL_DT
  ,QSTN_ID
  ,MOBILE_ORD_PAY_IND
  ,SUM(CASE WHEN DAYS_FROM_INSTALL BETWEEN -56 and -8 THEN TB_COUNT ELSE 0 END) AS TB_COUNT_PRE
  ,SUM(CASE WHEN DAYS_FROM_INSTALL BETWEEN 28 and 90 THEN TB_COUNT ELSE 0 END) AS TB_COUNT_POST
  ,SUM(CASE WHEN DAYS_FROM_INSTALL BETWEEN -56 and -8 THEN RSPNS_COUNT ELSE 0 END) AS RSPNS_COUNT_PRE
  ,SUM(CASE WHEN DAYS_FROM_INSTALL BETWEEN 28 and 90 THEN RSPNS_COUNT ELSE 0 END) AS RSPNS_COUNT_POST
  ,SUM(CASE WHEN DAYS_FROM_INSTALL BETWEEN -56 and -8 THEN PEAK_TB_COUNT ELSE 0 END) AS PEAK_TB_COUNT_PRE
  ,SUM(CASE WHEN DAYS_FROM_INSTALL BETWEEN 28 and 90 THEN PEAK_TB_COUNT ELSE 0 END) AS PEAK_TB_COUNT_POST
  ,SUM(CASE WHEN DAYS_FROM_INSTALL BETWEEN -56 and -8 THEN PEAK_RSPNS_COUNT ELSE 0 END) AS PEAK_RSPNS_COUNT_PRE
  ,SUM(CASE WHEN DAYS_FROM_INSTALL BETWEEN 28 and 90 THEN PEAK_RSPNS_COUNT ELSE 0 END) AS PEAK_RSPNS_COUNT_POST
FROM (
SELECT
   QSTN_ID
  ,TEST_STORE_NUM
  ,DOM_INSTALL_DT
  ,MOBILE_ORD_PAY_IND
  ,TRUNC(TRANS_DTM) - DOM_INSTALL_DT AS DAYS_FROM_INSTALL
  ,SUM(CASE WHEN RSPNS_ID = '7' THEN 1 ELSE 0 END) AS TB_COUNT
  ,SUM(1) AS RSPNS_COUNT
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
  ,ca.FSCL_PER_IN_YR_NUM
  ,ca.FSCL_WK_IN_YR_NUM
  ,ca.FSCL_WK_END_DT
  
  ,dom.TEST_STORE_NUM
  ,dom.DOM_INSTALL_DT
  
  ,pi.TRANS_END_TM_KEY
  
  ,tt.MOBILE_ORD_PAY_IND
  ,tt.ORD_MTHD_CD
FROM APPOTHER.AFT_CV_SRVY_RSPNS sr

JOIN APPCA.D_CAL ca
  ON ca.CAL_DT = TRUNC(sr.TRANS_DTM)
  
JOIN (SELECT DISTINCT TEST_STORE_NUM, DOM_INSTALL_DT FROM DEPT_CUST_INS.TN_DOM_TEST_STORES) dom
  ON sr.STORE_NUM = dom.TEST_STORE_NUM
    AND TRUNC(sr.TRANS_DTM) BETWEEN dom.DOM_INSTALL_DT - 56 AND dom.DOM_INSTALL_DT + 90

/* Create universal transaction ID from survey table and use that to join to POS table
/  Limit to U.S. and Canada and transactions after the survey began to improve performance */
JOIN APPCA.F_POS_LINE_ITEM pi
  ON TO_CHAR(sr.TRANS_DTM, 'YYYYMMDD') || TRIM(TO_CHAR(sr.STORE_NUM, '000000'))
      || TRIM(TO_CHAR(SUBSTR(sr.RGSTR_NUM, -1, 2),'00')) || sr.TRANS_ID = pi.TRANS_ID
    AND pi.CNTRY_CD = 'US'
    AND pi.BUS_DT >= '01-SEP-15'
  
JOIN APPCA.D_POS_LINE_ITEM_TRANS_TYPE tt
  ON pi.POS_LINE_ITEM_TRANS_TYPE_KEY = tt.POS_LINE_ITEM_TRANS_TYPE_KEY

WHERE sr.RSPNS_ID <> '9'
  --AND sr.QSTN_ID IN ('Q2_1','Q2_2','Q2_5')
  AND sr.QSTN_ID IN ('Q2_2')
)

GROUP BY
    QSTN_ID
  ,TEST_STORE_NUM
  ,DOM_INSTALL_DT
  ,MOBILE_ORD_PAY_IND
  ,TRUNC(TRANS_DTM) - DOM_INSTALL_DT
)

GROUP BY
   TEST_STORE_NUM
  ,DOM_INSTALL_DT
  ,QSTN_ID
  ,MOBILE_ORD_PAY_IND
;