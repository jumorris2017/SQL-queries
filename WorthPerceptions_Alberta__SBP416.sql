/* Worth perceptions for Alberta */

SELECT
   sr.QSTN_ID
  ,sr.STORE_NUM
  ,st.DIST_NUM
  ,st.DIST_DESCR
  ,st.AREA_NUM
  ,st.AREA_DESCR
  ,st.RGN_NUM
  ,st.RGN_DESCR
  ,st.CNTRY_CD_2_DGT_ISO AS COUNTRY_CD
  ,st.CNTRY_SUB_DIV_CD
  ,st.OWNR_TYPE_CD
  ,ca.FSCL_YR_NUM
  ,ca.FSCL_QTR_IN_YR_NUM
  ,ca.FSCL_PER_IN_YR_NUM
  ,ca.FSCL_WK_IN_YR_NUM
  ,SUM(CASE
    WHEN RSPNS_ID = '5' AND QSTN_ID = 'Q1' THEN COALESCE(w.WEIGHT_RT,1) /* coalesce returns non-null weight value, OR if null, returns 1 */
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
  
WHERE sr.TRANS_DTM >= '29-DEC-14'  -- offical launch date of CO store program
  --AND sr.QSTN_ID IN ('Q1','Q2_1','Q2_2','Q2_3','Q2_4','Q2_5','Q2_6','Q2_7','Q2_8')
  AND sr.QSTN_ID = 'Q2_2'  -- CC Only
  AND sr.RSPNS_ID <> '9'
  AND st.CNTRY_CD_2_DGT_ISO = 'US'
  AND st.OWNR_TYPE_CD = 'CO'
  AND st.CNTRY_SUB_DIV_CD = 'AB'
  --AND sr.STORE_NUM = 101  -- for testing

GROUP BY
   sr.QSTN_ID
  ,sr.STORE_NUM
  ,st.DIST_NUM
  ,st.DIST_DESCR
  ,st.AREA_NUM
  ,st.AREA_DESCR
  ,st.RGN_NUM
  ,st.RGN_DESCR
  ,st.CNTRY_CD_2_DGT_ISO
  ,st.CNTRY_SUB_DIV_CD
  ,st.OWNR_TYPE_CD
  ,ca.FSCL_YR_NUM
  ,ca.FSCL_QTR_IN_YR_NUM
  ,ca.FSCL_PER_IN_YR_NUM
  ,ca.FSCL_WK_IN_YR_NUM
;

