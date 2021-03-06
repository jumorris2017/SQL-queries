SELECT
   sr.STORE_NUM AS SBUX_STORE_NUM
  ,st.CNTRY_CD_2_DGT_ISO AS COUNTRY_CD
  ,COUNT(sr.CASE_ID) AS TOTAL_RSPNS_CNT
  ,COUNT(CASE WHEN TRUNC(sr.TRANS_DTM) >= '29-JAN-18' AND TRUNC(sr.TRANS_DTM) <= '25-FEB-18' THEN 1 END) AS FISCAL_FEB_RSPNS_CNT  -- only if it's the 1st day of the fiscal month --UPDATE NAME
  ,MIN(TRUNC(sr.TRANS_DTM)) AS EARLIEST_RSPNS_DT
  ,MAX(TRUNC(sr.TRANS_DTM)) AS LATEST_RSPNS_DT
FROM APPOTHER.AFT_CV_SRVY_RSPNS sr

LEFT JOIN APPCA.D_STORE_VERS st
  ON sr.STORE_NUM = st.STORE_NUM
    AND TRUNC(sr.TRANS_DTM) BETWEEN st.EFF_FROM_DT AND st.EFF_TO_DT

WHERE sr.QSTN_ID = 'Q1'
  AND TRUNC(sr.TRANS_DTM) >= '09-SEP-16'  -- first day of LS CE surveys
  AND st.OWNR_TYPE_CD = 'LS'
  
GROUP BY
   sr.STORE_NUM
  ,st.CNTRY_CD_2_DGT_ISO
  
ORDER BY
   st.CNTRY_CD_2_DGT_ISO
  ,sr.STORE_NUM
;
