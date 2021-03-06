--PULLS SD FOR CC BY TIME PERIOD
--416
WITH SQ AS (
SELECT DISTINCT
sr.QSTN_ID
,sr.CASE_ID
,ca.FSCL_YR_NUM
,ca.FSCL_QTR_IN_YR_NUM
,(CASE  WHEN sr.RSPNS_ID = '7' THEN COALESCE(w.WEIGHT_RT,1) ELSE 0 END) AS TB_SCORE

FROM APPOTHER.AFT_CV_SRVY_RSPNS sr

JOIN APPCA.D_CAL ca
  ON ca.CAL_DT = TRUNC(sr.TRANS_DTM)
  
JOIN APPCA.D_STORE_VERS st
  ON sr.STORE_NUM = st.STORE_NUM
    AND st.CURRENT_FLG = 'Y'
    AND st.OWNR_TYPE_CD = 'CO'
    AND st.CNTRY_CD_2_DGT_ISO = 'US' 

LEFT JOIN APPOTHER.CUST_INS_WEIGHTS w
  ON sr.CASE_ID = w.CASE_ID

  WHERE sr.QSTN_ID IN ('Q2_2')
  AND sr.RSPNS_ID <> '9'
  AND ca.FSCL_YR_NUM IN (2017,2018) AND ca.FSCL_QTR_IN_YR_NUM = 3
)
SELECT ROUND(STDDEV(TB_SCORE),4) AS SD_CC
FROM SQ
GROUP BY FSCL_YR_NUM