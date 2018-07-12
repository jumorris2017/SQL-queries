/* CE by channel -- CAW */ 

/*MOP Slide #1*/
/*export as CE_bychannel_byquarter.csv*/
SELECT DISTINCT
  --sr.STORE_NUM
  sr.QSTN_ID
  ,ca.FSCL_YR_NUM
  ,ca.FSCL_QTR_IN_YR_NUM
  ,tt.ORD_MTHD_CD
  --,st.OWNR_TYPE_CD
  --,st.CNTRY_CD_2_DGT_ISO
  --,st.DRIVE_THRU_IND
  ,SUM(CASE  WHEN sr.RSPNS_ID = '5' AND sr.QSTN_ID = 'Q1' THEN COALESCE(w.WEIGHT_RT,1) /* coalesce returns non-null weight value, OR if null, returns 1 */
    WHEN sr.RSPNS_ID = '7' THEN COALESCE(w.WEIGHT_RT,1) ELSE 0 END) AS TOTAL_TB
  ,SUM(COALESCE(w.WEIGHT_RT,1)) AS TOTAL_RSPNS
  ,ROUND((SUM(CASE  WHEN sr.RSPNS_ID = '5' AND sr.QSTN_ID = 'Q1' THEN COALESCE(w.WEIGHT_RT,1) WHEN sr.RSPNS_ID = '7' THEN COALESCE(w.WEIGHT_RT,1) ELSE 0 END)) / SUM(COALESCE(w.WEIGHT_RT,1)),4) AS TB_SCORE
  
FROM APPOTHER.AFT_CV_SRVY_RSPNS sr

JOIN APPCA.D_CAL ca
  ON ca.CAL_DT = TRUNC(sr.TRANS_DTM)
  
JOIN APPCA.D_STORE_VERS st
  ON sr.STORE_NUM = st.STORE_NUM
    AND st.CURRENT_FLG = 'Y'
    AND st.OWNR_TYPE_CD IN ('CO')
    AND st.CNTRY_CD_2_DGT_ISO IN ('US')
    --AND sr.STORE_NUM IN ('50621') --sodo reserve (SSC)

LEFT JOIN APPOTHER.CUST_INS_WEIGHTS w
  ON sr.CASE_ID = w.CASE_ID

/* Create universal transaction ID from survey table and use that to join to POS table
/  Limit to U.S. and Canada and transactions after the survey began to improve performance */
JOIN APPCA.F_POS_LINE_ITEM pi
  ON TO_CHAR(sr.TRANS_DTM, 'YYYYMMDD') || TRIM(TO_CHAR(sr.STORE_NUM, '000000'))
      || TRIM(TO_CHAR(SUBSTR(sr.RGSTR_NUM, -1, 2),'00')) || sr.TRANS_ID = pi.TRANS_ID

  
JOIN APPCA.D_POS_LINE_ITEM_TRANS_TYPE tt
  ON pi.POS_LINE_ITEM_TRANS_TYPE_KEY = tt.POS_LINE_ITEM_TRANS_TYPE_KEY
  WHERE tt.ORD_MTHD_CD IN ('CAFE','MOP','OTW')
  AND sr.QSTN_ID IN ('Q1','Q2_1','Q2_2','Q2_3','Q2_4','Q2_5','Q2_6','Q2_7','Q2_8')
  --AND sr.QSTN_ID IN ('Q2_2')
  AND sr.RSPNS_ID <> '9'
  AND ca.FSCL_YR_NUM = 2018
  AND ca.FSCL_QTR_IN_YR_NUM = 3
      
GROUP BY
  --sr.STORE_NUM
  sr.QSTN_ID
  ,ca.FSCL_YR_NUM
  ,ca.FSCL_QTR_IN_YR_NUM
  ,tt.ORD_MTHD_CD

ORDER BY
  tt.ORD_MTHD_CD
  ,sr.QSTN_ID

