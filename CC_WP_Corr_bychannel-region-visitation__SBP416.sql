
  
  
--Pulls customer transaction count by GUID for customers in the CE survey data who made an MOP transaction

--trans, cc, and wp
WITH bl AS
(select b.FSCL_YR_NUM
  ,b.FSCL_PER_IN_YR_NUM
  ,a.GUID_ID
  ,count(*) AS TRANS
  ,sq.TOTAL_TB_CC
  ,sq.TOTAL_TB_WP

from APPCA.F_SVC_FIN_TRANS a

inner join APPCA.D_CAL b 
  on a.BUS_DT = b.CAL_DT

inner join APPCA.D_SVC_TRANS_TYPE c 
  on a.SVC_TRANS_TYPE_KEY = c.SVC_TRANS_TYPE_KEY
  
JOIN APPCA.D_STORE_VERS st
  ON a.STORE_NUM = st.STORE_NUM
    AND st.CURRENT_FLG = 'Y'
    AND st.OWNR_TYPE_CD = 'CO'
    AND st.CNTRY_CD_2_DGT_ISO = 'US'

inner join (SELECT
  sr.GUID_USER_ID
  ,SUM(CASE WHEN sr.RSPNS_ID = '7' AND sr.QSTN_ID = 'Q2_2' THEN 1 ELSE 0 END) AS TOTAL_TB_CC
  ,SUM(CASE WHEN sr.RSPNS_ID = '7' AND sr.QSTN_ID = 'Q2_8' THEN 1 ELSE 0 END) AS TOTAL_TB_WP
  --,SUM(CASE WHEN sr.QSTN_ID = 'Q2_2' THEN 1 ELSE 0 END) AS TOTAL_RSPNS
  ,ca.FSCL_YR_NUM
  ,ca.FSCL_PER_IN_YR_NUM
  ,TRUNC(sr.TRANS_DTM) AS SVY_RSPN_DATE
FROM APPOTHER.AFT_CV_SRVY_RSPNS sr

JOIN APPCA.D_CAL ca
  ON ca.CAL_DT = TRUNC(sr.TRANS_DTM)

WHERE ((ca.FSCL_YR_NUM = 2018 AND ca.FSCL_PER_IN_YR_NUM = 9))
  AND sr.QSTN_ID IN ('Q2_2','Q2_8')
  AND sr.RSPNS_ID <> '9'

GROUP BY 
  sr.GUID_USER_ID
  ,ca.FSCL_YR_NUM
  ,ca.FSCL_PER_IN_YR_NUM
  ,TRUNC(sr.TRANS_DTM)
  ) sq
  
ON a.GUID_ID = sq.GUID_USER_ID
  AND b.FSCL_YR_NUM = sq.FSCL_YR_NUM
  AND b.FSCL_PER_IN_YR_NUM = sq.FSCL_PER_IN_YR_NUM

where c.SVC_INTRNL_RQST_NM = 'Redemption' 
  AND ((b.FSCL_YR_NUM = 2018 AND b.FSCL_PER_IN_YR_NUM = 9))
group by b.FSCL_YR_NUM
  ,b.FSCL_PER_IN_YR_NUM
  ,a.GUID_ID
  ,sq.TOTAL_TB_CC
  ,sq.TOTAL_TB_WP
  ) 
  select bl.FSCL_YR_NUM
  ,bl.FSCL_PER_IN_YR_NUM
  ,bl.GUID_ID
  ,bl.TRANS
  ,(CASE WHEN bl.TRANS <= 5 THEN 1
    WHEN bl.TRANS >5 AND bl.TRANS <=10 THEN 2
    WHEN bl.TRANS >10 AND bl.TRANS <=15 THEN 3
    WHEN bl.TRANS >15 AND bl.TRANS <=20 THEN 4
    WHEN bl.TRANS >20 AND bl.TRANS <=25 THEN 5
    WHEN bl.TRANS >25 THEN 6 ELSE NULL END) AS TRANS_GROUP
  ,bl.TOTAL_TB_CC
  ,bl.TOTAL_TB_WP
  from bl
  group by bl.FSCL_YR_NUM
  ,bl.FSCL_PER_IN_YR_NUM
  ,bl.GUID_ID
  ,bl.TRANS
  ,bl.TOTAL_TB_CC
  ,bl.TOTAL_TB_WP
  
  /* CE by channel -- CAW */ 

SELECT DISTINCT
  sr.STORE_NUM
  ,st.RGN_DESCR
  ,sr.QSTN_ID
  ,ca.FSCL_YR_NUM
  ,ca.FSCL_QTR_IN_YR_NUM
  ,tt.ORD_MTHD_CD
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
    AND st.OWNR_TYPE_CD = 'CO'

LEFT JOIN APPOTHER.CUST_INS_WEIGHTS w
  ON sr.CASE_ID = w.CASE_ID

/* Create universal transaction ID from survey table and use that to join to POS table
/  Limit to U.S. and Canada and transactions after the survey began to improve QTRformance */
JOIN APPCA.F_POS_LINE_ITEM pi
  ON TO_CHAR(sr.TRANS_DTM, 'YYYYMMDD') || TRIM(TO_CHAR(sr.STORE_NUM, '000000'))
      || TRIM(TO_CHAR(SUBSTR(sr.RGSTR_NUM, -1, 2),'00')) || sr.TRANS_ID = pi.TRANS_ID
    AND pi.CNTRY_CD = 'US'
    --AND pi.BUS_DT >= '01-OCT-17' AND pi.BUS_DT < '01-DEC-17'
  
JOIN APPCA.D_POS_LINE_ITEM_TRANS_TYPE tt
  ON pi.POS_LINE_ITEM_TRANS_TYPE_KEY = tt.POS_LINE_ITEM_TRANS_TYPE_KEY
  WHERE tt.ORD_MTHD_CD IN ('CAFE','MOP','OTW')
  AND sr.QSTN_ID IN ('Q2_2','Q2_8')
  AND sr.RSPNS_ID <> '9'
  AND ca.FSCL_YR_NUM = 2018 
  AND ca.FSCL_QTR_IN_YR_NUM = 3
      
GROUP BY
  sr.STORE_NUM
  ,st.RGN_DESCR
  ,sr.QSTN_ID
  ,ca.FSCL_YR_NUM
  ,ca.FSCL_QTR_IN_YR_NUM
  ,tt.ORD_MTHD_CD
ORDER BY
  sr.STORE_NUM
  ,sr.QSTN_ID
  ,ca.FSCL_YR_NUM
  ,ca.FSCL_QTR_IN_YR_NUM
  ,tt.ORD_MTHD_CD
  
  
  
 