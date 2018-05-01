/* Pulls customer transaction count by GUID for customers in the CE survey data who made an MOP transaction -- from Oliver */
WITH bl AS
(select b.FSCL_YR_NUM
  ,b.FSCL_QTR_IN_YR_NUM
  ,a.GUID_ID
  ,count(*) AS TRANS

from APPCA.F_SVC_FIN_TRANS a

inner join APPCA.D_CAL b 
  on a.BUS_DT = b.CAL_DT

inner join APPCA.D_SVC_TRANS_TYPE c 
  on a.SVC_TRANS_TYPE_KEY = c.SVC_TRANS_TYPE_KEY

inner join (SELECT
  sr.GUID_USER_ID
  ,ca.FSCL_YR_NUM
  ,ca.FSCL_QTR_IN_YR_NUM
FROM APPOTHER.AFT_CV_SRVY_RSPNS sr

JOIN APPCA.D_CAL ca
  ON ca.CAL_DT = TRUNC(sr.TRANS_DTM)
  
JOIN APPCA.D_STORE_VERS st
  ON sr.STORE_NUM = st.STORE_NUM
    --AND st.CURRENT_FLG = 'Y'
    AND st.OWNR_TYPE_CD = 'CO'
    AND st.CNTRY_CD_2_DGT_ISO = 'US'
  
JOIN APPCA.F_POS_LINE_ITEM pi
  ON TO_CHAR(sr.TRANS_DTM, 'YYYYMMDD') || TRIM(TO_CHAR(sr.STORE_NUM, '000000'))
      || TRIM(TO_CHAR(SUBSTR(sr.RGSTR_NUM, -1, 2),'00')) || sr.TRANS_ID = pi.TRANS_ID
    AND pi.CNTRY_CD = 'US'
    AND pi.BUS_DT >= '01-SEP-15'
    
JOIN APPCA.D_POS_LINE_ITEM_TRANS_TYPE tt
  ON pi.POS_LINE_ITEM_TRANS_TYPE_KEY = tt.POS_LINE_ITEM_TRANS_TYPE_KEY

WHERE ca.FSCL_YR_NUM >= 2017
  --AND ca.FSCL_QTR_IN_YR_NUM = 3
  AND sr.QSTN_ID IN ('Q2_2')
  AND sr.RSPNS_ID <> '9'
  AND tt.ORD_MTHD_CD IN ('MOP')
GROUP BY 
  sr.GUID_USER_ID
  ,ca.FSCL_YR_NUM
  ,ca.FSCL_QTR_IN_YR_NUM
  ) sq
  
ON a.GUID_ID = sq.GUID_USER_ID
  AND b.FSCL_YR_NUM = sq.FSCL_YR_NUM
  AND b.FSCL_QTR_IN_YR_NUM = sq.FSCL_QTR_IN_YR_NUM

where c.SVC_INTRNL_RQST_NM = 'Redemption' 
  and b.FSCL_YR_NUM >= 2017

group by a.GUID_ID
  ,b.FSCL_YR_NUM
  ,b.FSCL_QTR_IN_YR_NUM) 

SELECT 
  bl.FSCL_YR_NUM
  ,bl.FSCL_QTR_IN_YR_NUM
  ,AVG(bl.TRANS) FROM bl
GROUP BY 
  bl.FSCL_YR_NUM
  ,bl.FSCL_QTR_IN_YR_NUM



 /*Julie's*/

WITH bl AS
(select b.FSCL_YR_NUM
  ,b.FSCL_PER_IN_YR_NUM
  ,a.GUID_ID
  ,count(*) AS TRANS
  ,sq.TOTAL_TB
  ,sq.TOTAL_RSPNS

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
  ,SUM(CASE WHEN sr.RSPNS_ID = '7' AND sr.QSTN_ID = 'Q2_2' THEN 1 ELSE 0 END) AS TOTAL_TB
  ,SUM(CASE WHEN sr.QSTN_ID = 'Q2_2' THEN 1 ELSE 0 END) AS TOTAL_RSPNS
  ,ca.FSCL_YR_NUM
  ,ca.FSCL_PER_IN_YR_NUM
  ,TRUNC(sr.TRANS_DTM) AS SVY_RSPN_DATE
FROM APPOTHER.AFT_CV_SRVY_RSPNS sr

JOIN APPCA.D_CAL ca
  ON ca.CAL_DT = TRUNC(sr.TRANS_DTM)

WHERE ((b.FSCL_YR_NUM = 2018 AND b.FSCL_PER_IN_YR_NUM = 6) OR
           (b.FSCL_YR_NUM = 2017 AND b.FSCL_PER_IN_YR_NUM = 12) OR
           (b.FSCL_YR_NUM = 2017 AND b.FSCL_PER_IN_YR_NUM = 6) OR
           (b.FSCL_YR_NUM = 2016 AND b.FSCL_PER_IN_YR_NUM = 12))
  AND sr.QSTN_ID IN ('Q2_2')
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
  AND ((b.FSCL_YR_NUM = 2018 AND b.FSCL_PER_IN_YR_NUM = 6) OR
           (b.FSCL_YR_NUM = 2017 AND b.FSCL_PER_IN_YR_NUM = 12) OR
           (b.FSCL_YR_NUM = 2017 AND b.FSCL_PER_IN_YR_NUM = 6) OR
           (b.FSCL_YR_NUM = 2016 AND b.FSCL_PER_IN_YR_NUM = 12))
group by b.FSCL_YR_NUM
  ,b.FSCL_PER_IN_YR_NUM
  ,a.GUID_ID
  ,sq.TOTAL_TB
  ,sq.TOTAL_RSPNS
  ) 
SELECT 
  bl.FSCL_YR_NUM
  ,bl.FSCL_PER_IN_YR_NUM
  ,count(bl.GUID_ID) AS USER_COUNT
  ,bl.TRANS
  ,sum(bl.TOTAL_TB) AS TB_COUNT
  ,sum(bl.TOTAL_RSPNS) AS RSPSN_COUNT
FROM bl
GROUP BY 
  bl.FSCL_YR_NUM
  ,bl.FSCL_PER_IN_YR_NUM
  ,bl.TRANS
ORDER BY
  bl.FSCL_YR_NUM
  ,bl.FSCL_PER_IN_YR_NUM
  ,bl.TRANS