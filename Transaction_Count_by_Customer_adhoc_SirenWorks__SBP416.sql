                         

/* Pulls customer transaction count by GUID for customers in the CE survey data */

select b.FSCL_YR_NUM
  ,b.FSCL_PER_IN_YR_NUM
  ,a.GUID_ID
  --,sq.RSPNS_DT
  --,sq.STORE_NUM
  ,count(*) AS TRANS
  --,sq.TOTAL_TB
  --,sq.TOTAL_RSPNS

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
  --,sr.RSPNS_DT
  --,sr.STORE_NUM
  --,SUM(CASE WHEN sr.RSPNS_ID = '7' AND sr.QSTN_ID = 'Q2_2' THEN 1 ELSE 0 END) AS TOTAL_TB
  --,SUM(CASE WHEN sr.QSTN_ID = 'Q2_2' THEN 1 ELSE 0 END) AS TOTAL_RSPNS
  ,ca.FSCL_YR_NUM
  ,ca.FSCL_PER_IN_YR_NUM
  ,TRUNC(sr.TRANS_DTM) AS SVY_RSPN_DATE
FROM APPOTHER.AFT_CV_SRVY_RSPNS sr

JOIN APPCA.D_CAL ca
  ON ca.CAL_DT = TRUNC(sr.TRANS_DTM)

WHERE ((TRUNC(sr.TRANS_DTM) BETWEEN '08-MAR-17' AND '22-MAR-17') OR (TRUNC(sr.TRANS_DTM) BETWEEN '05-APR-17' AND '21-APR-17'))
  AND sr.QSTN_ID IN ('Q2_2')
  AND sr.RSPNS_ID <> '9'
GROUP BY 
  sr.GUID_USER_ID
  --,sr.RSPNS_DT
  --,sr.STORE_NUM
  ,ca.FSCL_YR_NUM
  ,ca.FSCL_PER_IN_YR_NUM
  ,TRUNC(sr.TRANS_DTM)
  ) sq
  
ON a.GUID_ID = sq.GUID_USER_ID

where c.SVC_INTRNL_RQST_NM = 'Redemption' 
  AND (b.FSCL_YR_NUM = 2017 and (b.FSCL_PER_IN_YR_NUM >= 7 AND b.FSCL_PER_IN_YR_NUM <= 10))
group by b.FSCL_YR_NUM
  ,b.FSCL_PER_IN_YR_NUM
  ,a.GUID_ID
  --,sq.RSPNS_DT
  --,sq.STORE_NUM
  --,sq.TOTAL_TB
  --,sq.TOTAL_RSPNS
ORDER BY 
  a.GUID_ID
  ,b.FSCL_YR_NUM
  ,b.FSCL_PER_IN_YR_NUM
