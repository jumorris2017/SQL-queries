/* Pulls number of transactions by GUID */

select b.FSCL_YR_NUM
  ,b.FSCL_PER_IN_YR_NUM
  ,a.GUID_ID
  ,count(*) AS TRANS
  ,count(DISTINCT a.STORE_NUM) AS UNIQUE_STORES

from APPCA.F_SVC_FIN_TRANS a

inner join APPCA.D_CAL b 
  on a.BUS_DT = b.CAL_DT

inner join APPCA.D_SVC_TRANS_TYPE c 
  on a.SVC_TRANS_TYPE_KEY = c.SVC_TRANS_TYPE_KEY
  
JOIN APPCA.D_STORE_VERS st
  ON a.STORE_NUM = st.STORE_NUM
    AND st.CURRENT_FLG = 'Y'
    --AND st.OWNR_TYPE_CD = 'CO'
    AND st.CNTRY_CD_2_DGT_ISO = 'US'

WHERE b.FSCL_YR_NUM = 2018 AND b.FSCL_PER_IN_YR_NUM = 8
AND c.SVC_INTRNL_RQST_NM = 'Redemption' 

GROUP BY 
  a.GUID_ID
  ,b.FSCL_YR_NUM
  ,b.FSCL_PER_IN_YR_NUM