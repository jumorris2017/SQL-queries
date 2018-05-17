--/* CAW */
--SBP416
--PEAK TRANSACTIONS ONLY

WITH SQ AS
      (SELECT
         POSH.GUID_ID
        ,STR.STORE_NUM
        ,ca.FSCL_YR_NUM
        ,ca.FSCL_PER_IN_YR_NUM
        ,COUNT(DISTINCT POSH.TRANS_ID) AS N_TRANS

      FROM APPCA.F_POS_HDR POSH
      
      LEFT OUTER JOIN APPCA.D_STORE_VERS STR
        ON POSH.STORE_VERS_KEY = STR.STORE_VERS_KEY
      
      INNER JOIN APPCA.D_CAL ca
        ON POSH.BUS_DT = ca.CAL_DT
      
      WHERE STR.OWNR_TYPE_CD = 'CO'
        AND STR.CNTRY_CD_2_DGT_ISO = 'US'
        AND POSH.GROSS_REV_LCL_AMT > 0
        AND ca.FSCL_YR_NUM = 2018 
        AND ca.FSCL_PER_IN_YR_NUM = 4
        AND POSH.TRANS_END_TM_KEY BETWEEN 70000 AND 95959 
        
      GROUP BY
         POSH.GUID_ID
        ,STR.STORE_NUM
        ,ca.FSCL_YR_NUM
        ,ca.FSCL_PER_IN_YR_NUM
        ), SQ2 AS
(SELECT 
 SQ.STORE_NUM
,SQ.FSCL_YR_NUM
,SQ.FSCL_PER_IN_YR_NUM
,(CASE WHEN SQ.N_TRANS < 26 THEN 0 WHEN SQ.N_TRANS >= 26 THEN 1 END) AS TRANS_CUST_26
,(CASE WHEN SQ.N_TRANS < 20 THEN 0 WHEN SQ.N_TRANS >= 20 THEN 1 END) AS TRANS_CUST_20
,(CASE WHEN SQ.N_TRANS < 15 THEN 0 WHEN SQ.N_TRANS >= 15 THEN 1 END) AS TRANS_CUST_15
,(CASE WHEN SQ.N_TRANS < 10 THEN 0 WHEN SQ.N_TRANS >= 10 THEN 1 END) AS TRANS_CUST_10
,(CASE WHEN SQ.N_TRANS < 5 THEN 0 WHEN SQ.N_TRANS >= 5 THEN 1 END) AS TRANS_CUST_5
,SQ.GUID_ID

FROM SQ

GROUP BY
 SQ.STORE_NUM
,SQ.FSCL_YR_NUM
,SQ.FSCL_PER_IN_YR_NUM
,SQ.N_TRANS
,SQ.GUID_ID)
SELECT 
 SQ2.STORE_NUM
,SQ2.FSCL_YR_NUM
,SQ2.FSCL_PER_IN_YR_NUM
,SUM(CASE WHEN SQ2.TRANS_CUST_26 = 1 THEN 1 ELSE 0 END) AS N_CUST_26
,SUM(CASE WHEN SQ2.TRANS_CUST_20 = 1 THEN 1 ELSE 0 END) AS N_CUST_20
,SUM(CASE WHEN SQ2.TRANS_CUST_15 = 1 THEN 1 ELSE 0 END) AS N_CUST_15
,SUM(CASE WHEN SQ2.TRANS_CUST_10 = 1 THEN 1 ELSE 0 END) AS N_CUST_10
,SUM(CASE WHEN SQ2.TRANS_CUST_5 = 1 THEN 1 ELSE 0 END) AS N_CUST_5
,COUNT(SQ2.GUID_ID) AS N_CUST
FROM SQ2
GROUP BY 
 SQ2.STORE_NUM
,SQ2.FSCL_YR_NUM
,SQ2.FSCL_PER_IN_YR_NUM