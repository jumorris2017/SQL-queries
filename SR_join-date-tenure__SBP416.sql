select v.GUID_USER_ID
,TRUNC(v.MBR_CREATED_ON_DT) AS MBR_JOIN_DT
,v.TIER_NM 

from appca.D_MBR_CUST_VW v

INNER JOIN (SELECT S1.GUID_USER_ID,
          ca.FSCL_YR_NUM,
          ca.FSCL_PER_IN_YR_NUM

          FROM APPOTHER.AFT_CV_SRVY_RSPNS S1

          INNER JOIN APPCA.D_STORE_VERS v1
            ON v1.STORE_NUM = S1.STORE_NUM
            AND v1.CURRENT_FLG = 'Y' 
            AND v1.CNTRY_CD_2_DGT_ISO = 'US' 

           JOIN APPCA.D_CAL ca
             ON ca.CAL_DT = TRUNC(S1.TRANS_DTM)
    
           WHERE (ca.FSCL_YR_NUM = 2018 AND ca.FSCL_PER_IN_YR_NUM = 6) OR
           (ca.FSCL_YR_NUM = 2017 AND ca.FSCL_PER_IN_YR_NUM = 12) OR
           (ca.FSCL_YR_NUM = 2017 AND ca.FSCL_PER_IN_YR_NUM = 6) OR
           (ca.FSCL_YR_NUM = 2016 AND ca.FSCL_PER_IN_YR_NUM = 12) 
          
          GROUP BY S1.GUID_USER_ID,
          ca.FSCL_YR_NUM,
          ca.FSCL_PER_IN_YR_NUM
      ) sq
      
ON v.GUID_USER_ID = sq.GUID_USER_ID
      
GROUP BY
v.GUID_USER_ID
,TRUNC(v.MBR_CREATED_ON_DT)
,v.TIER_NM 