/* quartlery comps */
/* Comp = (net discounted sales - net discounted sales LY ) / net discounted sales LY */
/* COSD/TSD = customer transactions / day count */

 SELECT ca.FSCL_YR_NUM
         --, ca.FSCL_WK_IN_YR_NUM
        , ca.FSCL_QTR_IN_YR_NUM
        --, ca.FSCL_PER_IN_YR_NUM
        --, org.OWNR_TYPE_CD
        --, org.CNTRY_CD
        --, org.DRIVE_THRU_IND
        , f.STORE_NUMBER AS STORE_NUM
        , SUM(f.NET_DISCOUNTED_SALES_AMT) "Sales"
        , SUM(f.NET_DISCOUNTED_SALES_LY_AMT) "LYSales"
        --, ROUND((SUM(f.NET_DISCOUNTED_SALES_AMT) - SUM(f.NET_DISCOUNTED_SALES_LY_AMT)) / SUM(f.NET_DISCOUNTED_SALES_LY_AMT),4) AS salescomp
        --, SUM(f.CUST_TRANS_CNT) "CustTrans" 
        --, SUM(f.ACTIVE_STORE_DAY_CNT) "day_count" 
        --, ROUND(SUM(f.CUST_TRANS_CNT) / SUM(f.ACTIVE_STORE_DAY_CNT),1) AS tsd

    FROM APPBUS.DFT_INTL_STORE_DAY_VW f      
        
        INNER JOIN APPDWH.ADT_STORE org
            ON f.STORE_NUMBER = org.STORE_NUM
            AND org.OWNR_TYPE_CD IN ('CO')
            AND org.CNTRY_CD IN ('US')
        
        INNER JOIN APPDWH.ADT_CAL ca
            ON f.BUSINESS_DATE = ca.CAL_DT
            AND ( (ca.FSCL_YR_NUM = 2018 AND ca.FSCL_QTR_IN_YR_NUM IN (1,2,3)) OR (ca.FSCL_YR_NUM = 2017) OR
      (ca.FSCL_YR_NUM = 2016 AND ca.FSCL_QTR_IN_YR_NUM IN (4)) )
        
        INNER JOIN APPBUS.AFT_STORE_COMP_PER_VW comp
            ON f.STORE_NUMBER = comp.STORE_NUMBER
            AND ca.FSCL_PER_BEG_DT = comp.FISCAL_PERIOD_BEGIN_DATE
            AND COMP_CODE = 'Y'
    
    GROUP BY ca.FSCL_YR_NUM
    ,f.STORE_NUMBER
    --, ca.FSCL_YR_NUM
    --, ca.FSCL_WK_IN_YR_NUM
    , ca.FSCL_QTR_IN_YR_NUM
    --, ca.FSCL_PER_IN_YR_NUM
    --, org.OWNR_TYPE_CD
    --, org.CNTRY_CD
    --, org.DRIVE_THRU_IND



/*  
--active store day count
SELECT f.STORE_NUMBER
, SUM(f.ACTIVE_STORE_DAY_CNT) AS day_count

FROM APPBUS.DFT_INTL_STORE_DAY_VW f

INNER JOIN APPDWH.DDM_RETAIL_ORG_STORE_DIST org
  ON f.STORE_NUMBER = org.STORE_NUM
  AND org.DIV_ORG_LVL_ID IN (3,6,106)  -- US CO stores
        
INNER JOIN APPDWH.ADT_CAL ca
  ON f.BUSINESS_DATE = ca.CAL_DT
  AND ca.FSCL_YR_NUM = 2018
  AND ca.FSCL_QTR_IN_YR_NUM = 1
  
GROUP BY f.STORE_NUMBER
*/