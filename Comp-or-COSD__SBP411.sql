/* quartlery comps */
/* Comp = (net discounted sales - net discounted sales LY ) / net discounted sales LY */
/* COSD/TSD = customer transactions / day count */

 SELECT ca.FSCL_YR_NUM
         --, ca.FSCL_WK_IN_YR_NUM
        , ca.FSCL_QTR_IN_YR_NUM
        --, ca.FSCL_PER_IN_YR_NUM
        --, org.RGN_ORG_LVL_DESCR
        --, f.STORE_NUMBER
        , SUM(f.NET_DISCOUNTED_SALES_AMT) "Sales"
        , SUM(f.NET_DISCOUNTED_SALES_LY_AMT) "LYSales"
        , ROUND((SUM(f.NET_DISCOUNTED_SALES_AMT) - SUM(f.NET_DISCOUNTED_SALES_LY_AMT)) / SUM(f.NET_DISCOUNTED_SALES_LY_AMT),4) AS salescomp
        , SUM(f.CUST_TRANS_CNT) "CustTrans" -- for COSD/TSDs
        , SUM(f.ACTIVE_STORE_DAY_CNT) "day_count" -- for COSD/TSDs
        , ROUND(SUM(f.CUST_TRANS_CNT) / SUM(f.ACTIVE_STORE_DAY_CNT),1) AS tsd

    FROM APPBUS.DFT_INTL_STORE_DAY_VW f      
        
        INNER JOIN APPDWH.DDM_RETAIL_ORG_STORE_DIST org
            ON f.STORE_NUMBER = org.STORE_NUM
            AND org.DIV_ORG_LVL_ID IN (3,6,106)  -- US CO stores
        
        INNER JOIN APPDWH.ADT_CAL ca
            ON f.BUSINESS_DATE = ca.CAL_DT
            AND ca.FSCL_YR_NUM in (2017,2018) 
            --OR (ca.FSCL_YR_NUM = 2016 AND ca.FSCL_PER_IN_YR_NUM >= 6))
            AND ca.FSCL_QTR_IN_YR_NUM = 2
            --AND f.BUSINESS_DATE BETWEEN '01-SEP-17' AND '30-NOV-17'
            --AND ca.FSCL_WK_IN_YR_NUM BETWEEN 41 AND 52
        
        INNER JOIN APPBUS.AFT_STORE_COMP_PER_VW comp
            ON f.STORE_NUMBER = comp.STORE_NUMBER
            AND ca.FSCL_PER_BEG_DT = comp.FISCAL_PERIOD_BEGIN_DATE
            AND COMP_CODE = 'Y'
    
    GROUP BY ca.FSCL_YR_NUM
    --f.STORE_NUMBER
    --, ca.FSCL_YR_NUM
    --, ca.FSCL_WK_IN_YR_NUM
    , ca.FSCL_QTR_IN_YR_NUM
    --, ca.FSCL_PER_IN_YR_NUM
    --,org.RGN_ORG_LVL_DESCR
    

    
    
    
    
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