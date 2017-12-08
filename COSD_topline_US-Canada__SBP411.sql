/* query 1  -- Canada stores, customer trans count, active store day count, COSD by store number & month */
SELECT COUNTRY, DATE_MONTH, DATE_YEAR, CASE WHEN SUM(ACTIVE_STORE_DAY_CNT) = 0 THEN 0 ELSE ROUND(SUM(CUST_TRANS_CNT) / SUM(ACTIVE_STORE_DAY_CNT),2) END AS COSD
FROM 
(SELECT
      org.DIV_ORG_LVL_ID
      ,to_char(m.BUSINESS_DATE,'MM') AS DATE_MONTH
      ,to_char(m.BUSINESS_DATE,'YYYY') AS DATE_YEAR

  -- Sum customer occassions
  ,SUM(m.CUST_TRANS_CNT) AS CUST_TRANS_CNT
  ,SUM(m.ACTIVE_STORE_DAY_CNT) AS ACTIVE_STORE_DAY_CNT
  -- COSD
  --,CASE WHEN SUM(m.ACTIVE_STORE_DAY_CNT) = 0 THEN 0 ELSE ROUND(SUM(m.CUST_TRANS_CNT) / SUM(m.ACTIVE_STORE_DAY_CNT),2) END AS cosd
  
  --CREATE COUNTRY INDICATOR
  , CASE WHEN org.DIV_ORG_LVL_ID IN (2) THEN 'CA' ELSE 'US' END AS COUNTRY

FROM APPBUS.DFT_INTL_STORE_DAY_VW m

  INNER JOIN APPDWH.DDM_RETAIL_ORG_STORE_DIST org
    ON m.STORE_NUMBER = org.STORE_NUM
    AND org.DIV_ORG_LVL_ID IN (1,3,106,2)  -- U.S. & Canada stores

WHERE m.BUSINESS_DATE >= '10-OCT-16'

GROUP BY
    org.DIV_ORG_LVL_ID
    ,to_char(m.BUSINESS_DATE,'MM')
    ,to_char(m.BUSINESS_DATE,'YYYY') 
) 
GROUP BY COUNTRY, DATE_MONTH, DATE_YEAR
ORDER BY COUNTRY, DATE_YEAR ASC, DATE_MONTH
;


/* query 2 -- quarterly comps for Canada */
SELECT FISCAL_YEAR_NUMBER, (ROUND(SUM(COMPS),3)) AS comps
FROM
(SELECT f.FISCAL_YEAR_NUMBER
        --, f.STORE_NUMBER
        , SUM(f.SALES_AMT) "MonthlySales"
        , SUM(f.TRANS_CNT) "MonthlyTrans"
        , SUM(f.SALES_LY_AMT) "LYMonthlySales"
        , SUM(f.TRANS_LY_CNT) "LYMonthlyTrans"
        , ((SUM(f.SALES_AMT)-SUM(f.SALES_LY_AMT))/SUM(f.SALES_LY_AMT)) "COMPS"
        --, comp.COMP_CODE
        --, org.DIV_ORG_LVL_ID
        --, f.FISCAL_WEEK_NUMBER
    FROM APPBUS.USR_CO_STORE_WEEK_HOUR_RPT f      
        INNER JOIN APPDWH.DDM_RETAIL_ORG_STORE_DIST org
            ON f.STORE_NUMBER = org.STORE_NUM
            AND org.DIV_ORG_LVL_ID IN (2)  -- Canada stores
        INNER JOIN APPBUS.AFT_STORE_COMP_PER_VW comp
            ON f.STORE_NUMBER = comp.STORE_NUMBER
            AND COMP_CODE = 'Y'
    WHERE f.FISCAL_YEAR_NUMBER > 2016
        AND f.FISCAL_WEEK_NUMBER BETWEEN '41' AND '52'
    GROUP BY f.FISCAL_YEAR_NUMBER
) 
GROUP BY FISCAL_YEAR_NUMBER
;

/* query 3 -- regression (outcome = CC) based on % home store, average partner tenure, and COSD */