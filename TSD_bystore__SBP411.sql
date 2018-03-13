--SBP411

 SELECT ca.FSCL_YR_NUM
        , SUM(f.CUST_TRANS_CNT) "CustTrans"
        , SUM(f.ACTIVE_STORE_DAY_CNT) "day_count"
        , ca.FSCL_PER_IN_YR_NUM
        , org.STORE_NUM
        , org.RGN_ORG_LVL_ID
        , org.RGN_ORG_LVL_DESCR
    FROM APPBUS.DFT_INTL_STORE_DAY_VW f      
        INNER JOIN APPDWH.DDM_RETAIL_ORG_STORE_DIST org
            ON f.STORE_NUMBER = org.STORE_NUM
            AND org.DIV_ORG_LVL_ID IN (3,6,106)  -- US CO stores
        INNER JOIN APPDWH.ADT_CAL ca
            ON f.BUSINESS_DATE = ca.CAL_DT
            AND ca.FSCL_YR_NUM = 2018 AND ca.FSCL_PER_IN_YR_NUM = 5
        INNER JOIN APPBUS.AFT_STORE_COMP_PER_VW comp
            ON f.STORE_NUMBER = comp.STORE_NUMBER
            AND ca.FSCL_PER_BEG_DT = comp.FISCAL_PERIOD_BEGIN_DATE
            AND COMP_CODE = 'Y'
    GROUP BY ca.FSCL_YR_NUM
    , ca.FSCL_PER_IN_YR_NUM
    , org.STORE_NUM
    , org.RGN_ORG_LVL_ID
    , org.RGN_ORG_LVL_DESCR