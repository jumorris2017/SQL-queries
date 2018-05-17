--SBP411
--PCT BREWED AND BLENDED

SELECT a.STORE_NUMBER
, COUNT(*) AS PROD_TOTAL
, SUM(CASE WHEN b.DEPT_CODE = '006' THEN 1 ELSE 0 END) AS PROD_BREWED
, SUM(CASE WHEN b.DEPT_CODE = '007' THEN 1 ELSE 0 END) AS PROD_BLENDED

FROM APPBUS.DFT_INTL_STORE_PROD_WEEK_VW a

JOIN APPBUS.USR_ATTRIBUTES_PRODUCT b 
  ON a.ITEM_NUMBER = b.ITEM_NUMBER 

JOIN APPDWH.ADT_CAL ca
  ON ca.FSCL_WK_BEG_DT = a.FSCL_WK_BEG_DT
  AND ca.FSCL_YR_NUM = 2018
  AND ca.FSCL_QTR_IN_YR_NUM = 2

GROUP BY a.STORE_NUMBER