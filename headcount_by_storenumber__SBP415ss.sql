--SELECT Org_ID, Job_Key, Period_Name, Period_Type, Partner_Headcount_Period, Org_Days_In_Period, EOP_Headcount, Avg_Headcount_Period, Turnover_Period, Term_Rate_Period, SOP_Headcount, Partner_Headcount_RY, Avg_Headcount_RY, Turnover_RY, Term_Rate_RY, Org_Days_In_RY 

SELECT
  --Period_Name
  FSCL_QTR_IN_YR_NUM
  ,FSCL_WK_IN_QTR_NUM
  ,AVG(Avg_Headcount_Period) AS 'AvgHeadcount'
  --,COUNT(DISTINCT STORE_NUM) AS 'Store_Cnt'
  ,STORE_NUM
FROM (
SELECT
   --Job_Key
  --,Period_Name
  c.FSCL_WK_IN_QTR_NUM
  ,c.FSCL_QTR_IN_YR_NUM
  ,s.STORE_NUM
  --,Partner_Headcount_Period
  --,Org_Days_In_Period
  ,EOP_Headcount
  ,Avg_Headcount_Period
  --,Turnover_Period
  --,Term_Rate_Period
  --,SOP_Headcount
FROM PDW_Bulk.PRODM.Org_Job_Measures ojm

INNER JOIN PDW_Bulk.PRODM.Stores_by_Org sbo
  ON ojm.Org_ID = sbo.OrgID

INNER JOIN PDW_Bulk.APPDWH.ADT_STORE s
	ON sbo.StoreNumNum = s.STORE_NUM
		AND s.CNTRY_CD = 'US'			-- country = US
		AND s.STORE_BRAND_CD = 'SBUX'	-- Starbucks branded stores
		AND s.OWNR_TYPE_CD = 'CO'		-- company-owned stores
    
INNER JOIN PDW_Bulk.PRODW.Calendar c
  ON c.FSCL_PER_END_IND = 'Y'
    AND ojm.Period_Name = c.FSCL_PER_IN_YR_CD
    AND c.FSCL_YR_NUM = 2018
    AND c.FSCL_QTR_IN_YR_NUM = 1
    AND c.FSCL_WK_IN_QTR_NUM BETWEEN 1 AND 8
    
WHERE ojm.Period_Type = 'FM'
  AND ojm.Job_Key = '50000362'  -- barista only
  AND ojm.Org_Days_In_Period > 0
  AND ojm.Partner_Headcount_Period > 0
) a

GROUP BY
  -- Period_Name
  FSCL_QTR_IN_YR_NUM
  ,FSCL_WK_IN_QTR_NUM
  ,STORE_NUM
ORDER BY
  FSCL_QTR_IN_YR_NUM
  ,FSCL_WK_IN_QTR_NUM
  ,STORE_NUM

