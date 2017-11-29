SELECT
  -- [CAL_DT]
  --,[FSCL_QTR_IN_YR_NUM]
  --,[FSCL_YR_NUM]
  [StoreNumNum] AS [Store Num]
  ,AVG([Company Tenure Years]) AS [Avg Compnay Tenure]
  
FROM (
SELECT 
	 [PRODM].[Org_and_Job_History].[Personnel_Number]
  ,[PRODM].[Org_and_Job_History].[Job_Key]
  ,ch.[Start_Date]
  ,[PRODW].[Calendar].[CAL_DT]
  ,[PRODW].[Calendar].[FSCL_QTR_IN_YR_NUM]
  ,(SELECT * FROM [PRODM].[YearsAsOf]
		(ch.[Start_Date],[PRODW].[Calendar].[CAL_DT])
	) AS [Company Tenure Years] 
	,[PRODM].[Stores_by_Org].[StoreNumNum]
FROM [PDW_Bulk].[PRODM].[Org_and_Job_History]
  
INNER JOIN [PDW_Bulk].[PRODW].[Calendar]
  ON [PDW_Bulk].[PRODW].[Calendar].[CAL_DT] BETWEEN [Org_and_Job_History].[Start_Date] AND [Org_and_Job_History].[End_Date]
    AND [PDW_Bulk].[PRODW].[Calendar].[FSCL_QTR_IN_YR_NUM] = 4
    AND [PDW_Bulk].[PRODW].[Calendar].[FSCL_YR_NUM] = 2017
    --AND [PDW_Bulk].[PRODW].[Calendar].[CAL_DT] < '03-01-2017'
  
INNER JOIN [PDW_Bulk].[PRODM].[Stores_by_Org]
  ON [PRODM].[Org_and_Job_History].[Organizational_Unit] = [PRODM].[Stores_by_Org].[OrgID]

INNER JOIN [APPDWH].[ADT_STORE] 
	ON [PRODM].[Stores_by_Org].[StoreNumNum] = [APPDWH].[ADT_STORE].[STORE_NUM]
		AND [APPDWH].[ADT_STORE].[CNTRY_CD] = 'CA'			-- country = US
		AND [APPDWH].[ADT_STORE].[STORE_BRAND_CD] = 'SBUX'	-- Starbucks branded stores
		AND [APPDWH].[ADT_STORE].[OWNR_TYPE_CD] = 'CO'		-- company-owned stores

LEFT JOIN [PRODM].[Company_History] ch
	ON [PRODM].[Org_and_Job_History].[Personnel_Number]= ch.[Personnel_Number]
			AND
		[PRODW].[Calendar].[CAL_DT] BETWEEN ch.[Start_Date] AND ch.[End_Date]
--    (SELECT [PRODW].[Calendar].[CAL_DT]
--    FROM [PDW_Bulk].[PRODW].[Calendar]
--    WHERE [PRODW].[Calendar].[FSCL_QTR_IN_YR_NUM] = 'FP-2016-9'
--      AND [PRODW].[Calendar].FSCL_PER_END_IND = 'Y') BETWEEN ch.[Start_Date] AND ch.[End_Date]

WHERE
  [PRODM].[Org_and_Job_History].[Job_Key] IN ('50000362','50000358','50018175','50000445')  --hourly positions
  --[PRODM].[Org_and_Job_History].[Job_Key] = '50000117'  --sm
) a

GROUP BY
  -- [CAL_DT]
  --,[FSCL_QTR_IN_YR_NUM]
  --,[FSCL_YR_NUM]
  [StoreNumNum]
  
  
  
  
