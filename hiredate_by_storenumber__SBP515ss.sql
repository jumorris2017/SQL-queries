USE PDW_Bulk
GO

SELECT --TOP 100
  COUNT(m.Personnel_Number) AS newhires
  --,m.Move_Date
  ,m.FSCL_YR_NUM
  ,m.FSCL_WK_IN_YR_NUM
  ,m.Move_Type
  --,m.ActionGroupId
  --,ACTION_GROUPS.ActionGroupName
  ,APPDWH.ADT_STORE.STORE_NUM
  --,m.[Action Type]
  --,m.[Reason for Action]
  --,rfa.[Reason for Action Text]
  --,m.Old_Job_Key
  --,mt.Position_Tenure / 365.25 AS 'Position Tenure (yrs)'
  --,mt.Job_Tenure / 365.25 AS 'Job Tenure (yrs)'
  --,mt.Company_Tenure / 365.25 AS 'Company Tenure (yrs)'
FROM PRODM.Movements m

INNER JOIN PRODM.Stores_by_Org
  ON m.New_Organizational_Unit = PRODM.Stores_by_Org.OrgID
  
INNER JOIN APPDWH.ADT_STORE 
    ON PRODM.Stores_by_Org.StoreNumNum = APPDWH.ADT_STORE.STORE_NUM
      AND APPDWH.ADT_STORE.CNTRY_CD = 'US'
      AND APPDWH.ADT_STORE.STORE_BRAND_CD = 'SBUX'	-- Starbucks branded stores
    	AND APPDWH.ADT_STORE.OWNR_TYPE_CD = 'CO'		-- company-owned stores
/*
LEFT JOIN PRODW.ACTION_GROUPS
  ON m.ActionGroupId = PRODW.ACTION_GROUPS.ActionGroupId
  
LEFT JOIN dbo.[REASON FOR ACTION TEXTS] rfa
  ON m.[Action Type] = rfa.[Action Type]
    AND m.[Reason for Action] = rfa.[Reason for Action]
    
LEFT JOIN PRODM.Movements_Tenure_Detail mt
  ON m.Move_Date = mt.Move_Date
    AND m.Personnel_Number = mt.Personnel_Number
*/
WHERE m.New_Job_Key = '50000362'
  AND m.FSCL_YR_NUM = 2018
  AND m.Move_Type = 1  -- external hires
 
GROUP BY
APPDWH.ADT_STORE.STORE_NUM
--,m.Move_Date
,m.FSCL_YR_NUM
,m.FSCL_WK_IN_YR_NUM
,m.Move_Type
ORDER BY
m.Move_Type

