SELECT
  sbo.StoreNumNum AS Store_Num
  ,drad.AreaAbbrev
  ,ojh.Personnel_Number, ojh.Start_Date, ojh.End_Date, Organizational_Unit, Job_Key, Org_Unit_Start_Date, Org_Unit_End_Date, Job_Key_Start_Date, Job_Key_End_Date, Org_Job_Store_Start_Date, Org_Job_Store_End_Date, Org_Store_Start_Date, Org_Store_End_Date 
  ,DATEDIFF(day, Org_Job_Store_Start_Date, GETDATE()) / 365.25 AS 'SM_Tenure_Store_Yrs'
  ,DATEDIFF(day, Job_Key_Start_Date, GETDATE()) / 365.25 AS 'SM_Tenure_Yrs'
  ,DATEDIFF(day, ch.Start_Date, GETDATE()) / 365.25 AS 'Company_Tenure_Yrs'

FROM PDW_Bulk.PRODM.Org_and_Job_History ojh

INNER JOIN PDW_Bulk.[PRODM].[Stores_by_Org] sbo
		ON ojh.Organizational_Unit = sbo.OrgID
    
INNER JOIN PDW_Bulk.PRODW.ORG_TREE_DRAD drad
  ON ojh.Organizational_Unit = drad.BaseOrgid
    AND drad.AreaAbbrev IN ('A010','A037')

INNER JOIN PDW_Bulk.PRODM.Company_History ch
		ON ojh.Personnel_Number = ch.Personnel_Number
			AND CONVERT(date, GETDATE()) BETWEEN ch.Start_Date AND ch.End_Date

WHERE ojh.Job_Key = '50000117'  -- Store Managers
  AND CONVERT(date, GETDATE()) BETWEEN ojh.Start_Date AND ojh.End_Date 
