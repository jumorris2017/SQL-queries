SELECT prtnr.[FY]
      ,prtnr.[FP]
      ,prtnr.[FP_End]
      ,prtnr.[Personnel Number]
      ,prtnr.[Status]
      ,prtnr.[Country]
      ,prtnr.[Company Code]
      ,prtnr.[Org_Unit]
      ,prtnr.[Org_Unit_Name]
	    ,prtnr.[Job_Number]
      ,prtnr.[Job_Title]
      ,prtnr.[Hire_Date]
      ,prtnr.[Job_Date]
      ,prtnr.[Position_Date]
	  , ROUND((DATEDIFF(DAY, prtnr.[Hire_Date], prtnr.[FP_End])/365.2425), 2) as 'SbuxTenure' /* limitations: lack of transparency into first hire date for rehires. */
	  , ROUND((DATEDIFF(DAY, prtnr.[Job_Date], prtnr.[FP_End])/365.2425), 2) as 'JobTenure'
	  , ROUND((DATEDIFF(DAY, prtnr.[Position_Date], prtnr.[FP_End])/365.2425), 2) as 'TimeInPosition'
	  ,org.[StoreAbbrev] as StoreNum
	  --,org.[StoreName]
      --,org.[DistrictOrgid]
      --,org.[DistrictAbbrev]
      --,org.[DistrictName]
      --,org.[AreaOrgid]
      --,org.[AreaAbbrev]
      --,org.[AreaName]
      --,org.[RegionOrgid]
      --,org.[RegionAbbrev]
      --,org.[RegionName]
      --,org.[DivisionOrgid]
      --,org.[DivisionAbbrev]
      --,org.[DivisionName]
  FROM [PDW_Bulk].[hpohlman].[PRO_BaseData] prtnr

   LEFT JOIN [PDW_Bulk].[PRODW].[ORG_TREE_DRAD] org
		on prtnr.Org_Unit = org.StoreOrgid
	WHERE prtnr.Country = 'US' 
    --AND prtnr.Status = 'Active'
		AND prtnr.FP = 5 AND prtnr.FY IN (2018)
		AND prtnr.Action_Group = 'Headcount'
		AND prtnr.Org_Segment = 'FMH'
		--AND prtnr.Job_Number IN (50000362, 50000358, 50000445, 50018175, 50000118) /* 50000117 is store manager */
