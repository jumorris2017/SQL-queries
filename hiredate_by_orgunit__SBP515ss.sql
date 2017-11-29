/****** Script for SelectTopNRows command from SSMS  ******/
SELECT 
      a.[Fiscal_Month]
    , a.[Fiscal_Year]
	  , a.Action_Group
	  , a.Org_Unit
	  , count(a.[Personnel Number]) as tally
    , b.StoreAbbrev
  FROM [PDW_Bulk].[hpohlman].[PRO_BaseData] a
  	LEFT JOIN [PDW_Bulk].[PRODW].[ORG_TREE_DRAD]  b
		on a.Org_Unit = b.StoreOrgid
  WHERE a.Action_Group IN ('Hire', 'Rehire')
    AND a.Fiscal_Year = 'FY-2017'
  GROUP BY a.Org_Unit, a.Action_Group, a.Fiscal_Month, a.Fiscal_Year, b.StoreAbbrev
  Order by a.Fiscal_Month, a.Fiscal_Year, a.Org_Unit, a.Action_Group
  