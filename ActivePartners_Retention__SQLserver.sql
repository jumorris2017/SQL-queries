/*Pulls active partners by PartnerID*/
SELECT * FROM [PDW_Bulk].[hpohlman].[PRO_BaseData]
WHERE [PDW_Bulk].[hpohlman].[PRO_BaseData].[Action_Group] = 'Headcount'
AND [PDW_Bulk].[hpohlman].[PRO_BaseData].[FY] = 2018
AND [PDW_Bulk].[hpohlman].[PRO_BaseData].[FP] = 4 


