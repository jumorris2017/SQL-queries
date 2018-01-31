/*Pulls active partners by PartnerID*/
SELECT * FROM [PDW_Bulk].[hpohlman].[PRO_BaseData]
WHERE [PDW_Bulk].[hpohlman].[PRO_BaseData].[Action_Group] = 'Headcount'
AND [PDW_Bulk].[hpohlman].[PRO_BaseData].[FY] = 2018
AND [PDW_Bulk].[hpohlman].[PRO_BaseData].[FP] = 4 



/*SM Tenure*/
SELECT a.[Personnel Number], b.STORE_NUM, b.STORE_MGR_FULL_NM FROM [PDW_Bulk].[hpohlman].[PRO_BaseData] a
  LEFT JOIN APPDWH.ADT_STORE b
    --on a.Org_Unit_Name = b.SM_STORE_NM
    on b.STORE_MGR_FULL_NM
  WHERE b.STORE_NUM IN (9233,14629,508,29701,12900)
  AND a.[Action_Group] = 'Headcount'
  AND a.[FY] = 2018
  AND a.[FP] = 4
  AND a.Job_Number = 50000117


SELECT STORE_NUM, STORE_MGR_FULL_NM FROM APPDWH.ADT_STORE
WHERE STORE_NUM IN (9233,14629,508,29701,12900)

select * from [PDW_Bulk].[hpohlman].[PRO_BaseData] a
WHERE a.[Last Name] in ('Cataldo', 'Martinez', 'Hernandex', 'Snaer', 'Medina')
  AND a.[Action_Group] = 'Headcount'
  AND a.[FY] = 2018
  AND a.[FP] = 4
  AND a.Job_Number = 50000117