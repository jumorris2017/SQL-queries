SELECT 
    b.PRTNR_ID as PersonnelNumber
    --,a.[SendDate]
    --,a.[KillDate]
    --,a.[surveyRespDate]
	  --month(a.surveyRespDate) as survMonth
	  --,year(a.surveyRespDate) as survYear
    ,DATEPART(week, a.surveyRespDate) as survWeek
    ,a.Question_ID
    ,a.RespID
    ,c.STORE_NUM_ASSIGNED
    --,c.STORE_NUM_WORKED
    --,c.[JOB_ID]
    --,c.[RANK]
    --,c.[SHIFT_LEN_MIN]
	FROM Surveys.MOODRING.Survey_Responses a
	LEFT JOIN [Surveys].[MOODRING].[PDW_PRTNR_ID_HASH] b
		ON a.PartnerID = b.PRTNR_ID_MD5_HASHED
  LEFT JOIN [Surveys].[MOODRING].[Last_Matching_Shift] c
		ON (a.caseID = c.caseID AND a.surveyRespDate = c.surveyRespDate)
    WHERE a.surveyRespDate > '10/08/2017 11:59:59 PM'
  GROUP BY 
    b.PRTNR_ID
    ,c.STORE_NUM_ASSIGNED
    ,a.Question_ID
    ,a.RespID
    ,DATEPART(week, a.surveyRespDate)


/*only SMs*/
SELECT 
    b.PRTNR_ID as PersonnelNumber
    --,a.[SendDate]
    --,a.[KillDate]
    --,a.[surveyRespDate]
	  --month(a.surveyRespDate) as survMonth
	  --,year(a.surveyRespDate) as survYear
    ,DATEPART(week, a.surveyRespDate) as survWeek
    ,a.Question_ID
    ,a.RespID
    ,c.STORE_NUM_ASSIGNED
    --,c.STORE_NUM_WORKED
    --,c.[JOB_ID]
    --,c.[RANK]
    --,c.[SHIFT_LEN_MIN]
    ,j.Job_Key
	FROM Surveys.MOODRING.Survey_Responses a
	LEFT JOIN [Surveys].[MOODRING].[PDW_PRTNR_ID_HASH] b
		ON a.PartnerID = b.PRTNR_ID_MD5_HASHED
	LEFT JOIN [PRODM].[Org_and_Job_History] j
    ON b.PRTNR_ID = j.Personnel_Number
  LEFT JOIN [Surveys].[MOODRING].[Last_Matching_Shift] c
		ON (a.caseID = c.caseID AND a.surveyRespDate = c.surveyRespDate)
    WHERE a.surveyRespDate > '10/08/2017 11:59:59 PM'
    AND j.Job_Key = '50000117'  --sm
  GROUP BY 
    b.PRTNR_ID
    ,c.STORE_NUM_ASSIGNED
    ,a.Question_ID
    ,a.RespID
    ,DATEPART(week, a.surveyRespDate)
    ,j.Job_Key






/*

SELECT 
    --b.[PRTNR_ID] as PersonnelNumber
    --,a.[SendDate]
    --,a.[KillDate]
    --,a.[surveyRespDate]
	  --month(a.surveyRespDate) as survMonth
	  --,year(a.surveyRespDate) as survYear
    DATEPART(week, a.surveyRespDate) as survWeek
    ,a.[Question_ID]
    ,COUNT(a.[RespID])
    --,c.[STORE_NUM_ASSIGNED]
    --,c.[STORE_NUM_WORKED]
    --,c.[JOB_ID]
    --,c.[RANK]
    --,c.[SHIFT_LEN_MIN]
	FROM Surveys.MOODRING.Survey_Responses a
	LEFT JOIN [Surveys].[MOODRING].[PDW_PRTNR_ID_HASH] b
		on a.PartnerID = b.PRTNR_ID_MD5_HASHED
	LEFT JOIN [Surveys].[MOODRING].[Last_Matching_Shift] c
		on (a.caseID = c.caseID AND a.surveyRespDate = c.surveyRespDate)
    WHERE a.surveyRespDate > '10/08/2017 11:59:59 PM'
  GROUP BY 
    --b.PRTNR_ID
    --,c.STORE_NUM_ASSIGNED
    a.Question_ID
    --,a.RespID
    ,DATEPART(week, a.surveyRespDate)
   ORDER BY 
       DATEPART(week, a.surveyRespDate)
       ,a.Question_ID

*/
