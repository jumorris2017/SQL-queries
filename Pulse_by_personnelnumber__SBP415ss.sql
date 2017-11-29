SELECT 
    --b.[PRTNR_ID] as PersonnelNumber
    --,a.[SendDate]
    --,a.[KillDate]
    --,a.[surveyRespDate]
	  --month(a.surveyRespDate) as survMonth
	  --,year(a.surveyRespDate) as survYear
    DATEPART(week, a.surveyRespDate) as survWeek
    ,a.[Question_ID]
    ,a.[RespID]
    ,c.[STORE_NUM_ASSIGNED]
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
    c.STORE_NUM_ASSIGNED
    ,a.Question_ID
    ,a.RespID
    ,DATEPART(week, a.surveyRespDate)
