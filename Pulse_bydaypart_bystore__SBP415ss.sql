/*export as "pulse_q1_byhour_bypartner_q1fy18.csv"*/
SELECT 
    b.PRTNR_ID as PRTNR_NUM
    --,DATEPART(week, a.surveyRespDate) as survWeek
    ,DATEPART(hour, c.START_DTM_LCL) as shift_start_hour
    ,c.BUS_DT
    ,(CASE WHEN a.Question_ID = 'Q1' THEN 1 ELSE 0 END) AS Q1_RESP
    ,(CASE WHEN a.RespID = 7 THEN 1 ELSE 0 END) AS Q1_TB
    ,c.STORE_NUM_ASSIGNED as STORE_NUM
	FROM Surveys.MOODRING.Survey_Responses a
	LEFT JOIN [Surveys].[MOODRING].[PDW_PRTNR_ID_HASH] b
		ON a.PartnerID = b.PRTNR_ID_MD5_HASHED
  LEFT JOIN [Surveys].[MOODRING].[Last_Matching_Shift] c
		ON (a.caseID = c.caseID AND a.surveyRespDate = c.surveyRespDate)
    WHERE c.BUS_DT > '10/01/2017 11:59:59 PM' AND c.BUS_DT <= '12/31/2017 11:59:59 PM'
    AND a.Question_ID = 'Q1'
  GROUP BY 
    b.PRTNR_ID 
    ,c.BUS_DT
    ,DATEPART(hour, c.START_DTM_LCL) 
    ,a.Question_ID
    ,a.RespID
    ,c.STORE_NUM_ASSIGNED
