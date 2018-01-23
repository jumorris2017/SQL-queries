--SELECT TOP 100 caseID, SendDate, KillDate, surveyRespDate, Question_ID, RespID, STORE_NUM_ASSIGNED, STORE_NUM_WORKED, JOB_ID, BUS_DT, START_DTM_LCL, END_DTM_LCL, [RANK], SHIFT_LEN_MIN, LoadDate 

SELECT
   Question_ID
  ,JOB_ID
  ,CASE
    WHEN (CAST(START_DTM_LCL AS time) >= CONVERT(time,'06:00:00',114) AND CAST(END_DTM_LCL AS time) <= CONVERT(time,'10:00:00',114)) THEN 'Morning'
    WHEN (CAST(START_DTM_LCL AS time) >= CONVERT(time,'10:00:00',114) AND CAST(END_DTM_LCL AS time) <= CONVERT(time,'15:00:00',114)) THEN 'Midday'
    WHEN (CAST(START_DTM_LCL AS time) >= CONVERT(time,'15:00:00',114) AND CAST(END_DTM_LCL AS time) <= CONVERT(time,'20:00:00',114)) THEN 'Evening'
    ELSE 'Other'
   END AS Shift_Daypart
  ,SUM(CASE WHEN RespID = '7' THEN 1 ELSE 0 END) AS TB_Count
  ,COUNT(*) AS Resp_Count
FROM Surveys.MOODRING.Last_Matching_Shift

WHERE BUS_DT >= '15-MAY-17'  -- national launch

GROUP BY
   Question_ID
  ,JOB_ID
  ,CASE
    WHEN (CAST(START_DTM_LCL AS time) >= CONVERT(time,'06:00:00',114) AND CAST(END_DTM_LCL AS time) <= CONVERT(time,'10:00:00',114)) THEN 'Morning'
    WHEN (CAST(START_DTM_LCL AS time) >= CONVERT(time,'10:00:00',114) AND CAST(END_DTM_LCL AS time) <= CONVERT(time,'15:00:00',114)) THEN 'Midday'
    WHEN (CAST(START_DTM_LCL AS time) >= CONVERT(time,'15:00:00',114) AND CAST(END_DTM_LCL AS time) <= CONVERT(time,'20:00:00',114)) THEN 'Evening'
    ELSE 'Other'
   END
   
   
