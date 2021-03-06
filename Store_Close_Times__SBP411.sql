
SELECT hr.STORE_NUMBER, hr.PERM_BUSINESS_CLOSE_TIME, c.CAL_DT
  FROM APPDWH.ADT_CAL c
LEFT JOIN APPBUS.ADT_STORE_HR_VW hr
  ON c.DAY_IN_CAL_WK_NUM = hr.DAY_OF_WEEK_NUMBER
LEFT JOIN APPDWH.ADT_STORE s
  ON hr.STORE_NUMBER = s.STORE_NUM
WHERE c.CAL_DT BETWEEN '16-APR-18' AND '24-JUN-18'
  AND s.AREA_NUM IN (121,137)



--TaskEvolved labor addition testing
--SBP415
SELECT
  a.PRTNR_ID
  ,a.QUESTION_ID
  ,a.RESP_ID
  ,s.RGN_NUM
  ,s.AREA_NUM
  ,a.STORE_NUM
  ,a.JOB_ID
  ,a.SHIFT_START_DTM_LCL
  ,a.SHIFT_END_DTM_LCL
  ,a.TPERIOD
FROM
(
SELECT
   sr.CASE_ID
  ,sr.PRTNR_ID
  ,sr.SEND_DT
  ,sr.KILL_DT
  ,sr.SURVEY_RESP_DT_ORIG
  ,sr.QUESTION_ID
  ,sr.RESP_ID
  ,prtnr.SAP_PRTNR_ID
  ,CASE WHEN h.ORG_ID IS NULL THEN 'Y' ELSE 'N' END AS SEPARATED_IND
  ,h.ORG_ID AS ASSIGNED_ORG_ID
  ,org.ORG_UNIT_TYPE_NM AS ASSIGNED_ORG_TYPE
  ,org.ORG_ABBREV_NM AS ASSIGNED_ORG_ABBREV
  ,tp.STORE_NUM
  ,tp.JOB_ID
  ,tp.SHIFT_ROLE_ID
  ,tp.TZ_MISSING_IND
  ,tp.SHIFT_START_DTM_LCL
  ,tp.SHIFT_END_DTM_LCL
  ,tp.SHIFT_START_DTM_PT
  ,tp.SHIFT_END_DTM_PT
  ,tp.TPERIOD
  ,ROW_NUMBER() OVER(PARTITION BY sr.CASE_ID, sr.QUESTION_ID ORDER BY tp.SHIFT_END_DTM_PT DESC) AS SHIFT_RANK
FROM SRV_DDV_RESPONSE sr

/* Get SAP Partner ID as that is what GLS uses. */
INNER JOIN D_PRTNR_VERS prtnr
  ON sr.PRTNR_ID = prtnr.PRTNR_ID
    AND prtnr.SNAPSHOT_DT = (SELECT MAX(SNAPSHOT_DT) FROM D_COST_CTR_VERS)
    AND TRUNC(sr.SURVEY_RESP_DT_ORIG) BETWEEN prtnr.EFF_FROM_DT AND prtnr.EFF_TO_DT

/* Get assigned org info as of survey response date. If partner separated this will be null, so use left join so these responses aren't excluded. */
LEFT JOIN F_HEADCOUNT_VERS h
  ON sr.PRTNR_ID = h.PRTNR_ID
    AND h.SNAPSHOT_DT = (SELECT MAX(SNAPSHOT_DT) FROM D_COST_CTR_VERS)
    AND TRUNC(sr.SURVEY_RESP_DT_ORIG) BETWEEN h.EFF_FROM_DT AND h.EFF_TO_DT
    
/* Get assigned org details. */
LEFT JOIN D_ORG_VERS org
  ON h.ORG_VERS_KEY = org.ORG_VERS_KEY

/* Timezone- and daylight savings-aware conversion of GLS timepunches from local time to Pacific Time
 * (Survey timestamps are in AMERICA/LOS_ANGELES timezone)
 */
LEFT JOIN (  --begin tp
  SELECT
     gls.PRTNR_NUM
    ,s.STORE_NUM
    ,gls.JOB_ID
    ,gls.SHIFT_ROLE_ID
    ,gls.BUS_DT
    ,gls.TPERIOD
    ,NVL(tzr.TZNAME,'AMERICA/LOS_ANGELES') AS TZNAME
     /* If there isn't a valid time zone in ADT_STORE assume AMERICA/LOS_ANGELES but flag it here */
    ,CASE WHEN tzr.TZNAME IS NULL THEN 'Y' ELSE 'N' END AS TZ_MISSING_IND
    ,gls.SAFE_START_DTM AS SHIFT_START_DTM_LCL
    ,CAST((FROM_TZ(CAST(gls.SAFE_START_DTM AS TIMESTAMP),NVL(tzr.TZNAME,'AMERICA/LOS_ANGELES')) AT TIME ZONE ('UTC'))AS DATE) AS SHIFT_START_DTM_UTC 
    ,CAST((FROM_TZ(CAST(gls.SAFE_START_DTM AS TIMESTAMP),NVL(tzr.TZNAME,'AMERICA/LOS_ANGELES')) AT TIME ZONE ('AMERICA/LOS_ANGELES'))AS DATE) AS SHIFT_START_DTM_PT
    ,gls.SAFE_END_DTM AS SHIFT_END_DTM_LCL
    ,CAST((FROM_TZ(CAST(gls.SAFE_END_DTM AS TIMESTAMP),NVL(tzr.TZNAME,'AMERICA/LOS_ANGELES')) AT TIME ZONE ('UTC'))AS DATE) AS SHIFT_END_DTM_UTC
    ,CAST((FROM_TZ(CAST(gls.SAFE_END_DTM AS TIMESTAMP),NVL(tzr.TZNAME,'AMERICA/LOS_ANGELES')) AT TIME ZONE ('AMERICA/LOS_ANGELES'))AS DATE) AS SHIFT_END_DTM_PT
  FROM
    (SELECT
       PRTNR_NUM
      ,STORE_NUM
      ,JOB_ID
      ,SHIFT_ROLE_ID
      ,BUS_DT
      ,(CASE WHEN BUS_DT BETWEEN '16-APR-18' AND '20-MAY-18' THEN '0_PRE'
        WHEN BUS_DT BETWEEN '21-MAY-18' AND '24-JUN-18' THEN '1_POST' ELSE 'NA' END) AS TPERIOD
      /* Check to make sure the start timestamp isn't during the leap forward at the start of DST, becasue it will crash the time zone conversion */
      ,TO_DATE(TO_CHAR(START_DTM,'YYYYMMDD') || 
        LPAD(
          CASE
            WHEN EXTRACT(MONTH FROM START_DTM) = 3
              AND TRUNC(START_DTM) = NEXT_DAY(NEXT_DAY(TRUNC(START_DTM, 'MM') - 1, 'Sunday'), 'Sunday')  --DST starts on 2nd Sunday
              AND TO_NUMBER(TO_CHAR(START_DTM, 'HH24MISS')) BETWEEN 20000 AND 25959
            THEN TO_NUMBER(TO_CHAR(START_DTM, 'HH24MISS')) - 10000
            ELSE TO_NUMBER(TO_CHAR(START_DTM, 'HH24MISS'))
          END
        ,6,0)
       ,'YYYYMMDDHH24MISS') AS SAFE_START_DTM
      /* Same for the end timestamp; Subtract from start timestamp and add to end timestamp so start time is never after end time */
      ,TO_DATE(TO_CHAR(END_DTM,'YYYYMMDD') || 
        LPAD(
          CASE
            WHEN EXTRACT(MONTH FROM END_DTM) = 3
              AND TRUNC(END_DTM) = NEXT_DAY(NEXT_DAY(TRUNC(END_DTM, 'MM') - 1, 'Sunday'), 'Sunday')  --DST starts on 2nd Sunday
              AND TO_NUMBER(TO_CHAR(END_DTM, 'HH24MISS')) BETWEEN 20000 AND 25959
            THEN TO_NUMBER(TO_CHAR(END_DTM, 'HH24MISS')) + 10000
            ELSE TO_NUMBER(TO_CHAR(END_DTM, 'HH24MISS'))
          END
        ,6,0)
       ,'YYYYMMDDHH24MISS') AS SAFE_END_DTM
      ,START_RSN_NM
      ,END_RSN_NM
    FROM APPDWH.AFT_GLS_PRTNR_TMCARD@SBP411

    WHERE CNTRY_CD = 'US'
      AND BUS_DT BETWEEN '16-APR-18' AND '24-JUN-18' -- TASKEVOLVED TEST
      --AND STORE_NUM IN (6565,9554,9692,10436) -- TASKEVOLVED TEST
    ) gls
    
  INNER JOIN APPDWH.ADT_STORE@SBP411 s
    ON gls.STORE_NUM = s.STORE_NUM

  /* Get official time zone descriptions to make sure everything from ADT_STORE is valid */
  LEFT JOIN (SELECT DISTINCT TZNAME FROM V$TIMEZONE_NAMES) tzr
    ON REGEXP_SUBSTR(s.TIME_ZONE_DESCR,'[^ ]+', 1, 2) = tzr.TZNAME
) tp
  ON prtnr.SAP_PRTNR_ID = tp.PRTNR_NUM
    AND sr.SURVEY_RESP_DT_ORIG > tp.SHIFT_START_DTM_PT
    AND sr.SURVEY_RESP_DT_ORIG <= tp.SHIFT_END_DTM_PT + 7  --shift has to have been in the last 7 days (tried using BUS_DT but doens't seem to be any faster)

--WHERE TRUNC(sr.SURVEY_RESP_DT_ORIG) = '09-JUN-17'  --for testing
) a

--INNER JOIN APPPDW.ADV_CAL c
--  ON TRUNC(a.SHIFT_END_DTM_LCL) = c.CAL_DT
  
LEFT JOIN APPPDW.ADV_STORE s
  ON a.STORE_NUM = s.STORE_NUM
    AND s.SNAPSHOT_DT = (SELECT MAX(SNAPSHOT_DT) FROM D_COST_CTR_VERS)

WHERE a.SHIFT_RANK = 1  --most recent shift prior to survey response date/time
  AND a.QUESTION_ID = 'Q1'
  AND s.AREA_NUM IN (121,137)
  --AND TO_NUMBER(TO_CHAR(a.SHIFT_START_DTM_LCL, 'HH24MISS')) <= 155000 AND TO_NUMBER(TO_CHAR(a.SHIFT_END_DTM_LCL, 'HH24MISS')) >= 155000 -- TASKEVOLVED TEST

GROUP BY a.PRTNR_ID
  ,a.QUESTION_ID
  ,a.RESP_ID
  ,s.RGN_NUM
  ,s.AREA_NUM
  ,a.JOB_ID
  ,a.STORE_NUM
  ,a.TPERIOD
  ,a.SHIFT_START_DTM_LCL
  ,a.SHIFT_END_DTM_LCL
