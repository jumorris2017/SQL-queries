/*
--TaskEvolved labor addition testing
--SBP415
WITH SQ AS (
SELECT
  (CASE WHEN s.RGN_NUM = 12 AND s.AREA_NUM IN (121,137) THEN 'WestTest' 
    WHEN s.RGN_NUM = 12 AND s.AREA_NUM NOT IN (121,137) THEN 'WestElse' 
    WHEN s.RGN_NUM NOT IN (12) THEN 'RestofUS' ELSE 'NA' END) AS REGION
  ,a.PRTNR_ID
  ,a.QUESTION_ID
  ,a.RESP_ID
  ,s.RGN_NUM
  ,s.AREA_NUM
  ,a.STORE_NUM
  ,a.JOB_ID
  ,a.SHIFT_START_DTM_LCL
  ,a.SHIFT_END_DTM_LCL
  ,(CASE WHEN TO_NUMBER(TO_CHAR(a.SHIFT_START_DTM_LCL, 'HH24MISS')) <= 155000 AND TO_NUMBER(TO_CHAR(a.SHIFT_END_DTM_LCL, 'HH24MISS')) >= 155000 THEN 'PM'
    ELSE 'RESTOFDAY' END) AS DAYPART
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

-- Get SAP Partner ID as that is what GLS uses. 
INNER JOIN D_PRTNR_VERS prtnr
  ON sr.PRTNR_ID = prtnr.PRTNR_ID
    AND prtnr.SNAPSHOT_DT = (SELECT MAX(SNAPSHOT_DT) FROM D_COST_CTR_VERS)
    AND TRUNC(sr.SURVEY_RESP_DT_ORIG) BETWEEN prtnr.EFF_FROM_DT AND prtnr.EFF_TO_DT

-- Get assigned org info as of survey response date. If partner separated this will be null, so use left join so these responses aren't excluded. 
LEFT JOIN F_HEADCOUNT_VERS h
  ON sr.PRTNR_ID = h.PRTNR_ID
    AND h.SNAPSHOT_DT = (SELECT MAX(SNAPSHOT_DT) FROM D_COST_CTR_VERS)
    AND TRUNC(sr.SURVEY_RESP_DT_ORIG) BETWEEN h.EFF_FROM_DT AND h.EFF_TO_DT
    
-- Get assigned org details. 
LEFT JOIN D_ORG_VERS org
  ON h.ORG_VERS_KEY = org.ORG_VERS_KEY

-- Timezone- and daylight savings-aware conversion of GLS timepunches from local time to Pacific Time
LEFT JOIN (  --begin tp
  SELECT
     gls.PRTNR_NUM
    ,s.STORE_NUM
    ,gls.JOB_ID
    ,gls.SHIFT_ROLE_ID
    ,gls.BUS_DT
    ,gls.TPERIOD
    ,NVL(tzr.TZNAME,'AMERICA/LOS_ANGELES') AS TZNAME
     -- If there isn't a valid time zone in ADT_STORE assume AMERICA/LOS_ANGELES but flag it here 
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
        WHEN BUS_DT BETWEEN '21-MAY-18' AND '12-AUG-18' THEN '1_POST' ELSE 'NA' END) AS TPERIOD
      --Check to make sure the start timestamp isn't during the leap forward at the start of DST, becasue it will crash the time zone conversion 
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
       ,'YYYYMMDDHH24MISS') AS SAFE_START_DT
      -- Same for the end timestamp; Subtract from start timestamp and add to end timestamp so start time is never after end time 
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
      AND BUS_DT BETWEEN '16-APR-18' AND '12-AUG-18' -- TASKEVOLVED TEST
      --AND STORE_NUM IN (6565,9554,9692,10436) -- TASKEVOLVED TEST
    ) gls
    
  INNER JOIN APPDWH.ADT_STORE@SBP411 s
    ON gls.STORE_NUM = s.STORE_NUM

  -- Get official time zone descriptions to make sure everything from ADT_STORE is valid 
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
  --AND s.AREA_NUM IN (121,137)
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
  ,a.SHIFT_END_DTM_LCL) 
SELECT 
  COUNT(DISTINCT STORE_NUM) AS STORE_COUNT
  ,COUNT(CASE WHEN QUESTION_ID = 'Q1' THEN QUESTION_ID END) AS RESP_TOTAL
  ,SUM(CASE WHEN RESP_ID = '7' AND QUESTION_ID = 'Q1' THEN 1 ELSE 0 END) AS TB_TOTAL
  ,ROUND(SUM(CASE WHEN RESP_ID = '7' AND QUESTION_ID = 'Q1' THEN 1 ELSE 0 END) / COUNT(CASE WHEN QUESTION_ID = 'Q1' THEN QUESTION_ID END),3)*100 AS TB_SCORE
  ,ROUND(AVG(CASE WHEN QUESTION_ID = 'Q1' THEN RESP_ID END),2) AVG_SCORE
  ,ROUND(STDDEV(RESP_ID),3) AS SD_SCORE
  ,REGION
  ,JOB_ID
  ,TPERIOD
  ,DAYPART
  
from SQ

GROUP BY REGION
  ,JOB_ID
  ,TPERIOD
  ,DAYPART
ORDER BY TPERIOD
  ,REGION
  ,DAYPART
  ,JOB_ID
*/



--TaskEvolved labor addition testing
--SBP415
WITH SQ AS (
SELECT
  (CASE WHEN s.RGN_NUM = 12 AND s.AREA_NUM IN (121,137) THEN 'WestTest' 
    WHEN s.RGN_NUM = 12 AND s.AREA_NUM NOT IN (121,137) THEN 'WestElse' 
    WHEN s.RGN_NUM NOT IN (12) THEN 'RestofUS' ELSE 'NA' END) AS REGION
  ,a.PRTNR_ID
  ,a.QUESTION_ID
  ,a.RESP_ID
  ,s.RGN_NUM
  ,s.AREA_NUM
  ,a.STORE_NUM
  ,a.JOB_ID
  ,a.SHIFT_START_DTM_LCL
  ,a.SHIFT_END_DTM_LCL
  ,(CASE WHEN TO_NUMBER(TO_CHAR(a.SHIFT_START_DTM_LCL, 'HH24MISS')) <= 125000 AND TO_NUMBER(TO_CHAR(a.SHIFT_END_DTM_LCL, 'HH24MISS')) >= 125000 THEN 'MID'
    WHEN TO_NUMBER(TO_CHAR(a.SHIFT_START_DTM_LCL, 'HH24MISS')) <= 155000 AND TO_NUMBER(TO_CHAR(a.SHIFT_END_DTM_LCL, 'HH24MISS')) >= 155000 THEN 'PM'
    ELSE 'RESTOFDAY' END) AS DAYPART
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
        WHEN BUS_DT BETWEEN '21-MAY-18' AND '12-AUG-18' THEN '1_POST' ELSE 'NA' END) AS TPERIOD
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
      AND BUS_DT BETWEEN '16-APR-18' AND '12-AUG-18' -- TASKEVOLVED TEST
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
  --AND s.AREA_NUM IN (121,137)
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
  ,a.SHIFT_END_DTM_LCL) 
SELECT 
  COUNT(DISTINCT STORE_NUM) AS STORE_COUNT
  ,COUNT(CASE WHEN QUESTION_ID = 'Q1' THEN QUESTION_ID END) AS RESP_TOTAL
  ,SUM(CASE WHEN RESP_ID = '7' AND QUESTION_ID = 'Q1' THEN 1 ELSE 0 END) AS TB_TOTAL
  ,ROUND(SUM(CASE WHEN RESP_ID = '7' AND QUESTION_ID = 'Q1' THEN 1 ELSE 0 END) / COUNT(CASE WHEN QUESTION_ID = 'Q1' THEN QUESTION_ID END),3)*100 AS TB_SCORE
  ,ROUND(AVG(CASE WHEN QUESTION_ID = 'Q1' THEN RESP_ID END),2) AVG_SCORE
  ,ROUND(STDDEV(RESP_ID),3) AS SD_SCORE
  ,REGION
  ,JOB_ID
  ,TPERIOD
  ,DAYPART
  
from SQ

GROUP BY REGION
  ,JOB_ID
  ,TPERIOD
  ,DAYPART
ORDER BY TPERIOD
  ,REGION
  ,DAYPART
  ,JOB_ID
  










--TaskEvolved labor addition testing
--SBP415
WITH SQ AS (
SELECT
  (CASE WHEN a.STORE_NUM IN (10009	,
10026	,
10051	,
10058	,
10082	,
10088	,
10095	,
10101	,
10109	,
10114	,
10119	,
10131	,
10158	,
10161	,
10177	,
10186	,
10214	,
10256	,
10259	,
10262	,
10276	,
10277	,
10286	,
10290	,
10301	,
10306	,
10330	,
10343	,
10346	,
10379	,
10394	,
10395	,
10401	,
10402	,
10429	,
10430	,
10432	,
10438	,
10466	,
10467	,
10477	,
10492	,
10535	,
10561	,
10623	,
10625	,
10632	,
10636	,
10648	,
10660	,
10665	,
10689	,
10695	,
10696	,
10703	,
10742	,
10787	,
10799	,
10800	,
10848	,
10849	,
10866	,
10909	,
10922	,
10954	,
10965	,
10969	,
10978	,
11065	,
11089	,
11093	,
11109	,
11123	,
11152	,
11190	,
11218	,
11239	,
11246	,
11259	,
11296	,
11309	,
11373	,
11466	,
11505	,
11519	,
11544	,
11545	,
11567	,
11575	,
11595	,
11603	,
11622	,
11645	,
11713	,
11741	,
11772	,
11798	,
11810	,
11844	,
11845	,
11855	,
11884	,
11926	,
11947	,
11949	,
11959	,
11966	,
11978	,
13213	,
13217	,
13220	,
13221	,
13243	,
13259	,
13260	,
13262	,
13263	,
13326	,
13336	,
13339	,
13448	,
13492	,
13531	,
13542	,
13549	,
13657	,
13673	,
13715	,
13726	,
13748	,
13888	,
13899	,
13903	,
13908	,
13917	,
13923	,
13929	,
13952	,
13961	,
13989	,
13991	,
14005	,
14006	,
14009	,
14012	,
14018	,
14023	,
14031	,
14046	,
14047	,
14071	,
14072	,
14142	,
14214	,
14305	,
14370	,
14431	,
14476	,
14526	,
14530	,
14719	,
14733	,
14832	,
15062	,
15430	,
15741	,
16092	,
16446	,
16518	,
16609	,
16876	,
16953	,
16990	,
17356	,
17388	,
17563	,
17747	,
17818	,
17854	,
17941	,
17957	,
18729	,
18734	,
18869	,
18980	,
19174	,
19282	,
19579	,
19670	,
19800	,
19821	,
19828	,
19836	,
19884	,
19895	,
19901	,
20173	,
20250	,
20537	,
20768	,
21070	,
21086	,
21142	,
21264	,
21313	,
21314	,
21756	,
21773	,
21793	,
21934	,
22003	,
2206	,
22118	,
2212	,
22121	,
22244	,
22294	,
22382	,
22417	,
22476	,
22479	,
22483	,
22526	,
22528	,
22553	,
22602	,
22637	,
22719	,
22825	,
22826	,
2285	,
2288	,
22881	,
2289	,
22943	,
2296	,
22998	,
23027	,
23127	,
23161	,
2322	,
23270	,
23319	,
23351	,
2340	,
23426	,
23465	,
23466	,
23517	,
23526	,
23558	,
2361	,
23650	,
23759	,
23872	,
23882	,
23917	,
23924	,
24162	,
24213	,
2424	,
24257	,
24325	,
24498	,
24499	,
24500	,
24503	,
24574	,
24661	,
24715	,
24852	,
24936	,
24938	,
2511	,
2512	,
25347	,
254	,
2558	,
2561	,
2589	,
2629	,
2633	,
2651	,
2657	,
2662	,
2685	,
2698	,
2717	,
2723	,
2724	,
2742	,
2795	,
2805	,
2808	,
2823	,
2837	,
286	,
2866	,
2916	,
2918	,
2931	,
2933	,
2968	,
2975	,
2981	,
3209	,
3227	,
3235	,
3236	,
3246	,
3249	,
3357	,
3378	,
3383	,
3398	,
3409	,
3416	,
3447	,
3459	,
3473	,
379	,
409	,
418	,
421	,
424	,
435	,
440	,
444	,
445	,
448	,
449	,
456	,
457	,
458	,
464	,
472	,
475	,
483	,
494	,
509	,
5215	,
5221	,
5227	,
5283	,
5294	,
5299	,
5314	,
5318	,
5329	,
5338	,
5343	,
5354	,
5362	,
5370	,
5405	,
5409	,
5427	,
5430	,
5448	,
5452	,
5459	,
5465	,
5467	,
5477	,
5493	,
5509	,
5511	,
5525	,
5538	,
5539	,
5554	,
5562	,
5573	,
5589	,
5590	,
5597	,
5608	,
561	,
5612	,
5633	,
5636	,
5693	,
5715	,
5721	,
5733	,
5734	,
5782	,
579	,
5791	,
5795	,
5828	,
5861	,
5872	,
5905	,
5915	,
5916	,
5924	,
5933	,
5959	,
5964	,
5966	,
5988	,
6215	,
6216	,
6222	,
6230	,
6244	,
6291	,
6296	,
6302	,
6307	,
6321	,
6332	,
6334	,
6335	,
6339	,
6352	,
636	,
6361	,
6379	,
6389	,
6391	,
6392	,
6442	,
6446	,
6448	,
6456	,
6463	,
6471	,
6473	,
6485	,
6495	,
6531	,
6537	,
6555	,
6566	,
6604	,
6609	,
6611	,
6612	,
6665	,
6667	,
6668	,
6680	,
6681	,
670	,
6701	,
6707	,
6731	,
6747	,
6749	,
6763	,
6771	,
6781	,
6782	,
6797	,
6821	,
6833	,
6861	,
6865	,
6869	,
6874	,
6878	,
6881	,
6884	,
692	,
6946	,
6956	,
6965	,
709	,
7208	,
7218	,
7286	,
7292	,
7310	,
7321	,
7337	,
7340	,
7401	,
7405	,
741	,
7459	,
7461	,
7492	,
7496	,
753	,
7614	,
7760	,
7805	,
782	,
7834	,
7838	,
7839	,
7931	,
7971	,
7981	,
8122	,
8129	,
8140	,
8201	,
8202	,
8215	,
8225	,
8299	,
8320	,
8359	,
8418	,
8428	,
8492	,
8527	,
8552	,
8604	,
8606	,
8631	,
8659	,
8679	,
8689	,
8698	,
8735	,
8736	,
8766	,
8771	,
8785	,
8814	,
8817	,
8830	,
8831	,
8832	,
8837	,
8840	,
8842	,
8843	,
8849	,
8879	,
8907	,
8932	,
8936	,
8941	,
8960	,
8966	,
8970	,
8982	,
8989	,
9215	,
9256	,
9274	,
9275	,
9291	,
9296	,
9297	,
9305	,
9357	,
9371	,
9376	,
9388	,
9414	,
9423	,
9448	,
9498	,
9531	,
9533	,
9539	,
9596	,
9600	,
9706	,
9715	,
9725	,
9732	,
9744	,
9754	,
9765	,
9774	,
9786	,
979	,
980	,
9814	,
9818	,
9843	,
9850	,
9852	,
9858	,
9871	,
9891	,
9904	,
9921	,
9931	,
9956	,
9964	,
9965	,
9968	,
9969	,
9974	,
9983	,
9984	,
9991	,
9993	
) THEN 'ControlStores' 
    ELSE 'NA' END) AS REGION
  ,a.PRTNR_ID
  ,a.QUESTION_ID
  ,a.RESP_ID
  ,s.RGN_NUM
  ,s.AREA_NUM
  ,a.STORE_NUM
  ,a.JOB_ID
  ,a.SHIFT_START_DTM_LCL
  ,a.SHIFT_END_DTM_LCL
  ,(CASE WHEN TO_NUMBER(TO_CHAR(a.SHIFT_START_DTM_LCL, 'HH24MISS')) <= 125000 AND TO_NUMBER(TO_CHAR(a.SHIFT_END_DTM_LCL, 'HH24MISS')) >= 125000 THEN 'MID'
    WHEN TO_NUMBER(TO_CHAR(a.SHIFT_START_DTM_LCL, 'HH24MISS')) <= 155000 AND TO_NUMBER(TO_CHAR(a.SHIFT_END_DTM_LCL, 'HH24MISS')) >= 155000 THEN 'PM'
    ELSE 'RESTOFDAY' END) AS DAYPART
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
        WHEN BUS_DT BETWEEN '21-MAY-18' AND '12-AUG-18' THEN '1_POST' ELSE 'NA' END) AS TPERIOD
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
      AND BUS_DT BETWEEN '16-APR-18' AND '12-AUG-18' -- TASKEVOLVED TEST
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
  --AND s.AREA_NUM IN (121,137)
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
  ,a.SHIFT_END_DTM_LCL) 
SELECT 
  COUNT(DISTINCT STORE_NUM) AS STORE_COUNT
  ,COUNT(CASE WHEN QUESTION_ID = 'Q1' THEN QUESTION_ID END) AS RESP_TOTAL
  ,SUM(CASE WHEN RESP_ID = '7' AND QUESTION_ID = 'Q1' THEN 1 ELSE 0 END) AS TB_TOTAL
  ,ROUND(SUM(CASE WHEN RESP_ID = '7' AND QUESTION_ID = 'Q1' THEN 1 ELSE 0 END) / COUNT(CASE WHEN QUESTION_ID = 'Q1' THEN QUESTION_ID END),3)*100 AS TB_SCORE
  ,ROUND(AVG(CASE WHEN QUESTION_ID = 'Q1' THEN RESP_ID END),2) AVG_SCORE
  ,ROUND(STDDEV(RESP_ID),3) AS SD_SCORE
  ,REGION
  ,JOB_ID
  ,TPERIOD
  ,DAYPART
  
from SQ

GROUP BY REGION
  ,JOB_ID
  ,TPERIOD
  ,DAYPART
ORDER BY TPERIOD
  ,REGION
  ,DAYPART
  ,JOB_ID