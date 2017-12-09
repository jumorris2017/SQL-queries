/* Warning - query takes about 3.5 hours to run when pulling all U.S. stores for a full year */

SELECT
   --STORE_NUM
  --,BUS_DT
  TIME_SID
  ,AVG(CAST(COMPANY_TENURE_DAYS AS numeric)) AS AVG_COMPANY_TENURE
FROM (
SELECT
   gls.PRTNR_NUM
  ,gls.STORE_NUM
  ,gls.TIME_SID
  ,CAST(gls.BUS_DT AS date) AS BUS_DT
  ,ch.Start_Date
  ,ch.End_Date
  ,DATEDIFF(day, ch.Start_Date, CAST(gls.BUS_DT AS date)) + 1 AS COMPANY_TENURE_DAYS
FROM 
  OPENQUERY(SBP411,
    'SELECT
       t.TIME_SID
      ,t.TIME_24HR_NM
      ,PRTNR_NUM
      ,STORE_NUM
      ,BUS_DT
      ,JOB_ID
      ,START_DTM
      ,END_DTM
    FROM APPDWH.AFT_GLS_PRTNR_TMCARD gls

    INNER JOIN APPDWH.ADT_CAL c
      ON gls.BUS_DT = c.CAL_DT
        AND c.FSCL_YR_NUM = 2017
        AND c.FSCL_WK_IN_YR_NUM BETWEEN 41 AND 52
    
    INNER JOIN APPDWH.ADT_TIME t
      ON t.TIME_SID BETWEEN TO_NUMBER(TO_CHAR(gls.START_DTM, ''HH24MISS'')) AND TO_NUMBER(TO_CHAR(gls.END_DTM, ''HH24MISS''))
        AND HR_BEG_IND = ''Y''
        AND t.TIME_SID BETWEEN 50000 AND 210000

    WHERE gls.JOB_ID IN (50000362,50000358,50018175)  -- barista, shift sup, shift mgr
      AND gls.CNTRY_CD = ''US''
      AND gls.STORE_NUM >0  -- for testing'
) AS gls

LEFT JOIN PDW_Bulk.PRODM.Company_History ch
  ON gls.PRTNR_NUM = CAST(ch.Personnel_Number AS numeric)
    AND CAST(gls.BUS_DT AS date) BETWEEN ch.Start_Date AND ch.End_Date
) a

GROUP BY
   --STORE_NUM
  --,BUS_DT
  TIME_SID
