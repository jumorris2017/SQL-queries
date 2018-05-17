WITH prtnr AS 
(SELECT DISTINCT
      SAP_PRTNR_ID
      ,ORIG_HIRE_DT
      ,MOST_RECENT_HIRE_DT
      FROM D_PRTNR_VERS
      WHERE EMP_STAT_CD = 'Active'
 ) SELECT 
      prtnr.SAP_PRTNR_ID AS PRTNR_NUM
      ,prtnr.ORIG_HIRE_DT
      ,prtnr.MOST_RECENT_HIRE_DT
      ,gls.STORE_NUM
      ,gls.JOB_ID
      ,gls.start_punch_tm
      ,gls.end_punch_tm
      ,gls.BUS_DT

FROM prtnr

RIGHT JOIN (
  SELECT * FROM (
    SELECT PRTNR_NUM, STORE_NUM, JOB_ID, BUS_DT
    ,to_char(start_dtm,'hh24miss') as start_punch_tm
    ,to_char(end_dtm,'hh24miss') as end_punch_tm
    ,ROW_NUMBER() OVER (PARTITION BY PRTNR_NUM ORDER BY END_DTM DESC) AS mostrec
    FROM APPDWH.AFT_GLS_PRTNR_TMCARD@SBP411  
    WHERE CNTRY_CD = 'US'
      AND BUS_DT BETWEEN '05-MAR-18' AND '11-MAR-18'
      AND JOB_ID IN (50000362,50000358,50000117)
      AND to_char(start_dtm,'hh24miss') <= 100000
      AND to_char(end_dtm,'hh24miss') >= 080000
    ) WHERE mostrec = 1
  ) gls 
  
ON prtnr.SAP_PRTNR_ID = gls.PRTNR_NUM


