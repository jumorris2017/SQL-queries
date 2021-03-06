

SELECT prtnr.SAP_PRTNR_ID
    ,SUM(CASE WHEN sr.QUESTION_ID = 'Q1' THEN 1 ELSE 0 END) AS Q1_RESP
    ,SUM(CASE WHEN sr.RESP_ID = 7 THEN 1 ELSE 0 END) AS Q1_7
    ,SUM(CASE WHEN sr.RESP_ID = 6 THEN 1 ELSE 0 END) AS Q1_6
    ,SUM(CASE WHEN sr.RESP_ID = 5 THEN 1 ELSE 0 END) AS Q1_5
    ,SUM(CASE WHEN sr.RESP_ID = 4 THEN 1 ELSE 0 END) AS Q1_4
    ,SUM(CASE WHEN sr.RESP_ID = 3 THEN 1 ELSE 0 END) AS Q1_3
    ,SUM(CASE WHEN sr.RESP_ID = 2 THEN 1 ELSE 0 END) AS Q1_2
    ,SUM(CASE WHEN sr.RESP_ID = 1 THEN 1 ELSE 0 END) AS Q1_1
    
FROM SRV_DDV_RESPONSE sr

LEFT JOIN (
  SELECT PRTNR_ID, SAP_PRTNR_ID FROM (
    SELECT PRTNR_ID, SAP_PRTNR_ID,
    ROW_NUMBER() OVER (PARTITION BY SAP_PRTNR_ID ORDER BY SNAPSHOT_DT DESC) AS mostrec
    FROM D_PRTNR_VERS
    WHERE PRTNR_CNTRY_CD = 'US'
    ) WHERE mostrec = 1
  ) prtnr 
ON sr.PRTNR_ID = prtnr.PRTNR_ID

/* Get assigned org info as of survey response date. If partner separated this will be null, so use left join so these responses aren't excluded. */
--LEFT JOIN F_HEADCOUNT_VERS h
  --ON sr.PRTNR_ID = h.PRTNR_ID
    --AND h.SNAPSHOT_DT = (SELECT MAX(SNAPSHOT_DT) FROM D_COST_CTR_VERS)
    
/* Get assigned org details. */
--LEFT JOIN D_ORG_VERS org
  --ON h.ORG_VERS_KEY = org.ORG_VERS_KEY
  
  WHERE sr.SURVEY_RESP_DT_ORIG BETWEEN '01-JAN-2017' AND '31-DEC-2017'
  AND sr.QUESTION_ID = 'Q1'
  
GROUP BY prtnr.SAP_PRTNR_ID
