/* Pulls the ALTMIDs associated with MIDs */
/* For Cambridge requests */

/* Counts SR transactions associated with MID/ALTMID combinations */
SELECT ac.MRCHT_ID, ac.ALT_MRCHT_ID, COUNT(*) FROM APPCA.F_CARD ac
    WHERE ac.MRCHT_ID IN (97317800001 ) 
    --AND ac.ALT_MRCHT_ID IN (53861 ) 
    AND ac.TRANS_DT > '18-may-18' 
    GROUP BY ac.MRCHT_ID, ac.ALT_MRCHT_ID
    ORDER BY ac.MRCHT_ID, ac.ALT_MRCHT_ID

/* Counts SR transactions associated with MID/ALTMID combinations */
SELECT ac.MRCHT_ID, ac.ALT_MRCHT_ID, COUNT(*) FROM APPCA.F_CARD ac
    WHERE ( (ac.ALT_MRCHT_ID IN (40167) AND ac.MRCHT_ID IN (97039100009 ))
      OR (ac.ALT_MRCHT_ID IN (48053,49410))
      OR (ac.ALT_MRCHT_ID IN (49913) AND ac.MRCHT_ID IN (97056000003)) )
    AND ac.TRANS_DT > '13-feb-18' 
    GROUP BY ac.MRCHT_ID, ac.ALT_MRCHT_ID


/* Laste date received SR transactions associated with MID/ALTMID combinations */
WITH SQ AS(
SELECT ac.MRCHT_ID, ac.ALT_MRCHT_ID, ac.STORE_NUM, TRUNC(ac.TRANS_DT) 
    , row_number() over(partition by ac.MRCHT_ID, ac.ALT_MRCHT_ID order by TRUNC(ac.TRANS_DT) desc) mostrecent
    FROM APPCA.F_CARD ac
      WHERE ac.STORE_NUM IN (29659,29894,29465,29466,49573,49580,49581,49583,51512,52989,54482,52994) 
      --AND ac.MRCHT_ID IN (97317800001)
      OR ac.ALT_MRCHT_ID IN (554,528,545,509,555,574,520,551,523,905,937,552) 
      --AND ac.TRANS_DT > '03-DEC-17' 
    ORDER BY ac.ALT_MRCHT_ID, TRUNC(ac.TRANS_DT) DESC 
) SELECT * FROM SQ
WHERE mostrecent = 1


/*SR transactions per month associated with MID/ALTMID combinations */
SELECT ac.MRCHT_ID, ac.ALT_MRCHT_ID, COUNT(*) AS TRANS FROM APPCA.F_CARD ac
    WHERE ac.MRCHT_ID IN (97317800001)
    AND ac.ALT_MRCHT_ID IN (75580,78029,101110112,101110113,104110445,105110127) 
    AND (ac.TRANS_DT >= '16-MAR-18' AND ac.TRANS_DT <= '16-APR-18')
GROUP BY  ac.MRCHT_ID, ac.ALT_MRCHT_ID


/* SURVEY COUNTS */
SELECT
   sr.STORE_NUM AS SBUX_STORE_NUM
  ,COUNT(sr.CASE_ID) AS TOTAL_RSPNS_CNT
  ,MIN(TRUNC(sr.TRANS_DTM)) AS EARLIEST_RSPNS_DT
  ,MAX(TRUNC(sr.TRANS_DTM)) AS LATEST_RSPNS_DT
FROM APPOTHER.AFT_CV_SRVY_RSPNS sr
WHERE sr.QSTN_ID = 'Q1'
  AND sr.STORE_NUM IN (27528, 54208)
  --AND TRUNC(sr.TRANS_DTM) >= '01-JAN-2018'
GROUP BY
   sr.STORE_NUM
ORDER BY 
   sr.STORE_NUM

/* SURVEY COUNTS DATA TABLE */
SELECT * FROM APPOTHER.AFT_CV_SRVY_RSPNS sr
WHERE sr.STORE_NUM IN (53861) 


/*
SELECT MRCHT_ID, ALT_MRCHT_ID, TRANS_DT
FROM
(
  SELECT ac.MRCHT_ID, ac.ALT_MRCHT_ID, ac.TRANS_DT,
    row_number() over(partition by ac.MRCHT_ID 
                        order by ac.TRANS_DT desc) seq
  FROM APPCA.F_CARD ac
  WHERE ac.MRCHT_ID IN (97270200009,97270300007,97267500007)
  AND ac.ALT_MRCHT_ID > 0
) 
WHERE seq = 1
*/

