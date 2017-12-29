/* Pulls the ALTMIDs associated with MIDs */
/* For Cambridge requests */

--SELECT * FROM APPEXT.SVC_MRCHT_LS_MAP 

SELECT ac.MRCHT_ID, ac.ALT_MRCHT_ID, COUNT(*) FROM APPCA.F_CARD ac
    WHERE ac.MRCHT_ID IN (97267400000,97268500006,97385500004) 
    AND ac.TRANS_DT > '17-NOV-17' -- Past month
    GROUP BY ac.MRCHT_ID, ac.ALT_MRCHT_ID

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

/* Counts SR transactions associated with MID/ALTMID combinations */
SELECT ac.MRCHT_ID, ac.ALT_MRCHT_ID, COUNT(*) FROM APPCA.F_CARD ac
    WHERE ac.ALT_MRCHT_ID IN (23113) AND ac.MRCHT_ID IN (97076400001)
    AND ac.TRANS_DT > '29-NOV-17' 
    GROUP BY ac.MRCHT_ID, ac.ALT_MRCHT_ID





