/* Pulls the ALTMIDs associated with MIDs */
/* For Cambridge requests */

--SELECT * FROM APPEXT.SVC_MRCHT_LS_MAP 

SELECT ac.MRCHT_ID, ac.ALT_MRCHT_ID, COUNT(*) FROM APPCA.F_CARD ac
    WHERE ac.MRCHT_ID IN (97270200009,97270300007,97267500007)
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