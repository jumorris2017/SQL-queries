/* Pulls the ALTMIDs associated with MIDs */
/* For Cambridge requests */

/* Counts SR transactions associated with MID/ALTMID combinations */
SELECT ac.STORE_NUM, ac.MRCHT_ID, ac.ALT_MRCHT_ID, COUNT(*) FROM APPCA.F_CARD ac
    WHERE ac.ALT_MRCHT_ID IN (49110) 
    --AND ac.MRCHT_ID IN (97056000003)
    AND ac.TRANS_DT > '25-DEC-18' 
    GROUP BY ac.STORE_NUM, ac.MRCHT_ID, ac.ALT_MRCHT_ID

/* Counts SR transactions associated with MID/ALTMID combinations */
SELECT ac.MRCHT_ID, ac.ALT_MRCHT_ID, COUNT(*) FROM APPCA.F_CARD ac
    WHERE ( (ac.ALT_MRCHT_ID IN (29954) AND ac.MRCHT_ID IN (97056000003 ))
      OR (ac.ALT_MRCHT_ID IN (1380) AND ac.MRCHT_ID IN (97056000003))
      OR (ac.ALT_MRCHT_ID IN (1155) AND ac.MRCHT_ID IN (97056000003))
      OR (ac.ALT_MRCHT_ID IN (1559) AND ac.MRCHT_ID IN (97056000003))
      OR (ac.ALT_MRCHT_ID IN (49913) AND ac.MRCHT_ID IN (97056000003))
      OR (ac.ALT_MRCHT_ID IN (1562) AND ac.MRCHT_ID IN (97056000003))
      OR (ac.ALT_MRCHT_ID IN (1151) AND ac.MRCHT_ID IN (97056000003))
      OR (ac.ALT_MRCHT_ID IN (1090) AND ac.MRCHT_ID IN (97056000003)) )
    AND ac.TRANS_DT > '07-DEC-17' 
    GROUP BY ac.MRCHT_ID, ac.ALT_MRCHT_ID


/* Laste date received SR transactions associated with MID/ALTMID combinations */
SELECT ac.MRCHT_ID, ac.ALT_MRCHT_ID, TRUNC(ac.TRANS_DT) FROM APPCA.F_CARD ac
    WHERE 
    ac.ALT_MRCHT_ID IN (6893) AND ac.MRCHT_ID IN (97056000003)
        --AND ac.TRANS_DT > '03-JUN-17' 
    ORDER BY TRUNC(ac.TRANS_DT) DESC 





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