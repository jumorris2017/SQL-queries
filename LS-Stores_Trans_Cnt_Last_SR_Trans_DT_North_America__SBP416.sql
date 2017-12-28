SELECT
   a.MRCHT_ID
  ,a.ALT_MRCHT_ID
  ,b.LAST_TRANS_DT
  ,a.TRANS_CNT AS FSCL_DEC_SR_TRANS_CNT -- UPDATE NAME
FROM (

SELECT
   f.MRCHT_ID
  ,f.ALT_MRCHT_ID
  ,f.FSCL_YR_NUM
  ,COUNT(*) AS TRANS_CNT
FROM APPCA.F_CARD f
    
WHERE f.SVC_TRANS_CTGY = 'Redemption'
  AND f.FSCL_YR_NUM = 2018
  AND f.FSCL_WK_IN_YR_NUM BETWEEN 9 AND 13  --UPDATE WEEKS
  AND f.SALES_CHNL_CD IN ('LS','ALTD','NYA')  -- not sure what ALTD is, but it's assocaited with a single MID for Albersons LS stores
                                              -- same with NYA (not yet associated?), but is is used for at least one LS merchant
  
GROUP BY
   f.MRCHT_ID
  ,f.ALT_MRCHT_ID
  ,f.FSCL_YR_NUM
) a

LEFT JOIN (
  SELECT
     MRCHT_ID
    ,ALT_MRCHT_ID
    ,MAX(TRUNC(TRANS_DT)) AS LAST_TRANS_DT
  FROM APPCA.F_CARD

  WHERE SVC_TRANS_CTGY = 'Redemption'
    AND FSCL_YR_NUM = 2018
    AND FSCL_WK_IN_YR_NUM BETWEEN 9 AND 13  --UPDATE WEEKS
    AND SALES_CHNL_CD IN ('LS','ALTD','NYA')

  GROUP BY
     MRCHT_ID
    ,ALT_MRCHT_ID
) b
  ON a.MRCHT_ID = b.MRCHT_ID
    AND a.ALT_MRCHT_ID = b.ALT_MRCHT_ID
    
ORDER BY a.MRCHT_ID, a.ALT_MRCHT_ID
;
