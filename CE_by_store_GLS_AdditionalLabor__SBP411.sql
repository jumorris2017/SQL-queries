SELECT
   c.FSCL_YR_NUM
  ,c.FSCL_WK_IN_YR_NUM
/*
  -- Total valid response counts by question
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_2' THEN ce.QSTN_ID END) AS Q2_2_Response_Total
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_1' THEN ce.QSTN_ID END) AS Q2_1_Response_Total
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_3' THEN ce.QSTN_ID END) AS Q2_3_Response_Total
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_4' THEN ce.QSTN_ID END) AS Q2_4_Response_Total
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_5' THEN ce.QSTN_ID END) AS Q2_5_Response_Total
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_6' THEN ce.QSTN_ID END) AS Q2_6_Response_Total
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_7' THEN ce.QSTN_ID END) AS Q2_7_Response_Total
  ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_8' THEN ce.QSTN_ID END) AS Q2_8_Response_Total
  
  -- Total top box responses
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_2' THEN 1 ELSE 0 END) AS Q2_2_TB_Cnt
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_1' THEN 1 ELSE 0 END) AS Q2_1_TB_Cnt
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_3' THEN 1 ELSE 0 END) AS Q2_3_TB_Cnt
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_4' THEN 1 ELSE 0 END) AS Q2_4_TB_Cnt
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_5' THEN 1 ELSE 0 END) AS Q2_5_TB_Cnt
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_6' THEN 1 ELSE 0 END) AS Q2_6_TB_Cnt
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_7' THEN 1 ELSE 0 END) AS Q2_7_TB_Cnt
  ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_8' THEN 1 ELSE 0 END) AS Q2_8_TB_Cnt
*/
  -- Compute top box scores for each question
  ,CASE WHEN COUNT(CASE WHEN ce.QSTN_ID = 'Q2_1' THEN ce.QSTN_ID END) = 0 THEN NULL
   ELSE TO_CHAR(SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_1' THEN 1 ELSE 0 END)
    / COUNT(CASE WHEN ce.QSTN_ID = 'Q2_1' THEN ce.QSTN_ID END),'0.0000') END
   AS Q2_1_TB_Score
  ,CASE WHEN COUNT(CASE WHEN ce.QSTN_ID = 'Q2_2' THEN ce.QSTN_ID END) = 0 THEN NULL
   ELSE TO_CHAR(SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_2' THEN 1 ELSE 0 END)
    / COUNT(CASE WHEN ce.QSTN_ID = 'Q2_2' THEN ce.QSTN_ID END),'0.0000') END
   AS Q2_2_TB_Score
  ,CASE WHEN COUNT(CASE WHEN ce.QSTN_ID = 'Q2_3' THEN ce.QSTN_ID END) = 0 THEN NULL
   ELSE TO_CHAR(SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_3' THEN 1 ELSE 0 END)
    / COUNT(CASE WHEN ce.QSTN_ID = 'Q2_3' THEN ce.QSTN_ID END),'0.0000') END
   AS Q2_3_TB_Score
  ,CASE WHEN COUNT(CASE WHEN ce.QSTN_ID = 'Q2_4' THEN ce.QSTN_ID END) = 0 THEN NULL
   ELSE TO_CHAR(SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_4' THEN 1 ELSE 0 END)
    / COUNT(CASE WHEN ce.QSTN_ID = 'Q2_4' THEN ce.QSTN_ID END),'0.0000') END
   AS Q2_4_TB_Score
  ,CASE WHEN COUNT(CASE WHEN ce.QSTN_ID = 'Q2_5' THEN ce.QSTN_ID END) = 0 THEN NULL
   ELSE TO_CHAR(SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_5' THEN 1 ELSE 0 END)
    / COUNT(CASE WHEN ce.QSTN_ID = 'Q2_5' THEN ce.QSTN_ID END),'0.0000') END
   AS Q2_5_TB_Score
  ,CASE WHEN COUNT(CASE WHEN ce.QSTN_ID = 'Q2_6' THEN ce.QSTN_ID END) = 0 THEN NULL
   ELSE TO_CHAR(SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_6' THEN 1 ELSE 0 END)
    / COUNT(CASE WHEN ce.QSTN_ID = 'Q2_6' THEN ce.QSTN_ID END),'0.0000') END
   AS Q2_6_TB_Score
  ,CASE WHEN COUNT(CASE WHEN ce.QSTN_ID = 'Q2_7' THEN ce.QSTN_ID END) = 0 THEN NULL
   ELSE TO_CHAR(SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_7' THEN 1 ELSE 0 END)
    / COUNT(CASE WHEN ce.QSTN_ID = 'Q2_7' THEN ce.QSTN_ID END),'0.0000') END
   AS Q2_7_TB_Score
  ,CASE WHEN COUNT(CASE WHEN ce.QSTN_ID = 'Q2_8' THEN ce.QSTN_ID END) = 0 THEN NULL
   ELSE TO_CHAR(SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_8' THEN 1 ELSE 0 END)
    / COUNT(CASE WHEN ce.QSTN_ID = 'Q2_8' THEN ce.QSTN_ID END),'0.0000') END
   AS Q2_8_TB_Score

FROM APPDWH.AFT_CV_SRVY_RSPNS ce

  INNER JOIN APPDWH.ADT_CAL c
    ON TRUNC(ce.TRANS_DTM) = c.CAL_DT
      AND c.FSCL_YR_NUM IN (2017,2018)
      AND c.FSCL_QTR_IN_YR_NUM = 3
      
  INNER JOIN APPDWH.ADT_STORE org
      ON ce.STORE_NUM = org.STORE_NUM
         AND org.OWNR_TYPE_CD IN ('CO')
         AND org.CNTRY_CD IN ('US')

WHERE ce.RSPNS_ID <> '9'  -- rspns_id = 9 for unanswered questions
  AND ce.QSTN_ID NOT IN ('Q1','Q11') -- these questions are not in Customer Connection or Store Operations scores
  AND ce.STORE_NUM IN (8401,8627,10357,8465,10354,13877,8535,8295,28032,5332,25043,6451,5708,27261,2855,2588,9496,14457,
  7617,7489,8700,28256,7789,24301,7348,29964,13386,14479,229,2264,2482,47565,2494,2363,2309,14256,231,9240,29398,7302,
  7673,7642,7307,7923,19024,7878,7627,7362,14442,11649,9241,7555,13587,7207,7858,842,7391,7888,8130,25139,7646,7277,831,
  7622,7761,7318,7227,7224,9269,7565,7804,7328,7773,20122,853,7741,7658,23761,19830,7812,7269,7835,28796,2950,833,822,7399,
  10377,7532,19385,11932,17087,7374,7314,23424,857,8988,16764,10155,7808,883,830,9463,7570,7219,13590,7356,17923,811,
  23738,2508,7450,868,22566,7797,26288,7258,7712,7752,10202,7382,10464,2747,876,19383,16439,7381,29352,2767,7228,24983,
  22915,7862,7992,11726,17019,9913,50017,8873,870,7831,7599,22253,885,874,7279,7364,2434,7218,7522,7535,18666,7400,7823,
  7531,7639,872,7702,7272,2798,49088,29720,7270,864,7336,7544,6285,7422,49513,6314,2917,28821,8218,9588) --high brew/high MOP

GROUP BY
   c.FSCL_WK_IN_YR_NUM
  ,c.FSCL_YR_NUM
ORDER BY
  c.FSCL_YR_NUM
  ,c.FSCL_WK_IN_YR_NUM
  
  