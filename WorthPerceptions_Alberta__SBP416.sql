/* Worth perceptions for Alberta */
/* export as "worthperceptions_alberta.csv" */
SELECT
   QSTN_ID
  ,CAL_YR_NUM
  ,CAL_MNTH_IN_YR_NUM
  ,CAL_MNTH_ABBR_NM
  ,ROUND((SUM(TOTAL_TB)/SUM(TOTAL_RSPNS)),4)*100 AS wp_tb_score
  ,area_agg
FROM
    (SELECT
       sr.QSTN_ID
      ,st.CNTRY_CD_2_DGT_ISO
      ,st.CNTRY_SUB_DIV_CD
      --,ca.FSCL_YR_NUM
      --,ca.FSCL_PER_IN_YR_NUM
      ,ca.CAL_YR_NUM
      ,ca.CAL_MNTH_IN_YR_NUM
      ,ca.CAL_MNTH_ABBR_NM
      ,SUM(CASE WHEN RSPNS_ID = '7' THEN COALESCE(w.WEIGHT_RT,1) ELSE 0 END) AS TOTAL_TB
      ,SUM(COALESCE(w.WEIGHT_RT,1)) AS TOTAL_RSPNS
      --,ROUND(SUM(CASE WHEN RSPNS_ID = '7' THEN COALESCE(w.WEIGHT_RT,1) ELSE 0 END) / SUM(COALESCE(w.WEIGHT_RT,1)),4)*100 AS wp_tb_score
      ,CASE WHEN (CNTRY_SUB_DIV_CD = 'AB' AND st.CNTRY_CD_2_DGT_ISO = 'CA') THEN 'AB' ELSE CNTRY_CD_2_DGT_ISO END as area_agg
    FROM APPOTHER.AFT_CV_SRVY_RSPNS sr
    
    LEFT JOIN APPOTHER.CUST_INS_WEIGHTS w
      ON sr.CASE_ID = w.CASE_ID
      
    INNER JOIN APPCA.D_CAL ca
      ON TRUNC(sr.TRANS_DTM) = ca.CAL_DT
      
    INNER JOIN APPCA.D_STORE_VERS st
      ON st.STORE_NUM = sr.STORE_NUM
        AND st.CURRENT_FLG = 'Y'
      
    WHERE --sr.TRANS_DTM >= '29-DEC-14'  -- offical launch date of CO store program
      sr.TRANS_DTM >= '30-NOV-15'  -- two years of data
      AND sr.QSTN_ID = 'Q2_8'  -- WP Only
      AND sr.RSPNS_ID <> '9'
      AND (st.CNTRY_CD_2_DGT_ISO = 'CA' OR st.CNTRY_CD_2_DGT_ISO = 'US')
      AND st.OWNR_TYPE_CD = 'CO'
      --AND st.CNTRY_SUB_DIV_CD = 'AB'
    
    GROUP BY
       sr.QSTN_ID
      ,st.CNTRY_CD_2_DGT_ISO
      ,st.CNTRY_SUB_DIV_CD
      ,ca.CAL_YR_NUM
      ,ca.CAL_MNTH_IN_YR_NUM
      ,ca.CAL_MNTH_ABBR_NM
      )
GROUP BY
   QSTN_ID
  ,CAL_YR_NUM
  ,CAL_MNTH_IN_YR_NUM
  ,CAL_MNTH_ABBR_NM
  ,area_agg
;



