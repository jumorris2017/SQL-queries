/* CE by channel -- CAW */ 

/*MOP Slide #1*/
/*export as CE_byhour_byquarter.csv*/
/*export as Speed_bydaypart_FY16Q4-FY18Q1.csv*/

SELECT DISTINCT
  TO_CHAR(sr.TRANS_DTM, 'HH24') AS HOUR
  ,(CASE WHEN TO_CHAR(sr.TRANS_DTM, 'HH24') < 11 THEN 1 -- am
        WHEN TO_CHAR(sr.TRANS_DTM, 'HH24') >=11 AND TO_CHAR(sr.TRANS_DTM, 'HH24') < 14 THEN 2 -- midday
        WHEN TO_CHAR(sr.TRANS_DTM, 'HH24') >=14 AND TO_CHAR(sr.TRANS_DTM, 'HH24') < 16 THEN 3 -- pm
        WHEN TO_CHAR(sr.TRANS_DTM, 'HH24') >=16 THEN 4 -- evening
        ELSE 0 
        END) AS DAY_PART
  ,ca.FSCL_YR_NUM
  ,ca.FSCL_QTR_IN_YR_NUM
  ,SUM(CASE  WHEN sr.RSPNS_ID = '5' AND sr.QSTN_ID = 'Q1' THEN COALESCE(w.WEIGHT_RT,1) /* coalesce returns non-null weight value, OR if null, returns 1 */
    WHEN sr.RSPNS_ID = '7' THEN COALESCE(w.WEIGHT_RT,1) ELSE 0 END) AS TOTAL_TB
  ,SUM(COALESCE(w.WEIGHT_RT,1)) AS TOTAL_RSPNS
  --,ROUND((SUM(CASE  WHEN sr.RSPNS_ID = '5' AND sr.QSTN_ID = 'Q1' THEN COALESCE(w.WEIGHT_RT,1) WHEN sr.RSPNS_ID = '7' THEN COALESCE(w.WEIGHT_RT,1) ELSE 0 END)) / SUM(COALESCE(w.WEIGHT_RT,1)),4)*100 AS SPEED_TB_SCORE
  
FROM APPOTHER.AFT_CV_SRVY_RSPNS sr

JOIN APPCA.D_CAL ca
  ON ca.CAL_DT = TRUNC(sr.TRANS_DTM)
  
JOIN APPCA.D_STORE_VERS st
  ON sr.STORE_NUM = st.STORE_NUM
    AND st.CURRENT_FLG = 'Y'
    AND st.OWNR_TYPE_CD = 'CO'

LEFT JOIN APPOTHER.CUST_INS_WEIGHTS w
  ON sr.CASE_ID = w.CASE_ID

/* Create universal transaction ID from survey table and use that to join to POS table
/  Limit to U.S. and Canada and transactions after the survey began to improve performance */
JOIN APPCA.F_POS_LINE_ITEM pi
  ON TO_CHAR(sr.TRANS_DTM, 'YYYYMMDD') || TRIM(TO_CHAR(sr.STORE_NUM, '000000'))
      || TRIM(TO_CHAR(SUBSTR(sr.RGSTR_NUM, -1, 2),'00')) || sr.TRANS_ID = pi.TRANS_ID
    AND pi.CNTRY_CD = 'US'
    --AND pi.BUS_DT >= '01-OCT-17' AND pi.BUS_DT < '01-DEC-17'
  
JOIN APPCA.D_POS_LINE_ITEM_TRANS_TYPE tt
  ON pi.POS_LINE_ITEM_TRANS_TYPE_KEY = tt.POS_LINE_ITEM_TRANS_TYPE_KEY
  --WHERE tt.ORD_MTHD_CD IN ('CAFE','MOP','OTW')
  --AND sr.QSTN_ID IN ('Q1', 'Q2_1','Q2_2','Q2_3','Q2_4','Q2_5','Q2_6','Q2_7','Q2_8')
  AND sr.QSTN_ID IN ('Q2_1') -- SPEED ONLY
  AND sr.RSPNS_ID <> '9'
  --AND sr.STORE_NUM = 5798
  AND ((ca.FSCL_YR_NUM = 2016  AND ca.FSCL_QTR_IN_YR_NUM = 4) OR (ca.FSCL_YR_NUM = 2017) OR (ca.FSCL_YR_NUM = 2018 AND ca.FSCL_QTR_IN_YR_NUM = 1))
  --AND ca.FSCL_YR_NUM >= 2016
GROUP BY
  TO_CHAR(sr.TRANS_DTM, 'HH24') 
  ,ca.FSCL_YR_NUM
  ,ca.FSCL_QTR_IN_YR_NUM






/* export as "cc-so_bystore-daypart_FY17Q1-FY18Q1.csv"*/
/* for Brooke/Kelly 1/8/17 */
/* use with "CE_bydaypart_byquarter.R */

SELECT DISTINCT
  TO_CHAR(sr.TRANS_DTM, 'HH24') AS HOUR
    ,(CASE WHEN TO_CHAR(sr.TRANS_DTM, 'HH24') < 11 THEN 1 -- am
        WHEN TO_CHAR(sr.TRANS_DTM, 'HH24') >=11 AND TO_CHAR(sr.TRANS_DTM, 'HH24') < 14 THEN 2 -- midday
        WHEN TO_CHAR(sr.TRANS_DTM, 'HH24') >=14 AND TO_CHAR(sr.TRANS_DTM, 'HH24') < 16 THEN 3 -- pm
        WHEN TO_CHAR(sr.TRANS_DTM, 'HH24') >=16 THEN 4 -- evening
        ELSE 0 
        END) AS DAY_PART
  ,sr.STORE_NUM
  ,sr.QSTN_ID
  ,ca.FSCL_YR_NUM
  ,ca.FSCL_QTR_IN_YR_NUM
  ,SUM(CASE  WHEN sr.RSPNS_ID = '5' AND sr.QSTN_ID = 'Q1' THEN COALESCE(w.WEIGHT_RT,1) /* coalesce returns non-null weight value, OR if null, returns 1 */
    WHEN sr.RSPNS_ID = '7' THEN COALESCE(w.WEIGHT_RT,1) ELSE 0 END) AS TOTAL_TB
  ,SUM(COALESCE(w.WEIGHT_RT,1)) AS TOTAL_RSPNS
  --,ROUND((SUM(CASE  WHEN sr.RSPNS_ID = '5' AND sr.QSTN_ID = 'Q1' THEN COALESCE(w.WEIGHT_RT,1) WHEN sr.RSPNS_ID = '7' THEN COALESCE(w.WEIGHT_RT,1) ELSE 0 END)) / SUM(COALESCE(w.WEIGHT_RT,1)),4)*100 AS SPEED_TB_SCORE
  
FROM APPOTHER.AFT_CV_SRVY_RSPNS sr

JOIN APPCA.D_CAL ca
  ON ca.CAL_DT = TRUNC(sr.TRANS_DTM)
  
JOIN APPCA.D_STORE_VERS st
  ON sr.STORE_NUM = st.STORE_NUM
    AND st.CURRENT_FLG = 'Y'
    AND st.OWNR_TYPE_CD = 'CO'

LEFT JOIN APPOTHER.CUST_INS_WEIGHTS w
  ON sr.CASE_ID = w.CASE_ID

/* Create universal transaction ID from survey table and use that to join to POS table
/  Limit to U.S. and Canada and transactions after the survey began to improve performance */
JOIN APPCA.F_POS_LINE_ITEM pi
  ON TO_CHAR(sr.TRANS_DTM, 'YYYYMMDD') || TRIM(TO_CHAR(sr.STORE_NUM, '000000'))
      || TRIM(TO_CHAR(SUBSTR(sr.RGSTR_NUM, -1, 2),'00')) || sr.TRANS_ID = pi.TRANS_ID
    AND pi.CNTRY_CD = 'US'
    --AND pi.BUS_DT >= '01-OCT-17' AND pi.BUS_DT < '01-DEC-17'
  
JOIN APPCA.D_POS_LINE_ITEM_TRANS_TYPE tt
  ON pi.POS_LINE_ITEM_TRANS_TYPE_KEY = tt.POS_LINE_ITEM_TRANS_TYPE_KEY
  --WHERE tt.ORD_MTHD_CD IN ('CAFE','MOP','OTW')
  --AND sr.QSTN_ID IN ('Q1','Q2_1','Q2_2','Q2_3','Q2_4','Q2_5','Q2_6','Q2_7','Q2_8')
  AND sr.QSTN_ID IN ('Q2_1','Q2_2','Q2_3','Q2_4','Q2_5','Q2_6','Q2_7')
  AND sr.RSPNS_ID <> '9'
  --AND sr.STORE_NUM = 5798
  AND ((ca.FSCL_YR_NUM = 2018 AND ca.FSCL_QTR_IN_YR_NUM = 1) OR ca.FSCL_YR_NUM = 2017)
  --AND ca.FSCL_YR_NUM = 2017
GROUP BY
  TO_CHAR(sr.TRANS_DTM, 'HH24') 
  ,sr.STORE_NUM
  ,sr.QSTN_ID
  ,ca.FSCL_YR_NUM
  ,ca.FSCL_QTR_IN_YR_NUM



