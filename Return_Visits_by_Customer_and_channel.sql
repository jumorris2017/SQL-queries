/* Pulls post- period spend and number of visits */

    WITH SR AS (
          SELECT S1.GUID_USER_ID,
          S1.STORE_NUM AS STORE_NUM,
          tt.ORD_MTHD_CD, 
          S1.CASE_ID AS CASE_ID,
          TRUNC(S1.TRANS_DTM) AS SURVEY_TRANS_DATE,
          AVG(CASE WHEN S1.QSTN_ID='Q1' THEN S1.RSPNS_ID END) AS RETURN,
          AVG(CASE WHEN S1.QSTN_ID='Q2_2' THEN S1.RSPNS_ID END) AS Q2_2,
          AVG(CASE WHEN S1.QSTN_ID='Q2_1' THEN S1.RSPNS_ID END) AS Q2_1,
          AVG(CASE WHEN S1.QSTN_ID='Q2_3' THEN S1.RSPNS_ID END) AS Q2_3,
          AVG(CASE WHEN S1.QSTN_ID='Q2_4' THEN S1.RSPNS_ID END) AS Q2_4,
          AVG(CASE WHEN S1.QSTN_ID='Q2_5' THEN S1.RSPNS_ID END) AS Q2_5,
          AVG(CASE WHEN S1.QSTN_ID='Q2_6' THEN S1.RSPNS_ID END) AS Q2_6,
          AVG(CASE WHEN S1.QSTN_ID='Q2_7' THEN S1.RSPNS_ID END) AS Q2_7

          FROM APPOTHER.AFT_CV_SRVY_RSPNS S1

          INNER JOIN APPCA.D_STORE_VERS v1
            ON v1.STORE_NUM = S1.STORE_NUM
            AND v1.CURRENT_FLG = 'Y' 
            AND v1.CNTRY_CD_2_DGT_ISO = 'US' 
            AND v1.OWNR_TYPE_CD IN ('CO')

          JOIN APPCA.D_CAL ca
            ON ca.CAL_DT = TRUNC(S1.TRANS_DTM)

          /* Create universal transaction ID from survey table and use that to join to POS table
          /  Limit to U.S. and Canada and transactions after the survey began to improve performance */
          JOIN APPCA.F_POS_LINE_ITEM pi
            ON TO_CHAR(S1.TRANS_DTM, 'YYYYMMDD') || TRIM(TO_CHAR(S1.STORE_NUM, '000000'))
                || TRIM(TO_CHAR(SUBSTR(S1.RGSTR_NUM, -1, 2),'00')) || S1.TRANS_ID = pi.TRANS_ID
            
          JOIN APPCA.D_POS_LINE_ITEM_TRANS_TYPE tt
            ON pi.POS_LINE_ITEM_TRANS_TYPE_KEY = tt.POS_LINE_ITEM_TRANS_TYPE_KEY
            
            WHERE tt.ORD_MTHD_CD IN ('CAFE','MOP','OTW')
            AND S1.RSPNS_ID <> '9'
            AND ca.FSCL_YR_NUM = 2018 
            AND ca.FSCL_QTR_IN_YR_NUM = 2
          
          GROUP BY S1.GUID_USER_ID,
          S1.STORE_NUM,
          tt.ORD_MTHD_CD, 
          S1.CASE_ID,
          TRUNC(S1.TRANS_DTM),
          TO_CHAR(S1.TRANS_DTM, 'HH24'),
          TO_CHAR(S1.TRANS_DTM, 'YYYYMMDD') || TRIM(TO_CHAR(S1.STORE_NUM, '000000'))|| ltrim(TO_CHAR(S1.RGSTR_NUM, '00')) || TRIM(to_char(S1.TRANS_ID))
    ),
SR2 AS (
    SELECT SR.GUID_USER_ID,
      SR.STORE_NUM,
      SR.ORD_MTHD_CD,
      SR.CASE_ID,
      SR.SURVEY_TRANS_DATE,
      SR.RETURN,
      SR.Q2_2,
      SR.Q2_1,
      SR.Q2_3,
      SR.Q2_4,
      SR.Q2_5,
      SR.Q2_6,
      SR.Q2_7,
      SUM(CASE WHEN POS1.BUS_DT > TRUNC(SR.SURVEY_TRANS_DATE) THEN POS1.GROSS_REV_LCL_AMT ELSE NULL END) AS SPEND_POST60D,
      COUNT(DISTINCT CASE WHEN POS1.BUS_DT > TRUNC(SR.SURVEY_TRANS_DATE) THEN POS1.TRANS_ID ELSE NULL END) AS VISITS_POST60D

    FROM SR
      LEFT OUTER JOIN APPCA.F_POS_HDR POS1
      ON SR.GUID_USER_ID = POS1.GUID_ID
        AND POS1.BUS_DT <= TRUNC(SR.SURVEY_TRANS_DATE)+ INTERVAL'60'DAY 
      
      GROUP BY SR.GUID_USER_ID,
      SR.STORE_NUM,
      SR.ORD_MTHD_CD,
      SR.CASE_ID,
      SR.SURVEY_TRANS_DATE,
      SR.RETURN,
      SR.Q2_2,
      SR.Q2_1,
      SR.Q2_3,
      SR.Q2_4,
      SR.Q2_5,
      SR.Q2_6,
      SR.Q2_7
)
SELECT * FROM SR2