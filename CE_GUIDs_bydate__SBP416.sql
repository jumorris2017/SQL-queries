/* Pulls pre- period number of visits */


      SELECT t.GUID_USER_ID
          ,t.GENDER
          ,t.COMBINED_AGE
          ,t.EDUCATION_MODEL
          ,t.MARITAL_STATUS
          ,t.OCCUPATION_GROUP_V2
          ,t.EST_HOUSEHOLD_INCOME_V5
          ,sq.FSCL_YR_NUM
          ,sq.FSCL_PER_IN_YR_NUM

          FROM DEPT_RET_AN.CUSTOMER_DEMOG t

      INNER JOIN (SELECT S1.GUID_USER_ID,
          --TRUNC(S1.TRANS_DTM) AS SURVEY_TRANS_DATE,
          ca.FSCL_YR_NUM,
          ca.FSCL_PER_IN_YR_NUM

          FROM APPOTHER.AFT_CV_SRVY_RSPNS S1

          INNER JOIN APPCA.D_STORE_VERS v1
            ON v1.STORE_NUM = S1.STORE_NUM
            AND v1.CURRENT_FLG = 'Y' 
            AND v1.CNTRY_CD_2_DGT_ISO = 'US' 
            --AND v1.OWNR_TYPE_CD IN ('LS','CO')

           JOIN APPCA.D_CAL ca
             ON ca.CAL_DT = TRUNC(S1.TRANS_DTM)
    
           WHERE (ca.FSCL_YR_NUM = 2018 AND ca.FSCL_PER_IN_YR_NUM = 6) OR
           (ca.FSCL_YR_NUM = 2017 AND ca.FSCL_PER_IN_YR_NUM = 12) OR
           (ca.FSCL_YR_NUM = 2017 AND ca.FSCL_PER_IN_YR_NUM = 6) OR
           (ca.FSCL_YR_NUM = 2016 AND ca.FSCL_PER_IN_YR_NUM = 12) 
          
          GROUP BY S1.GUID_USER_ID,
          --TRUNC(S1.TRANS_DTM),
          --TO_CHAR(S1.TRANS_DTM, 'HH24'),
          --TO_CHAR(S1.TRANS_DTM, 'YYYYMMDD') || TRIM(TO_CHAR(S1.STORE_NUM, '000000'))|| ltrim(TO_CHAR(S1.RGSTR_NUM, '00')) || TRIM(to_char(S1.TRANS_ID)),
          ca.FSCL_YR_NUM,
          ca.FSCL_PER_IN_YR_NUM
      ) sq

  ON t.GUID_USER_ID = sq.GUID_USER_ID

  WHERE t.SBUX_MATCH_LEVEL IN ('IND')
