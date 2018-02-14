SELECT t.GUID_USER_ID
,t.GENDER
,t.COMBINED_AGE
,t.EDUCATION_MODEL
,t.MARITAL_STATUS
,t.OCCUPATION_GROUP_V2
,t.EST_HOUSEHOLD_INCOME_V5
,sq.TRANS

FROM DEPT_RET_AN.CUSTOMER_TEST t

INNER JOIN (SELECT
  a.GUID_ID
  ,COUNT(*) AS TRANS
FROM APPCA.F_SVC_FIN_TRANS a

INNER JOIN APPCA.D_CAL b 
  ON a.BUS_DT = b.CAL_DT
  WHERE b.FSCL_YR_NUM = 2018 and b.FSCL_PER_IN_YR_NUM = 3

GROUP BY a.GUID_ID
) sq

ON t.GUID_USER_ID = sq.GUID_ID

WHERE t.SBUX_MATCH_LEVEL IN ('IND')
