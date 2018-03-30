SELECT prtnr.store_num
    , COUNT(DISTINCT CASE WHEN prtnr.INCLUDE_IN_HDCNT_FLG = 'Y' THEN prtnr.PRTNR_ID END) as Headcount -- count only one headcount per month for non-termed partners.
    , COUNT(DISTINCT CASE WHEN prtnr.EMP_STAT_CD = 'Withdrawn' THEN prtnr.PRTNR_ID END) as SepCount -- count terms in that FP. 
    , cal.FSCL_YR_NUM
    , cal.FSCL_PER_IN_YR_NUM
    , prtnr.STORE_OPEN_DAYS_IN_WK_CNT
    --, str.DIST_NUM
    --, str.AREA_NUM
    --, str.RGN_NUM
    --, str.ZONE_NUM
    --, str.LAT_NUM
    --, str.LONG_NUM

FROM TURNOVER_PRTNR_V prtnr

  LEFT JOIN ADV_CAL cal
      on prtnr.FSCL_PER_SID = cal.FSCL_PER_SID -- here be time monstyrs. 
        -- However you want to slice/aggregate by time, they exist in this table
      
  LEFT JOIN ADV_STORE str
    on prtnr.STORE_NUM = str.STORE_NUM -- DRAD and store type/characteristics. 
        -- However you want to slice/aggregate by DRAD, they exist in this table

WHERE cal.FSCL_PER_END_DT = cal.FSCL_WK_END_DT AND
    cal.FSCL_PER_IN_YR_NUM IN (4) AND  -- add or change fiscal period
    cal.FSCL_YR_NUM = 2018 AND -- add or change year
    str.SNAPSHOT_DT = '01-JAN-18' AND -- limit store snapshot to the FP start date after your last included FP
    str.CNTRY_CD = 'US' AND  -- US only? Canada?
    str.OWNR_TYPE_CD = 'CO' AND  -- company owned? ls?
    str.STORE_ASCD_CD = 'ACTIVE' -- store open. 
    AND prtnr.JOB_ID IN ('50000362','50000358')

GROUP BY prtnr.FSCL_PER_SID
    , prtnr.store_num
    , cal.FSCL_YR_NUM
    , cal.FSCL_PER_IN_YR_NUM
    , prtnr.STORE_OPEN_DAYS_IN_WK_CNT
    --, str.DIST_NUM
    --, str.AREA_NUM
    --, str.RGN_NUM
    --, str.ZONE_NUM
    --, str.LAT_NUM
    --, str.LONG_NUM
    
ORDER BY prtnr.FSCL_PER_SID
    , prtnr.store_num
    , cal.FSCL_YR_NUM
    , cal.FSCL_PER_IN_YR_NUM
    --, str.DIST_NUM
    --, str.AREA_NUM
    --, str.RGN_NUM
    --, str.ZONE_NUM
    --, str.LAT_NUM
    --, str.LONG_NUM
    

