WITH PARTNER_WEEKLY_HRS AS 
(
SELECT
   gls.PRTNR_NUM
  --,gls.JOB_ID
  --,gls.STORE_NUM AS 'GLS_Store_Num'
  ,sbo.StoreNumNum AS 'Home_Store_Num'
  --,CONVERT(date, gls.BUS_DT) as 'Date Worked'
  ,gls.FSCL_YR_NUM
  ,gls.FSCL_PER_IN_YR_NUM
  ,gls.FSCL_WK_IN_YR_NUM
  ,gls.FSCL_WK_CNT_NUM
  ,SUM(gls.TOT_HRS) AS 'Total_Hours_Worked'
  ,SUM(CASE WHEN gls.STORE_NUM = sbo.StoreNumNum THEN gls.TOT_HRS END) AS 'Home_Store_Hrs_Worked'
  ,SUM(CASE WHEN gls.STORE_NUM <> sbo.StoreNumNum THEN gls.TOT_HRS END) AS 'Away_Hrs_Worked'
FROM 
  OPENQUERY(SBP411,
      'select 
        a.prtnr_num
       ,a.job_id
       ,a.store_num
       ,a.bus_dt
       ,b.fscl_wk_in_yr_num
       ,b.fscl_wk_cnt_num
       ,b.fscl_per_in_yr_num
       ,b.fscl_yr_num
       ,cast(sum(a.sec_cnt)/3600 as decimal(15,2)) as tot_hrs
      from
       appdwh.aft_gls_prtnr_tot a
          inner join appdwh.adt_cal b 
            on a.bus_dt = b.cal_dt
              and b.fscl_yr_num = 2017
              and b.fscl_wk_in_yr_num between 23 and 26
              -- and b.fscl_wk_in_yr_num = 12
      where
        a.job_id = 50000362  -- baristas only
        -- a.job_id not in (50000117,50000445)  -- exclude sms and cafe attendants
          and cntry_cd = ''US''
          -- and a.store_num   <= 105
          -- and a.bus_dt = ''2014-11-20''
      group by
        a.prtnr_num
       ,a.job_id
       ,a.store_num
       ,a.bus_dt
       ,b.fscl_wk_in_yr_num
       ,b.fscl_wk_cnt_num
       ,b.fscl_per_in_yr_num
       ,b.fscl_yr_num') AS gls
         
  LEFT JOIN PDW_Bulk.dbo.[ORG ASSIGNMENT] AS oa
    ON gls.PRTNR_NUM = oa.[Personnel number]
      AND gls.BUS_DT BETWEEN oa.[Start Date] AND oa.[End Date]
      
  LEFT JOIN PDW_Bulk.PRODM.Stores_by_Org AS sbo
    ON oa.[Organizational Unit] = sbo.OrgID
    
--WHERE gls.STORE_NUM = sbo.StoreNumNum

GROUP BY
   gls.PRTNR_NUM
  --,gls.JOB_ID
  --,gls.STORE_NUM
  ,sbo.StoreNumNum
  ,gls.FSCL_YR_NUM
  ,gls.FSCL_PER_IN_YR_NUM
  ,gls.FSCL_WK_IN_YR_NUM
  ,gls.FSCL_WK_CNT_NUM
)


SELECT
  Home_Store_Num
  ,COUNT(DISTINCT CASE WHEN R4_Home_Hrs IS NOT NULL THEN PRTNR_NUM END) AS 'In_Store_Prtnr_Cnt'
  ,COUNT(CASE WHEN R4_Avg_Home_Hrs_per_Wk < 20 THEN PRTNR_NUM END) AS 'AvgLT20Hrs_Prtnr_Cnt'
  ,COUNT(CASE WHEN R4_Avg_Home_Hrs_per_Wk >= 32 THEN PRTNR_NUM END) AS 'AvgGT32Hrs_Prtnr_Cnt'
FROM
(
SELECT
   p1.PRTNR_NUM
  ,p1.Home_Store_Num
  ,p1.FSCL_YR_NUM
  ,p1.FSCL_PER_IN_YR_NUM
  ,p1.FSCL_WK_IN_YR_NUM
  ,p1.FSCL_WK_CNT_NUM
  ,p1.Total_Hours_Worked
  ,p1.Home_Store_Hrs_Worked
  ,p1.Away_Hrs_Worked
  ,SUM(p2.Total_Hours_Worked) AS 'R4_Tot_Hrs'
  ,SUM(p2.Home_Store_Hrs_Worked) AS 'R4_Home_Hrs'
  ,COUNT(p2.FSCL_WK_CNT_NUM) AS 'R4_Wks_Wrkd'
  ,SUM(p2.Home_Store_Hrs_Worked) / COUNT(p2.FSCL_WK_CNT_NUM) AS 'R4_Avg_Home_Hrs_per_Wk'
  ,SUM(p2.Total_Hours_Worked) / COUNT(p2.FSCL_WK_CNT_NUM) AS 'R4_Avg_Total_Hrs_per_Wk'
FROM PARTNER_WEEKLY_HRS p1

INNER JOIN PARTNER_WEEKLY_HRS p2
  ON p1.PRTNR_NUM = p2.PRTNR_NUM
    AND p2.FSCL_WK_CNT_NUM BETWEEN p1.FSCL_WK_CNT_NUM - 3 AND p1.FSCL_WK_CNT_NUM

--WHERE PRTNR_NUM = 1651617

GROUP BY
   p1.PRTNR_NUM
  ,p1.Home_Store_Num
  ,p1.FSCL_YR_NUM
  ,p1.FSCL_PER_IN_YR_NUM
  ,p1.FSCL_WK_IN_YR_NUM
  ,p1.FSCL_WK_CNT_NUM
  ,p1.Total_Hours_Worked
  ,p1.Home_Store_Hrs_Worked
  ,p1.Away_Hrs_Worked
) b

WHERE FSCL_WK_IN_YR_NUM = 26

GROUP BY Home_Store_Num