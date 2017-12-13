/*select distinct update_Dt from appext.aft_dc_store_wk_nc_tgt
where cntry_cd = 'US'
order by update_Dt desc*/
/* export as "traininghours_by_store_q1laborreport.csv" */

DEFINE BEGIN_DATE = '2-Oct-2017' -- BEGINNNG OF QUARTER
DEFINE QUARTER_END_DATE = '31-Dec-2017' -- LAST SUNDAY OF QUARTER

DEFINE END_DATE = '03-Dec-2017' -- LAST SUNDAY

--DEFINE UPDATE_DT = '29-Sep-17'  -- FIND BY RUNNING COMMENTED OUT QUERY ABOVE

Select a.store_num
              ,c.fscl_wk_in_yr_num
              ,sum(a.store_Train_hr_cnt) as WEEKLY_ACTUAL_TRAIN_HRS
              ,sum(a.actual_ops2_hr_cnt) as WEEKLY_ACTUAL_OPS2_HRS
        from appdwh.dft_Store_Labor_day  a
        join appdwh.adt_store b on a.store_num = b.store_num
        join appdwh.adt_cal c on a.bus_dt = c.cal_dt
        where b.zone_num in (3,6,8,106)
          and  b.CNTRY_CD = 'US'
          and b.ownr_type_CD = 'CO'
          and b.store_open_Dt is not null
          and (b.Store_term_dt is null or store_term_Dt >= '&END_DATE')
          and a.bus_dt between '&BEGIN_DATE' and '&END_DATE'
        group by a.store_num, c.fscl_wk_in_yr_num  
;
