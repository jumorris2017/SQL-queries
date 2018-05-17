select a.store_num
      --,a.bus_dt
      --,a.half_hr_sid
      ,sum(b.units) as tot_units
      ,sum(a.total_hr_cnt) as tot_hours
      ,cast(sum(b.units)/sum(a.total_hr_cnt) as decimal(15,2)) as UPLH -- NOTE THIS IS TPLH BASED ON ONLY CUSTOMER UNITS
  
from 
   --a) This select statements gets you total hours worked by half hour id
   (select l.store_num
        ,l.bus_dt
        ,t.half_hr_sid
        ,sum(actual_hr_cnt) as total_hr_cnt
        
        from appdwh.AFT_GLS_STORE_DAYPART_JOB l --labor hours by daypart
          --join to adt_time to get to half hour granularity
          inner join (select half_hr_sid, qtr_hr_sid
                        from appdwh.adt_time
                        group by half_hr_sid, qtr_hr_sid) t
            on t.qtr_hr_sid = l.qtr_hr_tm
          
        where l.shift_role_id in (101,1,18) -- ops2, ops, keyholder
          and l.actual_hr_cnt > 0 --only times when someone actually worked 
          --and t.half_hr_sid >= 80000 and t.half_hr_sid <= 100000 - PEAK
        group by l.store_num
        ,l.bus_dt
        ,t.half_hr_sid
    ) a
    
left join 
    --b) this select statement gets you the number of units sold by half hour
      (select store_num, bus_dt, half_hr_sid, SUM(nds_sales_qty) as units
                    from appdwh.aft_pos_intl_hdr
                    where pos_void_cd not in ('L','V') --exclude voided transactions
                      and cust_trans_cnt = 1 --only include 'accounting' transactions
                    group by store_num, bus_dt, half_hr_sid) b
      on a.store_num = b.store_num
        and a.bus_dt = b.bus_dt
        and a.half_hr_sid = b.half_hr_sid
        
--c) this brings in the retail org hierarchy to filter off    
inner join appdwh.adt_store o on o.store_num = a.store_num
      
where a.bus_dt between  '27-nov-17' and '25-feb-18' -- PLEASE ADJUST DATE RANGE HERE
     --and a.store_num in (101) --for test
     and o.CNTRY_CD = 'US' 
     and o.ownr_type_CD = 'CO'
     --and a.half_hr_sid >= 80000 and a.half_hr_sid <= 100000 -PEAK
     
  group by a.store_num
  --,a.bus_dt
  --,a.half_hr_sid
  
  order by a.store_num
  --,a.bus_dt
  --,a.half_hr_sid