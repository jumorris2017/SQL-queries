select round(totSeps * (364/storeDays / avgHC),3)*100 AS hrly_turn_12mo from 
(
select sum(totprtnrdays) prtDays, sum(rollup_store_days) as storeDays, sum(sep_cnt) as totSeps, (sum(totprtnrdays) / sum(rollup_store_days)) as avgHC
from (
SELECT * FROM LRIVERS.EXTAGGREGATETURNOVER 
where fscl_per_sid =  (select max(fscl_per_sid) from LRIVERS.EXTAGGREGATETURNOVER)
        and store_div in ('East Division', 'West Division')
        and job_id IN (50000358,50000362,50018175,50000445)
) a    
) b
