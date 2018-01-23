with d as
(select prtnr_num
      ,bus_dt
      ,job_id
      ,to_char(start_dtm,'hh24miss') as start_punch_tm
      ,to_char(end_dtm,'hh24miss') as end_punch_tm
      ,(CASE WHEN to_char(start_dtm,'hh24miss') > 130000 AND to_char(start_dtm,'hh24miss') < 160000 THEN 1 ELSE 0 END) AS PM_SHIFT
from appdwh.AFT_GLS_PRTNR_TMCARD
where bus_dt between '04-Sep-2017' and '10-Sep-2017' 
     AND job_id = 50000362 -- baristas only
) select d.PRTNR_NUM
     ,count(d.BUS_DT) AS SHIFTS_WORKED
     ,sum(d.PM_SHIFT) AS PM_SHIFTS
     ,ROUND(sum(d.PM_SHIFT) / count(d.BUS_DT),3) AS PM_PROP
     ,(CASE WHEN (sum(d.PM_SHIFT) / count(d.BUS_DT)) >= (1/3) THEN 1 ELSE 0 END) AS PM_DOM
from d
     GROUP BY d.PRTNR_NUM





with d as
(select prtnr_num
      ,bus_dt
      ,job_id
      ,to_char(start_dtm,'hh24miss') as start_punch_tm
      ,to_char(end_dtm,'hh24miss') as end_punch_tm
      ,(CASE WHEN to_char(start_dtm,'hh24miss') <= 60000 THEN 1 ELSE 0 END) AS EARLYAM_SHIFT
      ,(CASE WHEN to_char(start_dtm,'hh24miss') > 60000 AND to_char(start_dtm,'hh24miss') <= 100000 THEN 1 ELSE 0 END) AS AM_SHIFT
      ,(CASE WHEN to_char(start_dtm,'hh24miss') > 100000 AND to_char(start_dtm,'hh24miss') <= 130000 THEN 1 ELSE 0 END) AS MIDDAY_SHIFT
      ,(CASE WHEN to_char(start_dtm,'hh24miss') > 130000 AND to_char(start_dtm,'hh24miss') <= 160000 THEN 1 ELSE 0 END) AS PM_SHIFT
      ,(CASE WHEN to_char(start_dtm,'hh24miss') > 160000 THEN 1 ELSE 0 END) AS LATEPM_SHIFT
from appdwh.AFT_GLS_PRTNR_TMCARD
where bus_dt between '04-Sep-2017' and '10-Sep-2017' 
     AND job_id = 50000362 -- baristas only
) select d.PRTNR_NUM
     ,count(d.BUS_DT) AS SHIFTS_WORKED
     ,sum(d.EARLYAM_SHIFT) AS EARLYAM_SHIFTS
     ,sum(d.AM_SHIFT) AS AM_SHIFTS
     ,sum(d.MIDDAY_SHIFT) AS MIDDAY_SHIFTS
     ,sum(d.PM_SHIFT) AS PM_SHIFTS
     ,sum(d.LATEPM_SHIFT) AS LATEPM_SHIFTS
from d
     GROUP BY d.PRTNR_NUM