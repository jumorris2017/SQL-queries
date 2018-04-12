--415
with SQ as
(select a.SAP_PRTNR_ID AS PRTNR_ID
      ,ROW_NUMBER() OVER (PARTITION BY a.SAP_PRTNR_ID ORDER BY a.EFF_FROM_DT DESC) AS mostrec
      ,a.EMP_STAT_CD 
      ,c.JOB_NM
      --,c.JOB_GRP_NM
      ,(case when c.JOB_ABBREV_NM IN ('svp', 'evp', 'svp & gm', 'president', 'coo', 'chairman', 'cfo', 'ceo') then 'SVP+'
      when c.JOB_ABBREV_NM = 'vp' then 'VP'
      when c.JOB_ABBREV_NM = 'dir' then 'DIR'
      when c.JOB_ABBREV_NM = 'mgr sr' then 'SR MGR'
      when c.JOB_ABBREV_NM = 'manager' or CAST(REGEXP_REPLACE(c.PAY_GRADE_CD, '[^0-9]+', '') AS number) >= 25 then 'MGR'
      when c.PAY_GRADE_CD IN('23','24') then 'IC II'
      when CAST(REGEXP_REPLACE(c.PAY_GRADE_CD, '[^0-9]+', '') AS number) < 23 and  CAST(REGEXP_REPLACE(c.PAY_GRADE_CD, '[^0-9]+', '') AS number) >0 then 'IC I'
      when c.JOB_ABBREV_NM = 'cw' or a.EMP_STAT_CD = 'NonPartner Workforce' then 'CW'
      when c.JOB_ABBREV_NM = 'internship' then 'INTERN'
      end) as JOB_GROUP

from APPPDW.DDT_PRTNR a

join APPPDW.DFT_RELATIONSHIPS b
on a.SNAPSHOT_DT = b.SNAPSHOT_DT
and a.PRTNR_VERS_KEY = b.PRTNR_VERS_KEY
 
join APPPDW.DDV_POSN_JOB c
on a.SNAPSHOT_DT = c.SNAPSHOT_DT
and b.POSN_JOB_VERS_KEY = c.POSN_JOB_VERS_KEY
 
where a.SNAPSHOT_DT = to_date('02-26-2018', 'MM-DD-YYYY')
and a.EMP_STAT_CD IN ('Separated','Active')
--and a.SAP_PRTNR_ID IN (1966823)
) select * from SQ
WHERE mostrec = 1
