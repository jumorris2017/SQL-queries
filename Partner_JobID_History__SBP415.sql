--415 (PDW)
--Pulls partners' job id

select * from APPPDW.DDT_PRTNR a

join APPPDW.DFT_RELATIONSHIPS b
on a.SNAPSHOT_DT = b.SNAPSHOT_DT
and a.PRTNR_VERS_KEY = b.PRTNR_VERS_KEY

join APPPDW.DDV_POSN_JOB c
on a.SNAPSHOT_DT = c.SNAPSHOT_DT
and b.POSN_JOB_VERS_KEY = c.POSN_JOB_VERS_KEY

where a.SAP_PRTNR_ID IN ('01147421')
and a.SNAPSHOT_DT = to_date('02-26-2018', 'MM-DD-YYYY')

order by a.EFF_TO_DT 





