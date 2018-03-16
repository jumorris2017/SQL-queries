select * from SCAP_MASTER
    order by 'Add_Date' desc
select * from SCAP_Benefit_Data
    where CAP_Status = 'Active-Participant'
select CAP_Status, count(*) AS PRTNR_CNT from SCAP_Benefit_Data
    group by CAP_Status
    order by count(*) desc 
select * from SCAP_Benefit_Data_History
select * from SCAP_Benefit_Data_Manual
select * from SCAP_LOG
select * from SCAP_LOG_NEW
    order by 'Load Date' desc
    
    
    
select 
 
a.FP_End,
a.[Job_Number],
--a.[Personnel Number],
--a.SIP_Elig,
--a.SIP_Part,
SUM(a.SIP_Elig) As [SE],
SUM(a.SIP_Part) As [SP],
--SUM(Convert(decimal(12,2),a.SIP_Part))/SUM(Convert(decimal(12,2),a.SIP_Elig)) As [SIP Rate],

SUM(a.BFT_Elig) As [BE],
SUM(a.BFT_Part) As [BP],
--SUM(Convert(decimal(12,2),a.BFT_Part))/SUM(Convert(decimal(12,2),a.BFT_Elig)) As [BFT Rate],

SUM(a.RET_Elig) As [RE],
SUM(a.RET_Part) As [RP],
--SUM(Convert(decimal(12,2),a.RET_Part))/SUM(Convert(decimal(12,2),a.RET_Elig)) As [RET Rate]

--a.RET_Elig,
--a.RET_Part

from hpohlman.PRO_BaseData a

where a.FP_End = '01-29-2017'
and a.Action_Group = 'headcount'
and a.[Country] = 'US'
--and a.BFT_elig = 1
and a.[Job_Number] In(50000362,50000358,50000117)

Group By a.FP_End,a.[Job_Number]
