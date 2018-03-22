SELECT
 
a.FP_End,
a.[Job_Number],
--a.[Personnel Number],
--a.SIP_Elig,
--a.SIP_Part,
COUNT(*) AS N,
SUM(a.SIP_Elig) As [SE],
SUM(a.SIP_Part) As [SP],
SUM(Convert(decimal(12,2),a.SIP_Part))/COUNT(*) As [SIP Rate],

SUM(a.BFT_Elig) As [BE],
SUM(a.BFT_Part) As [BP],
SUM(Convert(decimal(12,2),a.BFT_Part))/COUNT(*) As [BFT Rate],

SUM(a.RET_Elig) As [RE],
SUM(a.RET_Part) As [RP],
SUM(Convert(decimal(12,2),a.RET_Part))/COUNT(*) As [RET Rate]

--a.RET_Elig,
--a.RET_Part

from hpohlman.PRO_BaseData a

where a.FP_End = '01-29-2017'
and a.Action_Group = 'headcount'
and a.[Country] = 'US'
--and a.BFT_elig = 1
and a.[Job_Number] In(50000362,50000358,50000117)

Group By a.FP_End, a.[Job_Number]
ORDER BY a.[Job_Number] desc






SELECT
 
a.FP_End,
a.[Personnel Number],
a.SIP_Part,
a.BFT_Part,
a.RET_Part,
a.[Job_Number]
from hpohlman.PRO_BaseData a

--where a.FP_End = '01-29-2017'
where a.FP_End = '12-31-2017'
and a.Action_Group = 'headcount'
and a.[Country] = 'US'
--and a.BFT_elig = 1
--and a.[Job_Number] In(50000362,50000358,50000117,50018175,50000118)


select * from hpohlman.PRO_BaseData
--where [Last Name] = 'Dorr'
--order by Job_Title