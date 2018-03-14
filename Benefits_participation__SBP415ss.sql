select a.FP_End,
a.[Personnel Number] AS PRTNR_ID,
a.SIP_Elig,
a.SIP_Part,
a.BFT_Elig,
a.BFT_Part,
a.RET_Elig,
a.RET_Part

from hpohlman.PRO_BaseData a

where a.Action_Group = 'headcount' 
and ( (a.FP_End >= '01-24-2015' and a.FP_End <= '02-04-2015') or 
(a.FP_End >= '01-24-2016' and a.FP_End <= '02-04-2016') or 
(a.FP_End >= '01-24-2017' and a.FP_End <= '02-04-2017') or
(a.FP_End >= '01-24-2018' and a.FP_End <= '02-04-2018') )

order by a.FP_END desc