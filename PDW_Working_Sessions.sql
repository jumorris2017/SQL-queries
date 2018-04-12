--PDW Working Sessions

--1) 4/12/18

--ALWAYS join on SNAPSHOT_DT
--ALWAYS restrict on SNAPSHOT_DT; otherwise, there will be duplicates

--get most recent snapshot date (not necessarily locked)
select * 
from D_PRTNR_VERS
WHERE SNAPSHOT_DT = (SELECT MAX(SNAPSHOT_DT) FROM D_PRTNR_VERS)

--get latest *locked* snapshot date
select CAL_DT, FSCL_PER_PREV_BEG_DT
from APPPDW.ADT_CAL
WHERE CAL_DT = TRUNC(SYSDATE) - 8

--think about views would be helpful for our work
--discuss how we want our job group variable to be created; if there is consistency across teams, they can build that
