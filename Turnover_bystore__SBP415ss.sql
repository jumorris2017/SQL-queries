--PDW_BULK
--turnover at store level

WITH sq AS (
SELECT
   CAST(ot.StoreAbbrev AS INT) AS 'StoreNum'
  --,ojm.Org_ID
  ,ojm.Period_Name
  --,pt.Start_Date
  ,SUM(ojm.Partner_Headcount_Period) AS 'PartnerDays'
  ,AVG(ojm.Org_Days_In_Period) AS 'Org_Days'
  ,SUM(ojm.Turnover_Period) AS 'Terms'

FROM PRODM.Org_Job_Measures ojm
       
INNER JOIN PRODW.ORG_TREE_DRAD ot
  ON ojm.Org_ID = ot.BaseOrgid
    AND ot.AreaAbbrev IN ('A058','A140')

INNER JOIN PRODM.Periods_Table pt
    ON ojm.Period_Name = pt.Period_Name
      AND pt.Start_Date BETWEEN '2018-01-01' AND '2018-02-28'  -- fiscal periods in calendar 2015
      AND pt.Period_Type = 'FM'

WHERE ojm.Job_Key NOT IN ('50000117','50000118')  --exclude SMs and ASMs

GROUP BY
   CAST(ot.StoreAbbrev AS INT)
  ,ojm.Org_ID
  ,ojm.Period_Name
  ,pt.Start_Date)

SELECT sq.StoreNum
, sq.Period_Name
, sq.Terms
, sq.PartnerDays
, sq.Org_Days
, (sq.PartnerDays/sq.Org_Days) AS AVG_HC
, (CASE WHEN sq.Terms>=1 THEN (sq.Terms*100) / (sq.PartnerDays/sq.Org_Days) ELSE 0 END) AS TERM_RATE

FROM sq
WHERE sq.PartnerDays >=1 AND sq.Org_Days>=1

GROUP BY sq.StoreNum
, sq.Period_Name
, sq.Terms
, sq.PartnerDays
, sq.Org_Days