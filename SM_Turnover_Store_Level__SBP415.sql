--415
--store level 12-month turnover
--SM

SELECT a.STORE_NUM, a.CBSA_CD, ROUND(SUM(a.SEPCOUNT)/SUM(a.HEADCOUNT),3) AS TURNOVER_SM_12MO

FROM

(SELECT
turn.FSCL_PER_SID,
turn.ORG_NM,
st.STORE_NUM,
st.CBSA_CD,
turn.STORE_DIV,
turn.ROLLUP_STORE_DAYS,
SUM(turn.AVG_HEAD_CNT) AS HEADCOUNT, 
(SUM(turn.SEP_CNT))/(turn.ROLLUP_STORE_DAYS/364) AS SEPCOUNT

FROM LRIVERS.EXTAGGREGATETURNOVER turn

-- join to ddv_org to get store number (org_abbrev_nm):
inner join apppdw.ddv_org o
ON turn.ORG_ID = o.ORG_ID
 
-- if you need to join to adv_store:
inner join apppdw.adv_store st
ON CAST(regexp_replace(o.org_abbrev_nm, '[^0-9]+', '') AS number) = st.store_num
AND o.snapshot_dt = st.snapshot_dt

/* Gets The most recent rolling 12 report. Table updates once per month*/
WHERE o.snapshot_dt = (select max(snapshot_dt) from apppdw.ddv_org)
AND turn.JOB_ID IN (50000117) 
AND turn.STORE_DIV IN ('West Division', 'East Division')
AND turn.AVG_HEAD_CNT > 0

GROUP BY
turn.FSCL_PER_SID,
turn.ORG_NM,
st.STORE_NUM,
st.CBSA_CD,
turn.STORE_DIV,
turn.ROLLUP_STORE_DAYS
) a

GROUP BY a.STORE_NUM, a.CBSA_CD