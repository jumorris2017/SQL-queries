--PDW (415)--

--USE HARDLINE TABLE (HPOHLMAN.ALL_POSN_CURRENT_HL)
with sq as (
SELECT SAP_PRTNR_ID, L3ORGID, L4ORGID, PRTNR_PERSONNEL_AREA_CD, JOB_ID AS PRTNR_COUNT,
  (CASE when L3ORGID = '60022676' then 'global_creative_studio_leanne_freeman'
     when L3ORGID = '60057900' then 'creative_and_global_design_liz_mueller'
     when L3ORGID = '60063301' AND L4ORGID NOT IN ('60060152') AND PRTNR_PERSONNEL_AREA_CD NOT IN ('US44') then 'global_retail_john_culver'
     when L3ORGID = '60047452' then 'global_strategy_and_marketing_matt_ryan'
     when L3ORGID = '60067686' AND PRTNR_PERSONNEL_AREA_CD NOT IN ('IT74','US04','US74','US10') AND JOB_ID NOT IN (50000113,50000346,50000345,50003178,50001502) then 'president_coo_roz_brewer' 
     when L3ORGID = '60000526' then 'law_and_corp_affairs_exec_paul_mutty'
     when L3ORGID = '60000089' then 'public_affairs_vivek_varma'
     when L3ORGID = '60063551' then 'siren_retail_cliff_burrows'
     when L3ORGID = '60028727' then 'sbux_tech_geri_martin_fickinger'
    else 'NA' END) AS LEADER1
FROM HPOHLMAN.ALL_POSN_CURRENT_HL
  WHERE EMP_STAT_CD = 'Active' 
  AND PRTNR_CNTRY_CD IN ('US','CA')
  AND JOB_ID NOT IN ('50000361')
  AND PRTNR_PERSONNEL_AREA_CD not in ('US01','US21','CN01','US07','AT01','IT27','G001','N001','CH01','US81','US27')
group by L3ORGID, L4ORGID, PRTNR_PERSONNEL_AREA_CD, JOB_ID, SAP_PRTNR_ID
order by L3ORGID, L4ORGID, PRTNR_PERSONNEL_AREA_CD, JOB_ID
) select sq.LEADER1, COUNT(*) AS partner_count
FROM SQ
  GROUP BY sq.LEADER1
  ORDER BY sq.LEADER1
  
--USE HARDLINE TABLE (HPOHLMAN.ALL_POSN_CURRENT_HL)
with sq as (
SELECT SAP_PRTNR_ID, L3ORGID, L4ORGID, L5ORGID, PRTNR_PERSONNEL_AREA_CD, JOB_ID AS PRTNR_COUNT,
  (CASE when L3ORGID = '60063301' AND L4ORGID = '60045778' AND PRTNR_PERSONNEL_AREA_CD NOT IN ('US44') then 'global_channel_development_tony_matta'
     when L4ORGID = '60068454' then 'americas_finance_rachel_ruggeri'
     when L3ORGID = '60067686' AND L4ORGID = '60007538' then 'canada_michael_conway'
     when L3ORGID = '60067686' AND L4ORGID = '60034978' then 'global_consumer_partner_insights_pam_greer'
     when L3ORGID = '60067686' AND L4ORGID = '60046400' then 'global_product_sandy_stark'
     when L3ORGID = '60067686' AND L4ORGID = '60060652' then 'global_store_dev_andy_adams'
     when L3ORGID = '60067686' AND L4ORGID = '60047676' AND PRTNR_PERSONNEL_AREA_CD NOT IN ('IT74','US04','US74','US10') then 'global_supply_chain_ops_hans_melotte'
     when L3ORGID = '60067686' AND L4ORGID = '60049525' AND JOB_ID NOT IN (50003178) then 'ls_us_americas_us_retail_mark_ring'
     when (L4ORGID IN ('60032850','60054100')  OR L5ORGID IN ('60049751','60063365','60063366')) AND JOB_ID NOT IN (50000113,50000346,50000345,50003178,50001502) then 'retail_store_ops_rossann_williams'
    else 'NA' END) AS LEADER2
FROM HPOHLMAN.ALL_POSN_CURRENT_HL
  WHERE EMP_STAT_CD = 'Active' 
  AND PRTNR_CNTRY_CD IN ('US','CA')
  AND JOB_ID NOT IN ('50000361')
  AND PRTNR_PERSONNEL_AREA_CD not in ('US01','US21','CN01','US07','AT01','IT27','G001','N001','CH01','US81','US27')
group by L3ORGID, L4ORGID, L5ORGID, PRTNR_PERSONNEL_AREA_CD, JOB_ID, SAP_PRTNR_ID
order by L3ORGID, L4ORGID, L5ORGID, PRTNR_PERSONNEL_AREA_CD, JOB_ID
) select sq.LEADER2, COUNT(*) AS partner_count
FROM SQ
  GROUP BY sq.LEADER2
  ORDER BY sq.LEADER2
  
  --**EXAMPLE**
select FIRST_NM, LAST_NM, JOB_ABBREV_NM, L3ORGID, L3ORG_NM, L4ORGID, L4ORG_NM, L5ORGID, L5ORG_NM FROM HPOHLMAN.ALL_POSN_CURRENT_HL
where  HPOHLMAN.ALL_POSN_CURRENT_HL.JOB_ABBREV_NM = 'svp'
  and EMP_STAT_CD = 'Active' 
  AND PRTNR_CNTRY_CD IN ('US','CA')
  AND JOB_ID NOT IN ('50000361')
  AND PRTNR_PERSONNEL_AREA_CD not in ('US01','US21','CN01','US07','AT01','IT27','G001','N001','CH01','US81','US27')
ORDER BY LAST_NM


--USE FUNCTIONAL TABLE (HPOHLMAN.ALL_POSN_CURRENT)
with sq as (
SELECT SAP_PRTNR_ID, L3ORGID, L4ORGID, PRTNR_PERSONNEL_AREA_CD, JOB_ID AS PRTNR_COUNT,
   (CASE when L3ORGID = '60040028' then 'cfo_scott_maw'  
    when L3ORGID = '60004957' then 'partner_resources_lucy_helm'
    when L3ORGID = '60067686' AND L4ORGID = '60047676' AND PRTNR_PERSONNEL_AREA_CD IN ('IT74','US04','US74','US10') then 'plants_dc_hans_melotte'
    when L3ORGID = '60067686' AND L4ORGID = '60049525' AND JOB_ID = 50003178 then 'licensed_dms_mark_ring'
    when L3ORGID = '60067686' AND L4ORGID = '60054100' AND JOB_ID IN (50000113,50000346,50000345,50003178,50001502) then 'retail_leadership_rosanne_williams'
    when L3ORGID = '60063301' AND PRTNR_PERSONNEL_AREA_CD = 'US44' then 'evolution_fresh_juicery_john_culver'
    else 'NA' END) AS FUNCTIONAL
FROM HPOHLMAN.ALL_POSN_CURRENT
  WHERE EMP_STAT_CD = 'Active' 
  AND PRTNR_CNTRY_CD IN ('US','CA')
  AND JOB_ID NOT IN ('50000361')
  AND PRTNR_PERSONNEL_AREA_CD not in ('US01','US21','CN01','US07','AT01','IT27','G001','N001','CH01','US81','US27')
group by L3ORGID, L4ORGID, PRTNR_PERSONNEL_AREA_CD, JOB_ID, SAP_PRTNR_ID
order by L3ORGID, L4ORGID, PRTNR_PERSONNEL_AREA_CD, JOB_ID
) select sq.FUNCTIONAL, COUNT(*) AS partner_count
FROM SQ
  GROUP BY sq.FUNCTIONAL
  ORDER BY sq.FUNCTIONAL

--hardline
--SELECT LAST_NM, FIRST_NM, L3ORG_NM, L3ORGID, L4ORG_NM, L4ORGID, PRTNR_PERSONNEL_AREA_NM, PRTNR_PERSONNEL_AREA_CD FROM HPOHLMAN.ALL_POSN_CURRENT_HL
--ORDER BY L3ORG_NM, L4ORG_NM

--functional
--SELECT LAST_NM, FIRST_NM, L3ORG_NM, L3ORGID, L4ORG_NM, L4ORGID, PRTNR_PERSONNEL_AREA_NM, PRTNR_PERSONNEL_AREA_CD FROM HPOHLMAN.ALL_POSN_CURRENT
--ORDER BY L3ORG_NM, L4ORG_NM


--angelia list  -- REMOVE FROM JOHN CULVER
--L4ORGID = 60060152 - Partner Resources Global Retail
  
--JEN FRISCH - ADD TO ROSANNE
--US PRO, Partner Resources West, Partner Resources East
--L5ORGID IN (60049751,60063365,60063366)

/*

with sq as (
SELECT SAP_PRTNR_ID, L3ORG_NM, L3ORGID, L4ORG_NM, L4ORGID, L5ORG_NM, L5ORGID, PRTNR_PERSONNEL_AREA_NM, PRTNR_PERSONNEL_AREA_CD, JOB_ID AS PRTNR_COUNT,
   (CASE when L3ORGID = '60040028' then 'cfo_scott_maw'  
    when L3ORGID = '60004957' then 'partner_resources_lucy_helm'
    when L3ORGID = '60067686' AND L4ORGID = '60047676' AND PRTNR_PERSONNEL_AREA_CD IN ('IT74','US04','US74','US10') then 'plants_dc_hans_melotte'
    when L3ORGID = '60067686' AND L4ORGID = '60049525' AND JOB_ID = 50003178 then 'licensed_dms_mark_ring'
    when L3ORGID = '60067686' AND L4ORGID = '60054100' AND JOB_ID IN (50000113,50000346,50000345,50003178,50001502) then 'retail_leadership_rosanne_williams'
    when L3ORGID = '60063301' AND PRTNR_PERSONNEL_AREA_CD = 'US44' then 'evolution_fresh_juicery_john_culver'
    else 'NA' END) AS FUNCTIONAL
FROM HPOHLMAN.ALL_POSN_CURRENT
  WHERE EMP_STAT_CD = 'Active' 
  AND PRTNR_CNTRY_CD IN ('US','CA')
  AND JOB_ID NOT IN ('50000361')
  AND PRTNR_PERSONNEL_AREA_CD not in ('US01','US21','CN01','US07','AT01','IT27','G001','N001','CH01','US81','US27')
group by SAP_PRTNR_ID, L3ORG_NM, L3ORGID, L4ORG_NM, L4ORGID, L5ORG_NM, L5ORGID, PRTNR_PERSONNEL_AREA_NM, PRTNR_PERSONNEL_AREA_CD, JOB_ID 
order by L3ORG_NM, L3ORGID, L4ORG_NM, L4ORGID, L5ORG_NM, L5ORGID, PRTNR_PERSONNEL_AREA_NM, PRTNR_PERSONNEL_AREA_CD, JOB_ID 
) select L3ORG_NM, L3ORGID, L4ORG_NM, L4ORGID, L5ORG_NM, L5ORGID, PRTNR_PERSONNEL_AREA_NM, PRTNR_PERSONNEL_AREA_CD, sq.FUNCTIONAL, COUNT(*) AS partner_count
FROM SQ
  GROUP BY FUNCTIONAL, L3ORG_NM, L3ORGID, L4ORG_NM, L4ORGID, L5ORG_NM, L5ORGID, PRTNR_PERSONNEL_AREA_NM, PRTNR_PERSONNEL_AREA_CD
  ORDER BY FUNCTIONAL, L3ORG_NM, L4ORG_NM, L5ORG_NM, PRTNR_PERSONNEL_AREA_NM
  
with sq as (
SELECT SAP_PRTNR_ID, L3ORG_NM, L3ORGID, L4ORG_NM, L4ORGID, L5ORG_NM, L5ORGID, PRTNR_PERSONNEL_AREA_NM, PRTNR_PERSONNEL_AREA_CD, JOB_ID AS PRTNR_COUNT,
  (CASE when L3ORGID = '60022676' then 'global_creative_studio_leanne_freeman'
     when L3ORGID = '60057900' then 'creative_and_global_design_liz_mueller'
     when L3ORGID = '60063301' AND L4ORGID NOT IN ('60060152') AND PRTNR_PERSONNEL_AREA_CD NOT IN ('US44') then 'global_retail_john_culver'
     when L3ORGID = '60047452' then 'global_strategy_and_marketing_matt_ryan'
     when L3ORGID = '60067686' AND PRTNR_PERSONNEL_AREA_CD NOT IN ('IT74','US04','US74','US10') AND JOB_ID NOT IN (50000113,50000346,50000345,50003178,50001502) then 'president_coo_roz_brewer' 
     when L3ORGID = '60000526' then 'law_and_corp_affairs_exec_paul_mutty'
     when L3ORGID = '60000089' then 'public_affairs_vivek_varma'
     when L3ORGID = '60063551' then 'siren_retail_cliff_burrows'
     when L3ORGID = '60028727' then 'sbux_tech_geri_martin_fickinger'
    else 'NA' END) AS LEADER1
FROM HPOHLMAN.ALL_POSN_CURRENT_HL
  WHERE EMP_STAT_CD = 'Active' 
  AND PRTNR_CNTRY_CD IN ('US','CA')
  AND JOB_ID NOT IN ('50000361')
  AND PRTNR_PERSONNEL_AREA_CD not in ('US01','US21','CN01','US07','AT01','IT27','G001','N001','CH01','US81','US27')
group by SAP_PRTNR_ID, L3ORG_NM, L3ORGID, L4ORG_NM, L4ORGID, L5ORG_NM, L5ORGID, PRTNR_PERSONNEL_AREA_NM, PRTNR_PERSONNEL_AREA_CD, JOB_ID 
order by L3ORG_NM, L3ORGID, L4ORG_NM, L4ORGID, L5ORG_NM, L5ORGID, PRTNR_PERSONNEL_AREA_NM, PRTNR_PERSONNEL_AREA_CD, JOB_ID 
) select L3ORG_NM, L3ORGID, L4ORG_NM, L4ORGID, L5ORG_NM, L5ORGID, PRTNR_PERSONNEL_AREA_NM, PRTNR_PERSONNEL_AREA_CD, sq.LEADER1, COUNT(*) AS partner_count
FROM SQ
  GROUP BY LEADER1, L3ORG_NM, L3ORGID, L4ORG_NM, L4ORGID, L5ORG_NM, L5ORGID, PRTNR_PERSONNEL_AREA_NM, PRTNR_PERSONNEL_AREA_CD
  ORDER BY LEADER1, L3ORG_NM, L4ORG_NM, L5ORG_NM, PRTNR_PERSONNEL_AREA_NM
  
with sq as (
SELECT SAP_PRTNR_ID, L3ORG_NM, L3ORGID, L4ORG_NM, L4ORGID, L5ORG_NM, L5ORGID, PRTNR_PERSONNEL_AREA_NM, PRTNR_PERSONNEL_AREA_CD, JOB_ID AS PRTNR_COUNT,
  (CASE when L3ORGID = '60063301' AND L4ORGID = '60045778' AND PRTNR_PERSONNEL_AREA_CD NOT IN ('US44') then 'global_channel_development_tony_matta'
     when L4ORGID = '60068454' then 'americas_finance_rachel_ruggeri'
     when L3ORGID = '60067686' AND L4ORGID = '60007538' then 'canada_michael_conway'
     when L3ORGID = '60067686' AND L4ORGID = '60034978' then 'global_consumer_partner_insights_pam_greer'
     when L3ORGID = '60067686' AND L4ORGID = '60046400' then 'global_product_sandy_stark'
     when L3ORGID = '60067686' AND L4ORGID = '60060652' then 'global_store_dev_andy_adams'
     when L3ORGID = '60067686' AND L4ORGID = '60047676' AND PRTNR_PERSONNEL_AREA_CD NOT IN ('IT74','US04','US74','US10') then 'global_supply_chain_ops_hans_melotte'
     when L3ORGID = '60067686' AND L4ORGID = '60049525' AND JOB_ID NOT IN (50003178) then 'ls_us_americas_us_retail_mark_ring'
     when (L4ORGID IN ('60032850','60054100')  OR L5ORGID IN ('60049751','60063365','60063366')) AND JOB_ID NOT IN (50000113,50000346,50000345,50003178,50001502) then 'retail_store_ops_rosanne_williams'
    else 'NA' END) AS LEADER2
FROM HPOHLMAN.ALL_POSN_CURRENT_HL
  WHERE EMP_STAT_CD = 'Active' 
  AND PRTNR_CNTRY_CD IN ('US','CA')
  AND JOB_ID NOT IN ('50000361')
  AND PRTNR_PERSONNEL_AREA_CD not in ('US01','US21','CN01','US07','AT01','IT27','G001','N001','CH01','US81','US27')
group by SAP_PRTNR_ID, L3ORG_NM, L3ORGID, L4ORG_NM, L4ORGID, L5ORG_NM, L5ORGID, PRTNR_PERSONNEL_AREA_NM, PRTNR_PERSONNEL_AREA_CD, JOB_ID 
order by L3ORG_NM, L3ORGID, L4ORG_NM, L4ORGID, L5ORG_NM, L5ORGID, PRTNR_PERSONNEL_AREA_NM, PRTNR_PERSONNEL_AREA_CD, JOB_ID 
) select L3ORG_NM, L3ORGID, L4ORG_NM, L4ORGID, L5ORG_NM, L5ORGID, PRTNR_PERSONNEL_AREA_NM, PRTNR_PERSONNEL_AREA_CD, sq.LEADER2, COUNT(*) AS partner_count
FROM SQ
  GROUP BY LEADER2, L3ORG_NM, L3ORGID, L4ORG_NM, L4ORGID, L5ORG_NM, L5ORGID, PRTNR_PERSONNEL_AREA_NM, PRTNR_PERSONNEL_AREA_CD
  ORDER BY LEADER2, L3ORG_NM, L4ORG_NM, L5ORG_NM, PRTNR_PERSONNEL_AREA_NM

  
  
  
--f
SELECT *
FROM HPOHLMAN.ALL_POSN_CURRENT
  WHERE EMP_STAT_CD = 'Active'
  AND PRTNR_CNTRY_CD IN ('US','CA')
  AND JOB_ID NOT IN ('50000361')
  AND PRTNR_PERSONNEL_AREA_CD not in ('US01','US21','CN01','US07','AT01','IT27','G001','N001','CH01','US81','US27')
  AND L3ORG_NM = 'Partner Resources'
  AND L4ORG_NM = 'Partner Resources Global Retail'
  AND L5ORG_NM = 'AP Partner Resources'

--hl
SELECT *
FROM HPOHLMAN.ALL_POSN_CURRENT_HL
  WHERE EMP_STAT_CD = 'Active' 
  AND PRTNR_CNTRY_CD IN ('US','CA')
  AND JOB_ID NOT IN ('50000361')
  AND PRTNR_PERSONNEL_AREA_CD not in ('US01','US21','CN01','US07','AT01','IT27','G001','N001','CH01','US81','US27')
  AND L3ORG_NM = 'Global Retail'
  AND L4ORG_NM = 'Global Retail Finance'
  AND L5ORG_NM = 'Global Retail Finance'

*/