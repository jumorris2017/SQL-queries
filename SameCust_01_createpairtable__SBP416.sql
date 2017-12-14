--CREATE TEMP TABLE OF SURVEY PAIRS
--THIS QUERY NEEDS TO BE RUN ONLY ONCE - IT CREATES A TABLE IN THE DEPT_CUST_INS SCHEMA WITH ALL THE SURVEY PAIRS FROM THE SAME CUSTOMER
--IT WAS RUN THE LAST TIME ON 12/13 AND CONTAINS SURVEYS STARTING 01-JAN-2015 TILL 13-DEC-2017
--WE CAN USE THIS TABLE TO THEN JOIN WITH OTHER TABLES TO GET FIELDS LIKE SPEND, VISITS, NUM OF ITEMS, ETC.

CREATE TABLE DEPT_CUST_INS.SURVEY_PAIRS PARALLEL 
   AS
(
SELECT S1.GUID_USER_ID,

--FIRST SURVEY
S1.CASE_ID AS CASE_ID_1,
TRUNC(S1.TRANS_DTM) AS TRANS_DTM_1,
TO_CHAR(S1.TRANS_DTM, 'YYYYMMDD') || TRIM(TO_CHAR(S1.STORE_NUM, '000000'))|| ltrim(TO_CHAR(S1.RGSTR_NUM, '00')) || TRIM(to_char(S1.TRANS_ID)) AS TRANS_ID_1,
AVG(CASE WHEN S1.QSTN_ID='Q2_8' THEN S1.RSPNS_ID END) AS WORTH_1,
AVG(CASE WHEN S1.QSTN_ID='Q2_2' THEN S1.RSPNS_ID END) AS CC_1,
AVG(CASE WHEN S1.QSTN_ID='Q2_1' THEN S1.RSPNS_ID END) AS SPEED_1,
AVG(CASE WHEN S1.QSTN_ID='Q2_4' THEN S1.RSPNS_ID END) AS ACCR_1,
AVG(CASE WHEN S1.QSTN_ID='Q2_5' THEN S1.RSPNS_ID END) AS BEV_1,
AVG(CASE WHEN S1.QSTN_ID='Q2_6' THEN S1.RSPNS_ID END) AS FOOD_1,
AVG(CASE WHEN S1.QSTN_ID='Q2_7' THEN S1.RSPNS_ID END) AS CLEAN_1,
AVG(CASE WHEN S1.QSTN_ID='Q2_3' THEN S1.RSPNS_ID END) AS ABVBYD_1,
AVG(CASE WHEN S1.QSTN_ID='Q1' THEN S1.RSPNS_ID END) AS RETURN_1,
--COUNT(CASE WHEN S1.QSTN_ID='Q2_8' THEN S1.RSPNS_ID END) AS N_ROWS_WORTH_1,

--SECOND SURVEY
S2.CASE_ID AS CASE_ID_2,
TRUNC(S2.TRANS_DTM) AS TRANS_DTM_2,
TO_CHAR(S2.TRANS_DTM, 'YYYYMMDD') || TRIM(TO_CHAR(S2.STORE_NUM, '000000'))|| ltrim(TO_CHAR(S2.RGSTR_NUM, '00')) || TRIM(to_char(S2.TRANS_ID)) AS TRANS_ID_2,
AVG(CASE WHEN S2.QSTN_ID='Q2_8' THEN S2.RSPNS_ID END) AS WORTH_2,
AVG(CASE WHEN S2.QSTN_ID='Q2_2' THEN S2.RSPNS_ID END) AS CC_2,
AVG(CASE WHEN S2.QSTN_ID='Q2_1' THEN S2.RSPNS_ID END) AS SPEED_2,
AVG(CASE WHEN S2.QSTN_ID='Q2_4' THEN S2.RSPNS_ID END) AS ACCR_2,
AVG(CASE WHEN S2.QSTN_ID='Q2_5' THEN S2.RSPNS_ID END) AS BEV_2,
AVG(CASE WHEN S2.QSTN_ID='Q2_6' THEN S2.RSPNS_ID END) AS FOOD_2,
AVG(CASE WHEN S2.QSTN_ID='Q2_7' THEN S2.RSPNS_ID END) AS CLEAN_2,
AVG(CASE WHEN S2.QSTN_ID='Q2_3' THEN S2.RSPNS_ID END) AS ABVBYD_2,
AVG(CASE WHEN S2.QSTN_ID='Q1'   THEN S2.RSPNS_ID END) AS RETURN_2
--COUNT(CASE WHEN S2.QSTN_ID='Q2_8' THEN S2.RSPNS_ID END) AS N_ROWS_WORTH_2

FROM APPOTHER.AFT_CV_SRVY_RSPNS S1
INNER JOIN APPOTHER.AFT_CV_SRVY_RSPNS S2
ON S1.GUID_USER_ID = S2.GUID_USER_ID
--S2 HAPPENED AFTER S1
AND TRUNC(S2.TRANS_DTM) > TRUNC(S1.TRANS_DTM)
--S2 HAPPENED WITHIN 59 DAYS OF S1
AND TRUNC(S2.TRANS_DTM) - TRUNC(S1.TRANS_DTM) <60 
WHERE S1.TRANS_DTM >='01-JAN-2015'
AND S2.TRANS_DTM >='01-JAN-2015'
--AND S1.GUID_USER_ID = '4CC6549F-047A-41BF-984D-2CFD8C40C330'
--AND S1.GUID_USER_ID = '00010A8C-DFF3-4380-BA23-26C5EBF6E4C6'
--AND S1.CASE_ID = '02201504240950280001544808'
GROUP BY S1.GUID_USER_ID,

--FIRST SURVEY
S1.CASE_ID,
TRUNC(S1.TRANS_DTM),
TO_CHAR(S1.TRANS_DTM, 'YYYYMMDD') || TRIM(TO_CHAR(S1.STORE_NUM, '000000'))|| ltrim(TO_CHAR(S1.RGSTR_NUM, '00')) || TRIM(to_char(S1.TRANS_ID)),

--SECOND SURVEY
S2.CASE_ID,
TRUNC(S2.TRANS_DTM),
TO_CHAR(S2.TRANS_DTM, 'YYYYMMDD') || TRIM(TO_CHAR(S2.STORE_NUM, '000000'))|| ltrim(TO_CHAR(S2.RGSTR_NUM, '00')) || TRIM(to_char(S2.TRANS_ID))
)