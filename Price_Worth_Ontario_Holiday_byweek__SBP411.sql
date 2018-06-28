WITH SQ AS
(SELECT
    c.FSCL_WK_IN_YR_NUM
    ,c.FSCL_YR_NUM
    ,(CASE WHEN ce.STORE_NUM IN (3515,4007,4014,4095,4097,4205,4206,4207,4209,4210,4214,4215,4226,4227,4229,4233,
      4238,4240,4241,4245,4249,4252,4254,4258,4265,4268,4276,4277,4278,4279,4282,4283,4287,4288,4290,4291,
      4292,4296,4303,4304,4307,4313,4314,4318,4319,4320,4323,4329,4336,4338,4340,4343,4344,4350,4357,4358,
      4366,4371,4384,4387,4395,4396,4408,4414,4424,4427,4430,4458,4469,4472,4476,4477,4478,4484,4488,4489,
      4494,4508,4509,4511,4516,4517,4523,4525,4531,4540,4551,4556,4572,4573,4575,4580,4581,4582,4584,4585,
      4594,4598,4606,4607,4608,4609,4612,4613,4621,4627,4635,4645,4647,4648,4649,4652,4657,4665,4666,4667,
      4675,4678,4683,4694,4696,4701,4702,4703,4709,4711,4716,4717,4718,4719,4720,4732,4733,4742,4743,4747,
      4756,4757,4761,4762,4764,4768,4772,4774,4777,4788,4789,4790,4791,4798,4800,4803,4804,4807,4815,4816,
      4817,4824,4829,4833,4835,4850,4856,4860,4863,4872,4874,4886,4892,4897,4901,4902,4904,4905,4906,4911,
      4912,4915,4916,4922,4927,4929,4932,4938,4944,4951,4957,4962,4965,4979,4980,4982,5161,6019,12467,13035,
      15121,15409,15559,15624,15700,15718,15720,15722,15726,15908,15928,15977,16078,16144,16201,16278,16300,
      16466,16467,16709,16867,16896,17016,17181,17203,17218,17220,17341,17342,17411,17557,17624,17627,17650,
      17843,17922,17995,18196,18433,18962,19572,19770,19795,19818,19841,19842,19865,20296,20593,20687,20834,
      21590,21784,21883,22091,22369,22642,22838,23048,23090,23244,23247,23274,24008,24209,24210,25138,27142,
      27173,20172,23283,48145,29392,47180,4960,47181,29393,23284,48418,23314,23691,24222,25370,25624,25747,
      25829,25915,26281,26578,27978,28081,29141,29175,29443,48986,52238,52243,52244,52245,49964,49533,50007,
      49532,49300,29969,50289,47563,49141,28043,50472,50490,51733,50942,52003,51937,51377,51132,47666,51922,
      49673,50879,52002,51899,48602,49671,50088,52762,52381,48809,52984,52184,52144,23275,53844,56224,53974,
      55381,55380,54812,54228,53060,54658,
      3896,3999,4208,4217,4220,4225,4232,4246,4251,4263,4270,4295,4300,4309,4312,4361,4363,4374,4386,4402,
      4438,4448,4455,4459,4460,4486,4495,4497,4507,4510,4518,4524,4636,4637,4646,4724,4765,4766,4779,4801,
      4802,4805,4830,4831,4832,4841,4859,4862,4869,4870,4998,6028,8178,15631,15689,15917,16140,16997,17340,
      17771,19355,19474,19507,19550,19664,19792,20622,20837,20991,21589,21792,21891,22038,22078,22368,22438,
      22836,22839,22866,23092,23246,23689,27133,28762,29903,47561,49897) THEN 1 ELSE 0 END) AS PRICE_INCREASE
    ,SUM(CASE WHEN ce.RSPNS_ID = '7' AND ce.QSTN_ID = 'Q2_8' THEN 1 ELSE 0 END) AS Q2_8_TB_COUNT
    ,COUNT(CASE WHEN ce.QSTN_ID = 'Q2_8' THEN ce.QSTN_ID END) AS Q2_8_RSP_COUNT

FROM APPDWH.AFT_CV_SRVY_RSPNS ce
  INNER JOIN APPDWH.ADT_CAL c
    ON TRUNC(ce.TRANS_DTM) = c.CAL_DT
      
  INNER JOIN APPDWH.ADT_STORE org
      ON ce.STORE_NUM = org.STORE_NUM
      AND org.OWNR_TYPE_CD IN ('CO')
      AND org.CNTRY_CD IN ('CA')
      --AND org.ST_CD = 'ON' -- ONTARIO
         
WHERE ce.RSPNS_ID <> '9'  -- rspns_id = 9 for unanswered questions
  AND ce.QSTN_ID NOT IN ('Q1','Q11') -- these questions are not in Customer Connection or Store Operations scores

GROUP BY
    c.FSCL_WK_IN_YR_NUM
    ,c.FSCL_YR_NUM
    ,ce.STORE_NUM
)
SELECT     
    SQ.FSCL_WK_IN_YR_NUM
    ,SQ.FSCL_YR_NUM
    ,SQ.PRICE_INCREASE
    ,ROUND(SUM(SQ.Q2_8_TB_COUNT)/SUM(SQ.Q2_8_RSP_COUNT),4) AS PRICE_SCORE
FROM SQ
GROUP BY
    SQ.FSCL_WK_IN_YR_NUM
    ,SQ.FSCL_YR_NUM
    ,SQ.PRICE_INCREASE
    
ORDER BY
    SQ.PRICE_INCREASE
    ,SQ.FSCL_YR_NUM
    ,SQ.FSCL_WK_IN_YR_NUM