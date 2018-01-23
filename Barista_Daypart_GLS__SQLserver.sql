--Pulls dominant daypart worked by personnel number and date
--Table created by James Liu

SELECT j.PERSONNELNUMBER, CONVERT(DATE,j.BUS_DT) AS BUS_DT, j.DC_DAYPART FROM jaliu.BaristaTurnover_Daypart_Shifts_Min_Labor_GLS j
  --WHERE CONVERT(DATE,j.BUS_DT) BETWEEN '10/23/2017' AND '10/30/2017'
  ORDER BY CONVERT(DATE,j.BUS_DT) desc


















