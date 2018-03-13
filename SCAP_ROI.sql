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
    
    
    
