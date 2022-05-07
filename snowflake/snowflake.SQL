select *
from LIMS_SAMPLE_C
limit 3 ;

-- CALL_C is not null
-- STATUS_C like 'Released'
-- CREATED_DATE

select distinct 
  STATUS_C
from LIMS_SAMPLE_C ;

select *
from INFORMATION_SCHEMA."TABLES"
order by TABLE_NAME ;

select CALL_C as result, count(*) as number 
from LIMS_SAMPLE_C
where
  CALL_C is not null 
    and 
  STATUS_C like 'Released'
    and 
  CREATED_DATE between '2022-05-06' and '2022-05-07'
group by CALL_C
order by number desc ;

select
  min(CREATED_DATE) as `from`,
  max(CREATED_DATE) as `to`,
  count(*) as samples
from
  LIMS_SAMPLE_C ;
