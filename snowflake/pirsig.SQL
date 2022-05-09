/*

ACCOUNT
  TODO

LIMS_SAMPLE_C
  SAMPLE_BARCODE_C
  NAME
  CREATED_DATE
 *SAMPLE_TYPE_C
  STATUS_C (regexp '[Rr]eleased')
  CALL_C (is not null)
  LIMS_PLATE_C
  LIMS_WELL_ID_C
  RACK_POSITION_ID_C
  RECEIVED_DATE_TIME_C
  TRF_C
  CLIENT_C
  FLIGHT_C
  N_1_CQ_1_C
  N_1_CQ_2_C
  N_2_CQ_1_C
  N_2_CQ_2_C
  RP_CQ_1_C
  RP_CQ_2_C

LIMS_PLATES_C
  TODO

LIMS_RACK_C
  TODO

LIMS_REQUISITION_SAMPLE_ASSOCIATION_C
  TODO

LIMS_PLATE_SAMPLE_ASSOCIATION_C
  TODO

*/


select *
from LIMS_SAMPLE_C
limit 3 ;


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


select distinct
  ID, NAME
from
  SALESFORCESNOWFLAKE.LIMS_SAMPLE_C ;


select distinct
  RECORD_TYPE_ID
from
  SALESFORCESNOWFLAKE.LIMS_SAMPLE_C 
limit 100 ;


----------------------------------------------


select
  distinct
  SAMPLE_TYPE_C
from
  SALESFORCESNOWFLAKE.LIMS_SAMPLE_C 
limit 100 ;


-- how many controls have we processed
-- this year and what do the quality metrics
-- have to say about them over time?


select
  count(*)
from
  SALESFORCESNOWFLAKE.LIMS_SAMPLE_C 
where
  CREATED_DATE like '2022%'
    and
  SAMPLE_TYPE_C is not null
limit 100 ;


select -- control aggregation SAMPLE_TYPE_C as sample_type,
  count(*) as no_samples
from
  SALESFORCESNOWFLAKE.LIMS_SAMPLE_C 
where
  SAMPLE_TYPE_C is not null
group by
  SAMPLE_TYPE_C ;


SELECT CALL_C AS `call`, count(*) AS total -- pos, neg, inv, inc counts (2022)
FROM SALESFORCESNOWFLAKE.LIMS_SAMPLE_C
WHERE CALL_C IS NOT NULL AND CREATED_DATE like '2022%'
GROUP BY CALL_C ;
  

-- (over time . . .) ----------------------------------------------------------


select -- aggregate quality data by week (2022)
  weekofyear(RECEIVED_DATE_TIME_C) as week,
  SAMPLE_TYPE_C as sample_type,
  CALL_C as call,
  count(*) as no_samples
  -- TODO as retest_perc, (?)
  -- TODO as inconclusive_perc,
  -- TODO as invalid_perc,
  -- TODO as positive_perc
from
  SALESFORCESNOWFLAKE.LIMS_SAMPLE_C 
where
  RECEIVED_DATE_TIME_C like '%2022%'
group by week, SAMPLE_TYPE_C, CALL_C  
order by week, SAMPLE_TYPE_C, CALL_C ;


select -- controls Cq performance
  SAMPLE_BARCODE_C as barcode,
  CREATED_DATE as created_date,
  SAMPLE_TYPE_C as sample_type,
  CALL_C as call,
  N_1_CQ_1_C as n1,
  N_2_CQ_1_C as n2,
  RP_CQ_1_C as rp
from
  SALESFORCESNOWFLAKE.LIMS_SAMPLE_C
where
  RECEIVED_DATE_TIME_C like '%2022%'
    and
  SAMPLE_TYPE_C not in ('Clinical Samples', 'Filler') ;


-- TODO -- inconclusive distribution over the plate (heat map) (?)


-- TODO -- what percentage of calls on negative controls were negative, positive - positive, NTC - NTC?
