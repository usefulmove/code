select
  CALL_C as call,
  count(*) as count,
  max(CREATED_DATE) as last_updated,
  (select count(*) from SALESFORCESNOWFLAKE.LIMS_SAMPLE_C where TESTING_RECORD_C = FALSE and SAMPLE_TYPE_C = 'Clinical Samples') as total_all_types
from SALESFORCESNOWFLAKE.LIMS_SAMPLE_C
where
  TESTING_RECORD_C = FALSE
    and
  SAMPLE_TYPE_C = 'Clinical Samples'
group by CALL_C
order by count desc ;

