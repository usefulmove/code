select
  count(*) / 1 as samples_per_hour,
  count(*) / 1 * 24 as equvalent_spd
from
  SALESFORCESNOWFLAKE.LIMS_SAMPLE_C as s
where
  TESTING_RECORD_C = FALSE
    and
  SAMPLE_TYPE_C = 'Clinical Samples'
    and
  REPORTED_DATE_TIME_C >=
    (select
      dateadd(hour, -1, max(REPORTED_DATE_TIME_C))
    from
      SALESFORCESNOWFLAKE.LIMS_SAMPLE_C)
