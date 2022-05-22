select
  s.SAMPLE_BARCODE_C,
  sbj.LAST_NAME_C,
  sbj.FIRST_NAME_C,
  sbj.CITY_C,
  sbj.ZIP_CODE_C,
  s.STATUS_C as SAMPLE_STATUS,
  trf.COLLECTION_DATE_TIME_C,
  s.CREATED_DATE,
  s.CALL_C,
  p.LIMS_PLATE_NAME_C,
  p.LIMS_STATUS_C as PLATE_STATUS,
  s.RECEIVED_DATE_TIME_C,
  s.REPORTED_DATE_TIME_C,
  s.REVIEWED_BY_C,
  s.N_1_CQ_1_C,
  s.N_2_CQ_1_C,
  s.RP_CQ_1_C,
  s.SAMPLE_TYPE_C,
  s.RACK_POSITION_ID_C,
  trf.SPECIMEN_SOURCE_C,
  trf.TRANSPORT_MEDIA_C,
  trf.DATA_SOURCE_C,
  r.RACK_NAME_NUMBER_C,
  e.NAME as EQUIPMENT_NAME
from
  SALESFORCESNOWFLAKE.LIMS_SAMPLE_C as s
    left join
  SALESFORCESNOWFLAKE.LIMS_REQUISITION_C as trf
      using (SAMPLE_BARCODE_C)
    left join
  SALESFORCESNOWFLAKE.LIMS_RACK_C as r
      on s.LIMS_RACK_ID_C = r.ID
    left join
  SALESFORCESNOWFLAKE.LIMS_PLATES_C as p
      on s.LIMS_PLATE_C = p.ID
    left join
  SALESFORCESNOWFLAKE.LIMS_EQUIPMENT_C as e
      on p.EXTRACTION_UNIT_C = e.ID
          left join
        SALESFORCESNOWFLAKE.LIMS_SUBJECT_C sbj
            on trf.SUBJECT_C = sbj.ID
      where
        LAST_NAME_C like 'Edmonds'
          and
        FIRST_NAME_C like 'Robert'
          and
        DOB_C like '1975-12-10'
      order by
        trf.COLLECTION_DATE_TIME_C desc
