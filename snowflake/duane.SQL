select
  s.SAMPLE_BARCODE_C as barcode,
  -- sbj.LAST_NAME_C as lname,
  -- sbj.FIRST_NAME_C as fname,
  -- sbj.CITY_C as city,
  -- sbj.ZIP_CODE_C as zipcode,
  s.STATUS_C as status,
  trf.COLLECTION_DATE_TIME_C as collection,
  -- s.CREATED_DATE as created,
  s.CALL_C as call,
  p.LIMS_PLATE_NAME_C as plate,
  p.LIMS_STATUS_C as plate_status,
  -- s.RECEIVED_DATE_TIME_C as received,
  -- s.REPORTED_DATE_TIME_C as reported,
  -- s.REVIEWED_BY_C as reviewed,
  s.N_1_CQ_1_C as n1,
  s.N_2_CQ_1_C as n2,
  s.RP_CQ_1_C as rp,
  -- s.SAMPLE_TYPE_C as sample_type,
  -- s.RACK_POSITION_ID_C as rack_pos,
  -- trf.SPECIMEN_SOURCE_C as source,
  -- trf.TRANSPORT_MEDIA_C as media,
  -- trf.DATA_SOURCE_C as data_source,
  -- r.RACK_NAME_NUMBER_C as rack_name,
  e.NAME as equip
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
limit 20
