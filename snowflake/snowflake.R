# Snowflake LIMS Archive

library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(ggsci)
library(DBI)

snow_db <-
  #connections::connection_open(
  DBI::dbConnect(
    odbc::odbc(),
    Driver       = "/opt/snowflake/snowflakeodbc/lib/universal/libSnowflake.dylib",
    Server       = "lha61820.snowflakecomputing.com",
f   UID          = "Tableau",
    PWD          = "Summer88@@",
    Database     = "FIVETRAN_DATABASE",
    Warehouse    = "FIVETRAN_WAREHOUSE",
    Schema       = "SALESFORCESNOWFLAKE"
  )

status <- 
  snow_db |> 
  dbGetQuery(
    "
      select
        STATUS_C as status,
        count(*) as count
      from SALESFORCESNOWFLAKE.LIMS_SAMPLE_C
      where
        CREATED_DATE like '2022-05%'
          and
        STATUS_C in ('Released', 'Received', 'In Process', 'Retest', 'Rack Retest Required')
      group by STATUS_C
      order by count desc
    "
  )

```{r}

summary_data <- 
  snow_db |> 
  dbGetQuery(
    "
      select -- summary of Snowflake data
        min(RECEIVED_DATE_TIME_C) as `from`,
        max(RECEIVED_DATE_TIME_C) as `to`,
        SAMPLE_TYPE_C as sample_type,
        count(*) as samples
      from
        SALESFORCESNOWFLAKE.LIMS_SAMPLE_C
      group by
        SAMPLE_TYPE_C
      order by
        samples desc
    "
  )

controls <-
  snow_db |> 
  dbGetQuery(
    "
      select -- controls Cq performance (2022)
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
        SAMPLE_TYPE_C not in ('Clinical Samples', 'Filler')  
    "
  )

controls |> jsonlite::write_json("controls.json")

aggregated <- 
  snow_db |> 
  dbGetQuery(
    "
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
      order by week, SAMPLE_TYPE_C, CALL_C 
    "
  )

aggregated |> jsonlite::write_json("aggregated.json")

```

```{r personal_samples}
dedmonds_samples <- 
  snow_db |> 
  dbGetQuery(
    "
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
    "
  )

dedmonds_samples |> View()
```

```{r collected_by_date}
date_samples <- 
  snow_db |> 
  dbGetQuery(
    "
      select
        s.SAMPLE_BARCODE_C,
        trf.COLLECTION_DATE_TIME_C,
        s.RECEIVED_DATE_TIME_C,
        s.STATUS_C as SAMPLE_STATUS,
        s.CREATED_DATE,
        s.LAST_MODIFIED_DATE,
        s.SYSTEM_MODSTAMP,
        s.CALL_C,
        p.LIMS_PLATE_NAME_C,
        p.LIMS_STATUS_C as PLATE_STATUS,
        s.REPORTED_DATE_TIME_C,
        s.REVIEWED_BY_C,
        s.N_1_CQ_1_C,
        s.N_2_CQ_1_C,
        s.RP_CQ_1_C,
        s.SAMPLE_TYPE_C,
        s.RACK_POSITION_ID_C,
        trf.SPECIMEN_SOURCE_C,
        trf.TRANSPORT_MEDIA_C,
        trf.TEST_LOCATION_C,
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
      where
        trf.COLLECTION_DATE_TIME_C like '2022-05-11%'
      order by
        trf.COLLECTION_DATE_TIME_C
    "
  )
```

```{r states}
zipcodes <- jsonlite::read_json("zipcodes.json", simplifyVector = TRUE)

states <- 
  snow_db |> 
  dbGetQuery(
    "
      select
        sbj.ZIP_CODE_C, a.NAME as ACCOUNT
      from
        SALESFORCESNOWFLAKE.LIMS_SAMPLE_C as s
          left join
        SALESFORCESNOWFLAKE.LIMS_REQUISITION_C as trf
            using (SAMPLE_BARCODE_C)
          left join
        SALESFORCESNOWFLAKE.LIMS_SUBJECT_C sbj
            on trf.SUBJECT_C = sbj.ID
          left join
        SALESFORCESNOWFLAKE.ACCOUNT as a
            on s.CLIENT_C = a.ID
      where
        s.RECEIVED_DATE_TIME_C regexp '2022-0[45].+'
          and 
        s.SAMPLE_TYPE_C like 'Clinical Samples'
      order by s.RECEIVED_DATE_TIME_C
    "
  ) |>
  mutate(
    zip = str_extract(ZIP_CODE_C, "[0-9]{5}")
  ) |> 
  left_join(zipcodes, by = c("zip" = "zipcode"))

state_ag <- 
  states |> 
  group_by(
    state
  ) |> 
  summarise(
    count = n()
  ) |> 
  arrange(
    -count
  )

state_acct_ag <- 
  states |> 
  group_by(
    ACCOUNT,
    state
  ) |> 
  summarise(
    count = n()
  ) |> 
  arrange(
    ACCOUNT,
    -count
  )

state_ag |> View()
state_acct_ag |> View()
```

snow_db <-
  DBI::dbDisconnect(
  )
