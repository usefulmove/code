# Snowflake LIMS Archive

library(dplyr)

snow_db <-
  #connections::connection_open(
  DBI::dbConnect(
    odbc::odbc(),
    Driver       = "/opt/snowflake/snowflakeodbc/lib/universal/libSnowflake.dylib",
    Server       = "lha61820.snowflakecomputing.com",
    UID          = "Tableau",
    PWD          = "Summer88@@",
    Database     = "FIVETRAN_DATABASE",
    Warehouse    = "FIVETRAN_WAREHOUSE",
    Schema       = "SALESFORCESNOWFLAKE"
  )

status <- 
  snow_db |> 
  DBI::dbGetQuery(
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

process_time <-
  status |> 
  filter(
    STATUS %in% c("Received", "In Process", "Retest", "Rack Retest Required")
  ) |> 
  summarise(
    sb_proces_time = sum(COUNT) / 1300.0 / 4.0
  )

snow_db |> DBI::dbDisconnect()

# return
cat(stringr::str_glue("SB process time remaining: {format(process_time$sb_proces_time, digits = 3)} hours"))