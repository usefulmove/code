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

from_date <- Sys.Date() - 7

status <- 
  snow_db |> 
  DBI::dbGetQuery(
    stringr::str_glue(
      "
        select
          STATUS_C as status,
          count(*) as count,
          max(LAST_MODIFIED_DATE) as last_updated
        from SALESFORCESNOWFLAKE.LIMS_SAMPLE_C
        where
          CREATED_DATE > '{from_date}'
            and
          STATUS_C in ('Released', 'Received', 'In Process', 'Retest', 'Rack Retest Required')
        group by STATUS_C
        order by count desc
      "
    )
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
c_released <- status |> filter(STATUS == "Released") |> pull(COUNT)
c_accessioned <- status |> filter(STATUS == "Received") |> pull(COUNT)
c_inprocess <- status |> filter(STATUS == "In Process") |> pull(COUNT)
c_retest <- status |> filter(STATUS == "Retest") |> pull(COUNT)
c_rackretest <- status |> filter(STATUS == "Rack Retest Required") |> pull(COUNT)
last_update <- stringr::str_extract(max(status$LAST_UPDATED), ".*[^UTC]$")

msg <-
  stringr::str_glue("\n
                     SB process time remaining: {format(process_time$sb_proces_time, digits = 3)} hours\n
                       released: {c_released}
                       accessioned: {c_accessioned}
                       in process: {c_inprocess}
                       retest: {c_retest}
                       rack retest req.: {c_rackretest}
                       (since {from_date})\n
                     (updated: {last_update})
                     \n")

print(msg)