# Snowflake LIMS Archive

library(dplyr)
library(stringr)
library(DBI)

snow_db <-
  dbConnect(
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
  dbGetQuery(
    str_glue(
      "
        select
          STATUS_C as status,
          count(*) as count,
          min(LAST_MODIFIED_DATE) as first_updated,
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

snow_db |> dbDisconnect()

process_time <-
  status |> 
  filter(
    STATUS %in% c("Received", "In Process", "Retest", "Rack Retest Required")
  ) |> 
  summarise(
    sb_proces_time = sum(COUNT) / 1300.0 / 4.0
  )


# return message
data_from_date <- str_extract(min(status$FIRST_UPDATED), ".*[^UTC]$")
data_to_date <- str_extract(max(status$LAST_UPDATED), ".*[^UTC]$")

return_msg <-
  str_glue("\n\nSB process time remaining: {format(process_time$sb_proces_time, digits = 3)} hours ( estimate )\n")

status_msgs <- 
  status |> 
    mutate(
      message = purrr::map2_chr(STATUS, COUNT, ~ str_glue("  {str_to_lower(.x)}: {.y}"))
    ) |> 
    pull(message)

for (i in 1:length(status_msgs)) return_msg <- return_msg |> str_glue("\n{status_msgs[[i]]}\n")

return_msg <- 
  return_msg |>
  str_glue("\n\n( start: {data_from_date} )\n") |>
  str_glue("\n( end: {data_to_date} )\n\n")

print(return_msg)
