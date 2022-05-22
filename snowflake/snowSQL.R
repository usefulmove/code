#!/usr/bin/end Rscript

# Snowflake LIMS Archive (SQL command line interface)

# usage:  % snowSQL <file>
#
# where: alias snowSQL="$REPO/code/snowflake/snowSQL.R"

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(DBI))

args <- commandArgs(trailingOnly = TRUE)

print(args) # debug only (REMOVE)
print(args[1]) # debug only (REMOVE)
print(getwd()) # debug only (REMOVE)

if (length(args) == 0) {
  stop("( error: at least one argument must be supplied (SQL file) )")
} else {
  file_name <- args[1]
  dir <- getwd()
}

print(str_glue("{dir}", "{file_name}")) # debug only (REMOVE)

print(str_glue("\n\n( connecting to Snowflake database )\n\n"))

SQL_query <- readr::read_file(str_glue("{dir}","{file_name}"))

print(SQL_query) # debug only (REMOVE)


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
          TESTING_RECORD_C = FALSE
            and
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


# build return message

add_message <- function(.string_name, .additional_string) {
  get(.string_name, envir = .GlobalEnv)
  assign(.string_name, return |> str_glue("{.additional_string}\n\r"), envir = .GlobalEnv)
}

return <-
  str_glue("\n\r\n\rSB process time remaining: {format(process_time$sb_proces_time, digits = 3)} hours ( estimate )\n\r")

status_returns <- 
  status |> 
    mutate(
      message = purrr::map2_chr(STATUS, COUNT, ~ str_glue("  {str_to_lower(.x)}: {format(.y, big.mark=\",\")}"))
    ) |> 
    pull(message)

for (i in 1:length(status_returns)) add_message("return", str_glue("{status_returns[[i]]}"))

add_message("return", "")

add_message(
  "return",
  str_glue("( start: {str_extract(min(status$FIRST_UPDATED), \".*[^UTC]$\")} )")
)

add_message(
  "return",
  str_glue("( end: {str_extract(max(status$LAST_UPDATED), \".*[^UTC]$\")} )")
)

print(return)
