#!/usr/bin/end Rscript

# Snowflake lab information management system (LIMS) archive command line interface

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(DBI))

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  stop("( error: at least one argument must be supplied (SQL file) )")
}

SQL_query <- readr::read_file(str_glue("{args[1]}"))

print(str_glue("\n\n( connecting to Snowflake database )\n\n"))

if (Sys.info()['sysname'] == "Linux") {
  snowflake_driver = "/usr/lib/snowflake/odbc/lib/libSnowflake.so" # linux
} else {
  snowflake_driver = "/opt/snowflake/snowflakeodbc/lib/universal/libSnowflake.dylib" # macOS
}

snow_db <-
  dbConnect(
    odbc::odbc(),
    Driver       = snowflake_driver,
    Server       = "lha61820.snowflakecomputing.com",
    UID          = "Tableau",
    PWD          = "Summer88@@",
    Database     = "FIVETRAN_DATABASE",
    Warehouse    = "FIVETRAN_WAREHOUSE",
    Schema       = "SALESFORCESNOWFLAKE"
  )

return <- snow_db |> dbGetQuery(str_glue("{SQL_query}"))

snow_db |> dbDisconnect()

print(as_tibble(return))
