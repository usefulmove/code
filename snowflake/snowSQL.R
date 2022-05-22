#!/usr/bin/end Rscript

# Snowflake LIMS Archive (SQL command line interface)

# usage:  % snowSQL <file>
#
# where: alias snowSQL="$REPO/code/snowflake/snowSQL.R"

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(DBI))

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  stop("( error: at least one argument must be supplied (SQL file) )")
} else {
  file_name <- args[1]
  dir <- getwd()
}

print(str_glue("\n\n( connecting to Snowflake database )\n\n"))

SQL_query <- readr::read_file(str_glue("{dir}/","{file_name}"))

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

return <- snow_db |> dbGetQuery(str_glue("{SQL_query}"))

snow_db |> dbDisconnect()

print(return)
