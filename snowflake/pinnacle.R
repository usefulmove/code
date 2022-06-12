library(tidyverse)
library(DBI)

snow_db <-
  dbConnect(
    odbc::odbc(),
    Driver       = "/opt/snowflake/snowflakeodbc/lib/universal/libSnowflake.dylib", # macSO
    #Driver       = "/usr/lib/snowflake/odbc/lib/libSnowflake.so", # linux
    Server       = "lha61820.snowflakecomputing.com",
    UID          = "Tableau",
    PWD          = "Summer88@@",
    Database     = "FIVETRAN_DATABASE",
    Warehouse    = "FIVETRAN_WAREHOUSE",
    Schema       = "SALESFORCESNOWFLAKE"
  )

clinical_samples <- 
  snow_db |> 
  dbGetQuery(
    str_glue(
      "
        select
          *
        from SALESFORCESNOWFLAKE.LIMS_SAMPLE_C
        where
          TESTING_RECORD_C = FALSE
            and
          SAMPLE_TYPE_C = 'Clinical Samples'
            and
          REPORTED_DATE_TIME_C >=
            (select
               dateadd(hour, -24, max(REPORTED_DATE_TIME_C))
             from
               SALESFORCESNOWFLAKE.LIMS_SAMPLE_C)
      "
    )
  )

snow_db |> dbDisconnect()

clinical_samples |>
  ggplot() +
    geom_histogram(
      aes(
        x = REPORTED_DATE_TIME_C
      ), 
      bins = 24,
      alpha = 0.7,
      color = "black",
      fill = "orange"
    ) +
    labs(
      subtitle = "SummerBio Output",
      x = "",
      y = "number of clinical samples reported"
    )
