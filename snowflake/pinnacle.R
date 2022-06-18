library(tidyverse)
library(DBI)
library(lubridate)

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

#clinical_samples <- # past 24 hours
#  snow_db |>
#  dbGetQuery(
#    str_glue(
#      "
#        select
#          *
#        from SALESFORCESNOWFLAKE.LIMS_SAMPLE_C
#        where
#          TESTING_RECORD_C = FALSE
#            and
#          SAMPLE_TYPE_C = 'Clinical Samples'
#            and
#          REPORTED_DATE_TIME_C >=
#            (select
#               dateadd(hour, -24, max(REPORTED_DATE_TIME_C))
#             from
#               SALESFORCESNOWFLAKE.LIMS_SAMPLE_C)
#      "
#    )
#  )

#clinical_samples <-
#  snow_db |>
#  dbGetQuery(
#    str_glue(
#      "
#        select
#          *
#        from SALESFORCESNOWFLAKE.LIMS_SAMPLE_C
#        where
#          TESTING_RECORD_C = FALSE
#            and
#          SAMPLE_TYPE_C = 'Clinical Samples'
#            and
#          REPORTED_DATE_TIME_C >= '2022-06-09 17:00:00'
#            and
#          REPORTED_DATE_TIME_C <= '2022-06-09 22:00:00'
#      "
#    )
#  )

clinical_plates <-
  snow_db |>
  dbGetQuery(
    str_glue(
      "
        select
          *
        from SALESFORCESNOWFLAKE.LIMS_PLATES_C
        where
          TESTING_RECORD_C = FALSE
            and
          CREATED_DATE >= '2022-05-30 00:00:00'
            and
          CREATED_DATE <= '2022-06-05 00:00:00'
      "
    )
  )

snow_db |> dbDisconnect()

# calculate maximum clinical sample processing rate based on plate creation
p <-
  clinical_plates |>
  arrange(CREATED_DATE)

sph_max <- 0
max_window_end <- as_datetime("2000-01-01")
for (i in 1:nrow(p)) {
   if (p$CREATED_DATE[i] > p$CREATED_DATE[1] + hours(1)) {
     previous_sph_max <- sph_max
     sph_max <-
       max(
         sph_max,
         nrow(
           p |>
             filter(
               CREATED_DATE > p$CREATED_DATE[i] - hours(1),
               CREATED_DATE <= p$CREATED_DATE[i]
             )
         ) * 93 * 4
       )
     if (sph_max != previous_sph_max) {
       max_window_end <- p$CREATED_DATE[i]
     }
   }
}
beepr::beep(3)
sph_max
max_window_end
