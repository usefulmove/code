library(tidyverse)
library(DBI)

# goal :: model interface for loading data from one data source (e.g. CSV)
#         into a different data source (e.g. remote database)

c <-
  cols_only(
    GEO_ID = col_character(),
    NAME = col_character(),
    P1_001N = col_integer(),
    P1_002N = col_integer(),
    P1_003N = col_integer(),
    P1_004N = col_integer(),
    P1_005N = col_integer(),
    P1_006N = col_integer(),
    P1_007N = col_integer(),
    P1_008N = col_integer()
  )
census <- readr::read_csv("~/Desktop/data.csv", col_types = c)

glimpse(census)

census <-
  census %>%
  filter(GEO_ID != "id") %>%
  mutate(
    geo_id = GEO_ID,
    name = NAME,
    total = P1_001N,
    total_one_race = P1_002N,
    white = P1_003N,
    black = P1_004N,
    native = P1_005N,
    asian = P1_006N,
    pac_isl = P1_007N,
    other = P1_008N,
  ) %>%
  select(
    geo_id,
    name,
    total,
    total_one_race,
    white,
    black,
    native,
    asian,
    pac_isl,
    other
  )

library_dob <- decode_getdob("library")

decode_senddob(
  library_dob,
  "us_demographics_census2020",
  census
  )

