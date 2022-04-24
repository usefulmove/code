library(tidyverse)
library(DBI)
library(jsonlite)
source("~/repos/code/decode/decode.R")

data <-
  decode_read(
    "library",
    "us_demographics_census2020"
  )

decode_overwrite(
  "code",
  "us_demographics_census2020",
  data
)


data <-
  decode_read(
    "library",
    "eu_countries"
  )

decode_overwrite(
  "code",
  "eu_countries",
  data
)


data <-
  decode_read(
    "library",
    "us_states"
  )

decode_overwrite(
  "code",
  "us_states",
  data
)
