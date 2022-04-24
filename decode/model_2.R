library(tidyverse)
library(DBI)
library(jsonlite)
source("~/repos/code/decode/decode.R")


# read from remote library database
dof_name <- "iso3166_country_codes"

data <-
  decode_read(
    "library",
    dof_name
  )

# write to local code repository library
decode_overwrite(
  "code",
  dof_name,
  data
)
