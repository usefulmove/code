library(tidyverse)
library(DBI)

comp <- read_csv("~/sb/hr/comp220420")

comp %>%
  jsonlite::write_json("~/sb/code/code/comp220420.json")

