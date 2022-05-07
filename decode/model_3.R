library(tidyverse)

employee_data <- jsonlite::read_json("~/Desktop/employee_dump.json", simplifyVector = TRUE)

source("~/repos/code/decode/decode.R")

decode_create("library", "employee", employee_data)
