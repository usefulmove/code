library(tidyverse)

employee_data <- jsonlite::read_json("~/Desktop/employee_dump.json", simplifyVector = TRUE)

source("~/repos/code/decode/decode.R")
source("~/repos/code/decode/.cora")

decode_overwrite("library", "employee", employee_data)
