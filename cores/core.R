library(tidyverse)
library(DBI)
library(dbplyr)
library(readr)
library(jsonlite)



core_tags <- read_json("~/repos/code/cores/core_tags.json", simplifyVector = TRUE)



dev_db <- dbConnect( # remote MySQL development database
  RMariaDB::MariaDB(),
  dbname = "b28kw3czffvrcmddjxri",
  host = "b28kw3czffvrcmddjxri-mysql.services.clever-cloud.com",
  port = 3306,
  user = "uregdz06i7ou4d65",
  password = "q6sGFI1tu1fsAvIPVFQh"
)



dbWriteTable(
  dev_db,
  "core_tags",
  core_tags,
  overwrite = TRUE
)



db_local <- dbConnect( # local MySQL database
  RMariaDB::MariaDB(),
  dbname = "minic",
  host = "127.0.0.1",
  port = 3306,
  user = "",
  password = ""
)

dbWriteTable(
  db_local,
  "tags_dups",
  tags_dups,
  overwrite = TRUE
)



tags_dups <- readr::read_csv("~/repos/code/cores/tags_inc_dups") # all tags (including duplicates)



tags_dups_id <- # duplicated tags with unique ids
  tags_dups %>%
  arrange(tag) %>%
  mutate(
    id = row_number()
  )

tags_dis_id <- # distinct (non-duplicate) tags with single id
  tags_dups_id %>%
  distinct(tag, .keep_all = TRUE)



tags_mult <- # tags that only occur more than one time in the list
  tags_dups_id %>%
  left_join(tags_dis_id, by = c("id" = "id")) %>%
  filter( # remove tags that only occur once
    is.na(tag.y)
  ) %>%
  mutate(
    tag = tag.x
  ) %>%
  distinct(tag) %>%
  filter(
    !is.null(tag),
    tag != "NA",
    str_length(tag) > 2 | tag == "69"
  )



dbWriteTable(
  dev_db,
  "core_tags",
  tags_mult,
  overwrite = TRUE
)
beepr::beep(2)



# backup core project tables
cores_backup <-
  dbGetQuery(
    dev_db,
    "select * from cores"
  )
core_tags_backup <-
  dbGetQuery(
    dev_db,
    "select * from core_tags"
  )
core_categories_backup <-
  dbGetQuery(
    dev_db,
    "select * from core_categories"
  )

jsonlite::write_json(
  cores_backup,
  "~/repos/code/cores/cores_22180754.json"
)
jsonlite::write_json(
  core_tags_backup,
  "~/repos/code/cores/core_tags_22180754.json"
)
jsonlite::write_json(
  core_categories_backup,
  "~/repos/code/cores/core_categories_22180754.json"
)

beepr::beep(2)
