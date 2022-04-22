library(tidyverse)
library(DBI)

o <- readr::read_csv("~/repos/code/amazon/data", locale = locale(encoding = "UTF-8"))

o <-
  o %>%
  mutate(
    `Order Date` = lubridate::mdy(`Order Date`),
    `List Price Per Unit` = as.numeric(str_remove_all(`List Price Per Unit`, "[$,]")),
    `Purchase Price Per Unit` = as.numeric(str_remove_all(`Purchase Price Per Unit`, "[$,]")),
    `Shipment Date` = lubridate::mdy(`Shipment Date`),
    `Item Subtotal` = as.numeric(str_remove_all(`Item Subtotal`, "[$,]")),
    `Item Subtotal Tax` = as.numeric(str_remove_all(`Item Subtotal Tax`, "[$,]")),
    `Item Total` = as.numeric(str_remove_all(`Item Total`, "[$,]"))
  )

db <-
  dbConnect(
    RMariaDB::MariaDB(),
    dbname = "financial",
    host = "coradbinstance.chkmsmjosdxs.us-west-1.rds.amazonaws.com",
    username = "",
    password = "",
    port = 3306
  )

db %>%
  dbGetQuery(
    "show tables;"
  )

db %>%
  dbWriteTable(
    "amazon",
    o %>%
      mutate(
        Seller = str_replace_all(Seller, "[^[:alnum:]]", ""),
        Title = str_replace_all(Title, "[^[:alnum:]]", "")
      ),
    append = TRUE
  )

db %>%
  dbDisconnect()

