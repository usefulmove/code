library(tidyverse)
library(tidyquery)
library(rvest)
source("~/repos/code/decode/decode.R")

flights2018 <- read_csv("2018.csv")
beepr::beep(5)

flights2018 %>%
  filter(
    DISTANCE == max(DISTANCE)
  ) %>%
  select(
    ORIGIN,
    DEST,
    DISTANCE,
    OP_CARRIER,
    OP_CARRIER_FL_NUM
  )

flights2018 %>%
  distinct(OP_CARRIER) %>%
  arrange(OP_CARRIER) %>%
  View()

flights2018 %>%
  filter(
    OP_CARRIER == "DL",
    ORIGIN == "OAK"
  ) %>%
  distinct(DEST)

flights2018 %>%
  filter(
    ORIGIN == "OAK" #| DEST == "OAK"
  )

flights2018 %>%
  query(
    " select ORIGIN, count(*) * 2 as count
      group by ORIGIN
      order by count desc
    "
  )

flights2018 %>%
  query(
    " select ORIGIN, avg(WEATHER_DELAY) as avg_delay
      group by ORIGIN
      order by avg_delay
    "
  )

flights2018 %>%
  query(
    " select ORIGIN, DEST, OP_CARRIER_FL_NUM, count(*) as TOTAL_FLIGHTS
      group by OP_CARRIER_FL_NUM, ORIGIN, DEST
      order by TOTAL_FLIGHTS desc
    "
  )

flights2018 %>%
  filter(
    ORIGIN %in% c("RDD")
  )


flights2018 %>%
  left_join(
    airline_codes,
    by = c("OP_CARRIER" = "code")
  ) %>%
  select(
    OP_CARRIER,
    name
  ) %>%
  distinct(
    OP_CARRIER,
    name
  )

flights2018 %>%
  left_join(
    airline_codes,
    by = c("OP_CARRIER" = "code")
  ) %>%
  group_by(name) %>%
  summarise(
    total_flights = n(),
    total_miles = sum(DISTANCE),
    miles_per_flight = total_miles / total_flights
  ) %>%
  arrange(-miles_per_flight)


# --------------------------------------------------------------------------------------- #

read_csv("2009.csv") %>%
  decode_append(
    "local",
    "us_flights"
  )

read_csv("2010.csv") %>%
  decode_append(
    "local",
    "us_flights"
  )

read_csv("2011.csv") %>%
  decode_append(
    "local",
    "us_flights"
  )

read_csv("2012.csv") %>%
  decode_append(
    "local",
    "us_flights"
  )

read_csv("2013.csv") %>%
  decode_append(
    "local",
    "us_flights"
  )

read_csv("2014.csv") %>%
  decode_append(
    "local",
    "us_flights"
  )

read_csv("2015.csv") %>%
  decode_append(
    "local",
    "us_flights"
  )

read_csv("2016.csv") %>%
  decode_append(
    "local",
    "us_flights"
  )

read_csv("2017.csv") %>%
  decode_append(
    "local",
    "us_flights"
  )

read_csv("2018.csv") %>%
  decode_append(
    "local",
    "us_flights"
  )

beepr::beep(2)


# a more functional approach -------------------------------------------------------- #

# define vector of year (values)

library(purrr)

years <- 2018:2009

years %>%
  map(
    function(y) read_csv(glue::glue("{y}.csv")) %>% decode_append("local","flights")
  )

(sqrt(5)-1)/2
