library(tidyverse)
library(DBI)

employee <- jsonlite::read_json("~/sb/code/code/comp/comp220420.json", simplifyVector = TRUE)

#employee %>%
  jsonlite::write_json("~/sb/code/code/comp/comp220420clean.json")

employee <-
  employee %>%
  mutate(
    employee_id = `Employee ID`,
    department_head = `Department Head`,
    name = `Full name`,
    hire_date = as.Date(lubridate::mdy(`Hire Date`)),
    position = `Position`,
    base_salary = as.numeric(str_remove_all(`Base Salary`, "[$,]")),
    benchmark25 = as.numeric(str_remove_all(`Benchmarking 25%...7`, "[$,]")),
    benchmark50 = as.numeric(str_remove_all(`Benchmarking 50%...8`, "[$,]")),
    benchmark75 = as.numeric(str_remove_all(`Benchmarking 75%...9`, "[$,]")),
    department = `Organization (level 2)`,
    manager = Supervisor,
    notes = Notes,
    proposed_position = `Proposed Promotion/New Title`,
    proposed_base = as.numeric(str_remove_all(`Proposed Base Salary`, "[$,]")),
    proposed_benchmark25 = as.numeric(str_remove_all(`Benchmarking 25%...17`, "[$,]")),
    proposed_benchmark50 = as.numeric(str_remove_all(`Benchmarking 50%...18`, "[$,]")),
    proposed_benchmark75 = as.numeric(str_remove_all(`Benchmarking 75%...19`, "[$,]"))
  ) %>%
  select(
    -`Employee ID`,
    -`Department Head`,
    -`Full name`,
    -`Hire Date`,
    -`Position`,
    -`Base Salary`,
    -`Benchmarking 25%...7`,
    -`Benchmarking 50%...8`,
    -`Benchmarking 75%...9`,
    -`Organization (level 2)`,
    -Supervisor,
    -Notes,
    -`Benchmarking 25%...17`,
    -`Benchmarking 50%...18`,
    -`Benchmarking 75%...19`,
    -`Proposed Promotion/New Title`,
    -`Proposed Base Salary`,
    -`Promotional Increase $`,
    -`Market Adjustment $`
  )


glimpse(employee)


employee %>%
  tidyquery::query(
    "
      select
        name, position, proposed_position, base_salary, proposed_base
      where
        proposed_position is not null
    "
  )

#manager_id
#department_id


db <- dbConnect(
  RMariaDB::MariaDB(),
  dbname = "development",
  username = "",
  password = "",
  port = 3306
)

dbGetQuery(
  db,
  "show databases"
)

dbWriteTable(
  db,
  "employee",
  employee,
  overwrite = TRUE
)

dbDisconnect(db)
