library(tidyverse)
library(DBI)

employee <- jsonlite::read_json("~/sb/code/code/comp/comp220420.json", simplifyVector = TRUE)

employee %>%
  jsonlite::write_json("~/sb/code/code/comp/comp220420clean.json")

employee %>%
  mutate(
    employee_id = `Employee ID`,
    department_head = `Department Head`,
    name = `Full name`,
    hire_date = `Hire Date`,
    position = `Position`,
    base_salary = `Base Salary`,
    benchmark25 = `Benchmarking 25%...7`,
    benchmark50 = `Benchmarking 50%...8`,
    benchmark75 = `Benchmarking 75%...9`,
    department = `Organization (level 2)`,
    manager = Supervisor,
    notes = Notes,
    benchmark25 = `Benchmarking 25%...17`,
    benchmark50 = `Benchmarking 50%...18`,
    benchmark75 = `Benchmarking 75%...19`
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
  ) %>%
  View()

#manager_id
#department_id
