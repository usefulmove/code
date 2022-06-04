#!Rscript

arg <- commandArgs(trailingOnly = TRUE)

print(stringr::str_glue("converting {arg[1]} to {arg[2]}"))

# read CSV file into dataframe
df <- readr::read_csv(arg[1])

# write dataframe to JSON
jsonlite::write_json(df, arg[2])
