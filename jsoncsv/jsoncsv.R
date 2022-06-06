#!Rscript

arg <- commandArgs(trailingOnly = TRUE)

print(stringr::str_glue("converting {arg[1]} to {arg[2]}"))

# read JSON file into dataframe
df <- jsonlite::read_json(arg[1], simplifyVector = TRUE)

# write dataframe to CSV
readr::write_csv(df, arg[2])
