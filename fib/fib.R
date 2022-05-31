#!/usr/bin/Rscript

# read argument
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  o <- 0
} else if (!is.numeric(as.numeric(args[1]))) {
  print("( error: non-numeric argument )")
  o <- 0
} else {
  o <- as.integer(args[1])
}

fib <- function(n) {
  if (n <= 0) {
    0
  } else if (n < 3) {
    1
  } else {
    fib(n - 1) + fib(n - 2)
  }
}

print(stringr::str_glue("{fib(o)}"))
