#!/usr/bin/Rscript

# read argument
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  out <- 0
} else if (!is.numeric(as.numeric(args[1]))) {
  print("( error: non-numeric argument )")
  out <- 0
} else {
  out <- as.integer(args[1])
}

fib <- function(n) {
  if (n < 2) {
    n
  } else {
    fib(n-1) + fib(n-2)
  }
}

print(stringr::str_glue("{fib(out)}"))