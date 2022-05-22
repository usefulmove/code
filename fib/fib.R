#!/usr/bin/Rscript

# read argument
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  o <- 0
} else if (!is.numeric(as.numeric(args[1]))) {
  print("( error: non-numeric argument )")
  o <- 0
} else {
  o <- as.numeric(args[1])
}

# truncate argument to integer and make 0 if negative
if (o < 0) {
  o <- 0
} else {
  o <- as.integer(o)
}

fib <- function(n) {
  if (n == 0) {
    0
  } else if ( (n == 1) || (n == 2) ) {
    1
  } else {
    fib(n - 1) + fib(n - 2)
  }
}

print(stringr::str_glue("{fib(o)}"))
