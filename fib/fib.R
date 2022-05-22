#!/usr/bin/R

# read argument
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  n <- 0
} else if (!is.numeric(as.numeric(args[1]))) {
  print("( error: non-numeric argument )")
  n <- 0
} else {
  n <- as.numeric(args[1])
}

# truncate argument to integer and make 0 if negative
if (n < 0) {
  n <- 0
} else {
  n <- n %/% 1
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

print(stringr::str_glue("{fib(n)}"))
