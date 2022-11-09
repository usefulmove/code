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

fib2 <- function(n, a = 0, b = 1) {
  if (n == 0) {
    a
  } else if  (n == 1) {
    b
  } else {
    fib2(n-1, b, a+b)
  }
}

print(stringr::str_glue("{fib2(out)}"))