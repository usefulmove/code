#!/usr/bin/env julia

# read argument
args = ARGS
if (length(args) == 0)
  out = 0
else
  out = parse(UInt64, args[1])
end

function fib(n::UInt64)
  if n < 2
    n
  else
    fib(n - 1) + fib(n - 2)
  end
end

println(string(fib(out), "\r"))