#!/usr/bin/env julia

# read argument
args = ARGS
if (length(args) == 0)
  o = 0
else
  o = parse(UInt64, args[1])
end

function fib(n::UInt64)
  if n <= 0
    0
  elseif (n < 3)
    1
  else
    fib(n - 1) + fib(n - 2)
  end
end


println(string(fib(o), "\r"))
