#!/usr/bin/env julia

# read argument
args = ARGS
if (length(args) == 0)
  out = 0
else
  out = parse(UInt64, args[1])
end

# recursive solution
function fib(n::UInt64)
  if n < 2
    n
  else
    fib(n - 1) + fib(n - 2)
  end
end

# tail recursive solution
function fib2(n::UInt64, a = 0, b = 1)
  if n == 0
    a
  elseif n == 1
    b
  else
    fib2(n - 1, b, a + b)
  end
end

println(string(fib2(out), "\r"))