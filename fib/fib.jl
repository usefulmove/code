#!/usr/bin/env julia

# read argument
args = ARGS
if (length(args) == 0)
  o = 0
else
  o = parse(Int64, args[1])
end

# truncate argument to integer and make 0 if negative
if (o < 0)
  o = 0
else
  o = floor(Int, o)
end

function fib(n)
  if n == 0
    0
  elseif (n == 1) || (n == 2)
    1
  else
    fib(n - 1) + fib(n - 2)
  end
end


println(
  string(
    string(fib(o)),
    "\r"
  )
)
