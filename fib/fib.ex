defmodule Fib do
  def fib(n) do
    cond do
      n < 2 -> n
      n == n -> fib(n-1) + fib(n-2)
    end
  end
end

IO.puts Fib.fib(38)