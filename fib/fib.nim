proc fib(n: int): int =
    case n
    of 0, 1:
        n
    else:
        fib(n - 1) + fib(n - 2)

echo fib(38)
