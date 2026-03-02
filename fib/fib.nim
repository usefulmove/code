proc fib(n: int): int =
    case n
    of 0, 1:
        n
    else:
        fib(n - 1) + fib(n - 2)


proc fib2(n: int, a: int = 0, b: int = 1): int =
    case n
    of 0:
        a
    of 1:
        b
    else:
        fib2(n - 1, b, a + b)


echo fib2(38)
