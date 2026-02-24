def fib(n: Int, a: Int = 0, b: Int = 1) -> Int:
    if n == 0:
        return a
    if n == 1:
        return b
    return fib(n - 1, b, a + b)


def main():
    print(fib(38))
