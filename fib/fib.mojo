from sys import argv


fn fib(n: Int, a: Int = 0, b: Int = 1) -> Int:
    if n == 0:
        return a
    if n == 1:
        return b
    return fib(n - 1, b, a + b)


fn main() raises:
    var args = argv()

    if len(args) < 2:
        print("  error: no argument provided")
        return

    var n: Int
    try:
        n = Int(args[1])
    except:
        print("  error: bad argument")
        return

    if n < 0:
        print("  error: argument must be non-negative")
        return

    print("  {}".format(fib(n)))
