package main

import "core:os"
import "core:fmt"
import "core:strconv"

fib :: proc(n: int) -> int {
    if n < 2 { return n }
    return fib(n-1) + fib(n-2)
}

fib2 :: proc(n: int, a: int = 0, b: int = 1) -> int {
    switch n {
        case 0:
            return a
        case 1:
            return b
        case:
            return fib2(n - 1, b, a + b)
    }
}

main :: proc() {
    if len(os.args) > 1 {
        n, ok := strconv.parse_int(os.args[1])
        if ok { fmt.printf("  %d\n", fib2(n)) }
        else { fmt.printf("  invalid input (%s)\n", os.args[1]) }
    }
}
