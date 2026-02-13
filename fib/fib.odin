package main

import "core:fmt"

fib :: proc(n: int) -> int {
    if n < 2 { return n }
    return fib(n-1) + fib(n-2)
}

main :: proc() {
    fmt.printf("result: %d\n", fib(38))
}
