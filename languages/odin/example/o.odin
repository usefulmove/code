package main

import "core:fmt"

main :: proc() {
    acc := 0
    for a in 0..=8 { acc += a * a }
    fmt.printf("  %d\n", acc)

    cube :: proc(a: int) -> int { return a * a * a }
    fmt.printf("  %d\n", cube(8))
}
