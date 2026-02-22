package main

import "core:fmt"

main :: proc() {
    sum := 0
    for a in 0..=8 { sum += a * a }
    fmt.printf("  %d\n", sum)

    cube :: proc(a: int) -> int { return a * a * a }
    fmt.printf("  %d\n", cube(8))
}
