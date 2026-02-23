package main

import "core:fmt"

main :: proc() {
    fold :: proc(f: proc(int, int) -> int, seed: int, slice: []int) -> int {
        acc: int = seed
        for e in slice { acc = f(acc, e) }
        return acc
    }

    data := [?]int{0, 1, 2, 3, 4, 5, 6, 7, 8}
    f := proc(acc, a: int) -> int { return acc + a * a }

    fmt.printf("  %d\n", fold(f, 0, data[:]))
}
