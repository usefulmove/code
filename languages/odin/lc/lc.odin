package main

import "core:fmt"
import "core:os"
import "core:strings"

main :: proc() {
    filepath: string

    if len(os.args) == 1 {
        fmt.printfln("  error: no argument")
        return
    }

    filepath = os.args[1]

    raw_data, ok := os.read_entire_file(filepath, context.allocator)
    defer delete(raw_data, context.allocator)

    if !ok {
        fmt.printfln("  error: could not read file (%s)", filepath)
        return
    }

    contents := string(raw_data)

    n_lines, n_words: int
    for line in strings.split_lines_iterator(&contents) {
        n_lines += 1
        words := strings.fields(line)
        n_words += len(words)
    }
    fmt.printfln("  file:  %s", filepath)
    fmt.printfln("  lines: %d", n_lines)
    fmt.printfln("  words: %d", n_words)
}
