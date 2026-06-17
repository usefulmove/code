import gleam/io
import gleam/int

pub fn main() {
    io.println(int.to_string(fib2(10)))
}

fn fib(n) {
    case n {
        0 -> 0
        1 -> 1
        _ -> fib(n-1) + fib(n-2)
    }
}

fn fib2(n) {
    fib2_helper(n, 0, 1)
}

fn fib2_helper(n, a, b) {
    case n {
        0 -> a
        1 -> b
        _ -> fib2_helper(n-1, b, a+b)
    }
}
