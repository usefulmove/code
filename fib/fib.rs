use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    println!("{}", fib(args[1].parse().unwrap()));
}

fn fib(n: u64) -> u64 {
    match n {
        0 | 1 => n,
        _     => fib(n-1) + fib(n-2),
    }
}