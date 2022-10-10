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

fn fib2(n: u64) -> u64 {
    fibonacci2(n, 0, 1)
}

fn fibonacci2(n: u64, a: u64, b: u64) -> u64 {
    if n == 0 {return a}
    if n == 1 {return b}
    fibonacci2(n-1, b, a+b)
}