use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    println!("{}", fib2(args[1].parse().unwrap()));
}

/* recursive solution */
fn fib(n: u64) -> u64 {
    match n {
        0 | 1 => n,
        _     => fib(n-1) + fib(n-2),
    }
}

/* tail recursive solution */
fn fib2(n: u64) -> u64 {
    fibonacci2(n, 0, 1)
}

fn fibonacci2(n: u64, a: u64, b: u64) -> u64 {
    match n {
        0 => a,
        1 => b,
        _ => fibonacci2(n-1, b, a+b),
    }
}