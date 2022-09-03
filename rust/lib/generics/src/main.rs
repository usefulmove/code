use std::ops::Mul;

fn main() {
    let a: u16 = 8;
    let b: i32 = 8;
    let c: f64 = 8.;

    println!(
        "u16:{} - i32:{} - f64:{}",
        cube(a),
        cube(b),
        cube(c),
    );
}

fn cube<T>(n: T) -> T
where T: Mul + Mul<Output = T> + Copy,
{
    n * n * n
}
