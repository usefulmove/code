fn main() {
    println!("{}", convert_to_base(55, 7));
    println!("{}", convert_from_base(106, 7));
}

fn convert_to_base(n: i32, base: i32) -> i32 {
    let mut num = n;
    let mut res = 0;
    let mut b = 1;
    while num != 0 {
        res += b * (num % base);
        b *= 10;
        num /= base;
    }
    res
}

fn convert_from_base(n: i32, base: i32) -> i32 {
    n.to_string()
        .chars()
        .rev()
        .enumerate()
        .fold(0, |sum, (i, c)| {
            sum + c.to_digit(10).unwrap() as i32 * base.pow(i as u32)
         })
}