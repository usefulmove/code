fn main() {
    let add2 = add(2);
    println!("add(2)(3) is {}", add2(3));
}

fn add(a: i32) -> impl Fn(i32) -> i32 {
    move |b| a + b
}