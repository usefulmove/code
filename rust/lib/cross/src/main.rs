fn main() {
    let a: Vec<i32> = vec![0, 1, 2, 3, 4, 5, 6, 7, 8];
    let b: Vec<i32> = vec![0, 1, 2, 3, 4, 5, 6, 7, 8];

    let cross_tuple: Vec<(i32, i32)> = a.iter()
        .flat_map(|x| {
            b.iter().map(move |y| (*x, *y))
        })
        .collect();

    println!("a..{:?}", a);
    println!("b..{:?}", b);
    println!("cross_tuple..{:?}", cross_tuple);
    println!("size..{:?}", cross_tuple.len());
}
