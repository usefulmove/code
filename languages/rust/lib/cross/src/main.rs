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

    triple();
}

fn triple() {
    let a: Vec<i32> = vec![0, 1, 2, 3, 4, 5, 6, 7, 8];
    let b: Vec<i32> = vec![0, 1, 2, 3, 4, 5, 6, 7, 8];
    let c: Vec<i32> = vec![0, 1, 2, 3, 4, 5, 6, 7, 8];

    let start_tuple: Vec<(i32, i32)> = a.iter()
        .flat_map(|x| {
            b.iter().map(move |y| (*x, *y))
        })
        .collect();

    let cross_tuple: Vec<(i32, i32, i32)> = start_tuple.iter()
        .flat_map(|xy| {
            c.iter().map(move |z| (xy.0, xy.1, *z))
        })
        .collect();

    println!("cross_tuple..{:?}", cross_tuple);
    println!("size..{:?}", cross_tuple.len());
}
