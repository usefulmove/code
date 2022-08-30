fn main() {
    let data: Vec<i32> = vec!(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
    println!("{:#?}", map_reduce(data));
}

fn map_reduce(data: Vec<i32>) -> i32 {
    let map: Vec<i32> =
        data
            .iter()
            .map(|x| x * x)
            .collect::<Vec<i32>>();
    let reduce: i32 =
        map
            .iter()
            .fold(0, |acc, x| acc + x);
    reduce
}