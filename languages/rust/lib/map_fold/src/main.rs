#![allow(dead_code)]

fn main() {
    let data: Vec<i32> = vec!(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
    println!(
        "map_reduce : {} - map_reduce2 : {} - reduce : {}",
        map_reduce(&data),
        map_reduce2(&data),
        reduce(&data),
    );
}

fn map_reduce(data: &Vec<i32>) -> i32 {
    let copy = (*data).clone();
    let map: Vec<i32> =
        copy
            .clone()
            .iter()
            .map(|x| x * x)
            .collect::<Vec<i32>>();
    let reduce: i32 =
        map
            .iter()
            .fold(0, |acc, x| acc + x);
    reduce
}

fn map_reduce2(data: &Vec<i32>) -> i32 {
    let copy = (*data).clone();
    let map: i32 =
        copy
            .clone()
            .iter()
            .map(|x| x * x)
            .collect::<Vec<i32>>()
            .to_vec()
            .iter()
            .fold(0, |acc, x| acc + x);
    map
}

fn reduce(data: &Vec<i32>) -> i32 {
    let copy = (*data).clone();
    let reduce: i32 =
        copy
            .clone()
            .iter()
            .fold(0, |acc, x| acc + (x * x));
    reduce
}
