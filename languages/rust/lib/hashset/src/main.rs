use std::collections::HashSet;

fn main() {
    let complete_set = HashSet::from([1, 2, 3, 4, 5, 6, 7, 8, 9]);
    let partial_set1 = HashSet::from([1, 2, 4, 5]);
    let partial_set2 = HashSet::from([6, 7, 8, 9]);
    let partial_set3 = HashSet::from([9]);

    let combined_partial = partial_set1
        .union(&partial_set2)
        .cloned()
        .collect::<HashSet<_>>()
        .union(&partial_set3)
        .cloned()
        .collect::<HashSet<_>>();

    let difference = complete_set
        .difference(&combined_partial)
        .cloned()
        .collect::<Vec<u8>>()[0];

    println!("complete set: {:?}", complete_set);
    println!("partial set (1): {:?}", partial_set1);
    println!("partial set (2): {:?}", partial_set2);
    println!("combined partial set: {:?}", combined_partial);
    println!("difference: {:?}", difference);
}
