fn main() {
    assert_eq!(
        are_numbers_ascending(String::from("1 box has 3 blue 4 red 6 green and 12 yellow marbles")),
        true,
    )
}

fn are_numbers_ascending(s: String) -> bool {
    s.split_whitespace()
        .filter_map(|s| s.parse::<i32>().ok())
        .fold((true, i32::MIN), |(ret, acc), val| {
            (ret && val > acc, val)
         })
        .0
}

#[test]

