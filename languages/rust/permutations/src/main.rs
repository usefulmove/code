use std::collections::HashSet;

fn main() {
    let nums = vec![0, 1];

    println!("{:?}", Solution::permute(nums));
}

#[allow(dead_code)]
struct Solution;

impl Solution {
    pub fn permute(nums: Vec<i32>) -> Vec<Vec<i32>> {
        let mut set: HashSet<Vec<i32>> = HashSet::new();

        for i in nums.iter() {
            for j in nums.iter() {
                for k in nums.iter() {
                    set.insert(vec![*i, *j, *k]);
                }
            }
        }

        set.iter().cloned()
            .filter(|vec| {
                vec[0] != vec[1] && vec[0] != vec[2] && vec[1] != vec[2]
            })
            .collect::<Vec<Vec<i32>>>()
    }
}

#[test]
fn test() {
    let nums = vec![1, 2, 3];
    let res = vec![[1, 2, 3], [1, 3, 2], [2, 1, 3], [2, 3, 1], [3, 1, 2], [3, 2, 1]];
    assert_eq!(Solution::permute(nums), res);
}