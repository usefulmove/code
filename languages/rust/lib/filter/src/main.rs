fn main() {
    println!("{}", arithmetic_triplets(vec![0,1,4,6,7,10], 3));
}

fn arithmetic_triplets(nums: Vec<i32>, diff: i32) -> i32 {
    // [1] scan elements of array
    (0..nums.len()-2)
    // [2] filter nums[i] which are starters for triples
    //     (i.e. those nums[i] for which nums[i]+diff and nums[i]+diff*2 was found)
        .filter(|&i|
            (i+1..nums.len())
                // [3] filter elements which are either nums[i]+diff or nums[i]+diff*2
                .filter(|&j| nums[j]-nums[i] == diff || nums[j]-nums[i] == diff*2)
                // [4] count filtered elements
                .count() == 2)
        .count() as i32
}