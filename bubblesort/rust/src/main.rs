fn main() {
    let mut o = vec![3,1,2,5,4];

    let mut recheck = true;

    let swap = |i: usize, j: usize, v: &mut Vec<i32>| -> bool {
        let tmp = v[i];
        v[i] = v[j];
        v[j] = tmp;

        true
    };

    while recheck {
        recheck = false;

        for i in 0..o.len()-1 {
            if o[i] > o[i + 1] {
                recheck = swap(i, i + 1, &mut o);
            }
        }
    }

    println!("{:?}", o);
}
