use rand;

fn main() {
    // create vector of random integers
    let mut o: Vec<u32> = Vec::new();
    for _ in 0..20 {
        o.push(rand::random::<u32>());
    }

    println!("orig: {:#?}", o);
    println!("qsort: {:#?}", qsort(o.clone()));
    o.sort();
    println!("sort (built-in): {:#?}", o);
}

fn qsort(vec: Vec<u32>) -> Vec<u32>  {
    let cop = &mut vec.clone();

    if cop.len() <= 1 {
        vec
    } else {
        // use last element as pivot
        let pindex: usize = cop.len()/2;
        swap(cop, pindex, cop.len()-1);
        let pivot: u32 = cop[cop.len()-1];
        
        let mut ins: usize = 0;
        for icomp in 0..=(cop.len()-2) {
            if cop[icomp] < cop[cop.len()-1] {
                swap(cop, icomp, ins);
                ins += 1;
            }
        }
        swap(cop, ins, cop.len()-1);

        let left: Vec<u32>;
        if ins == 0 {
            left = Vec::new();
        } else {
            left = Vec::from_iter(cop[0..=ins-1].iter().cloned());
        }
        let right: Vec<u32> = Vec::from_iter(cop[ins+1..].iter().cloned());

        let mut sorted = qsort(left);
        sorted.push(pivot);
        sorted.extend(qsort(right));

        sorted
    }

}

fn swap(vec: &mut Vec<u32>, i: usize, j: usize) {
    if vec.len() >= i && vec.len() >= j {
        let o = vec[i];
        vec[i] = vec[j];
        vec[j] = o;
    }
}
