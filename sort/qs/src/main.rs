use rand;

fn main() {
    // create vector of random integers
    let o: &mut Vec<u32> = &mut Vec::new();
    for _ in 0..5 {
        o.push(rand::random::<u32>());
    }
    //removelet o: &mut Vec<u32> = &mut vec![5, 2, 3, 1, 4];


    println!("orig: {:#?}", o);

    qsort(o);

    println!("qsort: {:#?}", o);
}

fn qsort(vec: &mut Vec<u32>) {
    if vec.len() > 1 {
        // use last element as pivot
        //println!("pivot: {}", vec[vec.len()-1]); // remove
        
        let mut ins: usize = 0;
        for icomp in 0..=(vec.len()-2) {
            //removeprintln!("in loop: comp={} ({}) ins={} ({})", icomp, vec[icomp], ins, vec[ins]);
            if vec[icomp] < vec[vec.len()-1] {
                swap(vec, icomp, ins);
                ins += 1;
            }
        }
        swap(vec, ins, vec.len()-1);

        //qsort(&vec[0..ins-1]);
        //qsort(&vec[ins+1..vec.len()-1]);
    }
}

fn swap(vec: &mut Vec<u32>, i: usize, j: usize) {
    //println!("swapping (i={}) {} (j={}) and {}", i, vec[i], j, vec[j]); // remove
    if vec.len() >= i && vec.len() >= j {
        let o = vec[i];
        vec[i] = vec[j];
        vec[j] = o;
    }
    //removeprintln!("post-swap: {:?}", vec.clone());
}
