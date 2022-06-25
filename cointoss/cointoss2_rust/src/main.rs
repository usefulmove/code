const TOTAL_TOSSES: u64 = 1e7 as u64;

fn main() {
  println!("simulating {} coin tosses", TOTAL_TOSSES);

  let pattern_a: Vec<bool> = vec![false, true, true];
  let pattern_b: Vec<bool> = vec![true, true, false];

  let mut tosses: Vec<bool> = Vec::new();

  let mut pattern_a_found: u64 = 0;
  let mut pattern_b_found: u64 = 0;

  for _i in 1..TOTAL_TOSSES {
    // toss and increment counters
    tosses.push(rand::random());

    // check for patterns
    if tosses == pattern_a {
      //println!("matched pattern a: {:?}", tosses);
      pattern_a_found += 1;
      tosses = Vec::new();
    } else if tosses == pattern_b {
      //println!("matched pattern b: {:?}", tosses);
      pattern_b_found += 1;
      tosses = Vec::new();
    }

    if tosses.len() == pattern_a.len() {
      Some(tosses.remove(0));
    }
  }

  println!("pattern a: {:?} - {:.5}", pattern_a, pattern_a_found as f64 / (pattern_a_found + pattern_b_found) as f64);
  println!("pattern b: {:?} - {:.5}", pattern_b, pattern_b_found as f64 / (pattern_a_found + pattern_b_found) as f64);
}
