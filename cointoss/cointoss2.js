#!node

const TOTAL_TOSSES = 1e7;

let pattern_a = [false, true, true]
let pattern_b = [true, true, false]

let tosses = [];

let pattern_a_found = 0;
let pattern_b_found = 0;

for (let i = 1; i <= TOTAL_TOSSES; i++) {
  // toss and increment counters
  tosses.push(Math.random() < 0.5);

  // check for patterns
  if ( equals(tosses, pattern_a) ) {
    pattern_a_found += 1;
    tosses = [];
  } else if ( equals(tosses, pattern_b) ) {
    pattern_b_found += 1;
    tosses = [];
  }

  if (tosses.length == pattern_a.length) {
    tosses.shift() // remove first element
  }
}

function equals(x, y) {
  if (x.length != y.length) {
    return false;
  }

  for (let i = 0; i < x.length; i++) {
    if (x[i] != y[i]) {
      return false;
    }
  }

  return true
}

console.log(pattern_a_found / (pattern_a_found + pattern_b_found));
console.log(pattern_b_found / (pattern_a_found + pattern_b_found));
