const TOTAL_TOSSES = 1e8;

let pattern_a = [false, true, true];
let pattern_b = [true, false, true];

let tosses = [];


let pattern_a_counter = 0;
let pattern_b_counter = 0;

let pattern_a_found = [];
let pattern_b_found = [];

for (let i = 1; i <= TOTAL_TOSSES; i++) {
  // toss and increment counters
  tosses.push(Math.random() < 0.5);
  pattern_a_counter += 1;
  pattern_b_counter += 1;

  if ( equals(tosses, pattern_a) && (pattern_a_counter >= pattern_a.length) ) {
    pattern_a_found.push(pattern_a_counter);
    pattern_a_counter = 0; // reset counter
  } else if ( equals(tosses, pattern_b) && (pattern_b_counter >= pattern_b.length) ) {
    pattern_b_found.push(pattern_b_counter);
    pattern_b_counter = 0; // reset counter
  }

  if (tosses.length == pattern_a.length) {
    tosses.shift() // remove first element
  }
}

function average(v) {
  s = 0;
  for (let i = 0; i < v.length; i++) {
    s += v[i];
  }
  return s / v.length;
};

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

console.log( average(pattern_a_found) );
console.log( average(pattern_b_found) );
