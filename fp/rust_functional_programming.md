# Functional Programming in Rust

*"Idiomatic Rust favors functional programming. It's a better fit for its ownership model."* — Sylvain Kerkour

## Core FP Concepts in Rust

### 1. Immutability (Default)
```rust
// Variables are immutable by default
let numbers = vec![1, 2, 3, 4, 5];
// numbers.push(6);  // Error! Cannot mutate

// Explicit mutability required
let mut mutable_numbers = vec![1, 2, 3, 4, 5];
mutable_numbers.push(6);  // OK

// Prefer immutable references
fn process(data: &[i32]) {  // Read-only slice
    for item in data {
        println!("{}", item);
    }
}
```

### 2. Pure Functions
```rust
// Pure function: same input → same output, no side effects
fn square(x: i32) -> i32 {
    x * x
}

// Not pure (has side effects)
static mut GLOBAL_COUNTER: i32 = 0;
fn impure_square(x: i32) -> i32 {
    unsafe { GLOBAL_COUNTER += 1; }  // Side effect!
    x * x
}
```

### 3. Higher-Order Functions (Functions as First-Class Objects)
```rust
// Function taking another function as parameter
fn apply_to_all<F>(vec: &[i32], func: F) -> Vec<i32>
where
    F: Fn(i32) -> i32,
{
    vec.iter().map(|&x| func(x)).collect()
}

// Closures (anonymous functions)
let square = |x: i32| x * x;
let add = |a: i32, b: i32| a + b;

// Using higher-order functions
let numbers = vec![1, 2, 3, 4, 5];
let squares = apply_to_all(&numbers, square);
```

---

## Rust Iterators: The Foundation

### Iterator Types
```rust
let numbers = vec![1, 2, 3, 4, 5];

// iter() - borrows elements (by reference)
for x in numbers.iter() {
    println!("{}", x);  // x is &i32
}

// into_iter() - takes ownership (consumes the collection)
for x in numbers.into_iter() {
    println!("{}", x);  // x is i32, numbers is now consumed
}

// iter_mut() - mutable references
let mut numbers = vec![1, 2, 3, 4, 5];
for x in numbers.iter_mut() {
    *x *= 2;  // Modify in place
}
```

### Lazy Evaluation
```rust
let numbers = vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

// Iterators are lazy - nothing computed yet
let result = numbers.iter()
    .filter(|&&x| x % 2 == 0)
    .map(|&x| x * x);

// Only computed when consumed
for x in result {
    println!("{}", x);  // 4, 16, 36, 64, 100
}

// Or collect into a new collection
let squares: Vec<i32> = numbers.iter()
    .filter(|&&x| x % 2 == 0)
    .map(|&x| x * x)
    .collect();
```

---

## Essential Iterator Methods

### map
```rust
let numbers = vec![1, 2, 3, 4, 5];

let squares: Vec<i32> = numbers.iter()
    .map(|&x| x * x)
    .collect();

let strings: Vec<String> = numbers.iter()
    .map(|x| x.to_string())
    .collect();

// With into_iter() for ownership
let doubled: Vec<i32> = numbers.into_iter()
    .map(|x| x * 2)
    .collect();
```

### filter
```rust
let numbers = vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

let evens: Vec<&i32> = numbers.iter()
    .filter(|&&x| x % 2 == 0)
    .collect();

let positive: Vec<i32> = numbers.into_iter()
    .filter(|&x| x > 0)
    .collect();

// filter_map - filter and map in one step
let parsed: Vec<i32> = vec!["1", "two", "3", "four", "5"]
    .into_iter()
    .filter_map(|s| s.parse::<i32>().ok())
    .collect();  // [1, 3, 5]
```

### fold / reduce
```rust
let numbers = vec![1, 2, 3, 4, 5];

// fold - with initial value
let sum = numbers.iter().fold(0, |acc, &x| acc + x);
let product = numbers.iter().fold(1, |acc, &x| acc * x);

// String concatenation
let words = vec!["hello", "world", "rust"];
let sentence = words.iter().fold(String::new(), |acc, &word| {
    if acc.is_empty() { word.to_string() } 
    else { format!("{} {}", acc, word) }
});

// reduce - uses first element as initial value
let sum = numbers.iter().copied().reduce(|acc, x| acc + x);  // Option<i32>
```

### flat_map
```rust
let nested = vec![vec![1, 2], vec![3, 4], vec![5, 6]];

// Flatten and transform
let doubled: Vec<i32> = nested.iter()
    .flat_map(|inner| inner.iter().map(|&x| x * 2))
    .collect();  // [2, 4, 6, 8, 10, 12]

// Generate multiple values from each input
let expanded: Vec<i32> = vec![1, 2, 3].into_iter()
    .flat_map(|x| vec![x, x * 10])
    .collect();  // [1, 10, 2, 20, 3, 30]
```

### flatten
```rust
let nested = vec![vec![1, 2, 3], vec![4, 5, 6], vec![7, 8, 9]];

let flat: Vec<i32> = nested.into_iter()
    .flatten()
    .collect();  // [1, 2, 3, 4, 5, 6, 7, 8, 9]

// Flatten Option
let options = vec![Some(1), None, Some(3), None, Some(5)];
let values: Vec<i32> = options.into_iter()
    .flatten()
    .collect();  // [1, 3, 5]

// Flatten Result
let results: Vec<Result<i32, &str>> = vec![Ok(1), Err("fail"), Ok(3)];
let successes: Vec<i32> = results.into_iter()
    .flatten()
    .collect();  // [1, 3]
```

### take / skip (drop)
```rust
let numbers = vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

let first_three: Vec<&i32> = numbers.iter().take(3).collect();  // [1, 2, 3]
let after_first: Vec<&i32> = numbers.iter().skip(1).collect();  // [2, 3, 4, ...]

// take_while / skip_while
let ascending: Vec<&i32> = numbers.iter()
    .take_while(|&&x| x < 5)
    .collect();  // [1, 2, 3, 4]

let after_threshold: Vec<&i32> = numbers.iter()
    .skip_while(|&&x| x < 5)
    .collect();  // [5, 6, 7, 8, 9, 10]
```

### rev (reverse)
```rust
let numbers = vec![1, 2, 3, 4, 5];

let reversed: Vec<&i32> = numbers.iter().rev().collect();  // [5, 4, 3, 2, 1]

// Combine with other operations
let last_two: Vec<&i32> = numbers.iter()
    .rev()
    .take(2)
    .collect();  // [5, 4]
```

### zip
```rust
let nums = vec![1, 2, 3];
let letters = vec!['a', 'b', 'c'];

let pairs: Vec<(i32, char)> = nums.into_iter()
    .zip(letters.into_iter())
    .collect();  // [(1, 'a'), (2, 'b'), (3, 'c')]

// With different lengths - stops at shorter
let short = vec![1, 2];
let long = vec!['a', 'b', 'c', 'd'];
let zipped: Vec<_> = short.into_iter()
    .zip(long.into_iter())
    .collect();  // [(1, 'a'), (2, 'b')]
```

### chain
```rust
let first = vec![1, 2, 3];
let second = vec![4, 5, 6];

let combined: Vec<i32> = first.into_iter()
    .chain(second.into_iter())
    .collect();  // [1, 2, 3, 4, 5, 6]

// Chain multiple iterators
let a = vec![1];
let b = vec![2, 3];
let c = vec![4, 5, 6];
let all: Vec<i32> = a.into_iter()
    .chain(b)
    .chain(c)
    .collect();  // [1, 2, 3, 4, 5, 6]
```

### scan
```rust
let numbers = vec![1, 2, 3, 4, 5];

// Running sum (like fold but yields intermediate values)
let running_sum: Vec<i32> = numbers.iter()
    .scan(0, |state, &x| {
        *state += x;
        Some(*state)
    })
    .collect();  // [1, 3, 6, 10, 15]
```

---

## Terminal Operations

### any / all
```rust
let numbers = vec![1, 2, 3, 4, 5];

let has_even = numbers.iter().any(|&x| x % 2 == 0);      // true
let all_positive = numbers.iter().all(|&x| x > 0);       // true
let all_even = numbers.iter().all(|&x| x % 2 == 0);      // false
```

### count
```rust
let numbers = vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

let total = numbers.iter().count();  // 10
let even_count = numbers.iter().filter(|&&x| x % 2 == 0).count();  // 5
```

### sum / product
```rust
let numbers = vec![1, 2, 3, 4, 5];

let sum: i32 = numbers.iter().sum();        // 15
let product: i32 = numbers.iter().product(); // 120

// Sum after transformation
let sum_of_squares: i32 = numbers.iter()
    .map(|&x| x * x)
    .sum();  // 55
```

### min / max
```rust
let numbers = vec![3, 1, 4, 1, 5, 9, 2, 6];

let min = numbers.iter().min();  // Some(&1)
let max = numbers.iter().max();  // Some(&9)

// With custom comparison
let words = vec!["hello", "world", "rust", "fp"];
let longest = words.iter().max_by_key(|s| s.len());  // Some(&"hello")
let shortest = words.iter().min_by_key(|s| s.len()); // Some(&"fp")

// min_by / max_by for full control
let max_by_last_char = words.iter()
    .max_by(|a, b| a.chars().last().cmp(&b.chars().last()));
```

### find / position
```rust
let numbers = vec![1, 2, 3, 4, 5];

let first_even = numbers.iter().find(|&&x| x % 2 == 0);  // Some(&2)
let position = numbers.iter().position(|&x| x == 3);     // Some(2)

// find_map - find and transform
let strings = vec!["one", "2", "three", "4"];
let first_number: Option<i32> = strings.iter()
    .find_map(|s| s.parse::<i32>().ok());  // Some(2)
```

### collect (into various types)
```rust
let numbers = vec![1, 2, 3, 4, 5];

// Into Vec
let doubled: Vec<i32> = numbers.iter().map(|&x| x * 2).collect();

// Into HashSet
use std::collections::HashSet;
let unique: HashSet<i32> = numbers.into_iter().collect();

// Into HashMap
use std::collections::HashMap;
let words = vec!["hello", "world"];
let word_lengths: HashMap<&str, usize> = words.iter()
    .map(|&word| (word, word.len()))
    .collect();

// Into String
let chars = vec!['h', 'e', 'l', 'l', 'o'];
let word: String = chars.into_iter().collect();

// Collect Results - fails fast on first error
let strings = vec!["1", "2", "3"];
let numbers: Result<Vec<i32>, _> = strings.iter()
    .map(|s| s.parse::<i32>())
    .collect();  // Ok([1, 2, 3])
```

---

## Composing Operations (Method Chaining)

```rust
let numbers = vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

// Complex transformation pipeline
let result: Vec<i32> = numbers.iter()
    .filter(|&&x| x % 2 == 0)        // [2, 4, 6, 8, 10]
    .map(|&x| x * x)                  // [4, 16, 36, 64, 100]
    .filter(|&x| x > 10)              // [16, 36, 64, 100]
    .take(2)                          // [16, 36]
    .collect();

// Calculate sum of transformed values
let sum: i32 = numbers.iter()
    .filter(|&&x| x % 2 == 0)
    .map(|&x| x * x)
    .filter(|&x| x > 10)
    .sum();  // 216
```

---

## Practical Examples

### Example 1: Data Processing Pipeline
```rust
use std::collections::HashMap;

#[derive(Debug)]
struct Employee {
    name: &'static str,
    department: &'static str,
    salary: f64,
    active: bool,
}

fn main() {
    let employees = vec![
        Employee { name: "Alice",   department: "Engineering", salary: 80000.0, active: true  },
        Employee { name: "Bob",     department: "Sales",       salary: 60000.0, active: false },
        Employee { name: "Charlie", department: "Engineering", salary: 90000.0, active: true  },
        Employee { name: "Diana",   department: "Marketing",   salary: 55000.0, active: true  },
        Employee { name: "Eve",     department: "Engineering", salary: 75000.0, active: true  },
    ];

    // 1. Names of active Engineering employees, sorted
    let mut eng_names: Vec<&str> = employees.iter()
        .filter(|e| e.active && e.department == "Engineering")
        .map(|e| e.name)
        .collect();
    eng_names.sort();
    println!("{:?}", eng_names);  // ["Alice", "Charlie", "Eve"]

    // 2. Average salary of active Engineering employees
    let eng: Vec<&Employee> = employees.iter()
        .filter(|e| e.active && e.department == "Engineering")
        .collect();
    let avg_salary: f64 = eng.iter().map(|e| e.salary).sum::<f64>() / eng.len() as f64;
    println!("${:.2}", avg_salary);  // $81666.67

    // 3. Department headcount and total salary (active only)
    let summary = employees.iter()
        .filter(|e| e.active)
        .fold(HashMap::<&str, (usize, f64)>::new(), |mut acc, e| {
            let entry = acc.entry(e.department).or_insert((0, 0.0));
            entry.0 += 1;
            entry.1 += e.salary;
            acc
        });

    let mut depts: Vec<_> = summary.iter().collect();
    depts.sort_by_key(|(dept, _)| *dept);
    for (dept, (count, total)) in depts {
        println!("{}: {} employees, ${:.0} total", dept, count, total);
    }
    // Engineering: 3 employees, $245000 total
    // Marketing:   1 employees, $55000 total
}
```

### Example 2: String Processing
```rust
fn main() {
    let words = vec!["hello", "world", "functional", "rust"];
    
    // Transform to uppercase, filter by length
    let result: Vec<String> = words.iter()
        .map(|s| s.to_uppercase())
        .filter(|s| s.len() > 5)
        .collect();
    
    println!("{:?}", result);  // ["FUNCTIONAL"]
    
    // Count characters across all words
    let total_chars: usize = words.iter()
        .map(|s| s.len())
        .sum();
    
    println!("Total characters: {}", total_chars);  // 25
    
    // Join with separator
    let sentence = words.join(" ");
    println!("{}", sentence);  // "hello world functional rust"
}
```

### Example 3: Working with Option and Result
```rust
fn main() {
    // Chain operations on Option
    let maybe_number = Some("42");
    let doubled: Option<i32> = maybe_number
        .and_then(|s| s.parse::<i32>().ok())
        .map(|n| n * 2);
    
    println!("{:?}", doubled);  // Some(84)
    
    // Filter None values
    let options = vec![Some(1), None, Some(3), None, Some(5)];
    let values: Vec<i32> = options.into_iter()
        .flatten()
        .collect();
    
    println!("{:?}", values);  // [1, 3, 5]
    
    // Collect Results - propagate errors
    let strings = vec!["1", "2", "3"];
    let numbers: Result<Vec<i32>, _> = strings.iter()
        .map(|s| s.parse::<i32>())
        .collect();
    
    println!("{:?}", numbers);  // Ok([1, 2, 3])
    
    // With an error
    let strings_with_error = vec!["1", "two", "3"];
    let numbers: Result<Vec<i32>, _> = strings_with_error.iter()
        .map(|s| s.parse::<i32>())
        .collect();
    
    println!("{:?}", numbers);  // Err(ParseIntError)
}
```

### Example 4: Enumerate and Indexed Operations
```rust
fn main() {
    let words = vec!["zero", "one", "two", "three"];
    
    // Enumerate adds index
    let indexed: Vec<(usize, &&str)> = words.iter()
        .enumerate()
        .collect();
    
    println!("{:?}", indexed);  // [(0, "zero"), (1, "one"), ...]
    
    // Find index of element
    let position = words.iter()
        .position(|&w| w == "two");
    
    println!("Position of 'two': {:?}", position);  // Some(2)
    
    // Filter by index
    let even_indexed: Vec<&str> = words.iter()
        .enumerate()
        .filter(|(i, _)| i % 2 == 0)
        .map(|(_, &word)| word)
        .collect();
    
    println!("{:?}", even_indexed);  // ["zero", "two"]
}
```

### Example 5: Grouping and Partitioning
```rust
use std::collections::HashMap;

fn main() {
    let numbers = vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    
    // Partition into two groups
    let (evens, odds): (Vec<i32>, Vec<i32>) = numbers.into_iter()
        .partition(|&x| x % 2 == 0);
    
    println!("Evens: {:?}", evens);  // [2, 4, 6, 8, 10]
    println!("Odds: {:?}", odds);    // [1, 3, 5, 7, 9]
    
    // Group by key
    let words = vec!["apple", "banana", "apricot", "blueberry", "cherry"];
    let grouped: HashMap<char, Vec<&str>> = words.iter()
        .fold(HashMap::new(), |mut acc, &word| {
            acc.entry(word.chars().next().unwrap())
                .or_insert_with(Vec::new)
                .push(word);
            acc
        });
    
    println!("{:?}", grouped);
    // {'a': ["apple", "apricot"], 'b': ["banana", "blueberry"], 'c': ["cherry"]}
}
```

### Example 6: Cartesian Product with itertools
```rust
use itertools::iproduct;

fn main() {
    let sizes = vec![38, 40, 42];
    let colors = vec!["red", "blue"];
    
    // Cartesian product
    let combinations: Vec<(i32, &str)> = iproduct!(sizes.iter(), colors.iter())
        .map(|(&s, &c)| (s, c))
        .collect();
    
    println!("{:?}", combinations);
    // [(38, "red"), (38, "blue"), (40, "red"), (40, "blue"), (42, "red"), (42, "blue")]
}
```

---

## Debugging Iterators

### inspect
```rust
let numbers = vec![1, 2, 3, 4, 5];

let result: Vec<i32> = numbers.iter()
    .inspect(|x| println!("Before filter: {}", x))
    .filter(|&&x| x % 2 == 0)
    .inspect(|x| println!("After filter: {}", x))
    .map(|&x| x * x)
    .inspect(|x| println!("After map: {}", x))
    .collect();
```

---

## Useful Crates

```toml
[dependencies]
itertools = "0.12"  # Additional iterator methods
rayon = "1.8"       # Parallel iterators
```

### itertools Examples
```rust
use itertools::Itertools;

let numbers = vec![1, 2, 3, 4, 5];

// unique
let unique: Vec<_> = vec![1, 2, 2, 3, 3, 3].into_iter().unique().collect();

// sorted
let sorted: Vec<_> = vec![3, 1, 4, 1, 5].into_iter().sorted().collect();

// group_by
let grouped = vec![1, 1, 2, 2, 2, 3].into_iter()
    .group_by(|&x| x);

// chunks
let chunks: Vec<Vec<_>> = vec![1, 2, 3, 4, 5].into_iter()
    .chunks(2)
    .into_iter()
    .map(|chunk| chunk.collect())
    .collect();  // [[1, 2], [3, 4], [5]]

// combinations
let combos: Vec<Vec<_>> = (1..=4).combinations(2).collect();
// [[1, 2], [1, 3], [1, 4], [2, 3], [2, 4], [3, 4]]

// permutations
let perms: Vec<Vec<_>> = (1..=3).permutations(2).collect();
// [[1, 2], [1, 3], [2, 1], [2, 3], [3, 1], [3, 2]]
```

### Parallel Iterators with Rayon
```rust
use rayon::prelude::*;

let numbers: Vec<i32> = (1..1000000).collect();

// Parallel map
let squares: Vec<i32> = numbers.par_iter()
    .map(|&x| x * x)
    .collect();

// Parallel filter
let evens: Vec<i32> = numbers.par_iter()
    .filter(|&&x| x % 2 == 0)
    .copied()
    .collect();

// Parallel sum
let sum: i32 = numbers.par_iter().sum();
```

---

## Key Takeaways

1. **Immutability by Default**: Rust encourages immutable data; use `mut` explicitly when needed
2. **Lazy Evaluation**: Iterators don't compute until consumed (`.collect()`, `.sum()`, etc.)
3. **Ownership Matters**: Choose `iter()`, `into_iter()`, or `iter_mut()` based on ownership needs
4. **Method Chaining**: Compose operations fluently with iterator adaptors
5. **Zero-Cost Abstractions**: Iterator chains compile to efficient loops
6. **Error Handling**: `Option` and `Result` integrate seamlessly with iterator methods
7. **Parallelism**: Rayon makes parallel iteration trivial with `par_iter()`
