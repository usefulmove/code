# Functional Programming in Modern C++

## Core FP Concepts in C++

### 1. Immutability
```cpp
#include <string>
#include <vector>

// Prefer const references and const values
void process(const std::vector<int>& data) {  // Read-only
    for (const auto& item : data) {
        // Can only read, not modify
    }
}

// Use const auto for immutable variables
const auto numbers = std::vector<int>{1, 2, 3, 4, 5};
```

### 2. Pure Functions
```cpp
// Pure function: same input → same output, no side effects
int square(int x) {
    return x * x;
}

// Not pure (has side effects)
int global_counter = 0;
int impure_square(int x) {
    global_counter++;  // Side effect!
    return x * x;
}
```

### 3. Higher-Order Functions (Functions as First-Class Objects)
```cpp
#include <functional>
#include <algorithm>
#include <vector>

// Function taking another function as parameter (template — no type erasure overhead)
template<typename F>
std::vector<int> apply_to_all(const std::vector<int>& vec, F func) {
    std::vector<int> result;
    result.reserve(vec.size());
    for (const auto& item : vec) {
        result.push_back(func(item));
    }
    return result;
}

// Lambda functions (anonymous functions)
auto square = [](int x) { return x * x; };
auto add    = [](int a, int b) { return a + b; };

// Closures — lambdas capture surrounding state
auto multiplier = [](int factor) {
    return [factor](int x) { return x * factor; };  // Returns a lambda
};

auto double_ = multiplier(2);
auto triple  = multiplier(3);
// double_(5) == 10, triple_(5) == 15

// Store and pass functions with std::function (type-erased, slight overhead)
std::function<int(int)> f = square;
auto result = apply_to_all({1, 2, 3, 4, 5}, f);  // {1, 4, 9, 16, 25}

// Prefer templates or auto parameters over std::function when possible
auto apply = [](const std::vector<int>& v, auto fn) {
    std::vector<int> out;
    for (int x : v) out.push_back(fn(x));
    return out;
};
```

---

## C++ Ranges: The Foundation

### Range Views (Lazy Evaluation)
```cpp
#include <ranges>
#include <vector>
#include <iostream>

int main() {
    std::vector<int> numbers = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    
    // Views don't compute immediately - they're lazy
    auto evens = numbers | std::views::filter([](int x) { 
        return x % 2 == 0; 
    });
    
    auto squares = evens | std::views::transform([](int x) { 
        return x * 2; 
    });
    
    // Only computed when iterated
    for (int x : squares) {
        std::cout << x << " ";  // 4 8 12 16 20
    }
    
    return 0;
}
```

---

## Essential Range Views

### filter
```cpp
auto evens = numbers 
    | std::views::filter([](int x) { return x % 2 == 0; });

auto positive = numbers 
    | std::views::filter([](int x) { return x > 0; });

auto strings_with_length_5 = words 
    | std::views::filter([](const auto& s) { return s.length() == 5; });
```

### transform
```cpp
auto squares = numbers 
    | std::views::transform([](int x) { return x * x; });

auto uppercase = names 
    | std::views::transform([](const std::string& s) { 
        std::string result = s;
        std::transform(result.begin(), result.end(), result.begin(), ::toupper);
        return result;
    });

auto lengths = strings 
    | std::views::transform(&std::string::length);  // Member function pointer
```

### take / drop
```cpp
auto first_three = numbers | std::views::take(3);      // [1, 2, 3]
auto after_first = numbers | std::views::drop(1);      // [2, 3, 4, 5, ...]
auto last_two = numbers | std::views::drop(2) 
                    | std::views::reverse()
                    | std::views::take(2);
```

### reverse
```cpp
auto reversed = numbers | std::views::reverse;
```

### zip (C++23)
```cpp
std::vector<int> nums = {1, 2, 3};
std::vector<char> letters = {'a', 'b', 'c'};

auto pairs = nums | std::views::zip(letters);
// Produces: [(1,'a'), (2,'b'), (3,'c')]

for (const auto& [n, ch] : pairs) {
    std::cout << n << ":" << ch << " ";  // 1:a 2:b 3:c
}
```

### cartesian_product (C++23)
```cpp
std::vector<int> sizes = {38, 40, 42};
std::vector<char> types = {'S', 'M', 'L'};

auto combinations = sizes | std::views::cartesian_product(types);
// Produces: [(38,'S'), (38,'M'), (38,'L'), (40,'S'), ...]

for (const auto& [size, type] : combinations) {
    std::cout << size << type << " ";
}
```

---

## Range Algorithms

### fold_left (C++23) and accumulate
```cpp
#include <numeric>
#include <ranges>

// Using accumulate (classic)
int sum = std::accumulate(numbers.begin(), numbers.end(), 0);
int product = std::accumulate(numbers.begin(), numbers.end(), 1, std::multiplies());

// Using fold_left (C++23)
int sum = std::ranges::fold_left(numbers, 0, std::plus());
int concatenated = std::ranges::fold_left(
    strings, 
    std::string(), 
    [](const std::string& acc, const std::string& s) {
        return acc.empty() ? s : acc + ", " + s;
    }
);
```

### any_of / all_of
```cpp
bool has_even = std::ranges::any_of(numbers, [](int x) { return x % 2 == 0; });
bool all_positive = std::ranges::all_of(numbers, [](int x) { return x > 0; });
```

### count
```cpp
int even_count = std::ranges::count_if(numbers, [](int x) { return x % 2 == 0; });
int number_of_fives = std::ranges::count(numbers, 5);
```

### min / max
```cpp
auto [min_val, max_val] = std::ranges::minmax(numbers);
auto max_by_length = std::ranges::max_element(
    strings, 
    [](const auto& a, const auto& b) { return a.length() < b.length(); }
);
```

### sort
```cpp
std::vector<int> sorted_numbers = numbers;
std::ranges::sort(sorted_numbers);

// With custom comparator
std::ranges::sort(strings, [](const auto& a, const auto& b) {
    return a.length() < b.length();
});
```

---

## Composing Operations (The Power of Pipes)

```cpp
#include <ranges>
#include <vector>
#include <iostream>
#include <numeric>

std::vector<int> numbers = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};

// Complex transformation pipeline
auto result = numbers
    | std::views::filter([](int x) { return x % 2 == 0; })      // [2, 4, 6, 8, 10]
    | std::views::transform([](int x) { return x * x; })        // [4, 16, 36, 64, 100]
    | std::views::filter([](int x) { return x > 10; })          // [16, 36, 64, 100]
    | std::views::take(2);                                      // [16, 36]

// Calculate sum of transformed values
int sum = std::accumulate(result.begin(), result.end(), 0);
// OR in C++23:
int sum = std::ranges::fold_left(result, 0, std::plus());
```

---

## Practical Examples

### Example 1: Data Processing Pipeline
```cpp
#include <algorithm>
#include <iostream>
#include <map>
#include <numeric>
#include <ranges>
#include <string>
#include <vector>

struct Employee {
    std::string name;
    std::string department;
    double salary;
    bool active;
};

int main() {
    std::vector<Employee> employees = {
        {"Alice",   "Engineering", 80000.0, true },
        {"Bob",     "Sales",       60000.0, false},
        {"Charlie", "Engineering", 90000.0, true },
        {"Diana",   "Marketing",   55000.0, true },
        {"Eve",     "Engineering", 75000.0, true },
    };

    // 1. Names of active Engineering employees, sorted
    std::vector<std::string> eng_names;
    for (const auto& e : employees)
        if (e.active && e.department == "Engineering")
            eng_names.push_back(e.name);
    std::ranges::sort(eng_names);
    for (const auto& n : eng_names) std::cout << n << " ";
    std::cout << "\n";  // Alice Charlie Eve

    // 2. Average salary of active Engineering employees
    auto eng = employees
        | std::views::filter([](const Employee& e) {
            return e.active && e.department == "Engineering";
          });

    double total = std::ranges::fold_left(
        eng, 0.0, [](double acc, const Employee& e) { return acc + e.salary; }
    );
    long count = std::ranges::distance(eng);
    std::cout << "$" << total / count << "\n";  // $81666.67

    // 3. Department headcount and total salary (active only)
    std::map<std::string, std::pair<int, double>> summary;
    for (const auto& e : employees | std::views::filter([](const Employee& e){ return e.active; })) {
        summary[e.department].first  += 1;
        summary[e.department].second += e.salary;
    }
    for (const auto& [dept, stats] : summary) {
        std::cout << dept << ": " << stats.first
                  << " employees, $" << stats.second << " total\n";
    }
    // Engineering: 3 employees, $245000 total
    // Marketing:   1 employees, $55000 total

    return 0;
}
```

### Example 2: String Processing
```cpp
#include <ranges>
#include <string>
#include <vector>
#include <iostream>
#include <algorithm>

std::string to_upper(const std::string& s) {
    std::string result = s;
    std::transform(result.begin(), result.end(), result.begin(), ::toupper);
    return result;
}

int main() {
    std::vector<std::string> words = {"hello", "world", "functional", "cpp"};
    
    // Transform to uppercase, filter by length, sort
    auto result = words
        | std::views::transform(to_upper)
        | std::views::filter([](const std::string& s) { return s.length() > 5; })
        | std::views::transform([](const std::string& s) { return s.length(); })
        | std::views::take(3);
    
    for (int len : result) {
        std::cout << len << " ";  // 10 5
    }
    std::cout << "\n";
    
    return 0;
}
```

### Example 3: Flatten (join) Nested Ranges
```cpp
#include <ranges>
#include <vector>
#include <iostream>

int main() {
    std::vector<std::vector<int>> matrix = {
        {1, 2, 3},
        {4, 5, 6},
        {7, 8, 9}
    };
    
    // Flatten 2D to 1D
    auto flattened = matrix | std::views::join;
    
    for (int x : flattened) {
        std::cout << x << " ";  // 1 2 3 4 5 6 7 8 9
    }
    std::cout << "\n";
    
    // Flatten and transform
    auto squares = matrix 
        | std::views::join 
        | std::views::transform([](int x) { return x * x; });
    
    for (int x : squares) {
        std::cout << x << " ";  // 1 4 9 16 25 36 49 64 81
    }
    std::cout << "\n";
    
    return 0;
}
```

### Example 4: Flatmap Pattern
```cpp
#include <ranges>
#include <vector>
#include <string>
#include <iostream>

int main() {
    std::vector<std::string> words = {"hello", "world", "fp"};
    
    // Split each string into characters (flatmap equivalent)
    auto chars = words 
        | std::views::transform([](const std::string& s) {
            return std::views::transform(s, [](char c) { return c; });
        })
        | std::views::join;
    
    for (char c : chars) {
        std::cout << c << " ";  // h e l l o w o r l d f p
    }
    std::cout << "\n";
    
    return 0;
}
```

### Example 5: take_while and drop_while
```cpp
#include <ranges>
#include <vector>
#include <iostream>

int main() {
    std::vector<int> numbers = {1, 2, 3, 4, 5, 4, 3, 2, 1};
    
    // Take while ascending
    auto ascending = numbers 
        | std::views::take_while([](int x) { return x < 5; });
    
    for (int x : ascending) {
        std::cout << x << " ";  // 1 2 3 4
    }
    std::cout << "\n";
    
    // Drop first 3 elements
    auto after_drop = numbers 
        | std::views::drop(3);
    
    for (int x : after_drop) {
        std::cout << x << " ";  // 4 5 4 3 2 1
    }
    std::cout << "\n";
    
    return 0;
}
```

---

## Recursion

C++ does **not** guarantee tail-call optimization (it's implementation-defined and rarely applied). For production code, prefer iterators/ranges over deep recursion.

### Regular recursion (stack grows with depth)
```cpp
int factorial(int n) {
    if (n <= 1) return 1;
    return n * factorial(n - 1);  // NOT tail-recursive
}
```

### Recursive lambdas require `std::function` (or a Y-combinator)
```cpp
#include <functional>

// std::function allows self-reference but adds type-erasure overhead
std::function<int(int)> fib = [&fib](int n) -> int {
    if (n < 2) return n;
    return fib(n - 1) + fib(n - 2);
};

// For performance-sensitive code, use a named struct with operator()
struct Fib {
    int operator()(int n) const {
        if (n < 2) return n;
        return (*this)(n - 1) + (*this)(n - 2);
    }
} fib_fast;
```

### Prefer iteration for accumulation
```cpp
#include <numeric>

// Iterative sum — no stack growth, works on any range
int sum = std::accumulate(numbers.begin(), numbers.end(), 0);

// Ranges fold (C++23)
int sum = std::ranges::fold_left(numbers, 0, std::plus{});
```

---

## Required Includes

```cpp
#include <ranges>      // views::transform, views::filter, views::take, views::drop, views::reverse, views::join, views::zip (C++23)
#include <algorithm>   // ranges::sort, ranges::max, ranges::min, ranges::count, ranges::unique, ranges::any_of, ranges::all_of
#include <numeric>     // std::accumulate, std::partial_sum, std::inclusive_scan
#include <functional>  // std::bind_front, std::function
```

---

## Key Takeaways

1. **Lazy Evaluation**: Views don't compute until iterated — efficient for large datasets
2. **Composability**: Chain operations with `|` for readable left-to-right pipelines
3. **Immutability**: Views create new views without modifying original data; use `const` aggressively
4. **Range Algorithms**: Use `std::ranges::` versions for consistency with views
5. **Templates over `std::function`**: Template parameters (`auto fn`) avoid type-erasure overhead; use `std::function` only when you need to store or erase a callable type
6. **No guaranteed TCO**: C++ does not guarantee tail-call optimization — prefer iterators and `std::ranges` over deep recursion
7. **C++23 Features**: `fold_left`, `views::zip`, `views::cartesian_product` add powerful operations
