# Functional Programming in Python

## Core FP Concepts in Python

### 1. Immutability
```python
# Python doesn't enforce immutability, but you can use immutable types
from typing import Tuple, FrozenSet

# Immutable types
numbers: Tuple[int, ...] = (1, 2, 3, 4, 5)  # Tuple instead of list
unique: FrozenSet[int] = frozenset({1, 2, 3})  # FrozenSet instead of set

# Avoid mutation - create new collections instead
original = [1, 2, 3]
modified = original + [4]  # New list, original unchanged

# Named tuples for immutable records
from collections import namedtuple
Point = namedtuple('Point', ['x', 'y'])
p = Point(3, 4)
# p.x = 5  # Error! Can't modify

# dataclasses with frozen=True
from dataclasses import dataclass

@dataclass(frozen=True)
class Person:
    name: str
    age: int

alice = Person("Alice", 30)
# alice.age = 31  # Error! Frozen dataclass
```

### 2. Pure Functions
```python
# Pure function: same input → same output, no side effects
def square(x: int) -> int:
    return x * x

# Not pure (has side effects)
global_counter = 0
def impure_square(x: int) -> int:
    global global_counter
    global_counter += 1  # Side effect!
    return x * x

# Not pure (depends on external state)
import random
def impure_random_add(x: int) -> int:
    return x + random.randint(1, 10)  # Non-deterministic!
```

### 3. Higher-Order Functions (Functions as First-Class Objects)
```python
from typing import Callable, List

# Function taking another function as parameter
def apply_to_all(items: List[int], func: Callable[[int], int]) -> List[int]:
    return [func(item) for item in items]

# Lambda functions (anonymous functions)
square = lambda x: x * x
add = lambda a, b: a + b

# Using higher-order functions
numbers = [1, 2, 3, 4, 5]
squares = apply_to_all(numbers, square)  # [1, 4, 9, 16, 25]
squares = apply_to_all(numbers, lambda x: x * x)  # Same result

# Functions returning functions
def multiplier(factor: int) -> Callable[[int], int]:
    return lambda x: x * factor

double = multiplier(2)
triple = multiplier(3)
print(double(5))  # 10
print(triple(5))  # 15
```

---

## Built-in Functional Tools

### map
```python
numbers = [1, 2, 3, 4, 5]

# map returns an iterator (lazy)
squares = map(lambda x: x * x, numbers)
print(list(squares))  # [1, 4, 9, 16, 25]

# With named function
def to_upper(s: str) -> str:
    return s.upper()

words = ["hello", "world"]
upper_words = list(map(to_upper, words))  # ["HELLO", "WORLD"]

# Multiple iterables
a = [1, 2, 3]
b = [10, 20, 30]
sums = list(map(lambda x, y: x + y, a, b))  # [11, 22, 33]
```

### filter
```python
numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

# filter returns an iterator (lazy)
evens = filter(lambda x: x % 2 == 0, numbers)
print(list(evens))  # [2, 4, 6, 8, 10]

# With named function
def is_positive(x: int) -> bool:
    return x > 0

mixed = [-2, -1, 0, 1, 2]
positives = list(filter(is_positive, mixed))  # [1, 2]

# Filter None values
items = [1, None, 2, None, 3]
non_none = list(filter(None, items))  # [1, 2, 3]
```

### reduce (functools)
```python
from functools import reduce

numbers = [1, 2, 3, 4, 5]

# Sum
total = reduce(lambda acc, x: acc + x, numbers)  # 15

# Product
product = reduce(lambda acc, x: acc * x, numbers)  # 120

# With initial value
total = reduce(lambda acc, x: acc + x, numbers, 0)  # 15

# String concatenation
words = ["hello", "world", "python"]
sentence = reduce(lambda acc, word: f"{acc} {word}" if acc else word, words, "")
# "hello world python"

# Find max (though max() is better)
maximum = reduce(lambda a, b: a if a > b else b, numbers)  # 5
```

---

## Comprehensions (Pythonic FP)

### List Comprehensions
```python
numbers = [1, 2, 3, 4, 5]

# map equivalent
squares = [x * x for x in numbers]  # [1, 4, 9, 16, 25]

# filter equivalent
evens = [x for x in numbers if x % 2 == 0]  # [2, 4]

# map + filter combined
even_squares = [x * x for x in numbers if x % 2 == 0]  # [4, 16]

# Nested (flatten)
matrix = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
flat = [x for row in matrix for x in row]  # [1, 2, 3, 4, 5, 6, 7, 8, 9]

# Cartesian product
colors = ["red", "blue"]
sizes = ["S", "M", "L"]
combinations = [(c, s) for c in colors for s in sizes]
# [("red", "S"), ("red", "M"), ("red", "L"), ("blue", "S"), ...]
```

### Generator Expressions (Lazy)
```python
numbers = [1, 2, 3, 4, 5]

# Generator expression - lazy evaluation
squares_gen = (x * x for x in numbers)

# Only computed when iterated
for sq in squares_gen:
    print(sq)

# Memory efficient for large datasets
large_range = range(1_000_000)
sum_of_squares = sum(x * x for x in large_range)  # No intermediate list
```

### Dict Comprehensions
```python
words = ["hello", "world", "python"]

# Create dict from list
word_lengths = {word: len(word) for word in words}
# {"hello": 5, "world": 5, "python": 6}

# Filter dict
scores = {"alice": 85, "bob": 60, "charlie": 90}
passed = {name: score for name, score in scores.items() if score >= 70}
# {"alice": 85, "charlie": 90}

# Invert dict
inverted = {v: k for k, v in {"a": 1, "b": 2}.items()}
# {1: "a", 2: "b"}
```

### Set Comprehensions
```python
numbers = [1, 2, 2, 3, 3, 3, 4, 4, 4, 4]

# Unique squares
unique_squares = {x * x for x in numbers}  # {1, 4, 9, 16}
```

---

## itertools Module

```python
import itertools
```

### chain (concatenate iterables)
```python
a = [1, 2, 3]
b = [4, 5, 6]
c = [7, 8, 9]

combined = list(itertools.chain(a, b, c))  # [1, 2, 3, 4, 5, 6, 7, 8, 9]

# Flatten nested iterables
nested = [[1, 2], [3, 4], [5, 6]]
flat = list(itertools.chain.from_iterable(nested))  # [1, 2, 3, 4, 5, 6]
```

### accumulate (scan/running totals)
```python
numbers = [1, 2, 3, 4, 5]

# Running sum
running_sum = list(itertools.accumulate(numbers))  # [1, 3, 6, 10, 15]

# Running product
import operator
running_product = list(itertools.accumulate(numbers, operator.mul))
# [1, 2, 6, 24, 120]

# Running max
running_max = list(itertools.accumulate(numbers, max))  # [1, 2, 3, 4, 5]
```

### takewhile / dropwhile
```python
numbers = [1, 2, 3, 4, 5, 4, 3, 2, 1]

# Take while condition is true
ascending = list(itertools.takewhile(lambda x: x < 5, numbers))
# [1, 2, 3, 4]

# Drop while condition is true
after = list(itertools.dropwhile(lambda x: x < 5, numbers))
# [5, 4, 3, 2, 1]
```

### islice (take/drop)
```python
numbers = range(10)

# Take first 3
first_three = list(itertools.islice(numbers, 3))  # [0, 1, 2]

# Skip first 3, take next 4
middle = list(itertools.islice(numbers, 3, 7))  # [3, 4, 5, 6]

# Every other element
every_other = list(itertools.islice(numbers, 0, None, 2))  # [0, 2, 4, 6, 8]
```

### product (Cartesian product)
```python
colors = ["red", "blue"]
sizes = ["S", "M", "L"]

combinations = list(itertools.product(colors, sizes))
# [("red", "S"), ("red", "M"), ("red", "L"), 
#  ("blue", "S"), ("blue", "M"), ("blue", "L")]

# Self product (permutations with repetition)
digits = [0, 1]
binary_pairs = list(itertools.product(digits, repeat=3))
# [(0,0,0), (0,0,1), (0,1,0), (0,1,1), (1,0,0), (1,0,1), (1,1,0), (1,1,1)]
```

### permutations / combinations
```python
items = [1, 2, 3]

# Permutations (order matters)
perms = list(itertools.permutations(items, 2))
# [(1, 2), (1, 3), (2, 1), (2, 3), (3, 1), (3, 2)]

# Combinations (order doesn't matter)
combos = list(itertools.combinations(items, 2))
# [(1, 2), (1, 3), (2, 3)]

# Combinations with replacement
combos_rep = list(itertools.combinations_with_replacement(items, 2))
# [(1, 1), (1, 2), (1, 3), (2, 2), (2, 3), (3, 3)]
```

### groupby
```python
from itertools import groupby

# Data must be sorted by the key first!
data = [
    ("fruit", "apple"),
    ("fruit", "banana"),
    ("vegetable", "carrot"),
    ("vegetable", "celery"),
    ("fruit", "cherry"),  # Won't group with earlier fruits!
]

# Group by first element
for key, group in groupby(data, key=lambda x: x[0]):
    print(f"{key}: {list(group)}")
# fruit: [("fruit", "apple"), ("fruit", "banana")]
# vegetable: [("vegetable", "carrot"), ("vegetable", "celery")]
# fruit: [("fruit", "cherry")]  # Separate group!

# Sort first for proper grouping
sorted_data = sorted(data, key=lambda x: x[0])
for key, group in groupby(sorted_data, key=lambda x: x[0]):
    print(f"{key}: {list(group)}")
```

### starmap
```python
from itertools import starmap

pairs = [(2, 3), (4, 5), (6, 7)]

# Apply function to unpacked tuples
products = list(starmap(lambda x, y: x * y, pairs))  # [6, 20, 42]

# Equivalent to
products = [x * y for x, y in pairs]
```

### zip_longest
```python
from itertools import zip_longest

a = [1, 2, 3]
b = ["a", "b"]

# Regular zip stops at shortest
print(list(zip(a, b)))  # [(1, "a"), (2, "b")]

# zip_longest continues with fill value
print(list(zip_longest(a, b, fillvalue=None)))
# [(1, "a"), (2, "b"), (3, None)]
```

---

## functools Module

```python
from functools import reduce, partial, lru_cache
```

### partial (Partial Application)
```python
from functools import partial

def power(base, exponent):
    return base ** exponent

square = partial(power, exponent=2)
cube = partial(power, exponent=3)

print(square(5))  # 25
print(cube(5))    # 125

# Practical example with map
def multiply(x, y):
    return x * y

double = partial(multiply, 2)
numbers = [1, 2, 3, 4, 5]
doubled = list(map(double, numbers))  # [2, 4, 6, 8, 10]
```

### lru_cache (Memoization)
```python
from functools import lru_cache

@lru_cache(maxsize=128)
def fibonacci(n: int) -> int:
    if n < 2:
        return n
    return fibonacci(n - 1) + fibonacci(n - 2)

# Fast due to caching
print(fibonacci(100))  # 354224848179261915075

# cache decorator (Python 3.9+) - unlimited cache
from functools import cache

@cache
def factorial(n: int) -> int:
    if n <= 1:
        return 1
    return n * factorial(n - 1)
```

### reduce
```python
from functools import reduce

numbers = [1, 2, 3, 4, 5]

# Sum
total = reduce(lambda acc, x: acc + x, numbers, 0)

# Flatten nested lists
nested = [[1, 2], [3, 4], [5, 6]]
flat = reduce(lambda acc, lst: acc + lst, nested, [])
# [1, 2, 3, 4, 5, 6]

# Build dict from pairs
pairs = [("a", 1), ("b", 2), ("c", 3)]
d = reduce(lambda acc, kv: {**acc, kv[0]: kv[1]}, pairs, {})
# {"a": 1, "b": 2, "c": 3}
```

---

## toolz Library (Advanced FP)

```bash
pip install toolz
```

### compose / pipe
```python
from toolz import compose, pipe

# compose - right to left
process = compose(
    str.upper,
    str.strip,
    lambda s: s.replace("_", " ")
)
result = process("  hello_world  ")  # "HELLO WORLD"

# pipe - left to right (more readable)
result = pipe(
    "  hello_world  ",
    lambda s: s.replace("_", " "),
    str.strip,
    str.upper
)  # "HELLO WORLD"

# With multiple arguments
from toolz import pipe
from toolz.curried import map, filter, reduce

result = pipe(
    [1, 2, 3, 4, 5],
    filter(lambda x: x % 2 == 0),
    map(lambda x: x * x),
    list
)  # [4, 16]
```

### curry
```python
from toolz import curry

@curry
def add(x, y, z):
    return x + y + z

# Partial application
add_5 = add(5)
add_5_10 = add_5(10)
result = add_5_10(15)  # 30

# Or chain calls
result = add(5)(10)(15)  # 30
```

### get / pluck
```python
from toolz import get, pluck

# get - safely access nested data
data = {"a": {"b": {"c": 1}}}
value = get(["a", "b", "c"], data)  # 1
value = get(["a", "x", "c"], data, default=None)  # None

# pluck - extract field from sequence of dicts
records = [
    {"name": "Alice", "age": 30},
    {"name": "Bob", "age": 25},
    {"name": "Charlie", "age": 35}
]
names = list(pluck("name", records))  # ["Alice", "Bob", "Charlie"]
```

### groupby (toolz version)
```python
from toolz import groupby

words = ["apple", "banana", "apricot", "blueberry", "cherry"]

# Group by first letter
grouped = groupby(lambda w: w[0], words)
# {"a": ["apple", "apricot"], "b": ["banana", "blueberry"], "c": ["cherry"]}

# Group numbers by even/odd
numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
grouped = groupby(lambda x: "even" if x % 2 == 0 else "odd", numbers)
# {"odd": [1, 3, 5, 7, 9], "even": [2, 4, 6, 8, 10]}
```

### partition
```python
from toolz import partition, partition_all

numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9]

# Partition into chunks (drops incomplete)
chunks = list(partition(3, numbers))
# [(1, 2, 3), (4, 5, 6), (7, 8, 9)]

# Partition all (keeps incomplete)
chunks = list(partition_all(4, numbers))
# [(1, 2, 3, 4), (5, 6, 7, 8), (9,)]
```

### sliding_window
```python
from toolz import sliding_window

numbers = [1, 2, 3, 4, 5]

windows = list(sliding_window(3, numbers))
# [(1, 2, 3), (2, 3, 4), (3, 4, 5)]
```

---

## Practical Examples

### Example 1: Data Processing Pipeline
```python
from dataclasses import dataclass
from collections import defaultdict
from functools import reduce
from toolz import pipe
from toolz.curried import filter, map

@dataclass
class Employee:
    name: str
    department: str
    salary: float
    active: bool

employees = [
    Employee("Alice",   "Engineering", 80000.0, True),
    Employee("Bob",     "Sales",       60000.0, False),
    Employee("Charlie", "Engineering", 90000.0, True),
    Employee("Diana",   "Marketing",   55000.0, True),
    Employee("Eve",     "Engineering", 75000.0, True),
]

# 1. Names of active Engineering employees, sorted
eng_names = sorted(
    e.name for e in employees
    if e.active and e.department == "Engineering"
)
print(eng_names)  # ["Alice", "Charlie", "Eve"]

# 2. Average salary of active Engineering employees
eng = [e for e in employees if e.active and e.department == "Engineering"]
avg_salary = sum(e.salary for e in eng) / len(eng)
print(f"${avg_salary:,.2f}")  # $81,666.67

# 3. Department headcount and total salary (active only), using toolz pipe
dept_summary = pipe(
    employees,
    filter(lambda e: e.active),
    lambda active: reduce(
        lambda acc, e: {
            **acc,
            e.department: {
                "count": acc.get(e.department, {}).get("count", 0) + 1,
                "total": acc.get(e.department, {}).get("total", 0.0) + e.salary,
            }
        },
        active,
        {}
    )
)
for dept, stats in sorted(dept_summary.items()):
    print(f"{dept}: {stats['count']} employees, ${stats['total']:,.0f} total")
# Engineering: 3 employees, $245,000 total
# Marketing:   1 employees, $55,000 total
```

### Example 2: String Processing
```python
words = ["hello", "world", "functional", "python"]

# Transform to uppercase, filter by length
result = [w.upper() for w in words if len(w) > 5]
print(result)  # ["FUNCTIONAL", "PYTHON"]

# Using map/filter
result = list(map(str.upper, filter(lambda w: len(w) > 5, words)))
print(result)  # ["FUNCTIONAL", "PYTHON"]

# Count total characters
total_chars = sum(map(len, words))
print(f"Total characters: {total_chars}")  # 25

# Join with separator
sentence = " ".join(words)
print(sentence)  # "hello world functional python"

# Word frequency
from collections import Counter
text = "the quick brown fox jumps over the lazy dog the fox"
word_counts = Counter(text.split())
print(word_counts.most_common(3))  # [("the", 3), ("fox", 2), ("quick", 1)]
```

### Example 3: Working with Optional Values
```python
from typing import Optional

def safe_divide(a: float, b: float) -> Optional[float]:
    return a / b if b != 0 else None

def safe_sqrt(x: float) -> Optional[float]:
    return x ** 0.5 if x >= 0 else None

# Chaining optional operations
def process(a: float, b: float) -> Optional[float]:
    result = safe_divide(a, b)
    if result is None:
        return None
    return safe_sqrt(result)

print(process(16, 4))   # 2.0
print(process(16, 0))   # None
print(process(-16, 4))  # None

# Using walrus operator (Python 3.8+)
def process_v2(a: float, b: float) -> Optional[float]:
    if (div := safe_divide(a, b)) is not None:
        return safe_sqrt(div)
    return None

# Filter None from list
items = [1, None, 2, None, 3, None]
non_none = [x for x in items if x is not None]
print(non_none)  # [1, 2, 3]
```

### Example 4: Enumerate and Indexed Operations
```python
words = ["zero", "one", "two", "three"]

# Enumerate
indexed = list(enumerate(words))
print(indexed)  # [(0, "zero"), (1, "one"), (2, "two"), (3, "three")]

# Find index
try:
    position = words.index("two")
    print(f"Position of 'two': {position}")  # 2
except ValueError:
    print("Not found")

# Filter by index
even_indexed = [word for i, word in enumerate(words) if i % 2 == 0]
print(even_indexed)  # ["zero", "two"]

# With start index
indexed_from_1 = list(enumerate(words, start=1))
print(indexed_from_1)  # [(1, "zero"), (2, "one"), ...]
```

### Example 5: Grouping and Partitioning
```python
from itertools import groupby
from collections import defaultdict

numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

# Partition into evens and odds
evens = [x for x in numbers if x % 2 == 0]
odds = [x for x in numbers if x % 2 != 0]
print(f"Evens: {evens}")  # [2, 4, 6, 8, 10]
print(f"Odds: {odds}")    # [1, 3, 5, 7, 9]

# Using groupby (data must be sorted!)
def is_even(x):
    return x % 2 == 0

sorted_nums = sorted(numbers, key=is_even)
for key, group in groupby(sorted_nums, key=is_even):
    print(f"{'Even' if key else 'Odd'}: {list(group)}")

# Group by key with defaultdict
words = ["apple", "banana", "apricot", "blueberry", "cherry"]
grouped = defaultdict(list)
for word in words:
    grouped[word[0]].append(word)

print(dict(grouped))
# {"a": ["apple", "apricot"], "b": ["banana", "blueberry"], "c": ["cherry"]}
```

### Example 6: Flatten Nested Structures
```python
from itertools import chain
from typing import List, Any

# Flatten one level
nested = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
flat = list(chain.from_iterable(nested))
print(flat)  # [1, 2, 3, 4, 5, 6, 7, 8, 9]

# Comprehension approach
flat = [x for sublist in nested for x in sublist]

# Recursive flatten (deep)
def deep_flatten(nested: List[Any]) -> List[Any]:
    result = []
    for item in nested:
        if isinstance(item, list):
            result.extend(deep_flatten(item))
        else:
            result.append(item)
    return result

deeply_nested = [1, [2, [3, [4, 5]]], 6, [7, 8]]
print(deep_flatten(deeply_nested))  # [1, 2, 3, 4, 5, 6, 7, 8]
```

### Example 7: Lazy Evaluation with Generators
```python
def fibonacci():
    """Infinite fibonacci generator"""
    a, b = 0, 1
    while True:
        yield a
        a, b = b, a + b

# Take first 10 fibonacci numbers
from itertools import islice
first_10 = list(islice(fibonacci(), 10))
print(first_10)  # [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]

# Generator pipeline
def squares(nums):
    for n in nums:
        yield n * n

def evens(nums):
    for n in nums:
        if n % 2 == 0:
            yield n

# Compose generators
numbers = range(1, 11)
result = list(evens(squares(numbers)))
print(result)  # [4, 16, 36, 64, 100]

# Using generator expressions
result = list(x * x for x in range(1, 11) if (x * x) % 2 == 0)
print(result)  # [4, 16, 36, 64, 100]
```

---

## Common Patterns

### map/filter vs Comprehensions
```python
numbers = [1, 2, 3, 4, 5]

# These are equivalent - prefer comprehensions for readability
squares_map = list(map(lambda x: x * x, numbers))
squares_comp = [x * x for x in numbers]

evens_filter = list(filter(lambda x: x % 2 == 0, numbers))
evens_comp = [x for x in numbers if x % 2 == 0]

# map/filter can be cleaner with existing functions
words = ["hello", "world"]
upper_map = list(map(str.upper, words))  # Cleaner
upper_comp = [w.upper() for w in words]  # Also good
```

### Avoid Mutating State
```python
# Bad - mutating state
def double_list_bad(numbers):
    for i in range(len(numbers)):
        numbers[i] *= 2
    return numbers

# Good - create new list
def double_list_good(numbers):
    return [x * 2 for x in numbers]

# Good - use map
def double_list_map(numbers):
    return list(map(lambda x: x * 2, numbers))
```

---

## Required Imports Summary

```python
# Built-in
from functools import reduce, partial, lru_cache, cache
import itertools
from itertools import (
    chain, accumulate, takewhile, dropwhile, islice,
    product, permutations, combinations, groupby,
    starmap, zip_longest
)
from collections import namedtuple, Counter, defaultdict
from dataclasses import dataclass
from typing import Callable, List, Optional, Tuple

# Third-party (pip install toolz)
from toolz import compose, pipe, curry, groupby, partition, sliding_window
from toolz.curried import map, filter, reduce
```

---

## Key Takeaways

1. **Comprehensions are Pythonic**: Prefer list/dict/set comprehensions over map/filter for readability
2. **Generators for Laziness**: Use generator expressions `(x for x in ...)` for memory efficiency
3. **Immutability by Convention**: Python doesn't enforce it, but use tuples, frozensets, and frozen dataclasses
4. **functools for FP**: `reduce`, `partial`, `lru_cache` are essential tools
5. **itertools for Iteration**: Powerful lazy iterators for complex transformations
6. **toolz for Composition**: `pipe` and `compose` enable elegant function composition
7. **No Tail Recursion Optimization**: Python doesn't optimize tail recursion; prefer iteration
