# Functional Programming Patterns

*["Idiomatic Rust favors functional programming. It's a better fit for its ownership model."](https://kerkour.com/rust-functional-programming)  Sylvain Kerkour*

---

## map
Transform each element of a collection.

| Language | Syntax |
|:---------|:-------|
| Scala | `map` |
| Rust | `iter.map(f)` |
| Kotlin | `arr.map(f)` · `seq.map(f)` |
| JavaScript | `arr.map(f)` |
| RamdaJS | `R.map(f, arr)` |
| Python | `map(f, iter)` · `[f(x) for x in iter]` |
| C++ | `views::transform(f)` · `ranges::transform` |
| OCaml | `List.map f lst` · `Seq.map` |

---

## filter
Keep only elements satisfying a predicate.

| Language | Syntax |
|:---------|:-------|
| Scala | `filter` |
| Rust | `iter.filter(pred)` |
| Kotlin | `arr.filter(pred)` · `arr.filterNot(pred)` |
| JavaScript | `arr.filter(pred)` |
| RamdaJS | `R.filter(pred, arr)` |
| Python | `filter(pred, iter)` · `[x for x in iter if pred(x)]` |
| C++ | `views::filter(pred)` · `ranges::copy_if` |
| OCaml | `List.filter pred lst` · `Seq.filter` |

---

## fold
Reduce a collection to a single value by accumulating with a function.

| Language | Syntax |
|:---------|:-------|
| Scala | `foldLeft` · `reduce` |
| Rust | `iter.fold(init, f)` · `iter.reduce(f)` |
| Kotlin | `fold(init, f)` · `reduce(f)` |
| JavaScript | `arr.reduce(f, init)` |
| RamdaJS | `R.reduce(f, init, arr)` |
| Python | `functools.reduce(f, iter)` |
| C++ | `ranges::fold_left` (C++23) · `std::accumulate` |
| OCaml | `List.fold_left f init lst` · `List.fold_right` |

---

## flatmap
Map then flatten one level — each element produces a list, results are concatenated.

| Language | Syntax |
|:---------|:-------|
| Scala | `collect` · `flatMap` |
| Rust | `iter.flat_map(f)` |
| Kotlin | `arr.flatMap(f)` |
| JavaScript | `arr.flatMap(f)` |
| RamdaJS | `R.chain(f, arr)` |
| Python | — |
| C++ | `views::transform(f) \| views::join` |
| OCaml | `List.concat_map f lst` |

---

## flatten
Collapse one level of nesting.

| Language | Syntax |
|:---------|:-------|
| Scala | `flatten` |
| Rust | `iter.flatten()` |
| Kotlin | `arr.flatten()` |
| JavaScript | `arr.flat()` |
| RamdaJS | `R.flatten(arr)` |
| Python | `(x for subseq in seq for x in subseq)` |
| C++ | `views::join` |
| OCaml | `List.flatten lst` · `List.concat` |

---

## curry
Partially apply a function by fixing some of its arguments.

| Language | Syntax |
|:---------|:-------|
| Scala | — |
| Rust | — |
| Kotlin | curried lambdas · custom `curried()` / `partial` |
| JavaScript | — |
| RamdaJS | `R.curry(f)` |
| Python | `toolz.curry(f)` · `functools.partial` |
| C++ | lambdas · `std::bind_front` |
| OCaml | native — all functions are curried by default |

---

## compose
Combine functions so the output of one feeds into the next.

| Language | Syntax |
|:---------|:-------|
| Scala | — |
| Rust | — |
| Kotlin | `let` chaining · custom `compose` / `andThen` |
| JavaScript | — |
| RamdaJS | `R.compose(f, g)` (right-to-left) · `R.pipe(f, g)` (left-to-right) |
| Python | `toolz.compose(f, g)` · `toolz.compose_left` as pipe |
| C++ | `\|` pipe operator · lambdas |
| OCaml | `\|>` pipe (left-to-right) · `@@` apply · manual `%` compose |

---

## scan
Like fold, but emits each intermediate accumulator value.

| Language | Syntax |
|:---------|:-------|
| Scala | `scanLeft` |
| Rust | `iter.scan(init, f)` |
| Kotlin | `runningFold(init, f)` · `scan(init, f)` (alias) |
| JavaScript | — |
| RamdaJS | `R.scan(f, init, arr)` |
| Python | `itertools.accumulate(iter, f)` |
| C++ | `std::inclusive_scan` · `std::partial_sum` |
| OCaml | custom via `List.fold_left` |

---

## sum
Sum all elements.

| Language | Syntax |
|:---------|:-------|
| Scala | `sum` |
| Rust | `iter.sum()` |
| Kotlin | `sum()` · `sumOf { }` |
| JavaScript | `arr.reduce((a, b) => a + b, 0)` |
| RamdaJS | `R.sum(arr)` |
| Python | `sum(iter)` |
| C++ | `ranges::fold_left` (C++23) · `std::accumulate` |
| OCaml | `List.fold_left (+) 0 lst` |

---

## count
Count elements (optionally matching a predicate).

| Language | Syntax |
|:---------|:-------|
| Scala | `size` · `count` |
| Rust | `iter.count()` · `len()` |
| Kotlin | `count()` · `count(pred)` · `size` |
| JavaScript | `arr.length` |
| RamdaJS | `R.count(pred, arr)` |
| Python | `len(lst)` |
| C++ | `ranges::count` · `ranges::distance` |
| OCaml | `List.length lst` |

---

## max / min
Find the largest or smallest element.

| Language | Syntax |
|:---------|:-------|
| Scala | `max` / `min` |
| Rust | `iter.max()` / `iter.min()` |
| Kotlin | `maxOrNull()` / `minOrNull()` · `maxByOrNull { }` / `minByOrNull { }` |
| JavaScript | `arr.reduce((a, b) => a > b ? a : b)` |
| RamdaJS | `R.max(arr)` / `R.min(arr)` |
| Python | `max(iter)` / `min(iter)` |
| C++ | `ranges::max` / `ranges::min` |
| OCaml | `List.fold_left max (List.hd lst) lst` / `List.fold_left min` |

---

## sort
Sort elements. Items marked `*` sort in place (mutate).

| Language | Syntax |
|:---------|:-------|
| Scala | `sorted` |
| Rust | `slice::sort*` |
| Kotlin | `sorted()` · `sortedBy { }` · `MutableList.sort()`* |
| JavaScript | `arr.sort((a, b) => a - b)*` |
| RamdaJS | `R.sort(comparator, arr)` |
| Python | `sorted(iter)` |
| C++ | `ranges::sort*` |
| OCaml | `List.sort compare lst` |

---

## reverse
Reverse the order of elements. Items marked `*` mutate.

| Language | Syntax |
|:---------|:-------|
| Scala | `reverse` |
| Rust | `iter.rev()` |
| Kotlin | `reversed()` · `asReversed()` (view) · `MutableList.reverse()`* |
| JavaScript | `arr.reverse()*` |
| RamdaJS | `R.reverse(arr)` |
| Python | `lst[::-1]` · `reversed(iter)` |
| C++ | `views::reverse` |
| OCaml | `List.rev lst` |

---

## rotate
Move elements cyclically (e.g. last element to front, or shift by n).

| Language | Syntax |
|:---------|:-------|
| Scala | `(o.takeRight(1) ::: o.dropRight(1))` · `(o.tail :+ o.head)` |
| Rust | `slice::rotate_right(n)*` · `slice::rotate_left(n)*` |
| Kotlin | `l.drop(n) + l.take(n)` · `l.takeLast(1) + l.dropLast(1)` |
| JavaScript | — |
| RamdaJS | `R.move(-1, 0, arr)` · `R.move(0, -1, arr)` |
| Python | `lst[n:] + lst[:n]` · `numpy.roll(arr, n)` |
| C++ | `ranges::rotate*` |
| OCaml | custom via `take`/`drop` + `@` |

Items marked `*` mutate.

---

## unique
Remove duplicate elements.

| Language | Syntax |
|:---------|:-------|
| Scala | `distinct` |
| Rust | `dedup*` |
| Kotlin | `distinct()` · `distinctBy { }` |
| JavaScript | `[...new Set(arr)]` |
| RamdaJS | `R.uniq(arr)` |
| Python | `list(set(seq))` |
| C++ | `ranges::unique*` |
| OCaml | `List.sort_uniq compare lst` |

Items marked `*` mutate (or require a sorted/mutable structure).

---

## take / drop
Take the first n elements, or skip the first n elements.

| Language | Syntax |
|:---------|:-------|
| Scala | `take(n)` / `drop(n)` |
| Rust | `iter.take(n)` / `iter.skip(n)` |
| Kotlin | `take(n)` / `drop(n)` · `takeWhile { }` / `dropWhile { }` |
| JavaScript | `arr.slice(0, n)` / `arr.slice(n)` |
| RamdaJS | `R.take(n, arr)` / `R.drop(n, arr)` |
| Python | `lst[:n]` / `lst[n:]` |
| C++ | `views::take(n)` / `views::drop(n)` |
| OCaml | custom `take n lst` / custom `drop n lst` · `Seq.take`/`Seq.drop` (≥4.14) |

---

## any / all
Test whether any or all elements satisfy a predicate.

| Language | Syntax |
|:---------|:-------|
| Scala | `exists(pred)` / `forall(pred)` |
| Rust | `iter.any(pred)` / `iter.all(pred)` |
| Kotlin | `any(pred)` / `all(pred)` · `none(pred)` |
| JavaScript | `arr.some(pred)` / `arr.every(pred)` |
| RamdaJS | `R.any(pred, arr)` / `R.all(pred, arr)` |
| Python | `any(pred(x) for x in iter)` / `all(...)` |
| C++ | `ranges::any_of` / `ranges::all_of` |
| OCaml | `List.exists pred lst` / `List.for_all pred lst` |

---

## zip
Pair elements from two collections by position (inner product / shortest wins).

| Language | Syntax |
|:---------|:-------|
| Scala | `zip` |
| Rust | `iter.zip(other)` |
| Kotlin | `a.zip(b)` · `zipWithNext()` |
| JavaScript | — |
| RamdaJS | `R.zip(a, b)` |
| Python | `zip(a, b)` |
| C++ | `views::zip` (C++23) |
| OCaml | `List.combine a b` |

---

## cartesian product
Produce all combinations of elements from two collections (outer product).

| Language | Syntax |
|:---------|:-------|
| Scala | — |
| Rust | `itertools::iproduct!(a, b)` |
| Kotlin | `a.flatMap { x -> b.map { y -> x to y } }` |
| JavaScript | — |
| RamdaJS | — |
| Python | `itertools.product(a, b)` · `[(x, y) for x in a for y in b]` |
| C++ | `views::cartesian_product` (C++23) |
| OCaml | `List.concat_map (fun x -> List.map (fun y -> (x,y)) b) a` |

---

## chain
Concatenate two or more collections end-to-end.

| Language | Syntax |
|:---------|:-------|
| Scala | `a ++ b` |
| Rust | `iter.chain(other)` |
| Kotlin | `a + b` |
| JavaScript | `a.concat(b)` |
| RamdaJS | `R.concat(a, b)` |
| Python | `itertools.chain(a, b)` |
| C++ | `views::concat` (C++26) |
| OCaml | `a @ b` · `List.append a b` |

---

<br>

## C++ Ranges Includes
```cpp
#include <ranges>      // views::transform, views::filter, views::take, views::drop, views::reverse, views::join, views::zip (C++23)
#include <algorithm>   // ranges::sort, ranges::max, ranges::min, ranges::count, ranges::unique, ranges::any_of, ranges::all_of, ranges::transform, ranges::copy_if
#include <numeric>     // std::accumulate, std::partial_sum, std::inclusive_scan
#include <functional>  // std::bind_front
```


<br>

## Concepts ([ref](https://www.baeldung.com/scala/functional-programming))
- Immutability
- Recursion ( Tail Recursion )
- Functions as first-class objects
- Pure Functions
- Function Composition ( Category Theory )
- Higher-Order Functions
- Anonymous Functions
- Closures
- Partial Functions
- Monads
- Currying


<br>

## Iterators in Rust

[Iterator in std::iter](https://doc.rust-lang.org/std/iter/trait.Iterator.html)

`iter` — borrows elements by reference

`into_iter` — takes ownership (consumes the collection)

`iter_mut` — mutable references

`iter.inspect(f)` — peek at values flowing through a pipeline without consuming them


<br>

## Collections vs Sequences in Kotlin

`List` / `Iterable` operations are eager — each step allocates an intermediate list

`Sequence` operations are lazy — call `.asSequence()` to opt in

`generateSequence(seed) { next }` — build a possibly infinite sequence

`sequence { yield(x) }` — imperative-style lazy generator


<br>

## Scala Tail Recursion
`import scala.annotation.tailrec`

`@tailrec` annotation before recursive function definition


<br>

## Python Caching
`from functools import cache`

`@cache` decorator before function definition
