# Functional Programming

*["Idiomatic Rust favors functional programming. It's a better fit for its ownership model."](https://kerkour.com/rust-functional-programming)  Sylvain Kerkour*


<br>

|  | Scala | Rust | JavaScript | RamdaJS | Python | C++ (std::ranges) |
|:--:|:--:|:--:|:--:|:--:|:--:|:--:|
| map | map | iter.map | map | R.map | map<br>( f(a) for a in iter ) | views::transform<br>ranges::transform |
| filter | filter | iter.filter | filter | R.filter | filter<br>( a for a in iter if predicate(a) ) | views::filter<br>ranges::copy_if |
| fold | foldLeft<br>reduce | iter.fold<br>iter.reduce | reduce | R.reduce | functools.reduce | ranges::fold_left (C++23)<br>std::accumulate |
| flatmap | collect<br>flatMap | iter.flat_map | flatMap | R.chain | | views::transform \| views::join |
| flatten | flatten | iter.flatten | flat() | R.flatten | ( a for subseq in seq for a in subseq ) | views::join |
| curry | TODO | TODO | | R.curry | toolz.curry | lambdas<br>std::bind_front |
| scan | scanLeft | iter.scan | | R.scan | itertools.accumulate | std::inclusive_scan<br>std::partial_sum |
| sum | sum | iter.sum | reduce | R.sum | sum | ranges::fold_left (C++23)<br>std::accumulate |
| count | size<br>count | iter.count<br>len | length | R.count | len | ranges::count<br>ranges::distance |
| max | max | iter.max | reduce | R.max | max | ranges::max |
| min | min | iter.min | reduce | R.min | min | ranges::min |
| sort | sorted | slice::sort* | sort((a,b) => a-b)* | R.sort | sorted | ranges::sort* |
| reverse | reverse | iter.rev | reverse* | R.reverse | `[::-1]`<br>reversed | views::reverse |
| compose | TODO | TODO | | R.compose<br>R.pipe | toolz.compose<br>toolz.compose_left as pipe | \| (pipe operator)<br>lambdas |
| drop | drop | skip | slice | R.drop | `[:-1]` | views::drop |
| take | take | take | slice | R.take | `[:1]` | views::take |
| any | exists | iter.any | some() | R.any | any | ranges::any_of |
| all | forall | iter.all | every() | R.all | all | ranges::all_of |
| zip<br>inner product | zip | iter.zip | ??? | R.zip | zip | views::zip (C++23) |
| outer product (Cartesian) | ??? | itertools::iproduct | ??? | | itertools.product<br>( (a, b) for a in as for b in bs ) | views::cartesian_product (C++23) |
| chain | ++ | iter.chain | concat | R.concat | itertools.chain | views::concat (C++26) |
| rotate | `(o takeRight 1) ::: (o dropRight 1)`<br>`o.tail :+ o.head` | slice.rotate_right<br>slice.rotate_left | ??? | R.move(-1)(0) <br> R.move(0)(-1) | `o[n:] + o[:n]`<br>`o[-n:] + o[:-n]`<br>`numpy.roll` | ranges::rotate* |
| unique | distinct | dedup | | R.uniq | set(seq) | ranges::unique* |

( *modifies a mutable data structure )


<br><br>
## C++ Ranges Includes
```cpp
#include <ranges>      // views::transform, views::filter, views::take, views::drop, views::reverse, views::join, views::zip (C++23)
#include <algorithm>   // ranges::sort, ranges::max, ranges::min, ranges::count, ranges::unique, ranges::any_of, ranges::all_of, ranges::transform, ranges::copy_if
#include <numeric>     // std::accumulate, std::partial_sum, std::inclusive_scan
#include <functional>  // std::bind_front
```


<br><br>
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


<br><br>
## Iterators in Rust

[Iterator in std::iter](https://doc.rust-lang.org/std/iter/trait.Iterator.html)

iter (by reference)
into_iter (owned)

Iter.inspect can be used to inspect values flowing through an iterator.


<br><br>
## Scala Tail Recursion
`import scala.annotation.tailrec`

`@tailrec` decorator before recursive function definition


<br><br>
## Python Caching
`from functools import cache`

`@cache` decorator before function definition
