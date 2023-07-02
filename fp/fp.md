# Functional Programming

*["Idiomatic Rust favors functional programming. It's a better fit for its ownership model."](https://kerkour.com/rust-functional-programming)  Sylvain Kerkour*


<br>

|  | Scala | Rust | JavaScript | RamdaJS | Python |
|:--:|:--:|:--:|:--:|:--:|:--:|
| map | map | iter.map | map | R.map | map<br>( f(a) for a in iter ) |
| filter | filter | iter.filter | filter | R.filter | filter<br>( a for a in iter if predicate(a) ) |
| fold | foldLeft<br>reduce | iter.fold<br>iter.reduce | reduce | R.reduce | functools.reduce |
| flatmap | collect<br>flatMap | iter.flat_map | flatMap | R.chain | |
| flatten | flatten | iter.flatten | flat() | R.flatten | ( a for subseq in seq for a in subseq ) |
| curry | TODO | TODO | | R.curry | toolz.curry |
| scan | scanLeft | iter.scan | | R.scan | itertools.accumulate |
| sum | sum | iter.sum | reduce | R.sum | sum |
| count | size<br>count | iter.count<br>len | length | R.count | len |
| max | max | iter.max | reduce | R.max | max |
| min | min | iter.min | reduce | R.min | min |
| sort | sorted | slice::sort* | sort((a,b) => a-b)* | R.sort | sorted |
| reverse | reverse | iter.rev | reverse* | R.reverse | `[::-1]`<br>reversed |
| compose | TODO | TODO | | R.compose<br>R.pipe | toolz.compose<br>toolz.compose_left as pipe |
| drop | drop | skip | slice | R.drop | `[:-1]` |
| take | take | take | slice | R.take | `[:1]` |
| any | exists | iter.any | some() | R.any | any |
| all | forall | iter.all | every() | R.all | all |
| zip<br>inner product | zip | iter.zip | ??? | R.zip | zip |
| outer product (Cartesian) | ??? | itertools::iproduct | ??? | | itertools.product<br>( (a, b) for a in as for b in bs ) |
| chain | ++ | iter.chain | concat | R.concat | itertools.chain |
| rotate | `(o takeRight 1) ::: (o dropRight 1)`<br>`o.tail :+ o.head` | slice.rotate_right<br>slice.rotate_left | ??? | R.move(-1)(0) <br> R.move(0)(-1) | `o[n:] + o[:n]`<br>`o[-n:] + o[:-n]`<br>`numpy.roll` |
| unique | distinct | dedup | | R.uniq | set(seq) |

( *modifies a mutable data structure )


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

`@tailrec` before recursive function definition
