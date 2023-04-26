# Functional Programming

*["Idiomatic Rust favors functional programming. It's a better fit for its ownership model."](https://kerkour.com/rust-functional-programming)  Sylvain Kerkour*


<br>

|  | Scala | Rust | JavaScript | RamdaJS | Python |
|:--:|:--:|:--:|:--:|:--:|:--:|
| map | map | iter.map | map | R.map | map |
| filter | filter | iter.filter | filter | R.filter | filter |
| flatmap | collect<br>flatMap | iter.flat_map | flatMap | R.chain | |
| fold | foldLeft<br>reduce | iter.fold<br>iter.reduce | reduce | R.reduce | functools.reduce |
| sum | sum | iter.sum | reduce | R.sum | sum |
| count | size<br>count | iter.count<br>len | length | R.count | len |
| max | max | iter.max | reduce | R.max | max |
| min | min | iter.min | reduce | R.min | min |
| sort | sorted | slice::sort* | sort((a,b) => a-b)* | R.sort | List.sort |
| reverse | reverse | iter.rev | reverse* | R.reverse | [::-1] |
| drop | drop | skip | slice | R.drop | ??? |
| take | take | take | slice | R.take | ??? |
| any | exists | iter.any | some() | R.any | any |
| all | forall | iter.all | every() | R.all | all |
| zip<br>inner product | zip | iter.zip | ??? | R.zip | zip |
| outer product | ??? | itertools::iproduct | ??? | | itertools.product |
| chain | ++ | iter.chain | concat | R.concat | itertools.chain |
| flatten | flatten | iter.flatten | flat() | R.flatten | [item for sublist in NESTED for item in sublist] |
| scan | scanLeft | iter.scan | | R.scan | itertools.accumulate |
| rotate | `(o takeRight 1) ::: (o dropRight 1)`<br>`o.tail :+ o.head` | slice.rotate_right<br>slice.rotate_left | ??? | R.move(-1)(0) <br> R.move(0)(-1) | `o[n:] + o[:n]`<br>`o[-n:] + o[:-n]`<br>`numpy.roll` |
| unique | distinct | dedup | | R.uniq | ??? |

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
