# Functional Programming

*["Idiomatic Rust favors functional programming. It's a better fit for its ownership model."](https://kerkour.com/rust-functional-programming)  Sylvain Kerkour*


<br>

|  | Rust | APL | Scala | Python | JavaScript |
|:--:|:--:|:--:|:--:|:--:|:--:|
| map | iter.map | ¨ | map | map | map |
| filter | iter.filter<br>iter.take<br>iter.skip<br>iter.nth | / (replicate) | filter | filter<br>itertools.takewhile | filter |
| filter & map<br>flat map | iter.filter_map<br>iter.flat_map | ??? | collect<br>flatMap | ??? | ??? |
| fold | iter.fold<br>iter.reduce | / (reduce)<br>⌿ (reduce first) | foldLeft<br>reduce | functools.reduce | reduce |
| sum | iter.sum| +/ | sum | sum | ??? |
| count | iter.count<br>len | ≢ (tally) | size<br>count | len | length |
| max | iter.max | ⌈/ | max | max | ??? |
| min | iter.min | ⌊/ | min | min | ??? |
| sort | slice::sort* | {⍵[⍋⍵]} | sorted | List.sort | sort((a,b) => a-b)* |
| reverse | iter.rev | ⌽ (rotate) | reverse | [::-1] | reverse* |
| drop | skip | ??? | drop | ??? | ??? |
| take | take | ??? | take | ??? | ??? |
| any | iter.any | ??? | exists | any | ??? |
| all | iter.all | ??? | forall | all | ??? |
| zip<br>inner product | iter.zip | . (product) | zip | zip | ??? |
| outer product | itertools::iproduct | ∘. | ??? | itertools.product | ??? |
| chain | iter.chain | ⍪ (catenate) | ++ | itertools.chain | ??? |
| flatten | iter.flatten | , (ravel)<br>∊ (enlist) (?) | flatten | [item for sublist in NESTED for item in sublist] | ??? |
| scan | iter.scan | \\ (scan)<br>⍀ (scan first) | scanLeft | itertools.accumulate | |
| rotate | slice.rotate_right<br>slice.rotate_left | ??? | `(o takeRight 1) ::: (o dropRight 1)`<br>`o.tail :+ o.head` | `o[n:] + o[:n]`<br>`o[-n:] + o[:-n]`<br>`numpy.roll` | ??? |
| unique?? |  | ∪ (down shoe) |  |  | ??? |
| remove duplicates | dedup | ??? | distinct | ??? | ??? |

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
