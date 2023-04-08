# Functional Programming

*["Idiomatic Rust favors functional programming. It's a better fit for its ownership model."](https://kerkour.com/rust-functional-programming)  Sylvain Kerkour*


<br>

|  | Rust | APL | Scala | Python | JavaScript | RamdaJS |
|:--:|:--:|:--:|:--:|:--:|:--:|:--:|
| map | iter.map | ¨ | map | map | map | R.map |
| filter | iter.filter<br>iter.take<br>iter.skip<br>iter.nth | / (replicate) | filter | filter<br>itertools.takewhile | filter | R.filter |
| flatmap | iter.flat_map | ??? | collect<br>flatMap | | | R.chain |
| fold | iter.fold<br>iter.reduce | / (reduce)<br>⌿ (reduce first) | foldLeft<br>reduce | functools.reduce | reduce | R.reduce |
| sum | iter.sum| +/ | sum | sum | reduce | R.sum |
| count | iter.count<br>len | ≢ (tally) | size<br>count | len | length | R.count |
| max | iter.max | ⌈/ | max | max | reduce | R.max |
| min | iter.min | ⌊/ | min | min | reduce | R.min |
| sort | slice::sort* | {⍵[⍋⍵]} | sorted | List.sort | sort((a,b) => a-b)* | R.sort |
| reverse | iter.rev | ⌽ (rotate) | reverse | [::-1] | reverse* | R.reverse |
| drop | skip | ??? | drop | ??? | slice | R.drop |
| take | take | ??? | take | ??? | slice | R.take |
| any | iter.any | ??? | exists | any | some() | R.any |
| all | iter.all | ??? | forall | all | every() | R.all |
| zip<br>inner product | iter.zip | . (product) | zip | zip | ??? | R.zip |
| outer product | itertools::iproduct | ∘. | ??? | itertools.product | ??? | |
| chain | iter.chain | ⍪ (catenate) | ++ | itertools.chain | ??? | ??? |
| flatten | iter.flatten | , (ravel)<br>∊ (enlist) (?) | flatten | [item for sublist in NESTED for item in sublist] | flat() | R.flatten |
| scan | iter.scan | \\ (scan)<br>⍀ (scan first) | scanLeft | itertools.accumulate | | R.scan |
| rotate | slice.rotate_right<br>slice.rotate_left | ??? | `(o takeRight 1) ::: (o dropRight 1)`<br>`o.tail :+ o.head` | `o[n:] + o[:n]`<br>`o[-n:] + o[:-n]`<br>`numpy.roll` | ??? | |
| unique | dedup | ∪ (down shoe) | distinct | ??? |  | R.uniq |

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
