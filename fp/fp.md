# Functional Programming

*["Idiomatic Rust favors functional programming. It's a better fit for its ownership model."](https://kerkour.com/rust-functional-programming)  Sylvain Kerkour*


<br>

| Function | Rust | APL | Scala | Python |
|:--:|:--:|:--:|:--:|:--:|
| map | iter.map | ¨ | map | map |
| filter | iter.filter<br>iter.take<br>iter.skip<br>iter.nth | / (replicate) | filter | filter<br>itertools.takewhile |
| filter & map | iter.filter_map |  |  |  |
| fold | iter.fold<br>iter.reduce | / (reduce)<br>⌿ (reduce first) | foldLeft<br>reduce | functools.reduce |
| sum | iter.sum| +/ | sum | sum |
| count | iter.count<br>len | ≢ (tally) | size<br>count | len |
| max | iter.max | ⌈/ | max | max |
| min | iter.min | ⌊/ | min | min |
| sort | slice::sort | {⍵[⍋⍵]} | sorted | List.sort |
| reverse | iter.rev | ⌽ (rotate) | reverse | [::-1] |
| any | iter.any | ??? | exists | any |
| all | iter.all | ??? | forall | all |
| zip<br>inner product | iter.zip | . (product) | zip | zip |
| outer product | itertools::iproduct | ∘. | ??? | itertools.product |
| chain | iter.chain | ⍪ (catenate) | ??? | itertools.chain |
| flatten | iter.flatten | , (ravel) | ??? | [item for sublist in NESTED for item in sublist] |
| flatmap | iter.flat_map | ??? | flatMap | ??? |
| scan | iter.scan | \\ (scan)<br>⍀ (scan first) | scanLeft | itertools.accumulate |
| rotate | slice.rotate_right<br>slice.rotate_left | ??? | ??? | l[n:] + l[:n]<br>l[-n:] + l[:-n]<br>numpy.roll |
| unique?? |  | ∪ (down shoe) |  |  |
| remove duplicates | dedup | ??? | distinct | ??? |


<br><br>
## Iterators in Rust

[Iterator in std::iter](https://doc.rust-lang.org/std/iter/trait.Iterator.html)

iter (by reference)
into_iter (owned)

Iter.inspect can be used to inspect values flowing through an iterator.