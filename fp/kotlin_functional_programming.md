# Functional Programming in Kotlin

Kotlin is a multi-paradigm language that blends object-oriented and functional styles. Its standard library is heavily functional: `val` bindings, read-only collections, higher-order functions, nullable types, algebraic data types via `sealed` classes, and an expressive `Result` type for recoverable errors. With a few small helpers you can add currying and function composition, but the idiomatic route is to lean on collection/sequence operators and scope functions.

## Core FP Concepts in Kotlin

### 1. Immutability

```kotlin
// val = read-only binding, var = mutable binding. Prefer val.
val numbers = listOf(1, 2, 3, 4, 5)
// numbers = listOf(6, 7, 8)  // Error! Cannot reassign a val

// Read-only collections: no add/remove methods on the interface
val extended = numbers + 6            // [1, 2, 3, 4, 5, 6] — new list
val combined = numbers + listOf(6, 7) // [1, 2, 3, 4, 5, 6, 7]

// Note: read-only != deeply immutable. listOf returns a read-only *view*;
// the underlying implementation could still be mutated elsewhere.
// For true persistent/immutable collections use kotlinx.collections.immutable:
// val pl = persistentListOf(1, 2, 3); val pl2 = pl.add(4)  // pl unchanged

// Data classes are immutable records when all properties are val
data class Point(val x: Double, val y: Double)

val p = Point(3.0, 4.0)
// p.x = 5.0  // Error! val property

// Copy is the idiomatic "functional update"
val moved = p.copy(x = 10.0)  // Point(x=10.0, y=4.0)

// Mutable collections exist but are avoided in FP-style code
val mutable = mutableListOf(1, 2, 3)
mutable.add(4)  // Works, but mutates shared state — avoid in FP
```

### 2. Pure Functions

```kotlin
// Pure function: same input -> same output, no side effects
fun square(x: Int): Int = x * x

// Curried-style pure function returning a function
fun multiplier(factor: Int): (Int) -> Int = { x -> x * factor }

// Not pure (captures and mutates external state)
var callCount = 0
fun impureSquare(x: Int): Int {
    callCount++          // Side effect!
    return x * x
}

// Not pure (nondeterminism / I/O)
fun impureRandomAdd(x: Int): Int = x + (1..10).random()
```

### 3. Higher-Order Functions (Functions as First-Class Objects)

```kotlin
// Functions are values; they can be passed, returned, and stored
fun applyToAll(items: List<Int>, f: (Int) -> Int): List<Int> = items.map(f)

// Function types
val square: (Int) -> Int = { x -> x * x }
val add: (Int, Int) -> Int = { a, b -> a + b }

// Single-parameter lambdas get the implicit name `it`
val doubled = listOf(1, 2, 3).map { it * 2 }  // [2, 4, 6]

// Return functions from functions (closures)
fun makeAdder(n: Int): (Int) -> Int = { x -> x + n }
val add5 = makeAdder(5)
val add10 = makeAdder(10)
// add5(3) == 8, add10(3) == 13

// Function references
fun isEven(x: Int): Boolean = x % 2 == 0
val evens = listOf(1, 2, 3, 4).filter(::isEven)  // [2, 4]

// Use the higher-order function
val squares = applyToAll(listOf(1, 2, 3, 4, 5), square)  // [1, 4, 9, 16, 25]
```

---

## Collections vs Sequences: Eager vs Lazy

Kotlin has two worlds:

- `Iterable` / `List` operations are **eager** — each step builds an intermediate list.
- `Sequence` operations are **lazy** — nothing runs until a terminal operation consumes it.

```kotlin
val numbers = (1..1_000_000).toList()

// Eager: builds a list after filter AND after map
val eager = numbers
    .filter { it % 2 == 0 }
    .map { it * it }
    .take(3)
    .toList()

// Lazy: no intermediate lists; short-circuits at take(3)
val lazyResult = numbers.asSequence()
    .filter { it % 2 == 0 }
    .map { it * it }
    .take(3)
    .toList()  // [4, 16, 36]

// Build lazy / potentially infinite sequences
val naturals = generateSequence(1) { it + 1 }        // 1, 2, 3, ...
val fibs = generateSequence(0 to 1) { (a, b) -> b to a + b }
    .map { it.first }                                 // 0, 1, 1, 2, 3, 5, ...

val firstTenFibs = fibs.take(10).toList()
// [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]

// sequence { } with explicit yield / yieldAll
val powersOfTwo = sequence {
    var n = 1
    while (true) {
        yield(n)
        n *= 2
    }
}
val firstFive = powersOfTwo.take(5).toList()  // [1, 2, 4, 8, 16]
```

Rule of thumb: use `List` for small/medium data and readability; call `.asSequence()` for large pipelines or infinite sources.

---

## Essential Collection Operations

### map

```kotlin
val numbers = listOf(1, 2, 3, 4, 5)

val squares = numbers.map { it * it }                  // [1, 4, 9, 16, 25]
val labels = numbers.map { n -> "n=$n" }               // [n=1, n=2, ...]

// Index-aware and null-dropping variants
val withIndex = numbers.mapIndexed { i, n -> "$i:$n" } // [0:1, 1:2, ...]
val parsed = listOf("1", "two", "3").mapNotNull { it.toIntOrNull() } // [1, 3]
```

### filter

```kotlin
val numbers = (1..10).toList()

val evens = numbers.filter { it % 2 == 0 }             // [2, 4, 6, 8, 10]
val odds = numbers.filterNot { it % 2 == 0 }           // [1, 3, 5, 7, 9]
val indexed = numbers.filterIndexed { i, _ -> i < 3 }  // [1, 2, 3]

// Combine filter + transform in one step
val positives = listOf(-2, -1, 0, 1, 2).filter { it > 0 }  // [1, 2]

// Drop nulls from a list of optionals
val values = listOf(1, null, 2, null, 3).filterNotNull()   // [1, 2, 3]
```

### fold / reduce

```kotlin
val numbers = listOf(1, 2, 3, 4, 5)

// fold takes an explicit initial value
val sum = numbers.fold(0) { acc, n -> acc + n }        // 15
val product = numbers.fold(1) { acc, n -> acc * n }    // 120

// reduce uses the first element as the initial value (throws on empty)
val sum2 = numbers.reduce { acc, n -> acc + n }         // 15
val safe = emptyList<Int>().reduceOrNull { a, b -> a + b } // null

// foldRight / reduceRight process right-to-left
val asString = listOf("a", "b", "c").foldRight("") { n, acc -> n + acc }  // "abc"

// Multiple aggregates in one pass
data class Stats(val count: Int, val total: Int, val min: Int, val max: Int)
val stats = numbers.fold(Stats(0, 0, Int.MAX_VALUE, Int.MIN_VALUE)) { s, n ->
    Stats(s.count + 1, s.total + n, minOf(s.min, n), maxOf(s.max, n))
}
```

### flatMap / flatten

```kotlin
val nested = listOf(listOf(1, 2), listOf(3, 4), listOf(5, 6))

val flat = nested.flatten()                            // [1, 2, 3, 4, 5, 6]

// flatMap = map then flatten one level
val doubled = nested.flatMap { inner -> inner.map { it * 2 } }
// [2, 4, 6, 8, 10, 12]

// Expand each element into several
val expanded = listOf(1, 2, 3).flatMap { listOf(it, it * 10) }
// [1, 10, 2, 20, 3, 30]

// Split text into words
val words = listOf("hello world", "foo bar").flatMap { it.split(" ") }
// [hello, world, foo, bar]
```

### sort / reverse

```kotlin
val numbers = listOf(3, 1, 4, 1, 5, 9, 2, 6)

val sorted = numbers.sorted()                          // [1, 1, 2, 3, 4, 5, 6, 9]
val desc = numbers.sortedDescending()                  // [9, 6, 5, 4, 3, 2, 1, 1]
val byKey = listOf("banana", "fig", "apple").sortedBy { it.length }
// [fig, apple, banana]

data class Person(val name: String, val age: Int)
val people = listOf(Person("Bob", 25), Person("Alice", 30))
val byAge = people.sortedWith(compareBy<Person> { it.age }.thenBy { it.name })

// In-place sorting mutates — only on MutableList / arrays
val mutable = mutableListOf(3, 1, 2)
mutable.sort()                                         // [1, 2, 3] — mutated

// Reverse: reversed() returns a new list, asReversed() is a view
val reversed = numbers.reversed()                      // [6, 2, 9, 5, 1, 4, 1, 3]
val view = numbers.asReversed()                        // lazy view, no copy
```

### take / drop

```kotlin
val numbers = (1..10).toList()

val firstThree = numbers.take(3)                       // [1, 2, 3]
val afterThree = numbers.drop(3)                       // [4, 5, 6, 7, 8, 9, 10]
val lastTwo = numbers.takeLast(2)                      // [9, 10]
val withoutLast = numbers.dropLast(2)                  // [1, 2, 3, 4, 5, 6, 7, 8]

val ascending = listOf(1, 2, 3, 4, 5, 4, 3).takeWhile { it < 5 } // [1, 2, 3, 4]
val after = listOf(1, 2, 3, 4, 5, 4, 3).dropWhile { it < 5 }     // [5, 4, 3]
```

### distinct (unique)

```kotlin
val nums = listOf(1, 2, 2, 3, 3, 3)
val unique = nums.distinct()                           // [1, 2, 3]

// Unique by key, keeping first occurrence
data class User(val id: Int, val name: String)
val users = listOf(User(1, "Alice"), User(2, "Bob"), User(1, "Alice Clone"))
val uniqueById = users.distinctBy { it.id }
// [User(1, Alice), User(2, Bob)]
```

### any / all / none

```kotlin
val numbers = listOf(1, 2, 3, 4, 5)

val hasEven = numbers.any { it % 2 == 0 }              // true
val allPositive = numbers.all { it > 0 }               // true
val noNegatives = numbers.none { it < 0 }              // true
val isEmpty = numbers.isEmpty()                        // false
```

### count / sum / min / max

```kotlin
val numbers = listOf(3, 1, 4, 1, 5, 9, 2, 6)

val total = numbers.count()                            // 8
val evenCount = numbers.count { it % 2 == 0 }          // 3
val sum = numbers.sum()                                // 31
val sumOfSquares = numbers.sumOf { it * it }           // 173

// maxOrNull/minOrNull are null-safe; since Kotlin 1.7 max()/min() throw on empty
val largest = numbers.maxOrNull()                      // 9
val smallest = numbers.minOrNull()                     // 1

val words = listOf("hello", "world", "fp")
val longest = words.maxByOrNull { it.length }          // hello
val shortest = words.minByOrNull { it.length }         // fp
val totalLength = words.sumOf { it.length }            // 12
```

### zip / unzip / zipWithNext

```kotlin
val nums = listOf(1, 2, 3)
val letters = listOf("a", "b", "c")

val pairs = nums.zip(letters)                          // [(1, a), (2, b), (3, c)]
val sums = nums.zip(listOf(10, 20, 30)) { a, b -> a + b }  // [11, 22, 33]

val (left, right) = pairs.unzip()                      // [1,2,3], [a,b,c]

val adjacent = listOf(1, 2, 3, 4).zipWithNext()        // [(1,2), (2,3), (3,4)]
val deltas = listOf(1, 3, 6, 10).zipWithNext { a, b -> b - a }  // [2, 3, 4]

// Zip stops at the shorter collection
val short = listOf(1, 2).zip(listOf("a", "b", "c"))    // [(1, a), (2, b)]
```

### partition / groupBy / associate

```kotlin
val numbers = (1..10).toList()

// partition splits into a Pair of lists in one pass
val (evens, odds) = numbers.partition { it % 2 == 0 }
// evens = [2, 4, 6, 8, 10], odds = [1, 3, 5, 7, 9]

// groupBy builds Map<K, List<T>>
val words = listOf("apple", "banana", "apricot", "blueberry")
val byFirst = words.groupBy { it.first() }
// {a=[apple, apricot], b=[banana, blueberry]}

// Grouping + eachCount gives frequency counts
val freq = words.groupingBy { it.first() }.eachCount() // {a=2, b=2}

// associateBy indexes by key; associateWith maps key -> computed value
val byLength = words.associateWith { it.length }
// {apple=5, banana=6, apricot=7, blueberry=9}

// associate builds an arbitrary Map
val lengths = words.associate { it to it.length }
```

### chunked / windowed

```kotlin
val numbers = (1..9).toList()

val chunks = numbers.chunked(4)                        // [[1,2,3,4], [5,6,7,8], [9]]
val windows = numbers.windowed(3, step = 1)            // [[1,2,3], [2,3,4], ...]

// Sliding-window sums
val windowSums = numbers.windowed(3).map { it.sum() }  // [6, 9, 12, ...]
```

### find / join

```kotlin
val numbers = listOf(1, 2, 3, 4, 5)

val firstEven = numbers.firstOrNull { it % 2 == 0 }    // 2
val found = numbers.find { it > 3 }                    // 4
val index = numbers.indexOfFirst { it == 3 }           // 2

val sentence = listOf("hello", "world", "kotlin").joinToString(" ")  // "hello world kotlin"
val csv = numbers.joinToString(prefix = "[", postfix = "]")          // "[1, 2, 3, 4, 5]"
```

---

## Function Composition & Piping

Kotlin has no built-in `compose`/`pipe`, but `let` chaining and two tiny infix helpers cover it.

```kotlin
// Pipe with let: pass a value into a function, left-to-right
val result = "  hello_world  "
    .replace("_", " ")
    .trim()
    .uppercase()
// "HELLO WORLD"

// Custom compose / andThen (right-to-left and left-to-right)
infix fun <A, B, C> ((A) -> B).andThen(g: (B) -> C): (A) -> C = { a -> g(this(a)) }
infix fun <A, B, C> ((B) -> C).compose(g: (A) -> B): (A) -> C = { a -> this(g(a)) }

val double: (Int) -> Int = { it * 2 }
val toLabel: (Int) -> String = { "value=$it" }

val doubleThenLabel = double andThen toLabel      // left-to-right
val labelOfDouble = toLabel compose double        // right-to-left, same result
// doubleThenLabel(5) == "value=10"

// Scope function `run`/`let` lets you treat any value as a pipeline stage
fun process(s: String): String = s
    .let { it.replace("_", " ") }
    .let { it.trim() }
    .let { it.uppercase() }
```

---

## Currying & Partial Application

Not native, but trivial to add. A curried function is a chain of single-argument functions.

```kotlin
// Currying: (A, B) -> C  ==>  (A) -> (B) -> C
fun <A, B, C> ((A, B) -> C).curried(): (A) -> (B) -> C =
    { a -> { b -> this(a, b) } }

val add: (Int, Int) -> Int = { a, b -> a + b }
val add5 = add.curried()(5)
add5(3)  // 8

// Curried by hand: each lambda returns the next
val multiply: (Int) -> (Int) -> Int = { a -> { b -> a * b } }
val triple = multiply(3)
triple(5)  // 15

// Partial application helpers
fun <A, B, C> ((A, B) -> C).partial(a: A): (B) -> C = { b -> this(a, b) }
fun <A, B, C> ((A, B) -> C).partialRight(b: B): (A) -> C = { a -> this(a, b) }

val double2 = add.partial(2)          // add(2, _)
val inc = add.partialRight(1)         // add(_, 1)
// double2(10) == 12, inc(10) == 11
```

---

## Nullable Types as Maybe

Kotlin replaces `Maybe`/`Option` with null-safety built into the type system.

```kotlin
fun safeDivide(a: Int, b: Int): Int? = if (b == 0) null else a / b

// Safe call ?. , Elvis ?: , and let for chaining
val doubled = safeDivide(10, 2)?.let { it * 2 } ?: -1   // 10
val missing = safeDivide(10, 0)?.let { it * 2 } ?: -1   // -1

// `let` is the "map" for nullable values: transform only when present
val mapped = safeDivide(10, 2)?.let { it * 2 }          // 10

// Chaining optional operations: each step is a `?.let`
val chained = safeDivide(16, 2)?.let { safeDivide(it, 4) }  // 2

// If you want named combinators, do NOT reuse the names `map`/`flatMap`:
// those already exist on Iterable and Result, and a blanket
// `fun <T, R> T?.map(...)` would hijack ordinary collection/Result calls.
// Prefer distinct names such as `fmap`/`bind`:
fun <T, R> T?.fmap(f: (T) -> R): R? = this?.let(f)
fun <T, R> T?.bind(f: (T) -> R?): R? = this?.let(f)

val mappedByHelper = safeDivide(10, 2).fmap { it * 2 }               // 10
val chainedByHelper = safeDivide(16, 2).bind { safeDivide(it, 4) }   // 2

// Guard with takeIf / takeUnless
val positive = listOf(5, -3).map { it.takeIf { n -> n > 0 } }  // [5, null]

// Drop nulls from a list and map in one step
val results = listOf("1", "two", "3").mapNotNull { it.toIntOrNull() }  // [1, 3]
```

---

## Result for Recoverable Errors

`Result<T>` is Kotlin's built-in `Try`/`Either` — a success value or a `Throwable`. Use `runCatching` to capture thrown exceptions.

```kotlin
import kotlin.math.sqrt

fun safeSqrt(x: Double): Double {
    require(x >= 0) { "negative input" }
    return sqrt(x)
}

val result: Result<Double> = runCatching { safeSqrt(16.0) }

// map transforms Success, passes Failure through unchanged
val doubled: Result<Double> = result.map { it * 2 }

// mapCatching also catches exceptions thrown by the transform
val risky = result.mapCatching { 1.0 / it }

// fold is the exhaustive way to consume a Result
val message = result.fold(
    onSuccess = { "ok: $it" },
    onFailure = { "error: ${it.message}" }
)

// Chaining side effects while keeping the Result
runCatching { safeSqrt(-4.0) }
    .onSuccess { println("got $it") }
    .onFailure { println("failed: ${it.message}") }  // failed: negative input

// Defaults / alternatives
val value = result.getOrNull() ?: -1.0
val recovered = runCatching { safeSqrt(-1.0) }.getOrElse { 0.0 }  // 0.0

// NOTE: kotlin.Result intentionally has NO flatMap.
// Implement it yourself when you need to chain functions returning Result:
inline fun <T, R> Result<T>.flatMap(transform: (T) -> Result<R>): Result<R> =
    fold(onSuccess = transform, onFailure = { Result.failure(it) })

fun parse(s: String): Result<Int> = runCatching { s.toInt() }
fun reciprocal(n: Int): Result<Double> = runCatching { 1.0 / n }

val chained = parse("4").flatMap { reciprocal(it) }  // Success(0.25)
```

> Kotlin's guidance: use `Result` to capture *unexpected* exceptions for later processing. For domain failures that callers must handle, prefer nullable types or a `sealed` result type.

---

## Sealed Classes & Pattern Matching (Algebraic Data Types)

`sealed` hierarchies are Kotlin's algebraic data types. `when` over them is checked for exhaustiveness at compile time.

```kotlin
sealed interface Shape {
    data class Circle(val radius: Double) : Shape
    data class Rectangle(val width: Double, val height: Double) : Shape
    data class Triangle(val a: Double, val b: Double, val c: Double) : Shape
}

fun area(shape: Shape): Double = when (shape) {
    is Shape.Circle -> Math.PI * shape.radius * shape.radius
    is Shape.Rectangle -> shape.width * shape.height
    is Shape.Triangle -> {
        val s = (shape.a + shape.b + shape.c) / 2.0
        sqrt(s * (s - shape.a) * (s - shape.b) * (s - shape.c))
    }
    // no `else` needed — compiler knows all cases are covered
}

// Model optional/error states as ADTs
sealed interface ParseResult<out T> {
    data class Success<T>(val value: T) : ParseResult<T>
    data class Failure(val message: String) : ParseResult<Nothing>
}

// Destructuring data classes and pairs
val (width, height) = Shape.Rectangle(3.0, 4.0)
val (first, second) = 1 to "one"

// `when` as an expression over values
fun describe(n: Int): String = when {
    n < 0 -> "negative"
    n == 0 -> "zero"
    else -> "positive"
}
```

---

## Recursion & Tail Recursion

The `tailrec` modifier makes the compiler emit a loop for tail-recursive calls, avoiding stack growth.

```kotlin
// NOT tail-recursive — stack grows with n
fun factorial(n: Int): Int = if (n <= 1) 1 else n * factorial(n - 1)

// Tail-recursive with an accumulator — optimized to a loop
tailrec fun factorialTr(n: Int, acc: Int = 1): Int =
    if (n <= 1) acc else factorialTr(n - 1, acc * n)

tailrec fun fibonacci(n: Int, a: Int = 0, b: Int = 1): Int =
    if (n == 0) a else fibonacci(n - 1, b, a + b)

// tailrec only applies when the recursive call is the very last operation
// and the function is neither open nor a lambda.

// Mutual recursion is not tail-call optimized
fun isEven(n: Int): Boolean = if (n == 0) true else isOdd(n - 1)
fun isOdd(n: Int): Boolean = if (n == 0) false else isEven(n - 1)

// For large accumulation, prefer fold over recursion
val sum = (1..1_000_000).fold(0L) { acc, n -> acc + n }
```

---

## Lazy Evaluation with Sequences

Sequences compute on demand and can represent infinite data.

```kotlin
// Infinite naturals, filtered and mapped lazily
val evenSquares = generateSequence(1) { it + 1 }
    .filter { it % 2 == 0 }
    .map { it * it }
    .take(5)
    .toList()  // [4, 16, 36, 64, 100]

// sequence { } gives imperative-style generators
val collatz = sequence {
    var n = 27L
    while (n != 1L) {
        yield(n)
        n = if (n % 2L == 0L) n / 2 else 3 * n + 1
    }
    yield(1L)
}

// Convert to and from sequences
val fromList = listOf(1, 2, 3).asSequence()
val backToList = fromList.toList()

// Sequences are consumed once unless constrained
val once = generateSequence(1) { it + 1 }.constrainOnce()
```

---

## Immutable Persistent Collections (kotlinx.collections.immutable)

For genuinely immutable, structurally-shared collections, use the official `kotlinx-collections-immutable` library.

```kotlin
// build.gradle.kts: implementation("org.jetbrains.kotlinx:kotlinx-collections-immutable:0.3.8")
import kotlinx.collections.immutable.*

val list = persistentListOf(1, 2, 3)
val list2 = list.add(4)          // list is unchanged: [1, 2, 3]
val list3 = list2.remove(1)      // [2, 3, 4]

val map = persistentMapOf("a" to 1, "b" to 2)
val map2 = map.put("c", 3)       // map unchanged

val set = persistentSetOf(1, 2, 3)
val set2 = set.add(4)

// Convert from standard collections
val p = listOf(1, 2, 3).toPersistentList()
val pm = mapOf("a" to 1).toPersistentMap()
```

---

## Practical Examples

### Example 1: Data Processing Pipeline

```kotlin
data class Employee(
    val name: String,
    val department: String,
    val salary: Double,
    val active: Boolean,
)

val employees = listOf(
    Employee("Alice",   "Engineering", 80000.0, true),
    Employee("Bob",     "Sales",       60000.0, false),
    Employee("Charlie", "Engineering", 90000.0, true),
    Employee("Diana",   "Marketing",   55000.0, true),
    Employee("Eve",     "Engineering", 75000.0, true),
)

// 1. Names of active Engineering employees, sorted
val engNames = employees
    .filter { it.active && it.department == "Engineering" }
    .map { it.name }
    .sorted()
// [Alice, Charlie, Eve]

// 2. Average salary of active Engineering employees
val avgEngSalary = employees
    .filter { it.active && it.department == "Engineering" }
    .map { it.salary }
    .average()
// 81666.666...

// 3. Department headcount and total salary (active only)
val deptSummary = employees
    .filter { it.active }
    .groupBy { it.department }
    .mapValues { (_, emps) -> emps.size to emps.sumOf { it.salary } }

deptSummary.forEach { (dept, stats) ->
    val (count, total) = stats
    println("$dept: $count employees, \$$total total")
}
// Engineering: 3 employees, $245000.0 total
// Marketing: 1 employees, $55000.0 total
```

### Example 2: String Processing

```kotlin
val words = listOf("hello", "world", "functional", "kotlin")

// Uppercase words longer than 5 characters
val result = words
    .filter { it.length > 5 }
    .map { it.uppercase() }
// [FUNCTIONAL, KOTLIN]

// Total character count
val totalChars = words.sumOf { it.length }  // 26

// Join with separator
val sentence = words.joinToString(" ")  // "hello world functional kotlin"

// Word frequency, sorted by count descending
val text = "the quick brown fox jumps over the lazy dog the fox"
val frequency = text.split(" ")
    .groupingBy { it }
    .eachCount()
    .entries
    .sortedByDescending { it.value }

frequency.take(3).forEach { (word, n) -> println("$word: $n") }
```

### Example 3: Chaining Nullable Operations

```kotlin
fun safeDiv(a: Int, b: Int): Int? = if (b == 0) null else a / b
fun safeSqrt(n: Int): Double? = if (n < 0) null else sqrt(n.toDouble())

// Chain with ?.let, short-circuiting on the first null
fun process(a: Int, b: Int): Double? =
    safeDiv(a, b)?.let { safeSqrt(it) }

println(process(16, 1))   // 4.0
println(process(16, 0))   // null
println(process(-16, 1))  // null

// Collect the successful results of a mapping, dropping failures
val values = listOf(12, 0, 4, 5).mapNotNull { safeDiv(60, it) }
// [5, 15, 12]
```

### Example 4: Grouping and Partitioning

```kotlin
// Partition evens and odds in one pass
val (evens, odds) = (1..10).partition { it % 2 == 0 }
// evens = [2, 4, 6, 8, 10], odds = [1, 3, 5, 7, 9]

data class Order(val id: Int, val customer: String, val total: Double)

val orders = listOf(
    Order(1, "Alice", 120.0),
    Order(2, "Bob", 40.0),
    Order(3, "Alice", 75.0),
    Order(4, "Charlie", 200.0),
)

// Group orders by customer, then sum each customer's spend
val spendByCustomer = orders
    .groupBy { it.customer }
    .mapValues { (_, list) -> list.sumOf { it.total } }
// {Alice=195.0, Bob=40.0, Charlie=200.0}

// Index by id for O(1) lookup
val byId = orders.associateBy { it.id }
println(byId[3]?.customer)  // Alice
```

### Example 5: Sealed Types as a Pipeline

```kotlin
sealed interface Expr {
    data class Num(val value: Double) : Expr
    data class Add(val left: Expr, val right: Expr) : Expr
    data class Mul(val left: Expr, val right: Expr) : Expr
    data class Neg(val expr: Expr) : Expr
}

fun eval(expr: Expr): Double = when (expr) {
    is Expr.Num -> expr.value
    is Expr.Add -> eval(expr.left) + eval(expr.right)
    is Expr.Mul -> eval(expr.left) * eval(expr.right)
    is Expr.Neg -> -eval(expr.expr)
}

fun render(expr: Expr): String = when (expr) {
    is Expr.Num -> expr.value.toString()
    is Expr.Add -> "(${render(expr.left)} + ${render(expr.right)})"
    is Expr.Mul -> "(${render(expr.left)} * ${render(expr.right)})"
    is Expr.Neg -> "(-${render(expr.expr)})"
}

val expr = Expr.Mul(Expr.Add(Expr.Num(2.0), Expr.Num(3.0)), Expr.Neg(Expr.Num(4.0)))
println("${render(expr)} = ${eval(expr)}")  // ((2.0 + 3.0) * (-4.0)) = -20.0
```

### Example 6: Lazy Sequence Pipeline

```kotlin
// Take the first 10 numbers divisible by 3 whose square is > 100
val result = generateSequence(1) { it + 1 }
    .filter { it % 3 == 0 }
    .map { it * it }
    .filter { it > 100 }
    .take(10)
    .toList()
// [144, 225, 324, 441, 576, 729, 900, 1089, 1296, 1521]

// Fibonacci as an infinite sequence
val fibs = generateSequence(0 to 1) { (a, b) -> b to a + b }.map { it.first }
println(fibs.take(10).toList())  // [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
```

---

## Common Patterns

### Scope Functions

These five standard-library functions run a block with a receiver, differing in what `this`/`it` refer to and what they return.

| Function | Receiver | Returns |
|:---------|:---------|:--------|
| `let` | `it` | block result |
| `run` | `this` | block result |
| `with` | `this` | block result |
| `apply` | `this` | the receiver |
| `also` | `it` | the receiver |

```kotlin
val numbers = mutableListOf(1, 2, 3)

// also — side effect while keeping the value (like tap/tee)
val logged = numbers.also { println("before: $it") }

// let — transform a value or scope a nullable
val length = "hello".let { it.length }  // 5

// run — configure and compute
val config = run {
    val a = 1
    val b = 2
    a + b  // 3
}
```

### Avoid Mutation — Build New Values

```kotlin
// Bad: mutate an array in place
fun doubleBad(xs: IntArray) {
    for (i in xs.indices) xs[i] *= 2  // mutation
}

// Good: return a new collection
fun doubleGood(xs: List<Int>): List<Int> = xs.map { it * 2 }

// Bad: mutate a shared list
val shared = mutableListOf(1, 2, 3)
fun addBad(x: Int) { shared.add(x) }

// Good: return the updated list
fun addGood(xs: List<Int>, x: Int): List<Int> = xs + x
```

### Prefer Sequences for Large Pipelines

```kotlin
// Eager: allocates a list at every stage
fun eager(nums: List<Int>) = nums
    .filter { it % 2 == 0 }
    .map { it * it }

// Lazy: single pass, no intermediates
fun lazy(nums: List<Int>) = nums.asSequence()
    .filter { it % 2 == 0 }
    .map { it * it }
    .toList()
```

---

## Quick Reference

### Iterable / List operations (eager)

| Function | Description |
|:---------|:------------|
| `map { }` / `mapIndexed { }` | Transform each element (with index) |
| `mapNotNull { }` | Transform and drop null results |
| `filter { }` / `filterNot { }` | Keep / drop matching elements |
| `filterNotNull()` | Drop nulls |
| `fold(init) { }` | Reduce left-to-right with initial value |
| `reduce { }` / `reduceOrNull { }` | Reduce using first element as seed |
| `foldRight` / `reduceRight` | Reduce right-to-left |
| `flatMap { }` / `flatten()` | Map-then-flatten / collapse one level |
| `sorted()` / `sortedBy { }` / `sortedWith` | Non-mutating sort |
| `reversed()` / `asReversed()` | New list / lazy view |
| `take(n)` / `drop(n)` | Prefix / suffix slices |
| `takeWhile { }` / `dropWhile { }` | Condition-based slicing |
| `distinct()` / `distinctBy { }` | Remove duplicates |
| `any { }` / `all { }` / `none { }` | Predicate tests |
| `count()` / `count { }` | Element counts |
| `sum()` / `sumOf { }` | Numeric sums |
| `maxOrNull()` / `minOrNull()` | Extremes (null on empty) |
| `maxByOrNull { }` / `minByOrNull { }` | Extremes by key |
| `zip(other)` / `unzip()` / `zipWithNext()` | Pairwise combinations |
| `partition { }` | Split into `Pair<List, List>` |
| `groupBy { }` / `groupingBy { }.eachCount()` | Group / frequency count |
| `associate { }` / `associateBy { }` / `associateWith { }` | Build maps |
| `chunked(n)` / `windowed(n)` | Fixed-size chunks / sliding windows |
| `firstOrNull { }` / `find { }` / `indexOfFirst { }` | Search |
| `joinToString(sep)` | Join to a `String` |

### Sequence operations (lazy)

| Function | Description |
|:---------|:------------|
| `asSequence()` | Opt into lazy evaluation |
| `generateSequence(seed) { next }` | Build a (possibly infinite) sequence |
| `sequence { yield(x) }` | Imperative generator |
| `map` / `filter` / `take` / `drop` | Same names as collections, but lazy |
| `fold` / `reduce` | Terminal operations that consume the sequence |
| `toList()` / `toSet()` / `toMap()` | Materialize |

### Scope functions

| Function | Receiver | Returns |
|:---------|:---------|:--------|
| `let` | `it` | block result |
| `run` | `this` | block result |
| `with` | `this` | block result |
| `apply` | `this` | receiver |
| `also` | `it` | receiver |

### Result extensions

| Function | Description |
|:---------|:------------|
| `runCatching { }` | Capture a thrown exception as `Result` |
| `Result.success(v)` / `Result.failure(e)` | Construct outcomes |
| `map { }` / `mapCatching { }` | Transform Success (catching or not) |
| `recover { }` / `recoverCatching { }` | Transform Failure |
| `fold(onSuccess, onFailure)` | Exhaustively consume |
| `getOrNull()` / `getOrDefault(d)` / `getOrElse { }` | Extract with fallback |
| `getOrThrow()` | Re-throw the failure |
| `onSuccess { }` / `onFailure { }` | Side-effect peek |
| `flatMap { }` | **Not in stdlib** — define your own |

---

## Required Imports / Dependencies

```kotlin
// Most of the standard library needs no imports
import kotlin.math.sqrt          // math functions
import kotlin.math.PI

// Persistent immutable collections (optional dependency)
// build.gradle.kts: implementation("org.jetbrains.kotlinx:kotlinx-collections-immutable:0.3.8")
import kotlinx.collections.immutable.*
```

Language/version notes:

- `runningFold`, `scan`, `runningReduce`, `sumOf`, `chunked`, `windowed`, `zipWithNext` require **Kotlin 1.4+**.
- Returning `Result<T>` from a function requires **Kotlin 1.5+**.
- `max()`/`min()` throw on empty since **Kotlin 1.7**; prefer `maxOrNull()`/`minOrNull()`.

---

## Key Takeaways

1. **`val` by default**: Prefer immutable bindings and read-only collections; use `var`/`mutableListOf` only when you must.
2. **Read-only ≠ immutable**: `List` is a read-only interface. For true persistent immutability, use `kotlinx.collections.immutable`.
3. **Lists are eager, Sequences are lazy**: Call `.asSequence()` for large pipelines or infinite sources; `generateSequence` / `sequence { }` build lazy streams.
4. **Null-safety replaces Maybe**: `?.`, `?:`, `let`, `takeIf`, `mapNotNull`, and `filterNotNull` express optional chaining without a monad.
5. **`Result` replaces Try/Either**: `runCatching` + `map`/`fold`/`recover` handle recoverable failures — but there is no `flatMap` in the stdlib.
6. **Sealed types are ADTs**: Exhaustive `when` over a `sealed` hierarchy gives compile-time safety.
7. **`tailrec` gives TCO**: Annotate accumulator-style recursive functions to avoid stack overflow.
8. **No built-in curry/compose**: Add two-line `curried()`, `partial()`, `compose`, and `andThen` helpers, or lean on `let` piping.
9. **Scope functions read well**: `let`, `run`, `apply`, `also`, `with` cover transform, configure, and side-effect cases.
10. **Pure data + pure functions**: `data class` records and top-level functions with no shared mutable state are the foundation of FP-style Kotlin.
