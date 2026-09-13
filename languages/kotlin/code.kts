/* 
 * Kotlin code examples
 */

import kotlin.math.sqrt

fun double(n: Int): Int = n + n

val square: (Int) -> Int = {n -> n * n}

fun cube(n: Int): Int {
   return n * n * n
}

val golden: Double = (sqrt(5.0) - 1.0) / 2.0

fun hello(name: String = "mundo"): String {
    return "hola $name."
}

println(double(1))
println("${square(10)}")
println(cube(8))
println("golden ratio: $golden")
println(hello())
println(hello("cora"))

// lambda
println( {n: Int -> n * n * n}(2) )

// higher-order functions
fun fold(f: (Int, Int) -> Int, ns: Array<Int>, seed: Int = 0): Int {
    var acc = seed // accumulator
    for (n in ns) {
	acc = f(n, acc)
    }
    return acc
}

fun add(a: Int, b: Int): Int = a + b
val arr: Array<Int> = arrayOf(1, 2, 3, 4, 5, 6, 7, 8)
println(fold(::add, arr))

fun f(n: Int, acc: Int): Int = acc + n * n
println(fold(::f, arr))
