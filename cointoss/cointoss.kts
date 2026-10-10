fun toss(): Int = (0..1).random()

fun count(
    pattern: List<Int>,
    coins: List<Int> = listOf(),
    tosses: Int = 0,
): Int =
    when {
        coins.size < pattern.size ->
            count(pattern, coins + toss(), tosses + 1)
        coins == pattern -> tosses
        else ->
            count(pattern, coins.drop(1) + toss(), tosses + 1)
    }

fun test(pattern: List<Int>, cycles: Int): Double =
    (0..<cycles)
        .map { count(pattern) }
        .average()

val res101 = test(listOf(1,0,1), 2_000_000)
val res001 = test(listOf(0,0,1), 2_000_000)

println("  ${"%.3f".format(res101)}")
println("  ${"%.3f".format(res001)}")
