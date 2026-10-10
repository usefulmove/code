typealias Coins = List<Int>

fun getCoins(vararg coins: Int): Coins = coins.toList()

fun toss(times: Int = 1): Coins =
    List(times) { (0..1).random() }

tailrec fun countToPattern(
    pattern: Coins,
    coins: Coins = getCoins(),
    tosses: Int = 0,
): Int =
    when {
        coins.size < pattern.size ->
            countToPattern(
                pattern,
                coins + toss(pattern.size - coins.size),
                tosses + pattern.size - coins.size
            )
        coins == pattern -> tosses
        else ->
            countToPattern(pattern, coins.drop(1) + toss(), tosses + 1)
    }

fun runSimulation(pattern: Coins, cycles: Int): Double =
    (0..<cycles)
        .map { countToPattern(pattern) }
        .average()

val res101 = runSimulation(getCoins(1,0,1), 2_000_000)
val res001 = runSimulation(getCoins(0,0,1), 2_000_000)
val res10101 = runSimulation(getCoins(1,0,1,0,1), 1_000_000)
val res00000 = runSimulation(getCoins(0,0,0,0,0), 1_000_000)

println("  101:   ${"%.3f".format(res101)}")
println("  001:   ${"%.3f".format(res001)}")
println("  10101: ${"%.3f".format(res10101)}")
println("  00000: ${"%.3f".format(res00000)}")
