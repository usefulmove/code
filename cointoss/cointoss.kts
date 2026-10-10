typealias Coins = List<Int>

fun getCoins(vararg coins: Int): Coins =
    List(coins.size) { 0 }
        .withIndex()
        .map { (i, _) -> coins[i] }

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

println("  ${"%.3f".format(res101)}")
println("  ${"%.3f".format(res001)}")
