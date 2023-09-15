object Currying:
    @main
    def main(args: String*) =
        val add = (x: Int) => (y: Int) => x + y
        val add2 = add(2)

        println(add2(3))
        println(add(2)(3))