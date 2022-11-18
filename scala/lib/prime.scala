def isPrime(n: Int): Boolean = {
    @scala.annotation.tailrec
    def isPrimeTailrec(div: Int): Boolean = {
        if (div > Math.sqrt(n)) true
        else n % div != 0 && isPrimeTailrec(div + 1)
    }
    isPrimeTailrec(2)
}    
