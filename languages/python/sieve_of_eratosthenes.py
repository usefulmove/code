def count_primes_less_than(n: int) -> int:
    # returns the number of prime numbers less than n according to the the
    # Sieve of Eratosthenes method

    if n < 2:
        return 0

    sieve = [1] * n
    sieve[0] = sieve[1] = 0

    for current in range(2, int(n**0.5) + 1):
        if sieve[current] == 1:  # if current is prime
            for multiple in range(current * current, n, current):
                sieve[multiple] = 0  # mark multiples of current as not prime

    return sum(sieve)
