let rec fib (n: int) = 
    if n < 2 then
        n
    else
        (fib (n - 1)) + (fib (n - 2))

printfn "%d" (fib 10)