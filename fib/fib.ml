let rec fib n =
  match n with
  | 0 -> 0
  | 1 -> 1
  | _ -> fib (n - 1) + fib (n - 2)

let rec fib2 n a b =
  match n with
  | 0 -> a
  | 1 -> b
  | _ -> fib2 (n - 1) b (a + b)

let () =
  print_int (fib2 38 0 1)
