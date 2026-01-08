let rec fib n =
  match n with
  | 0 -> 0
  | 1 -> 1
  | _ -> fib (n - 1) + fib (n - 2)

let rec fib2 n =
  let rec fib2_helper n a b =
    match n with
    | 0 -> a
    | 1 -> b
    | _ -> fib2_helper (n - 1) b (a + b)
  in fib2_helper n 0 1

let () =
  print_int (fib2 38)
