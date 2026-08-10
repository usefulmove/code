import Html exposing (text)


main =
  let
    res = fib2 38
    out = String.fromInt res
  in
    text out
  

fib : Int -> Int
fib n =
  case n of
    0 -> n
    1 -> n
    _ -> fib (n-1) + fib (n-2)


fib2 : Int -> Int
fib2 n =
  let
    fibonacci2 : Int -> Int -> Int -> Int
    fibonacci2 x a b =
      case x of
        0 -> a
        1 -> b
        _ -> fibonacci2 (x-1) b (a+b)
  in
     fibonacci2 n 0 1
