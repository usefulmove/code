import Html exposing (text)


main =
  let
    res = fib2 38
    out = String.fromInt res
  in
    text out
  

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
