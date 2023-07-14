module Main where

import Prelude

import Effect.Console (logShow)

fib :: Int -> Int
fib n | n < 2 = n
      | otherwise = fib (n - 1) + fib (n - 2)

main = do
  logShow $ fib 38