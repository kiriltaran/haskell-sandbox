module Fibonacci where

fibonacci :: Integer -> Integer
fibonacci n | n>= 0 = calc n
            | otherwise = error "args must be >= 0"
  where
    calc 0 = 0
    calc 1 = 1
    calc n = calc (n - 1) + calc (n - 2) 