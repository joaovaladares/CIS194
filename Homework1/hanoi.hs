{-# OPTIONS_GHC -Wall #-}

module Hanoi where

type Peg = String

type Move = (Peg, Peg)

-- Tower of Hanoi solver for 3 pegs
-- e.g.: hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]
-- For this, we will approach with recursion dividing into 3 sub tasks
-- 1. Move n-1 discs from source to temporary peg
-- 2. Move the remaining disc from source to destiny peg
-- 3. Move n-1 discs from temporary to destiny peg
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n src dst tmp
  | n <= 0 = []
  | n == 1 = [(src, dst)]
  | otherwise =
      hanoi (n - 1) src tmp dst
        ++ hanoi 1 src dst tmp
        ++ hanoi (n - 1) tmp dst src

-- The idea here is the same, but we have 4 pegs 
-- This leaves us with a new case when we have only 3 discs 
-- The rest runs the same as hanoi with 3 pegs
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n src dst tmp1 tmp2
  | n <= 0 = []
  | n == 1 = [(src, dst)]
  | n == 3 = [(src, tmp1), (src, tmp2), (src, dst), (tmp2, dst), (tmp1, dst)]
  | otherwise =
      hanoi4 (n - 1) src tmp1 dst tmp2
        ++ hanoi4 1 src dst tmp1 tmp2
        ++ hanoi4 (n - 1) tmp1 dst src tmp2
