{-# OPTIONS_GHC -Wall #-}

module CreditCard where

import Data.Char (ord)

{- This exercise consists of implementing a validator for Credit Card Number
   The Algorithm follow this steps:

   Double the value of every second digit beginning from the right.
   That is, the last digit is unchanged; the second-to-last digit is
   doubled; the third-to-last digit is unchanged; and so on.
   For example, [1,3,8,6] becomes [2,3,16,6].

   Add the digits of the doubled values and the undoubled digits
   from the original number. For example, [2,3,16,6] becomes
   2+3+1+6+6 = 18.

   Calculate the remainder when the sum is divided by 10. For the
   above example, the remainder would be 8.
   If the result equals 0, then the number is valid
-}

-- We then to define the following:
-- toDigits         :: Integer   -> [Integer]
-- toDigitsRev      :: Integer   -> [Integer]
-- doubleEveryOther :: [Integer] -> [Integer]
-- sumDigits        :: [Integer] -> [Integer]
-- validate         :: [Integer] -> Bool

--Converts a char (digit) to it's integer form
charToInt :: Char -> Integer
charToInt ch = fromIntegral (ord ch) - fromIntegral (ord '0')

-- Separates the digits of an Integer into a list of Integer
-- 0 or negative Integers should return an empty list
toDigits :: Integer -> [Integer]
toDigits num
  | num <= 0  = []
  | otherwise = map charToInt (show num)

-- Naive implementation of reverseList (since numbers here are finite i.e. 
-- Credit Card numbers, we don't care much about performance)
reverseList :: [a] -> [a]
reverseList [] = []
reverseList xs = last xs : reverseList (init xs)

-- Same as toDigits above but returns the digits in reverse order 
-- e.g. 1234 == [4, 3, 2, 1]
toDigitsRev :: Integer -> [Integer]
toDigitsRev num = reverseList (toDigits num)

-- Doubles the value of every second number beginning from the right
-- e.g. [8, 7, 6, 5] == [16, 7, 12, 5]
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []  = []
doubleEveryOther [x] = [x]
doubleEveryOther xs  = reverseList (zipWith (*) (cycle[1,2]) (reverseList xs))

-- Calculates the sum of all digits
-- e.g. [16, 7, 12, 5] = 1 + 6 + 7 + 1 + 2 + 5 = 22
sumDigits :: [Integer] -> Integer
sumDigits xs = sum (map sumOfDigits xs)
  where
    sumOfDigits n 
      | n < 10    = n
      | otherwise = (n `mod` 10) + sumOfDigits (n `div` 10)

-- Validates the card number by calculating the remainder when the sum of digits 
-- is divided by 10. If the result equals 0, then the number is valid
-- e.g. validate 4012888888881881  = True
--      validate 4012888888881882  = False
--      validate -4012888888881881 = False
validate :: Integer -> Bool
validate n
  | n > 0     = sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0
  | otherwise = False

