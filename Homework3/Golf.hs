module Golf where

import Data.List (transpose)

-- First list is the input itself, second list contains only every second
-- element of the input list, etc.
-- > skips "ABCD"        == ["ABCD", "BD", "C", "D"]
-- > skips "hello!"      == ["hello!", "el!", "l", "o", "!"]
-- > skips [1]           == [[1]]
-- > skips [True, False] == [[True,False], [False]]
-- > skips []            == []
skips :: [a] -> [[a]]
skips xs = [nth i xs | i <- [1 .. length xs]]

-- Given a number and a list, returns only the n-th values of the list
-- The idea here is the following: we first zip our list xs to create a new
-- list of pairs containing the original values and it's correspondent index
-- then we filter out all pairs that its' index is a multiple of n and keep the
-- value. e.g. nth 2 [1, 2, 3, 4] = [2, 4]
nth :: Int -> [a] -> [a]
nth n xs = [x | (i, x) <- zip [1 ..] xs, i `mod` n == 0]

-- Returns the local maximum of a list. Local maximum is an element that is
-- greater than both the element before and immediately after it.
-- > localMaxima [2,9,5,6,1] == [9,6]
-- > localMaxima [2,3,4,1,5] == [4]
-- > localMaxima [1,2,3,4,5] == []
localMaxima :: [Integer] -> [Integer]
localMaxima (x : y : z : xs)
  | y > x && y > z = y : localMaxima (y : z : xs)
  | otherwise = localMaxima (y : z : xs)
localMaxima _ = []

-- Generates a histogram given a list 
-- > histogram [1,1,1,5] ==
--  *
--  *
--  *   *
-- ==========
-- 0123456789
-- This function is divided in 5 parts:
-- 1. Occurrence counting for each number (occurrences)
-- 2. Finding maximum count in occurrences (maximumCounts)
-- 3. Generating the lines of our histogram (histogramLines)
-- 4. Rotating the histogram since it's horizontally aligned (transposing)
-- 5. Constructing our histogram (histogramBody)
histogram :: [Integer] -> String
histogram xs =
  let occurrences = countOccurrences xs
      maximumCounts = maximum occurrences
      histogramLines = map (generateLine maximumCounts) occurrences
      rotatedHistogram = transpose histogramLines
      histogramBody = unlines $ reverse rotatedHistogram
   in histogramBody ++ "==========\n" ++ "0123456789\n"

-- Counts how many times each value occurs on the list and returns it
-- > countOccurrences [1,4,5,4,6,6,3,4,2,4,9] == [0,1,1,1,4,1,2,0,0,1]
countOccurrences :: [Integer] -> [Int]
countOccurrences xs = map (\n -> length $ filter (== n) xs) [0 .. 9]

-- Generates our line for a given number
generateLine :: Int -> Int -> String
generateLine maximumCounts n = replicate n '*' ++ replicate (maximumCounts - n) ' '
