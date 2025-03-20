-- Haskell is space sensitive
{-# OPTIONS_GHC -Wall -Wno-type-defaults -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fwarn-tabs #-}

-- Turn on warnings

module Examples where

import Test.QuickCheck (quickCheck)
import Prelude hiding (map)

isThisWorking :: String
isThisWorking = "Yes"

--
-- List examples
--

-- Return the nth element of a list, counting from 0.
nth :: [a] -> Int -> a
nth [] _ = error "Index out of bounds"
nth (x : _) 0 = x
nth (_ : xs) n
  | n < 0 = error "Index out of bounds"
  | otherwise = nth xs (n - 1)

prop_nth_1_through_5 :: Bool
prop_nth_1_through_5 = nth [1 .. 5] 2 == 3

-- Map a function over the elements of a list
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x : xs) = f x : map f xs

prop_map_inc :: Bool
prop_map_inc = map (+ 1) [1, 2, 3, 4, 5] == [2, 3, 4, 5, 6]

-- Append two lists
append :: [a] -> [a] -> [a]
append xs ys = foldr (:) ys xs

prop_append_length :: [Int] -> [Int] -> Bool
prop_append_length xs ys = length (append xs ys) == length xs + length ys

-- Double every element
doubleEveryElement :: [Int] -> [Int]
doubleEveryElement = map (* 2)

prop_double_length :: [Int] -> Bool
prop_double_length xs = length (doubleEveryElement xs) == length xs

--
-- Partial application
--

-- Increment an integer
inc :: Int -> Int
inc x = x + 1

prop_inc :: Int -> Bool
prop_inc x = inc x == x + 1

-- A function that squares all elements in a list.
squareAll :: [Int] -> [Int]
squareAll = map (^ 2)

prop_squareAll :: Bool
prop_squareAll = squareAll [1 .. 5] == [x * x | x <- [1 .. 5]]

-- To convert x from Celsius to Fahrenheit, compute x * 9/5 + 32
celsiusToFahrenheit :: Float -> Float
celsiusToFahrenheit x = x * 9 / 5 + 32

prop_celsius0 :: Bool
prop_celsius0 = celsiusToFahrenheit 0 == 32

prop_celsius100 :: Bool
prop_celsius100 = celsiusToFahrenheit 100 == 212

-- Return every third element of a list
everyThird :: [a] -> [a]
everyThird [] = []
everyThird (_ : _ : x : xs) = x : everyThird xs
everyThird _ = []

prop_everyThird :: Bool
prop_everyThird = everyThird [1, 2, 3, 4, 5, 6, 7] == [3, 6]

-- | This main function runs all HSpec tests
main :: IO ()
main = do
  quickCheck prop_nth_1_through_5
  quickCheck prop_map_inc
  quickCheck prop_append_length
  quickCheck prop_double_length
  quickCheck prop_inc
  quickCheck prop_squareAll
  quickCheck prop_celsius0
  quickCheck prop_celsius100
  quickCheck prop_everyThird
