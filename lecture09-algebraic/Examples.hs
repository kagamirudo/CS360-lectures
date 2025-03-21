-- Haskell is space sensitive
{-# OPTIONS_GHC -Wall -Wno-type-defaults -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fwarn-tabs #-}

-- Turn on warnings

--
-- You can start GHCi at the UNIX prompt with the command `stack ghci`.
--
-- You can load this file into GHCi by typing the command `:load Examples.hs` at
-- the GHCi prompt.
--

module Examples where

import Test.QuickCheck (quickCheck)

-- Inductive definitions of lists

data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Ord, Show)

mymap :: (a -> b) -> List a -> List b
mymap _ Nil = Nil
mymap f (Cons x xs) = Cons (f x) (mymap f xs)

prop_map_inc :: Bool
prop_map_inc =
  mymap (+ 1) (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil))))
    == Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil)))

myappend :: List a -> List a -> List a
myappend Nil ys = ys
myappend (Cons x xs) ys = Cons x (myappend xs ys)

prop_append :: Bool
prop_append = myappend xs ys == Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))
  where
    xs = Cons 1 (Cons 2 Nil)
    ys = Cons 3 (Cons 4 Nil)

myreverse :: List a -> List a
myreverse Nil = Nil
myreverse (Cons x xs) = myappend (myreverse xs) (Cons x Nil)

prop_reverse :: Bool
prop_reverse = myreverse (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))) == Cons 4 (Cons 3 (Cons 2 (Cons 1 Nil)))

-- | This main function runs all tests
--
-- `main` is executed when you compile and run this program. We'll cover `do`
-- notation later in the course. You can add quickCheck tests here.
--
-- To run the tests, type `main` at the GHCi prompt.
main :: IO ()
main = do
  quickCheck prop_map_inc
  quickCheck prop_append
  quickCheck prop_reverse
