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
-- Partial and total functions
--

--
-- Define a safe head function
--

data MaybeHead
  = NoHead
  | JustHead Int

safeHeadInts :: [Int] -> MaybeHead
safeHeadInts [] = NoHead
safeHeadInts (x : _) = JustHead x

safeHead :: (Enum a) => [a] -> MaybeHead
safeHead [] = NoHead
safeHead (x : _) = JustHead (fromEnum x) -- Assuming we want to convert to Int for demonstration

--
-- Type classes
--

data Foo = F Int | G Char

instance Eq Foo where
  (F x) == (F y) = x == y
  (G x) == (G y) = x == y
  _ == _ = False
