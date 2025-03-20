-- Nim example from chapter 10 of Programming in Haskell,
-- Graham Hutton, Cambridge University Press, 2016.

-- Start ghci with `stack ghci`, then `:load Nim.hs` and evaluate `nim`
-- to play Nim.

module Main where

import Data.Char

-- Game utilities

next :: Int -> Int
next 1 = 2
next 2 = 1

type Board = [Int]

initial :: Board
initial = [5, 4, 3, 2, 1]

finished :: Board -> Bool
finished = all (== 0)

valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row - 1) >= num

move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r, n) <- zip [1 ..] board]
  where
    update r n = if r == row then n - num else n

-- IO utilities

putRow :: Int -> Int -> IO ()
putRow row num = do
  putStr (show row ++ ": ")
  putStrLn (unwords (replicate num "*"))

putBoard :: Board -> IO ()
putBoard board = mapM_ (\(r, n) -> putRow r n) (zip [1 ..] board)

getDigit :: String -> IO Int
getDigit prompt = do
  putStr prompt
  x <- getChar
  newline
  if isDigit x
    then
      return (digitToInt x)
    else do
      putStrLn "ERROR: Invalid digit"
      getDigit prompt

newline :: IO ()
newline = putChar '\n'

-- Game of nim

play :: Board -> Int -> IO ()
play board player =
  do
    newline
    putBoard board
    if finished board
      then do
        newline
        putStr "Player "
        putStr (show (next player))
        putStrLn " wins!!"
      else do
        newline
        putStr "Player "
        putStrLn (show player)
        row <- getDigit "Enter a row number: "
        num <- getDigit "Stars to remove : "
        if valid board row num
          then
            play (move board row num) (next player)
          else do
            newline
            putStrLn "ERROR: Invalid move"
            play board player

nim :: IO ()
nim = play initial 1

main :: IO ()
main = nim
