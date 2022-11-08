-- LoopOver
-- https://github.com/NordEther/LoopOver

module Main (main) where

import Lib
import Data.List
import Data.Char
import System.Random
import Data.List.Split
import Control.Monad

main :: IO ()

fiveby = do
    g <- newStdGen
    let f = chunksOf 5 . take 25 . nub $ (randomRs ('a','y') g :: [Char])
    let h = chunksOf 5 $ take 25 ['a'..]
    if (f == h)
      then main
    else do
      putStrLn (" ")
      let o = grid5 f
      putStrLn (formatGrid o)
      --todo f
      check

main = do
    g <- newStdGen
    let f = chunksOf 3 . take 9 . nub $ (randomRs ('a','i') g :: [Char])
    let h = chunksOf 3 $ take 9 ['a'..]
    if (f == h)
      then main
    else do
      putStrLn (" ")
      let o = grid f
      putStrLn (formatGrid o)
      todo f

todo f = do
    if f == (chunksOf 3 $ take 9 ['a'..])
      then do
        check
    else do
      putStrLn "Choose row or column to advance (1-6)"
      input2 <- getLine
      let x = (read input2 :: Int) + (-1)
      putStrLn (" ")
      if  6 == x
        then do
        check
      else if elem x [0..6] == False
        then do
          putStrLn ("Please enter a row or column number")
          todo f

      else if 3 <= x
      then  do
        let y = x + (-3)
        let line1 = rotate ((transpose  f) !! y) 1
        let cubeb = transpose $ insertAt line1 y $ remove y $ transpose (f)
        let cubeo = grid cubeb
        putStrLn (" ")
        putStrLn (formatGrid cubeo)
        todo cubeb
        
      else do
        let line1 = rotate (f !! x) (1)
        let cubec = remove x f
        let cubed = insertAt line1 x cubec
        let cubedo = grid cubed
        putStrLn (" ")
        putStrLn (formatGrid cubedo)
        todo cubed

check = do
    putStrLn ("Puzzle Solved! You Rock!")
    putStrLn ("Do you wish to play again (1) or exit (2)?")
    input2 <- getLine
    let x = (read input2 :: Int)
    if x == 1
        then 
                main
    else
        putStrLn ("Goodbye!")
