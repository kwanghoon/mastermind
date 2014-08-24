--------------------------------------------------------------------------------- 
-- Module      : The Mastermind Game
-- Copyright   : (c) Kwanghoon Choi, 2014
-- License     : BSD3
--
-- Maintainer  : lazyswamp@gmail.com
-- Stability   : Expermental
-- Portability : Portable
--
-- The mastermind game to guess a number in your mind.
--
--------------------------------------------------------------------------------- 

module Main where

import Prelude hiding (getLine)
import Data.Char
import System.Exit

--------------------------------------------------------------------------------- 
-- The MasterMind Game
--
-- Excercise 3.12, 
--   in Ch.3, Introduction to Functional Programming, Prentice-Hall 
--   (by R. Bird and P. Wadler). 
--  
--   In a version of the game MasterMind, one player thinks of an n-digit number,
--   while the other player repeatedly tries to guess it. After each guess, 
--   player 1 scores the guess by stating the number of bulls and cows. A bull 
--   is a correct digit in the correct place. A cow is a digit appearing in the 
--   secret number, but not in the correct place. No digit is scored more than 
--   once. For example, if the secret code is 2113, then:
--
--                                       1234   scores 03
--                                       1111   scores 20
--                                       1212   scores 12
--
--    Using (--), construct a function score which takes a code and a guess and
--    returns the number of bulls and cows. 
--------------------------------------------------------------------------------- 


-- Takes a code (as a list of integers) and a guess (also as a list of integers)
-- and returns the number of bulls and cows. 
score :: [Int] -> [Int] -> (Int, Int)
score code guess = (bulls, cows)
  where
    (bulls,theRestOfcodeGuess) = 
      countBulls (zip code guess) [] 0 
    
    cows = countCows (unzip theRestOfcodeGuess) 0
    
    countBulls [] theRest bulls = (bulls, theRest)
    countBulls ((c,g):codeGuess) theRest bulls = 
      if c==g then countBulls codeGuess theRest (bulls+1) 
      else countBulls codeGuess (theRest ++ [(c,g)]) bulls 
           
    countCows (theRestOftheCode, []) cows = cows
    countCows (theRestOftheCode, g:theRestOftheGuess) cows =
      let (n,theRestOfTheCode') = findToElim g theRestOftheCode [] 
      in  countCows (theRestOfTheCode', theRestOftheGuess) (cows+n)
      
    findToElim x []     prev = (0, prev)
    findToElim x (y:ys) prev = 
      if x==y then (1, prev++ys) 
      else findToElim x ys (prev ++[y])
           
-- Tests the score function
testScore :: Bool
testScore =           
  score [2,1,1,3] [1,2,3,4] == (0,3) &&
  score [2,1,1,3] [1,1,1,1] == (2,0) &&
  score [2,1,1,3] [1,2,1,2] == (1,2)
  
  
-- The Main interface
main :: IO ()
main = do
  putStrLn "How many digits? "
  words <- getLine "" []
  case words of
    (n:_) -> if all isDigit n then loop [] (nDigitNumbers (read n :: Int))
             else do { putStrLn "Not a number"; main }
    _     -> do { putStrLn "Oops";  main }

-- Generates all n-digit numbers
nDigitNumbers :: Int -> [[Int]]
nDigitNumbers 0 = [[]]
nDigitNumbers n = [ n:ns | ns <- nDigitNumbers (n-1), n <- [0..9] ]

-- A message for user's choice
msg :: String
msg = "Enter your choice: y with nothing, or n with bulls and cows (say, n 1 3)"

-- Repeats asking the mastermind's guesses to keep consistency with 
-- the list of user's scores until now. 
loop :: [([Int],(Int,Int))] -> [[Int]] -> IO ()
loop ps xs = do
  putStrLn msg
  loop' ps xs

loop' :: [([Int],(Int,Int))] -> [[Int]] -> IO ()
loop' ps xs = do
  (x,xs') <- next ps xs
  putStr (show x)
  putStr " ? "
  ans <- getAnswer x
  case ans of
   Nothing -> return ()
   Just bc -> loop' ((x,bc):ps) xs'

-- Gets an answer 'y' or 'n # #' from user to return Nothing if the mastermind's
-- guess is correct or to return Just (#,#) otherwise. (#,#) is a pair of
-- bulls and cows.
getAnswer :: [Int] -> IO (Maybe (Int, Int))
getAnswer x = do
  words <- getLine "" []
  case words of
    ("y":_) -> return Nothing
    ("n":bulls:cows:_) -> 
        if all isDigit bulls && all isDigit cows 
        then return (Just (read bulls, read cows))
        else do { putStrLn msg; putStr (show x); putStr " ? "; getAnswer x }
    _ -> do { putStrLn msg; putStr (show x); putStr " ? "; getAnswer x }

-- Gets a list of words from user
getLine :: String -> [String] -> IO [String]
getLine word line = do
  ch <- getChar  
  if ch == '\n' && word == "" then return (reverse line) 
  else if ch == '\n' && word /= "" then return (reverse (word:line))
  else if ch == ' ' then getLine "" (word:line)
  else getLine (word ++[ch]) line

-- Finds the next guess by verifying the consistency with all the answers
-- from user. If no more guess is available, he or she must answer 
-- inconsistently. Otherwise, the mastermind will always find the next guess.
next :: [([Int], (Int,Int))] -> [[Int]] -> IO ([Int], [[Int]])
next ps []     =  
  do putStrLn "Something wrong: inconsistent answers! "
     putStrLn (show ps)
     exitWith (ExitFailure (-1))
next ps (x:xs) = 
  if all (\(guess,bc) -> score x guess==bc) ps 
  then return (x, xs) 
  else next ps xs
