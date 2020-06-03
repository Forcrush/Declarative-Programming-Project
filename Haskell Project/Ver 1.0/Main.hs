module Main where

import System.Environment
import System.Exit
import Proj1


-- | Main program.  Gets the target from the command line (as three
--   separate command line arguments, each a note letter (upper case)
--   followed by an octave number.  Runs the user's initialGuess and
--   nextGuess functions repeatedly until they guess correctly.
--   Counts guesstime, and prints a bit of running commentary as it goes.
main :: IO ()
main = do
  args <- getArgs
  let target = args
  let test = head args
  if length args == 3 then do
    let (guess,state) = initialGuess
    loop target guess state 1
    else do
    putStrLn "Usage: Main Ship1 Ship2 Ship3"
    putStrLn "Where Ship1 Ship2 Ship3 are 3 different location between A1 and H4"
    exitFailure


loop :: [String] -> [String] -> Proj1.GameState -> Int -> IO ()
loop target guess state guesstime = do
  if (Nothing `elem` (map toLocation target)) == False then do
    putStrLn $ "Your guess " ++ show guesstime ++ ":  " ++ show guess
    let answer = feedback target guess
    putStrLn $ "Feedback:  " ++ show answer
    if answer == (3,0,0) then do
      putStrLn $ "You got it at " ++ show guesstime ++ " guesses!"
      else do
      let (guess',state') = nextGuess (guess,state) answer
      loop target guess' state' (guesstime+1)
    else do
    putStrLn "Invalid Ship"
    exitFailure