module Main where

import System.Environment
import System.Random

--generates a random data file for 20141208.hs to process

main = do
  [num] <- getArgs
  output <- genOutput $ read num
  putStr output

genOutput :: Int -> IO String
genOutput num = do
  g <- getStdGen
  return $ map charFromI $ take num $ randomRs (0,3) g
       
charFromI :: Int -> Char
charFromI i | i == 0 = 'A'
            | i == 1 = 'C'
            | i == 2 = 'G'
            | i == 3 = 'T'
            | otherwise = undefined