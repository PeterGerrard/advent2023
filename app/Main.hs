module Main (main) where

import Day00 as Day0
import Day02 as Day2
import Day03 as Day3
import Day04 as Day4
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let numString = head args
  let num = read numString :: Integer
  let inputFileName = "Day" ++ replicate (length numString) '0' ++ numString ++ ".txt"
  inputFile <- readFile $ "inputs/" ++ inputFileName
  putStr $
    ( case num of
        0 -> Day0.someFunc
        2 -> Day2.solve
        3 -> Day3.solve
        4 -> Day4.solve
        _ -> const "Day not implemented"
    )
      inputFile
