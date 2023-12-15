module Main (main) where

import Day00 as Day0
import Day02 as Day2
import Day03 as Day3
import Day04 as Day4
import Day05 as Day5
import Day06 as Day6
import Day07 as Day7
import Day08 as Day8
import Day09 as Day9
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
        5 -> Day5.solve
        6 -> Day6.solve
        7 -> Day7.solve
        8 -> Day8.solve
        9 -> Day9.solve
        _ -> const "Day not implemented"
    )
      inputFile
