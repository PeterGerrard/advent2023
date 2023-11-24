module Main (main) where

import Day00 as Day0
import Day01 as Day1
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    interact $ case read (head args) :: Integer of
                    0 -> Day0.someFunc
                    1 -> Day1.someFunc
                    _ -> const "Day not implemented"
