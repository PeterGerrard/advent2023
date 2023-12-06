module Day06 (solve) where

import Data.Char (isDigit)

solve :: String -> String
solve s = show (part1, "")
  where
    part1 = product . map options $ parse1 s

parse1 :: String -> [(Integer, Integer)]
parse1 s = zip (parseLine ts) (parseLine ds)
  where
    [ts, ds] = lines s
    parseLine = parseLine' [] . dropWhile (not . isDigit)

parse2 :: String -> (Integer, Integer)
parse2 s = (parseLine ts, parseLine ds)
  where
    [ts, ds] = lines s
    parseLine = read . filter isDigit . dropWhile (not . isDigit)

parseLine' :: [Integer] -> String -> [Integer]
parseLine' acc [] = reverse acc
parseLine' acc (' ' : xs) = parseLine' acc xs
parseLine' acc xs = parseLine' (read n : acc) ys
  where
    (n, ys) = span isDigit xs

distanceTravelled :: Integer -> Integer -> Integer
distanceTravelled t a = (t - a) * a

options :: (Integer, Integer) -> Int
options (t, d) = length . filter (> d) $ map (distanceTravelled t) [0 .. t]