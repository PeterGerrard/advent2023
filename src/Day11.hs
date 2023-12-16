module Day11 (solve) where

import Data.Ix (Ix (inRange))
import Data.List (nub)

solve :: String -> String
solve = show . (\g -> (part 1 g, part 999999 g)) . parseGalaxy

data Galaxy = Galaxy [(Int, Int)] [Int] [Int]
  deriving (Show)

parseGalaxy :: String -> Galaxy
parseGalaxy = toGalaxy . map fst . filter ((== '#') . snd) . concat . zipWith (\y -> zipWith (\x c -> ((x, y), c)) [0 ..]) [0 ..] . lines

toGalaxy :: [(Int, Int)] -> Galaxy
toGalaxy gs = Galaxy gs [x | x <- [0 .. maxx], x `notElem` xs] [y | y <- [0 .. maxy], y `notElem` ys]
  where
    xs = nub $ map fst gs
    ys = nub $ map snd gs
    maxx = maximum xs
    maxy = maximum ys

distance :: Galaxy -> Int -> (Int, Int) -> (Int, Int) -> Int
distance (Galaxy _ emptyxs emptyys) age (a, b) (c, d) = (x2 - x1) + (y2 - y1) + age * (length (filter (inRange (x1, x2)) emptyxs) + length (filter (inRange (y1, y2)) emptyys))
  where
    x1 = min a c
    x2 = max a c
    y1 = min b d
    y2 = max b d

part :: Int -> Galaxy -> Int
part age g@(Galaxy gs _ _) = sum [distance g age p1 p2 | p1 <- gs, p2 <- gs, p1 < p2]