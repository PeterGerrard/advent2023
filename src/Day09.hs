module Day09 (solve) where

import qualified Data.Bifunctor

solve :: String -> String
solve = show . toInts . foldl1 (both (+)) . map (getAdj . map toRational) . parse
  where
    toInts :: (Rational, Rational) -> (Integer, Integer)
    toInts = Data.Bifunctor.bimap round round

getWs :: [Rational] -> [(Integer, Rational)]
getWs xs = if power == 0 then [x] else x : getWs (zipWith (-) xs (map (\n -> mult * (n ^ power)) [0 ..]))
  where
    x@(power, mult) = getPoly xs

getAdj :: [Rational] -> (Rational, Rational)
getAdj xs = (f (-1), f n)
  where
    ws = getWs xs
    n = toRational (length xs)
    f a = sum $ map (\(power, mult) -> mult * (a ^ power)) ws

parse :: String -> [[Integer]]
parse = map (map read . splitOn (== ' ')) . lines

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn f xs = as : splitOn f (if null bs then bs else tail bs)
  where
    (as, bs) = break f xs

getPoly :: [Rational] -> (Integer, Rational)
getPoly = go 1 1
  where
    go :: Integer -> Rational -> [Rational] -> (Integer, Rational)
    go _ _ [] = error "not enough info"
    go _ _ [_] = error "not enough info"
    go d f (x : xs) = if all (== x) xs then (d - 1, x / f) else go (d + 1) (f * toRational d) (zipWith (-) xs (x : xs))

both :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
both f (x, y) (z, w) = (f x z, f y w)
