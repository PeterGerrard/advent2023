{-# LANGUAGE TupleSections #-}

module Day03
  ( solve,
  )
where

import Control.Arrow (second)
import Data.Bifunctor (bimap)
import Data.Char (isDigit)
import Data.Either (lefts, rights)
import Data.Set (fromList, member)

type Pos = (Int, Int)

solve :: String -> String
solve = show . (\g -> (part1 g, part2 g)) . parse

parseLine :: String -> [Either (Char, Int) (Integer, (Int, Int))]
parseLine = go [] 0
  where
    go acc _ [] = acc
    go acc i (x : xs)
      | x == '.' = go acc (i + 1) xs
      | isDigit x = go (Right (read (x : ys), (i, i + length ys)) : acc) (i + length ys + 1) zs
      | otherwise = go (Left (x, i) : acc) (i + 1) xs
      where
        (ys, zs) = span isDigit xs

parse :: String -> ([(Char, Pos)], [(Integer, (Pos, Pos))])
parse = foldl (\(ss, ns) (j, rs) -> (ss ++ map (second (,j)) (lefts rs), ns ++ map (second (both (,j))) (rights rs))) ([], []) . zip [0 ..] . map parseLine . lines

both :: (a -> b) -> (a, a) -> (b, b)
both f (a, b) = (f a, f b)

part1 :: ([(Char, Pos)], [(Integer, (Pos, Pos))]) -> Integer
part1 (ss, ns) = sum . map fst $ filter (any (`member` hs) . adj . snd) ns
  where
    hs = fromList $ map snd ss

adj :: (Pos, Pos) -> [Pos]
adj ((x1, y1), (x2, y2)) = [(x, y1 - 1) | x <- [x1 - 1 .. x2 + 1]] ++ [(x1 - 1, y) | y <- [y1 .. y2]] ++ [(x2 + 1, y) | y <- [y1 .. y2]] ++ [(x, y2 + 1) | x <- [x1 - 1 .. x2 + 1]]

gears :: ([(Char, Pos)], [(Integer, (Pos, Pos))]) -> [(Pos, (Integer, Integer))]
gears (ss, xs) = map (bimap snd (toPair . map fst)) . filter ((== 2) . length . snd) . map (\x -> (x, filter (isAdj (snd x)) xs)) $ filter ((== '*') . fst) ss
  where
    isAdj :: Pos -> (Integer, (Pos, Pos)) -> Bool
    isAdj (x, y) (_, ((x1, y1), (x2, y2))) = x1 - 1 <= x && x <= x2 + 1 && y1 - 1 <= y && y <= y2 + 1

toPair :: [a] -> (a, a)
toPair [a, b] = (a, b)
toPair _ = error "Wrong length"

part2 :: ([(Char, Pos)], [(Integer, (Pos, Pos))]) -> Integer
part2 = sum . map (uncurry (*) . snd) . gears