{-# LANGUAGE TupleSections #-}

module Day10 (solve) where

import Data.Map (Map, elems, empty, insert, keys, member, unions, (!), (!?))
import Data.Maybe (catMaybes, isNothing)

solve :: String -> String
solve = show . maximum . elems . (\(s, m) -> getDistances m s) . parse

data Direction = North | East | South | West
  deriving (Eq, Show)

data Pipe = Ground | Pipe [Direction] | Start
  deriving (Eq, Show)

parseElement :: Char -> Pipe
parseElement '|' = Pipe [North, South]
parseElement '-' = Pipe [East, West]
parseElement 'L' = Pipe [North, East]
parseElement 'J' = Pipe [North, West]
parseElement '7' = Pipe [South, West]
parseElement 'F' = Pipe [South, East]
parseElement '.' = Ground
parseElement 'S' = Start
parseElement _ = error "Unsupported"

printElement :: Pipe -> Char
printElement (Pipe [North, South]) = '|'
printElement (Pipe [East, West]) = '-'
printElement (Pipe [North, East]) = 'L'
printElement (Pipe [North, West]) = 'J'
printElement (Pipe [South, West]) = '7'
printElement (Pipe [South, East]) = 'F'
printElement Ground = '.'
printElement Start = 'S'
printElement _ = error "Unsupported"

parseRow :: Int -> String -> (Maybe (Int, Int), Map (Int, Int) Pipe)
parseRow y = foldl (\(spos, m) (x, p) -> (if p == Start then Just (x, y) else spos, insert (x, y) p m)) (Nothing, empty) . zip [0 ..] . map parseElement

parse :: String -> ((Int, Int), Map (Int, Int) Pipe)
parse = (\(ss, ms) -> (head (catMaybes ss), unions ms)) . unzip . zipWith parseRow [0 ..] . lines

printMap :: Map (Int, Int) Pipe -> String
printMap m = unlines [[printElement (m ! (x, y)) | x <- [0 .. maxx]] | y <- [0 .. maxy]]
  where
    ps = keys m
    maxx = maximum $ map fst ps
    maxy = maximum $ map snd ps

printDistances :: Map (Int, Int) Int -> String
printDistances m = unlines [[printDistance (x, y) | x <- [0 .. maxx]] | y <- [0 .. maxy]]
  where
    ps = keys m
    maxx = maximum $ map fst ps
    maxy = maximum $ map snd ps
    printDistance p = case m !? p of
      Nothing -> '.'
      Just x -> head $ show x

move :: (Int, Int) -> Direction -> (Int, Int)
move (x, y) North = (x, y - 1)
move (x, y) East = (x + 1, y)
move (x, y) South = (x, y + 1)
move (x, y) West = (x - 1, y)

getDistances :: Map (Int, Int) Pipe -> (Int, Int) -> Map (Int, Int) Int
getDistances m s = go empty [(s, 0)]
  where
    go :: Map (Int, Int) Int -> [((Int, Int), Int)] -> Map (Int, Int) Int
    go acc [] = acc
    go acc ((p, n) : ps) = if isNothing (m !? p) || m ! p == Ground || member p acc then go acc ps else go (insert p n acc) (ps ++ map (,n + 1) (getAdj p))
    getAdj :: (Int, Int) -> [(Int, Int)]
    getAdj p = case m !? p of
      Nothing -> []
      Just Ground -> []
      Just (Pipe ds) -> map (move p) ds
      Just Start -> map (move p) [North, East, South, West]
