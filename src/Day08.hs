module Day08 (solve) where

import Data.List (nub)
import qualified Data.Map as Map

solve :: String -> String
solve s = show (part1 inp, "")
  where
    inp = parse s
    part1 = length . takeWhile (/= "ZZZ") . route

-- part2 = length . takeWhile (any ((/= 'Z') . last)) . ghostRoute

data Move = L | R
  deriving (Eq, Show, Read, Ord)

parsePath :: String -> [Move]
parsePath = map (read . (: []))

parseNode :: String -> Map.Map (String, Move) String -> Map.Map (String, Move) String
parseNode s = Map.insert (n, L) l . Map.insert (n, R) r
  where
    n = takeWhile (/= ' ') s
    l = take 3 . drop 1 $ dropWhile (/= '(') s
    r = take 3 . drop 2 $ dropWhile (/= ',') s

parse :: String -> ([Move], Map.Map (String, Move) String)
parse s = (parsePath (head ls), foldl (flip parseNode) Map.empty (drop 2 ls))
  where
    ls = lines s

route :: ([Move], Map.Map (String, Move) String) -> [String]
route (ms, m) = go "AAA" ms
  where
    go p [] = go p ms
    go p (x : xs) = p : go (m Map.! (p, x)) xs

getTimes :: ([Move], Map.Map (String, Move) String) -> String -> (String, Integer, [Integer])
getTimes (ms, m) s = go 0 [] ms s
  where
    go :: Integer -> [Integer] -> [Move] -> String -> (String, Integer, [Integer])
    go n acc [] p = (p, n, reverse acc)
    go n acc (x : xs) p = go (n + 1) (if last p == 'Z' then n : acc else acc) xs (m Map.! (p, x))

getDistanceLookup :: ([Move], Map.Map (String, Move) String) -> Map.Map String (String, Integer, [Integer])
getDistanceLookup inp@(_, m) = Map.fromList . map (\x -> (x, getTimes inp x)) $ Map.elems m