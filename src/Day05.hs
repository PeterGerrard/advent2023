module Day05
  ( solve,
  )
where

import Data.Char (isDigit)

data Lookup = Lookup Range Range
  deriving (Show)

newtype Map = Map [Lookup]
  deriving (Show)

data Range = R Int Int
  deriving (Show)

solve :: String -> String
solve s = show (solve' (simple ss), solve' (pairs ss))
  where
    (ss, m) = parse s
    solve' = minimum . map getLower . concatMap (`lookupSeed` m)
    simple = map (\x -> R x (x + 1))
    pairs [] = []
    pairs [_] = error "need even"
    pairs (x : y : xs) = R x (x + y) : pairs xs

getLower :: Range -> Int
getLower (R x _) = x

parse :: String -> ([Int], [Map])
parse s = (map read . splitOn ' ' . drop 7 $ head xs, parseMaps (tail xs))
  where
    xs = lines s

parseMaps :: [String] -> [Map]
parseMaps = reverse . go [] (Map [])
  where
    go :: [Map] -> Map -> [String] -> [Map]
    go acc m [] = m : acc
    go acc m (x : xs)
      | null x = go (m : acc) (Map []) xs
      | isDigit (head x) = go acc (updateMap m x) xs
      | otherwise = go acc m xs

updateMap :: Map -> String -> Map
updateMap (Map ls) s = Map (parseLookup s : ls)

parseLookup :: String -> Lookup
parseLookup s = head [Lookup (R y (y + range)) (R x (x + range)) | (x, xs) <- reads s, (y, ys) <- reads xs, let range = read ys]

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn c xs = as : splitOn c (if null bs then bs else tail bs)
  where
    (as, bs) = break (== c) xs

lookupSeed :: Range -> [Map] -> [Range]
lookupSeed = foldl (\rs m -> concatMap (`lookupInMap` m) rs) . (: [])

lookupInMap :: Range -> Map -> [Range]
lookupInMap r (Map []) = [r]
lookupInMap r (Map ((Lookup sourceRange targetRange) : ls)) = maybe [] (`lookupInMap` Map ls) under ++ maybe [] (\(R x y) -> [R (x + off) (y + off)]) match ++ maybe [] (`lookupInMap` Map ls) over
  where
    (under, match, over) = overlap r sourceRange
    off = shift sourceRange targetRange

shift :: Range -> Range -> Int
shift (R x _) (R y _) = y - x

overlap :: Range -> Range -> (Maybe Range, Maybe Range, Maybe Range)
overlap (R l1 u1) (R l2 u2) = (toRange l1 (min u1 l2), toRange (max l1 l2) (min u1 u2), toRange (max l1 u2) u1)
  where
    toRange x y = if x < y then Just (R x y) else Nothing