module Day02
  ( solve,
  )
where

import Data.Char (isLetter)

solve :: String -> String
solve inp = show (part1, part2)
  where
    games = map parseGame . lines $ inp
    part1 = sum . map getId . filter (canHandle 12 13 14) $ games
    part2 = sum . map ((\(x, y, z) -> x * y * z) . getMin) $ games

data Colour = Red | Blue | Green
  deriving (Eq, Show)

instance Read Colour where
  readsPrec _ input = case x of
    "red" -> [(Red, ys)]
    "green" -> [(Green, ys)]
    "blue" -> [(Blue, ys)]
    _ -> []
    where
      (x, ys) = span isLetter $ dropWhile (not . isLetter) input

data Selection = Shown Colour Integer
  deriving (Show)

instance Read Selection where
  readsPrec _ input = [(Shown c n, ys) | (n, xs) <- reads input, (c, ys) <- reads xs]

newtype SelectionGroup = Group [Selection]
  deriving (Show)

parseSelectionGroup :: String -> SelectionGroup
parseSelectionGroup = Group . map read . splitOn (== ',')

data Game = Game Integer [SelectionGroup]
  deriving (Show)

getId :: Game -> Integer
getId (Game g _) = g

parseGame :: String -> Game
parseGame xs = Game g (map parseSelectionGroup (splitOn (== ';') ys))
  where
    [(g, _ : _ : ys)] = reads (drop 5 xs)

canHandle :: Integer -> Integer -> Integer -> Game -> Bool
canHandle red green blue (Game _ xs) = all canHandle' xs
  where
    canHandle' (Group ys) = all canHandle'' ys
    canHandle'' (Shown c n) = case c of
      Blue -> n <= blue
      Red -> n <= red
      Green -> n <= green

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn f xs = as : splitOn f (if null bs then bs else tail bs)
  where
    (as, bs) = break f xs

getMin :: Game -> (Integer, Integer, Integer)
getMin (Game _ xs) = getMin' 0 0 0 (concat $ getSelections xs)
  where
    getSelections [] = []
    getSelections (Group gs : ys) = gs : getSelections ys
    getMin' r g b [] = (r, g, b)
    getMin' r g b (Shown Red n : ys) = getMin' (max n r) g b ys
    getMin' r g b (Shown Green n : ys) = getMin' r (max n g) b ys
    getMin' r g b (Shown Blue n : ys) = getMin' r g (max n b) ys
