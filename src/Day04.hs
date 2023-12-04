module Day04
  ( solve,
  )
where

import Data.Set (Set, fromList, intersection)

solve :: String -> String
solve s = show (part1 cards, part2 cards)
  where
    cards = map parseCard $ lines s

data Card = Card Integer (Set Integer) (Set Integer)
  deriving (Show)

parseCard :: String -> Card
parseCard s = Card (read n) (parseList xs) (parseList ys)
  where
    (cs, zs) = break (== ':') s
    (xs, ys) = break (== '|') zs
    n = last $ splitOn (== ' ') cs
    parseList = fromList . map read . filter (not . null) . splitOn (== ' ') . tail

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn f xs = as : splitOn f (if null bs then bs else tail bs)
  where
    (as, bs) = break f xs

winningNumbers :: Card -> Set Integer
winningNumbers (Card _ ws ns) = intersection ws ns

part1 :: [Card] -> Int
part1 = sum . map ((\x -> if x == 0 then 0 else 2 ^ (x - 1)) . length . winningNumbers)

part2 :: [Card] -> Int
part2 = go 0 [] . map (length . winningNumbers)
  where
    go :: Int -> [(Int, Int)] -> [Int] -> Int
    go acc _ [] = acc
    go acc wins (x : xs) = go (amount + acc) (filter ((> 0) . snd) ((amount, x) : map (\(a, y) -> (a, y - 1)) wins)) xs
      where
        amount = 1 + sum (map fst wins)