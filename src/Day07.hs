module Day07 (solve) where

import Data.Char (isDigit)
import Data.List (sort, sortBy)
import qualified Data.Map as Map

solve :: String -> String
solve s = show (part1 hs, part2 hs)
  where
    hs = parse s

data Card = C Int | T | J | Q | K | A
  deriving (Eq, Ord)

instance Show Card where
  show (C n) = show n
  show T = "T"
  show J = "J"
  show Q = "Q"
  show K = "K"
  show A = "A"

instance Read Card where
  readsPrec _ [] = []
  readsPrec _ (x : xs)
    | isDigit x = [(C (read [x]), xs)]
    | x == 'T' = [(T, xs)]
    | x == 'J' = [(J, xs)]
    | x == 'Q' = [(Q, xs)]
    | x == 'K' = [(K, xs)]
    | x == 'A' = [(A, xs)]
    | otherwise = []

compare2 :: Card -> Card -> Ordering
compare2 (C x) (C y) = compare x y
compare2 T T = EQ
compare2 J J = EQ
compare2 Q Q = EQ
compare2 K K = EQ
compare2 A A = EQ
compare2 _ A = LT
compare2 A _ = GT
compare2 _ K = LT
compare2 K _ = GT
compare2 _ Q = LT
compare2 Q _ = GT
compare2 _ T = LT
compare2 T _ = GT
compare2 _ (C _) = LT
compare2 (C _) _ = GT

newtype Hand = H [Card]
  deriving (Eq, Show, Ord)

instance Read Hand where
  readsPrec _ xs = [(H [a, b, c, d, e], es) | (a, as) <- reads xs, (b, bs) <- reads as, (c, cs) <- reads bs, (d, ds) <- reads cs, (e, es) <- reads ds]

compareHand2 :: Hand -> Hand -> Ordering
compareHand2 (H xs) (H ys) = (\x -> if null x then EQ else head x) . dropWhile (== EQ) $ zipWith compare2 xs ys

data HandType = HighCard | OnePair | TwoPair | ThreeKind | FullHouse | FourKind | FiveKind
  deriving (Show, Eq, Ord)

handScore :: Hand -> HandType
handScore (H cs) = case maximum xs of
  5 -> FiveKind
  4 -> FourKind
  3 -> if 2 `elem` xs then FullHouse else ThreeKind
  2 -> if length (filter (== 2) xs) > 1 then TwoPair else OnePair
  _ -> HighCard
  where
    s = foldl (\y x -> Map.insertWith (+) x 1 y) Map.empty cs
    xs :: [Integer]
    xs = map snd $ Map.toList s

handScore2 :: Hand -> HandType
handScore2 (H cs) =
  if null xs
    then FiveKind
    else case maximum xs of
      5 -> FiveKind
      4 -> if js >= 1 then FiveKind else FourKind
      3 -> if js >= 2 then FiveKind else if js == 1 then FourKind else if 2 `elem` xs then FullHouse else ThreeKind
      2 -> if js >= 3 then FiveKind else if js == 2 then FourKind else if js == 1 && length (filter (== 2) xs) > 1 then FullHouse else if js == 1 then ThreeKind else if length (filter (== 2) xs) > 1 then TwoPair else OnePair
      _ -> if js >= 4 then FiveKind else if js == 3 then FourKind else if js == 2 then ThreeKind else if js == 1 then OnePair else HighCard
  where
    js = length $ filter (== J) cs
    s = foldl (\y x -> Map.insertWith (+) x 1 y) Map.empty (filter (/= J) cs)
    xs :: [Integer]
    xs = map snd $ Map.toList s

parse :: String -> [(Hand, Integer)]
parse = map parseLine . lines
  where
    parseLine xs = (read (take 5 xs), read (drop 6 xs))

part1 :: [(Hand, Integer)] -> Integer
part1 = sum . zipWith (\r (_, _, b) -> r * b) [1 ..] . sort . map (\(h, b) -> (handScore h, h, b))

part2 :: [(Hand, Integer)] -> Integer
part2 = sum . zipWith (\r (_, _, b) -> r * b) [1 ..] . sortBy comp . map (\(h, b) -> (handScore2 h, h, b))
  where
    comp (a, b, c) (d, e, f) = case compare a d of
      LT -> LT
      GT -> GT
      EQ -> case compareHand2 b e of
        LT -> LT
        GT -> GT
        EQ -> compare c f