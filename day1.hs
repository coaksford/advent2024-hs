import Data.List
import qualified Data.Map.Strict as Map

part1 = do
  contents <- readFile "day1input1.txt"
  let pairs = fmap parseRow $ fmap words $ lines contents
  let (left, right) = splitRow ([],[]) pairs
  let left2 = sort left
  let right2 = sort right
  let distances = fmap getDistance $ zip left2 right2
  let distanceSum = foldl (+) 0 distances
  print distanceSum

part2 = do
  contents <- readFile "day1input1.txt"
  let pairs = fmap parseRow $ fmap words $ lines contents
  let (left, right) = splitRow ([],[]) pairs
  let searchHaystack = sumCountsMatching right
  let matchedmultiples = foldl (+) 0 $ fmap searchHaystack left
  print matchedmultiples

parseRow :: [String] -> (Int, Int)
parseRow (first:second:_) = (read first, read second)
parseRow (_) = (0,0)

splitRow :: ([Int], [Int]) -> [(Int, Int)] -> ([Int], [Int])
splitRow input [] = input
splitRow input [(left, right)] = (left:(fst input), right:(snd input))
splitRow input (x:xs) = 
  (splitRow ((fst x):(fst input), (snd x):(snd input)) xs)

getDistance :: (Int, Int) -> Int
getDistance (x, y) = abs $ y - x

sumCountsMatching :: Integral a => [a] -> a -> a
sumCountsMatching haystack needle = foldl (+) 0 [ x | x <- haystack, x == needle]

