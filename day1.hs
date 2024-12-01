main = do
  contents <- readFile "day1input1.txt"
  print contents
  print $ fmap parseRow $ fmap words $ lines contents

parseRow :: [String] -> (Int, Int)
parseRow (first:second:_) = (read first, read second)
parseRow (_) = (0,0)

getDistance :: (Int, Int) -> Int
getDistance (x, y) = abs $ y - x

