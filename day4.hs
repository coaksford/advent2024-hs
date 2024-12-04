import Data.List

sample =  [ "MMMSXXMASM",
            "MSAMXMSMSA",
            "AMXSXMAAMM",
            "MSAMASMSMX",
            "XMASAMXAMM",
            "XXAMMXXAMA",
            "SMSMSASXSS",
            "SAXAMASAAA",
            "MAMMMXMMMM",
            "MXMXAXMASX" ]

part1 = do
  contents <- readFile "input4.txt"
  let searchArea = lines contents
  print $ search searchArea

search :: [String] -> Int
search input =
  sum (fmap searchHorizontal input) -- forward
  + sum (fmap searchHorizontal (fmap reverse input)) --backward
  + (searchVertical input) -- downward
  + (searchVertical (reverse input)) --upward
  + (searchDiagonal input) -- NWSE
  + (searchDiagonal (reverse input)) -- NESW
  + (searchDiagonal (fmap reverse input)) -- SWNE
  + (searchDiagonal (reverse (fmap reverse input))) -- SENW

searchHorizontal :: String -> Int
searchHorizontal "" = 0
searchHorizontal input =
  let rest = searchHorizontal (tail input)
  in if (take 4 input) == "XMAS" then 1 + rest else rest

searchVertical :: [String] -> Int
searchVertical (x:m:a:s:rest) =
  let columnQuads = zip4 x m a s
  in foldl (+) 0 (fmap (\quad -> if isXmas quad then 1 else 0) columnQuads)
  + searchVertical (m:a:s:rest)
searchVertical _ = 0

isXmas :: (Char, Char, Char, Char) -> Bool
isXmas (x, m, a, s) =
  x == 'X' && m == 'M' && a == 'A' && s == 'S'

searchDiagonal:: [String] -> Int
searchDiagonal (x:m:a:s:rest) =
  let columnQuads = zip4 x (drop 1 m) (drop 2 a) (drop 3 s)
  in foldl (+) 0 (fmap (\quad -> if isXmas quad then 1 else 0) columnQuads)
  + searchDiagonal (m:a:s:rest)
searchDiagonal _ = 0

