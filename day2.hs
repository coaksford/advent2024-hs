part1 = do
  contents <- readFile "input2.txt"
  let rows = fmap words $ lines contents
  let reports = fmap toReport rows
  let levelChecks = fmap levelDifferenceOk reports
  let safeCount = foldl (+) 0 [ if isSafe x == Safe then 1 else 0 | x <- levelChecks]
  print safeCount

type Level = Int
type Report = [Level]

data ReportCheck = Bad | Descending | Ascending | End 
  deriving (Show, Eq)

data ReportSafety = Safe | Unsafe
  deriving (Show, Eq)

toReport :: [String] -> Report
toReport (word:[]) = [read word]
toReport (word:words) = read word:toReport words

isSafe :: ReportCheck -> ReportSafety
isSafe Bad = Unsafe
isSafe _ = Safe

levelDifferenceOk :: [Level] -> ReportCheck
levelDifferenceOk (_:[]) = End
levelDifferenceOk (a:b:levels)
  | next /= Bad && next /= Ascending && (a - b >= 1 && a - b <= 3) = Descending
  | next /= Bad && next /= Descending && (a - b >= -3 && a - b <= -1) = Ascending
  | otherwise = Bad
  where next = levelDifferenceOk (b:levels)

parsedTestInputs :: [Report]
parsedTestInputs = [[7,6,4,2,1],[1,2,7,8,9],[9,7,6,2,1],[1,3,2,4,5],[8,6,4,4,1],[1,3,6,7,9]]
