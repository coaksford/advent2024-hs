part1 = do
  contents <- readFile "input2.txt"
  let rows = fmap words $ lines contents
  let reports = fmap toReport rows
  let levelChecks = fmap levelDifferenceOk reports
  let safeCount = foldl (+) 0 [ if isSafe x == Safe then 1 else 0 | x <- levelChecks]
  print safeCount

part2 = do
  contents <- readFile "input2.txt"
  let rows = fmap words $ lines contents
  let reports = fmap toReport rows
  let dampenedSafeCount = foldl (+) 0 [ if reportWithAnyOmittedIsSafe report then 1 else 0 | report <- reports]
  print dampenedSafeCount

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

reportWithoutIndex :: Int -> Report -> Report
reportWithoutIndex index report =
  let (before, after) = splitAt index report in
  before ++ tail after

isAscending :: Report -> Bool
isAscending (_:[]) = True
isAscending (a:b:rest) =
  (ascendingInRange a b) && (isAscending (b:rest))

isDescending :: Report -> Bool
isDescending (_:[]) = True
isDescending (a:b:rest) =
  (descendingInRange a b) && (isDescending (b:rest))

ascendingInRange :: Int -> Int -> Bool
ascendingInRange a b = let difference = a - b in (difference >= -3) && (difference <= -1)

descendingInRange :: Int -> Int -> Bool
descendingInRange a b = let difference = a - b in (difference >= 1) && (difference <= 3)

parsedTestInputs :: [Report]
parsedTestInputs = [[7,6,4,2,1],[1,2,7,8,9],[9,7,6,2,1],[1,3,2,4,5],[8,6,4,4,1],[1,3,6,7,9]]

reportWithAnyOmitted :: Report -> (Report -> Bool) -> Bool
reportWithAnyOmitted report rule = foldl (||) False $ [ rule (reportWithoutIndex removedIndex report) | removedIndex <- [0..(length report)]]

reportWithAnyOmittedIsSafe :: Report -> Bool
reportWithAnyOmittedIsSafe report = (reportWithAnyOmitted report isAscending) || (reportWithAnyOmitted report isDescending)
