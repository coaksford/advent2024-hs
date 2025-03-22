import Data.List
import Data.List.Split

type PageNumber = Int
type PageSet = [PageNumber]
type Rule = (PageNumber, PageNumber)

separateRulesFromPages :: [String] -> ([String], [String])
separateRulesFromPages ("":lines) = ([], lines)
separateRulesFromPages (line:lines) = let (rules, pageSets) = separateRulesFromPages lines
  in (line:rules, pageSets)

parseRule :: String -> Rule
parseRule input = let (first:second:_) = splitOn "|" input
                   in (read first, read second)

parsePageSet :: String -> PageSet
parsePageSet input = fmap read $ splitOn "," input

pageSetContains :: PageSet -> PageNumber -> Bool
pageSetContains (page:[]) needle = page == needle
pageSetContains (page:pages) needle = if page == needle then True else pageSetContains pages needle

ruleApplies :: Rule -> PageSet -> Bool
ruleApplies _ (page:[]) = True
ruleApplies (before,successor) (page:pages) = let
    matches = page == successor
    orderingOk = (not (pageSetContains pages before))
    restIsOk = ruleApplies (before, successor) pages
    in restIsOk && if matches then orderingOk else True

checkAllRules :: [Rule] -> PageSet -> Bool
checkAllRules rules pageSet = 
  let 
    mappings = fmap ruleApplies rules
    mapped = mappings <*> pure pageSet
  in foldl (&&) True mapped

getMiddlePage :: PageSet -> PageNumber
getMiddlePage pageSet = pageSet !! (div (length pageSet) 2)

getMiddleIfOk :: [Rule] -> PageSet -> PageNumber
getMiddleIfOk rules pageSet = if checkAllRules rules pageSet then getMiddlePage pageSet else 0

part1 = do
  contents <- readFile "day5input.txt"
  let input = lines contents
  let (ruleStrings, pageSetStrings) = separateRulesFromPages input
  let rules = fmap parseRule ruleStrings
  let pageSets = fmap parsePageSet pageSetStrings
  let pageSum = foldl (+) 0 (fmap (getMiddleIfOk rules) pageSets)
  print pageSum

isWrong :: [Rule] -> PageSet -> Bool
isWrong rules pageSet = not (checkAllRules rules pageSet)

fixPageSetByAllRules :: [Rule] -> PageSet -> PageSet
fixPageSetByAllRules [] pageSet = pageSet
fixPageSetByAllRules (rule:rules) pageSet = if ruleApplies rule pageSet
                                  then fixPageSetByAllRules rules pageSet
                                  else fixPageSetByAllRules rules (fixPageSet rule pageSet)

fixPageSet :: Rule -> PageSet -> PageSet
fixPageSet _ [] = []
fixPageSet (before, successor) (page:pages) = if page == successor
                                            then before:successor:(fixPageSet (before,successor) pages)
                                            else  if page == before
                                                  then fixPageSet (before,successor) pages
                                                  else page:(fixPageSet (before,successor) pages)

part2 = do
  contents <- readFile "day5input.txt"
  let input = lines contents
  let (ruleStrings, pageSetStrings) = separateRulesFromPages input
  let rules = fmap parseRule ruleStrings
  let pageSets = fmap parsePageSet pageSetStrings
  let wrongPageSets = filter (isWrong rules) pageSets
  -- I'm not proud that I had to fix it twice to get the right answer, but it worked.
  let fixedPageSets = fmap (fixPageSetByAllRules rules) $ fmap (fixPageSetByAllRules rules) wrongPageSets
  let pageSum = foldl (+) 0 (fmap getMiddlePage fixedPageSets)
  print pageSum

-- cabal update
-- cabal install --lib split
