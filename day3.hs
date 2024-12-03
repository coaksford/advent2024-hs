import Data.Char

testData = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

part1 = do
  contents <- readFile "input3.txt"
  let total = findMatch contents
  print total
   
findMatch :: String -> Integer
findMatch "" = 0 
findMatch input = if (take 4 input) == "mul("
                  then 
                    let 
                      rest1 = drop 4 input
                      num1 = read (takeWhile isDigit rest1) :: Integer
                      (hopefullyComma:rest2) = (dropWhile isDigit rest1) 
                      num2 = read (takeWhile isDigit rest2) :: Integer
                      (hopefullyParen:rest3) = (dropWhile isDigit rest2) 
                    in
                    if hopefullyParen == ')' && hopefullyComma == ',' && num1 < 1000 && num1 > -1 && num2 < 1000 && num2 > -1
                    then (num1 * num2) + findMatch rest3
                    else findMatch $ tail input
                  else findMatch $ tail input

