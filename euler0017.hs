import Data.Char

english n
    | n == 0 = "zero"
    | n == 1 = "one"
    | n == 2 = "two"
    | n == 3 = "three"
    | n == 4 = "four"
    | n == 5 = "five"
    | n == 6 = "six"
    | n == 7 = "seven"
    | n == 8 = "eight"
    | n == 9 = "nine"

    | n == 10 = "ten"
    | n == 11 = "eleven"
    | n == 12 = "twelve"
    | n == 13 = "thirteen"
    | n == 15 = "fifteen"
    | n == 18 = "eighteen"
    | n < 20 = (english ones) ++ "teen"

    | n == 20 = "twenty"
    | n == 30 = "thirty"
    | n == 40 = "forty"
    | n == 50 = "fifty"
    | n == 60 = "sixty"
    | n == 70 = "seventy"
    | n == 80 = "eighty"
    | n == 90 = "ninety"
    | n < 100 = (english tens) ++ "-" ++ english ones

    | n < 1000 && subHundreds == 0 = (english hundreds) ++ " hundred"
    | n < 1000 = (english hundreds) ++ " hundred and " ++ english subHundreds
    
    | n == 1000 = "one thousand"
    | otherwise = undefined
    where ones = n `mod` 10
          tens = n `div` 10 * 10
          hundreds = n `div` 100
          subHundreds = n `mod` 100

main = print $ length $ filter isAlpha $ concat $ map english [1..1000]
