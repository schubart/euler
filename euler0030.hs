import Data.Char

test n = n == sum $ map (^5) digitToInt $ show n

main = print $ filter test [2..10000]
