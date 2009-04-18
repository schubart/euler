pal x = asString == reverse asString
    where asString = show x

main = print $ maximum $ filter pal [ x * y | x <- [100..999], y <- [x..999] ]