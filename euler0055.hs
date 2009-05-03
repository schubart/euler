isLychrel n = not $ any isPalindrome $ lychrelSeq n
    where isPalindrome n = show n == (reverse $ show n)
          lychrelSeq n = take 50 $ tail $ iterate next n
              where next n = n + (read $ reverse $ show n)

main = print $ length $ filter isLychrel [1..10000]
