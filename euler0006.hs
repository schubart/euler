sumOfSquares x = sum [ x * x | x <- [1..x] ]
squareOfSum x = s * s
    where s = x * (x + 1) `div` 2

main = print $ (squareOfSum 100) - (sumOfSquares 100)
