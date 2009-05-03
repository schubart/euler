import Data.List

tri = 1 : zipWith (+) [2..] tri

-- From problem 21.
divs n = nub $ n : 1 : concat [ [x, n `div` x] | x <- [2..root], 
                                                      n `mod` x == 0 ]
    where
      root = (round . sqrt . encodeFloat n) 0

main = print $ head $ filter ((>500) . length . divs) $ tri