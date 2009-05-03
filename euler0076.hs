import Data.Array

-- p is the "intermediate function":
-- http://en.wikipedia.org/wiki/Integer_partition#Intermediate_function
partitions n = p 1 n
    -- Using an array for memonization
    where cache = listArray ((1,1),(n,n)) [ p x y | x <- [1..n], y <- [1..n] ]
          p k n
              | k >  n    = 0
              | k == n    = 1
              | otherwise = (cache ! (k + 1, n)) + (cache ! (k , n - k))

-- Minus 1 because "100" is not a sum according to the problem definition.
main = print $ (partitions 100) - 1
