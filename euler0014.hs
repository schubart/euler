import Data.Array
import Data.List

-- Must be compiled with -O. Cache overflow otherwise.
-- More on memoization: http://www.haskell.org/haskellwiki/Memoization

collatz_array max = 
    cache
    where cache = listArray (1, max) $ map collatz [1..max]
              where collatz x
                        | x == 1    = 1
                        | even x    = 1 + maybe_cached_collatz (x `div` 2)
                        | otherwise = 1 + maybe_cached_collatz (3 * x + 1)
                        where maybe_cached_collatz x 
                                  | x <= max  = cache ! x
                                  | otherwise = collatz x

main = print $ maximumBy compareSnd $ assocs $ collatz_array 999999
       where compareSnd x y = (snd x) `compare` (snd y)
