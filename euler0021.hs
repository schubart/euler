import Data.List

-- Calculation of divisors based on achiles' code (10/15/2007), but fixes
-- case of perfect squares (why does his code provide the right solution
-- in spit of this issue?)

-- When we find one divisor, we automatically find a second divisor.
-- Example: 3 divides 12, 12 div 3 is 4 -> 4 divides 12.
-- Exception: Perfect square numbers.
-- Example: 3 divides 9, but only consider 3 once (using nub.)
divs n = 1 : concat [ nub [x, n `div` x] | x <- [2..root], n `mod` x == 0 ]
    where
      root = (round . sqrt . encodeFloat n) 0

main = print $ sum $ filter isAmicable [1..9999]
    where
      isAmicable n = (n /= (d n)) && (n == (d (d n)))
      -- d: As in problem defintion.
      d = sum . divs

