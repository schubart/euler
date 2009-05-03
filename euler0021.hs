-- TODO: Slow. (~ 35 sec)

main = print $ sum $ filter isAmicable [1..9999]
    where
      isAmicable n = (n /= (d n)) && (n == (d (d n)))
      -- d: As in problem defintion.
      d = sum . properDivisors
      properDivisors n = 1 : filter ((==0) . (n `mod`)) [2..(n `div` 2)]
