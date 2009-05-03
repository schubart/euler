-- TODO: Slow (~ 20 sec)

primes = 2 : filter ((==1) . length . primeFactors) [3,5..]

primeFactors n = factors n primes
    where factors n (p:ps)
              | p * p > n      = [n]
              | n `mod` p == 0 = p : factors (n `div` p) (p:ps)
              | otherwise      = factors n ps

main = print $ sum $ takeWhile (< 2 * 10^6) primes
