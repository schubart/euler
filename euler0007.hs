primes = 2 : filter ((==1) . length . primeFactors) [3,5..]

primeFactors n = factors n primes
    where factors n (p:ps)
              | p * p > n      = [n]
              | n `mod` p == 0 = p : factors (n `div` p) (p:ps)
              | otherwise      = factors n ps

-- Indices start from 0 -> 10001st number is at index 10000.
main = print $ primes !! 10000
