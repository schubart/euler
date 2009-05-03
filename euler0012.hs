import Data.List

primes = 2 : filter ((==1) . length . primeFactors) [3,5..]

primeFactors n = factors n primes
    where factors n (p:ps)
              | p * p > n      = [n]
              | n `mod` p == 0 = p : factors (n `div` p) (p:ps)
              | otherwise      = factors n ps

-- http://mathschallenge.net/index.php?section=faq&ref=number/number_of_divisors
numDivs n = product $ map (+1) $ map length $ group $ primeFactors n

tri = 1 : zipWith (+) [2..] tri

main = print $ find ((>500) . numDivs) tri
