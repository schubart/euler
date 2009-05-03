import Data.List

-- Observation: All hexagonal numbers are also triangle numbers, so can
-- ignore the triangle numbers.

pentas = map (\n -> n * (3 * n - 1) `div` 2) [1..]
hexas  = map (\n -> n * (2 * n - 1))         [1..]

commons aa@(a:as) bb@(b:bs)
    | a == b    = a : commons as bs
    | a < b     = commons as bb
    | otherwise = commons aa bs

main = print $ find (> 40755) $ commons pentas hexas