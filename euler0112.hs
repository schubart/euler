import Data.List

isIncreasing x = and $ zipWith (<=) x (tail x)
isDecreasing x = and $ zipWith (>=) x (tail x)
isBouncy x = not (isIncreasing x) && not (isDecreasing x)

density :: [Bool] -> [(Int, Int)]
density xs = scanl aggregate (0, 0) xs
    where aggregate (a, b) t = if t
                               then (a + 1, b + 1)
                               else (a,     b + 1)

-- TODO prettier, use Ratio type?
main = print $ find (\(a, b) -> a * 100 >= b * 99) $ tail $ density $ map (isBouncy . show) [1..]
