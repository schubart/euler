ways :: Int -> [Int] -> Int
ways 0 _  = 1 -- Target 0p: One solution, available coins don't matter.
ways _ [] = 0 -- Some target but no coins left: No solution.
ways target (c:cs) = sum [ ways (target - x) cs | x <- [0,c..target] ]

main = print $ ways 200 [200, 100, 50, 20, 10, 5, 2, 1]
