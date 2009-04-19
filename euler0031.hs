numSols :: Int -> [Int] -> Int
numSols 0 _  = 1 -- Target 0p: One solution, available coins don't matter.
numSols _ [] = 0 -- Some target but no coins left: No solution.
numSols target (c:cs) = sum [ combos (target - x) cs | x <- [0,c..target] ]

main = print $ combos 200 [200, 100, 50, 20, 10, 5, 2, 1]
