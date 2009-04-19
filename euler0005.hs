-- TODO foldr1 vs. foldl1?
-- TODO foldl1' works for larger numbers without stack overflow. Why?
main = print $ foldl1 lcm [1..20]