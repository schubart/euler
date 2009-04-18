-- Path length is 40 steps. Need to choose 20 steps in which to move
-- horizontally (the other 20 steps move verically.) 40 over 20:
-- http://en.wikipedia.org/wiki/Combinations#Number_of_k-combinations_from_a_set 
fact n = product [1..n]
main = print $ (fact 40) `div` (fact 20)^2