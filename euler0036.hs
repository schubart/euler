import Numeric
import Data.Char

isSolution n = isPalindrome decString && isPalindrome binString
    where decString = show n
          binString = showIntAtBase 2 intToDigit n ""
          isPalindrome xs = xs == reverse xs

main = print $ sum $ filter isSolution [1,3..999999]
