import Data.List (nub)

answer = (length . nub) [x^y | x <- [2..100], y <- [2..100]]
