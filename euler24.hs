import Data.List (sort)

isLastPermutation :: [Int] -> Bool
isLastPermutation [x] = True
isLastPermutation (x:xs) = x > maximum xs && isLastPermutation xs

-- Extract the smallest number in the list that's bigger than the first argument.
-- Precondition : The list must be sorted in reverse order (must pass "isLastPermutation")
-- Precondition : The element must exist (the function assumes there is at least one
--                element bigger than the first argument)
extractSmallestBiggerThan :: Int -> [Int] -> (Int, [Int])
extractSmallestBiggerThan n [x] = (x, [])
extractSmallestBiggerThan n (x1:x2:xs)
  | x2 < n = (x1, x2:xs)
  | otherwise = put x1 $ extractSmallestBiggerThan n (x2:xs)
  where put n (x, l) = (x, n:l)

nextPermutation :: [Int] -> [Int]
nextPermutation [] = []
nextPermutation l | isLastPermutation l = l
nextPermutation (x:xs)
  | isLastPermutation xs = let (small, remaining) = extractSmallestBiggerThan x xs in small:sort (x:remaining)
  | otherwise = x:nextPermutation xs

nthPermutation :: Int -> [Int] -> [Int]
nthPermutation 1 l = l
nthPermutation n l = nthPermutation (n-1) (nextPermutation l)

answer = filter (/=' ') . unwords . map show $ nthPermutation 1000000 [0,1,2,3,4,5,6,7,8,9]
