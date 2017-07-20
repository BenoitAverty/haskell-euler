import qualified Data.Set as Set

divisors :: Integer -> [Integer]
divisors n = 1:divisors' 2 n
  where
    divisors' k n
      | k*k > n = []
      | k*k == n = [k]
      | n `mod` k == 0 = k:(n `div` k):recurse
      | otherwise = recurse
      where recurse = divisors' (k+1) n

isAbundant :: Integer -> Bool
isAbundant n = isAbundant' 0 (divisors n) n
  where
    isAbundant' currSum [] n = n < currSum
    isAbundant' currSum (x:xs) n
      | n < currSum = True
      | otherwise = isAbundant' (currSum+x) xs n

smallAbundantNumbers :: [Integer]
smallAbundantNumbers = filter isAbundant [1..28123]

sumsOf :: [Integer] -> Set.Set Integer
sumsOf [] = Set.empty
sumsOf (x:xs) = (Set.fromList $ map (+x) (x:xs)) `Set.union` sumsOf xs
--
answer :: Integer
answer = sum $ Set.difference (Set.fromList [1..28123]) (sumsOf smallAbundantNumbers)
