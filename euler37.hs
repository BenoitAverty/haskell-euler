import Data.List.Ordered (minus, union, unionAll)

nDigits :: Integer -> Integer
nDigits = (+1) . truncate . logBase 10 . fromIntegral

primes = 2 : 3 : minus [5,7..] (unionAll [[p*p, p*p+2*p..] | p <- tail primes])

isPrime :: Integer -> Bool
isPrime n = n `elem` takeWhile (<=n) primes

isTruncatableFromLeft :: Integer -> Bool
isTruncatableFromLeft n
  | n < 10 = isPrime n
  | otherwise = isTruncatableFromLeft (n `div` 10) && isPrime n

isTruncatableFromRight :: Integer -> Bool
isTruncatableFromRight n
  | n < 10 = isPrime n
  | otherwise = isTruncatableFromRight (n `mod` 10 ^ (nDigits n - 1)) && isPrime n

answer = sum $ take 11 $ filter (\x -> isTruncatableFromRight x && isTruncatableFromLeft x) $ drop 4 primes
