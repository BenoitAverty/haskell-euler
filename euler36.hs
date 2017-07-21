showBase2 :: Integer -> String
showBase2 0 = "0"
showBase2 1 = "1"
showBase2 n
  | odd n = '1':showBase2 (n `div` 2)
  | even n = '0':showBase2 (n `div` 2)

isPalindromic10 :: Integer -> Bool
isPalindromic10 n = show n == (reverse . show) n

isPalindromic2 :: Integer -> Bool
isPalindromic2 n = showBase2 n == (reverse . showBase2) n

answer = sum $ filter (\x -> isPalindromic2 x && isPalindromic10 x) [1..1000000]
