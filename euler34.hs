fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n-1)

digits :: Integer -> [Integer]
digits 0 = []
digits n = (n `mod` 10) : digits (n `div` 10)

isCurious :: Integer -> Bool
isCurious n = (foldr sumFact 0 (digits n)) == n
  where sumFact curr acc = acc + fact curr

answer = sum $ filter isCurious [3..50000] -- Found with luck... there are only 2 numbers : 145 and 40585
