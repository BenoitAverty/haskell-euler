import Data.Time.Calendar
import Data.Time.Calendar.Easter

isFirstOfMonth :: Day -> Bool
isFirstOfMonth = isFirstOfMonth' . toGregorian
  where isFirstOfMonth' (_, _, d) = d == 1

numFirstSundays :: Day -> Day -> Int
numFirstSundays start end
  | start > end = 0
  | isFirstOfMonth start = 1 + recurse
  | otherwise = recurse
  where recurse = numFirstSundays (sundayAfter start) end

answer :: Int
answer = numFirstSundays start end
  where start = sundayAfter.pred $ fromGregorian 1901 1 1
        end = fromGregorian 2000 12 31
