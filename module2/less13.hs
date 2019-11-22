inc :: Num a => a -> a
inc x = x + 1

addD :: Num a => a -> a -> a
addD x y = (x + y) * 2

cycleSucc :: (Bounded a, Enum a, Eq a) => a -> a
cycleSucc n = if n == maxBound
              then minBound
              else succ n
