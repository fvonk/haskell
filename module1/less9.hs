-- remove
import Data.Char

myRemove f [] = []
myRemove f (x:xs) = if f x
  then myRemove f xs
  else x : myRemove f xs

--
myProduct xs = foldl (*) 1 xs


--  elem x [...]

myElem e xs = length (filter (==e) xs) > 0
  -- if e == x
  -- then True
  -- else myElem e xs

-- palindrom
isPalindrome xs = xs == reverse xs
ignoreIsPalindrome xs = isPalindrome (map toLower (filter (/=' ') xs))

-- harmonic
harmonic n = sum (take n values)
  where pairs = zip (cycle [1.0]) [1.0, 2.0 ..]
        values = map (\a -> (fst a) / (snd a)) pairs
