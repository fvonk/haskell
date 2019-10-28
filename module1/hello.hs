--hello.hs my first Haskell-file!
-- main = do

doubleDouble x = dubs*2
  where dubs = x*2

doubleD x = (\dubs -> dubs*2) x*2

overwrite x = let x = 2
              in let x = 3
              in let x = 4
              in x

overwrite2 x = (\x -> x) ((\x -> 4) ((\x -> 3) 2))
-- overwrite3 x = (\x -> (\x -> (\x -> x) 4) 3) 2
over x = (\_ -> (\_ -> (\x -> x) 4) 3) 2

check n = if n `mod` 3 == 0
          then n - 2
          else 3 * n + 1
  -- calcChange owed given = if change > 0
  --                         then change
  --                         else 0
  --   where
  --     change = given - owed
