--hello.hs my first Haskell-file!
-- main = do

makeChange x y = (\x -> if x > 0 then x else 0)
                 (x-y)

counter x = let x = x + 1
            in
              let x = x + 1
              in
                x

cube = (\x -> x^3)

counterLambda x = (\x -> (\x -> x+1) x+1) x

doubelL x = (\y -> y*2) x*2

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
