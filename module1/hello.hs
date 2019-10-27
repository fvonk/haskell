--hello.hs my first Haskell-file!
-- main = do
check n = if n `mod` 3 == 0
          then n - 2
          else 3 * n + 1
  -- calcChange owed given = if change > 0
  --                         then change
  --                         else 0
  --   where
  --     change = given - owed
