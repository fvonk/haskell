isEmtpy [] = True
isEmtpy _ = False

myHead (x:_) = x
myHead [] = error "Head is absent"

--

myTail [] = []
myTail (_:xs) = xs

--

-- myGCD a b = case a `mod` b of
--   0 -> b
--   _ -> myGCD b (a `mod` b)

myGCD a 0 = a
myGCD a b = myGCD b (a `mod` b)
--
