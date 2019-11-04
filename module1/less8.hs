myDrop 0 [] = []
myDrop 0 (x:xs) = x:xs
myDrop n (x:xs) = (n-1) `myDrop` xs

--

myTake _ [] = []
myTake 0 _ = []
myTake n (x:xs) = x:(myTake (n-1) xs)

--
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

--
myCycle (x:xs) = x:myCycle (xs++[x])

--

myReverse [] = []
myReverse (x:xs) = (myReverse xs)++[x]
--

fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fast counter = fastFib 1 1 counter
fastFib _ _ 1 = 1
fastFib _ n2 2 = n2
fastFib n1 n2 counter = fastFib n2 (n1+n2) (counter-1)

--

ackermann 0 n = n + 1
ackermann m 0 = ackermann (m-1) 1
ackermann m n = ackermann (m-1) (ackermann m (n-1))
