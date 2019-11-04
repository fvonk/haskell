myRepeat x = cycle [x]

--

subseq start end list = take (end - start) (drop start list)

--

inFirstHalf e list = let middle = length list `div` 2
  in
    elem e (take middle list)
