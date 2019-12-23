import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import Data.STRef

-- task 2
b3 :: UArray Int Int
b3 = listArray (0,10) [0, 0, 12, 4, 5, -1, 0, 0, 34, 0, 1]

replaceZeros :: UArray Int Int -> UArray Int Int
replaceZeros arr = runSTUArray $ do
  stArray <- thaw arr
  let end = (snd . bounds) arr
  forM_ [0 .. end] $
    \i -> do
      val <- readArray stArray i
      let isZero = val == 0
      when isZero $ do
        writeArray stArray i (-1)
  return stArray


-- task 1
b1 :: UArray Int Bool
b1 = listArray (0,5) $ (take 6 $ cycle [True])

b2 :: UArray Int Bool
b2 = listArray (0,5) $ (take 6 $ cycle [False])

cross :: (UArray Int Bool, UArray Int Bool) -> Int ->
          UArray Int Bool
cross (array1, array2) idx = runSTUArray $ do
  stArray1 <- thaw array1 :: ST s (STUArray s Int Bool)
  stArray2 <- thaw array2 :: ST s (STUArray s Int Bool)
  let end = (snd . bounds) array1
  forM_ [0 .. end] $
    \i -> do
      let start = i >= idx
      when start $ do
        val1 <- readArray stArray1 i
        val2 <- readArray stArray2 i
        writeArray stArray1 i val2
        writeArray stArray2 i val1
  return stArray1

--

bubbleSort :: UArray Int Int -> UArray Int Int
bubbleSort myArray = runSTUArray $ do
  stArray <- thaw myArray
  let end = (snd . bounds) myArray
  forM_ [1 .. end] $
    \i -> do
      forM_ [0 .. (end - i)] $
        \j -> do
          val <- readArray stArray j
          nextVal <- readArray stArray (j + 1)
          let outOfOrder = val > nextVal
          when outOfOrder $ do
            writeArray stArray j nextVal
            writeArray stArray (j + 1) val
  return stArray


myData :: UArray Int Int
myData = listArray (0,5) [7, 6, 4, 8, 10, 2]

myData2 :: UArray Int Int
myData2 = listToUArray [7, 6, 4, 8, 10, 2]


swapST :: (Int, Int) -> (Int, Int)
swapST (x, y) = runST $ do
  x' <- newSTRef x
  y' <- newSTRef y
  writeSTRef x' y
  writeSTRef y' x
  xfinal <- readSTRef x'
  yfinal <- readSTRef y'
  return (xfinal, yfinal)


listToSTUArray :: [Int] -> ST s (STUArray s Int Int)
listToSTUArray vals = do
  let end = length vals - 1
  myArray <- newArray (0, end) 0
  forM_ [0 .. end] $
    \i -> do
      let val = vals !! i
      writeArray myArray i val
  return myArray

listToUArray :: [Int] -> UArray Int Int
listToUArray vals = runSTUArray $ do
  let end = length vals - 1
  myArray <- newArray (0, end) 0
  forM_ [0 .. end] $
    \i -> do
      let val = vals !! i
      writeArray myArray i val
  return myArray
  -- runSTUArray $ listToSTUArray vals


zA :: UArray Int Bool
zA = array (0,9) [(3, True)]

oneIndexArray :: UArray Int Bool
oneIndexArray = array (1, 10) $ zip [1 .. 10] $ cycle [True]

checkArray :: UArray Int Bool
checkArray = array (0, 4) [(1, True), (2, True)]

zA1 :: UArray Int Int
zA1 = array (0,9) []
