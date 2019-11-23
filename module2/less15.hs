-- Cipher

class Cipher a where
  encode :: a -> String -> String
  decode :: a -> String -> String

data Rot = Rot
instance Cipher Rot where
  encode Rot text = rotEncoder text
  decode Rot text = rotDecoder text

data OneTimePad = OTP String
instance Cipher OneTimePad where
  encode (OTP pad) text = applyOTP pad text
  decode (OTP pad) text = applyOTP pad text


myOTP :: OneTimePad
myOTP = OTP (cycle [minBound .. maxBound])

--  PAD

myPad :: String
myPad = "Shhhhhhhh"

applyOTP' :: String -> String -> [Bits]
applyOTP' pad text = map (\pair -> (fst pair) `xor` (snd pair)) (zip padBits textBits)
  where padBits = map charToBits pad
        textBits = map charToBits text

applyOTP :: String -> String -> String
applyOTP pad text = map bitsToChar (applyOTP' pad text)

encoderDecoder :: String -> String
encoderDecoder = applyOTP myPad

-- XOR

xorBool :: Bool -> Bool -> Bool
xorBool v1 v2 = (v1 || v2) && (not (v1 && v2))

xorPair :: (Bool, Bool) -> Bool
xorPair (v1, v2) = xorBool v1 v2

xor :: [Bool] -> [Bool] -> [Bool]
xor l1 l2 = map xorPair (zip l1 l2)

--

type Bits = [Bool]

intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n = if remainder == 0
               then False : intToBits' nextVal
               else True : intToBits' nextVal
  where remainder = n `mod` 2
        nextVal = n `div` 2

--

maxBits :: Int
maxBits = length (intToBits' maxBound)

intToBits :: Int -> Bits
intToBits n = leadingFalses ++ reversedBits
  where reversedBits = reverse (intToBits' n)
        missingBits = maxBits - (length reversedBits)
        leadingFalses = take missingBits (cycle [False])

intToBinaryString n = foldl (++) "" (map (\x -> show (fromEnum x)) (intToBits n))
--

charToBits :: Char -> Bits
charToBits c = intToBits (fromEnum c)

--

bitsToInt :: Bits -> Int
bitsToInt bits = sum (map (\x -> 2^(snd x)) trueLocations)
  where size = length bits
        indices = [size - 1, size - 2 .. 0]
        trueLocations = filter (\x -> fst x == True) (zip bits indices)

--

bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits)


-- ROT


rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN aSize c = toEnum rotation
  where half = aSize `div` 2
        offset = fromEnum c + half
        rotation = offset `mod` aSize

rotChar :: Char -> Char
rotChar c = rotN aSize c
  where aSize = 1 + fromEnum (maxBound :: Char)

rotNdecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNdecoder n c = toEnum rotation
  where halfN = n `div` 2
        offset = if even n
                 then fromEnum c + halfN
                 else fromEnum c + halfN + 1
        rotation = offset `mod` n

rotEncoder :: String -> String
rotEncoder text = map rotChar text
  where aSize = 1 + fromEnum (maxBound :: Char)
        rotChar = rotN aSize

rotDecoder :: String -> String
rotDecoder text = map rotCharDecoder text
  where aSize = 1 + fromEnum (maxBound :: Char)
        rotCharDecoder = rotNdecoder aSize
