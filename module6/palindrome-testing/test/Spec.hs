import Lib
import Test.QuickCheck
import Test.QuickCheck.Instances
import Data.Char (isPunctuation)
import Data.Text as T
import Data.Text.IO as TIO

assert :: Bool -> T.Text -> T.Text -> IO ()
assert test passStatement failStatement =
  if test
  then TIO.putStrLn passStatement
  else TIO.putStrLn failStatement


main :: IO ()
main = do
  TIO.putStrLn "\nTesting"
  -- quickCheck prop_punctuationInvariant

  -- quickCheckWith stdArgs {maxSuccess = 1005}
  --                prop_punctuationInvariant
  quickCheck prop_punctuationInvariant
  quickCheck prop_reverseInvariant
  quickCheck prop_lowerInvariant
  quickCheck prop_stripInvariant

  -- assert (isPalindrome "aba") "success 1" "fail 1"
  -- assert (isPalindrome "aba!") "success 2" "fail 2"
  -- assert ((not . isPalindrome) "abo") "success 3" "fail 3"
  -- assert (isPalindrome "cooc.") "success 4" "fail 4"
  TIO.putStrLn "Done!"


  -- PROP

prop_stripInvariant text = preprocess text == preprocess noStripText
  where
    noStripText = stripWhiteSpace text

prop_lowerInvariant text = preprocess text == preprocess noLowerText
  where
    noLowerText = toLowerCase text

prop_punctuationInvariant text = preprocess text == preprocess noPuncText
  where
    noPuncText = stripPunctuation text

prop_reverseInvariant text = isPalindrome text
  == ((isPalindrome . T.reverse) text)
