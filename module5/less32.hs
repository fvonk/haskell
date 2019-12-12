import Control.Monad
import GHC.Base
import Data.Char

-- task2

daysInYearUnSugar :: [Int]
daysInYearUnSugar = do
  [Jan .. Dec] >>=
    (\month ->
      days month >>=
        (\day ->
          return day
        )
    )


daysInYearDo :: [Int]
daysInYearDo = do
  month <- [Jan .. Dec]
  day   <- days month
  return day

-- task1

monthEnds :: [Int]
monthEnds = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

daysInYear2 = [day | end <- monthEnds, day <- [1 .. end]]

daysInYear2Do :: [Int]
daysInYear2Do = do
  end <- monthEnds
  day <- [1 .. end]
  return day

daysInYear2UnSugar :: [Int]
daysInYear2UnSugar =
  monthEnds >>=
    (\end ->
      [1 .. end] >>=
        (\day ->
          return day
        )
    )



data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
  deriving (Show, Eq, Ord, Read, Enum)
days :: Month -> [Int]
days month
  | elem month [Jan, Mar, May, Jul, Aug, Oct, Dec] = [1 .. 31]
  | elem month [Apr, Jun, Sep, Nov] = [1 .. 30]
  | otherwise = [1 .. 28]


daysInYear :: [Int]
daysInYear = [day | month <- [Jan .. Dec], day <- days month]


--


prefixCapitalize :: [String]
prefixCapitalize = ["Mr " ++ (toUpper . head) val : (tail val) | val <- ["brown", "red", "blue", "orange"]]

-- mFilter :: (a -> Bool) -> [a] -> [a]
-- mFilter f x = do
--   val <- x
--   guard (f val)
--   return val

mFilter :: (a -> Bool) -> [a] -> [a]
mFilter f x = do
  x >>=
    (\val ->
      guard (f val) >>
      return val
    )

mGuard :: Alternative f => Bool -> f ()
mGuard True = pure ()
mGuard False = empty
