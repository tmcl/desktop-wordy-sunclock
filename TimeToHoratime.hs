{-# LANGUAGE OverloadedStrings #-}
module TimeToHoratime( dayOfWeek, horaTime, horaRaw ) where

import Coordinates
import Data.Time
import Data.Time.Calendar.OrdinalDate
import NumbersToWords
import Text.Printf
import Horatime

rtWords :: RomanTime -> String
rtWords t = makeRomanTimeWords a b
   where
      h = timepart t + (1/24)
      a = floor h
      b = floor $ (h - fromInteger (floor h)) * 12

nexthour :: (Num a, Ord a) => a -> a
nexthour a 
   | 0 <= a && a < 23 = a + 1
   | otherwise        = 0

smallpart :: Integer -> String
smallpart 0  = ""
smallpart 1  = "one"
smallpart 2  = "two"
smallpart 3  = "a quarter"
smallpart 4  = "a third"
smallpart 5  = "five"
smallpart 6  = "halfway"
smallpart 7  = "seven"
smallpart 8  = "two-thirds"
smallpart 9  = "three-quarters"
smallpart 10 = "ten"
smallpart 11 = "eleven"
smallpart 12 = "through"
smallpart x  = show x

flipSmallParts :: Num a => a -> a
flipSmallParts n = 6 + (-1) * (n-6)

bigpart :: Integer -> Integer -> String
bigpart x y | x == 12   = "sunrise"
            | x == 18   = "midday"
            | x == 0    = "sunset"
            | x ==  6   = "midnight"
            | x <  12   = cardinal x ++ say "watch" y
            | x >  12   = cardinal (x-12) ++ " hour"
            | otherwise = "foob?"
        -- where
                -- say str = y == 0 ? " " ++ str $ ""

say :: String -> Integer -> String
say str y
   | y == 0 = " " ++ str
   | otherwise = ""

data Refers = Same | Next
   deriving Eq

refers :: Integer -> Refers
refers  0 = Same
refers  6 = Same
refers 12 = Same
refers 18 = Same
refers  _ = Next

shownHourPart :: Integer -> Integer -> String
shownHourPart a b = bigpart (hourToReference a (refers a) b) b
   where
      hourToReference h Next 0 = h
      hourToReference h Next _ = nexthour h
      hourToReference h _    _ = h

pronoun :: Integer -> Integer -> String
pronoun a b
   | b == 0                      = ""
   | refers a == Same            = "after "
   | refers (nexthour a) == Same = "to "
   | otherwise                   = "through "

shownSmallPart :: Integer -> Integer -> String
shownSmallPart eqhour twelfth 
   | length st == 0 = st
   | otherwise      = st ++ " "
   where
      st = shownSmallPart' eqhour twelfth
      shownSmallPart' a
         | refers (nexthour a) == Same = smallpart . flipSmallParts
         | otherwise                   = smallpart

makeRomanTimeWords :: Integer -> Integer -> String
makeRomanTimeWords a b = shownSmallPart a b ++ pronoun a b ++ shownHourPart a b

horaTime :: Location -> ZonedTime -> String
horaTime l zt = (rtWords  $ romantime l zt)

horaRaw :: Location -> ZonedTime -> String
horaRaw l zt = printf "%.3f" (timepart $ romantime l zt)
      
dayOfWeek :: Location -> ZonedTime -> String
dayOfWeek l zt = dayname ++ cardinal dom ++ " of " ++ month
   where
      dd = duskday l zt
      (_, _, dom) = toGregorian dd
      month = fmtMonth dd
      dayname = fmtDayname dd ++ " the "

fmtDayname :: Day -> String
fmtDayname = formatTime defaultTimeLocale "%#A"
fmtMonth :: Day -> String
fmtMonth   = formatTime defaultTimeLocale "%#B"

