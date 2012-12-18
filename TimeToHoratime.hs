{-# LANGUAGE OverloadedStrings #-}
module TimeToHoratime where


import Coordinates
import Swrapper
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime
import NumbersToWords
import Text.Printf

data RomanTime = Watch Float | Hora Float

timepart :: RomanTime -> Float
timepart (Watch x) = x
timepart (Hora x)  = x + 12

instance Show RomanTime where
	show = rtWords

--rtWords = show . timepart

rtWords :: RomanTime -> String
rtWords t = makeRomanTimeWords a b
	where
		h = timepart t + (1/24)
		a = floor h
		b = floor $ (h - (fromInteger $ floor h)) * 12

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

bigpart :: Integer -> String
bigpart x | x == 12   = "sunrise"
          | x == 18   = "midday"
          | x == 0    = "sunset"
          | x ==  6   = "midnight"
          | x <  12   = cardinal x ++ " watch"
          | x >  12   = cardinal (x-12) ++ " hour"
          | otherwise = "foob?"

data Refers = Same | Next
	deriving Eq

refers :: Integer -> Refers
refers  0 = Same
refers  6 = Same
refers 12 = Same
refers 18 = Same
refers  _ = Next

shownHourPart :: Integer -> Integer -> String
shownHourPart a b = bigpart $ hourToReference a (refers a) b
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
shownSmallPart a b 
	| length st == 0 = st
   | otherwise      = st ++ " "
	where
		st = shownSmallPart' a b
		shownSmallPart' a
			| refers (nexthour a) == Same = smallpart . flipSmallParts
			| otherwise                   = smallpart

makeRomanTimeWords :: Integer -> Integer -> String
makeRomanTimeWords a b = shownSmallPart a b ++ pronoun a b ++ shownHourPart a b

horaTime :: Location -> ZonedTime -> String
horaTime l zt = (show rt) ++ "; " ++ rawnumber --(show $ timepart rt)
	where
		rt = romantime l zt
		rawnumber = printf "%.3f" (timepart rt)
      
suntimes :: Location -> Day -> (UTCTime, UTCTime, UTCTime, UTCTime)
suntimes location day = ( sunset  yesterday location
                        , sunrise today     location
                        , sunset  today     location
                        , sunrise tomorrow  location )
   where 
      today = day
      yesterday = addDays (-1) day
      tomorrow = addDays 1 day


romantime :: Location -> ZonedTime -> RomanTime
romantime location zdt = romantime' sunriseSunsetTimes dt
   where
      dt = zonedTimeToUTC zdt
      sunriseSunsetTimes = suntimes location $ localDay $ zonedTimeToLocalTime zdt


romantime' :: (UTCTime, UTCTime, UTCTime, UTCTime) -> UTCTime -> RomanTime
romantime' (yss, tsr, tss, msr) dt
	| sinceYss > 0 && sinceTsr < 0 = watch dt yss tsr
	| sinceTsr > 0 && sinceTss < 0 = hora  dt tsr tss
   | sinceTss > 0 && sinceMsr < 0 = watch dt tss msr
   | sinceYss < 0 = Hora 100
   | otherwise    = Watch 100
	where
		sinceYss = dt `diffUTCTime` yss
		sinceTsr = dt `diffUTCTime` tsr
		sinceTss = dt `diffUTCTime` tss
		sinceMsr = dt `diffUTCTime` msr

watch :: UTCTime -> UTCTime -> UTCTime -> RomanTime
watch dt sunsetTime sunriseTime = Watch $ calc12th dt sunsetTime sunriseTime

hora :: UTCTime -> UTCTime -> UTCTime -> RomanTime
hora  dt sunriseTime sunsetTime = Hora  (calc12th dt sunriseTime sunsetTime)
 

--calc12th :: Integral b => UTCTime -> UTCTime -> UTCTime -> b
calc12th :: UTCTime -> UTCTime -> UTCTime -> Float
calc12th dt start end = timeSinceStart / twelfthLength
   where
      totalDuration = fromInteger $ floor $ end `diffUTCTime` start
      twelfthLength = totalDuration / 12
      timeSinceStart = fromInteger $ floor $ dt `diffUTCTime` start
