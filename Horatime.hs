{-# LANGUAGE OverloadedStrings #-}
module Horatime where


import Coordinates
import Swrapper
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime

data RomanTime = Watch Float | Hora Float

timepart :: RomanTime -> Float
timepart (Watch x) = x
timepart (Hora x)  = x + 12

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

duskday :: Location -> ZonedTime -> Day
duskday location zdt = addDays dayOffset today
	where
		today = localDay $ zonedTimeToLocalTime zdt
		dayOffset 
			| afterDark = 1
			| otherwise = 0
		afterDark = zonedTimeToUTC zdt > duskTime
		duskTime = dusk today location

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
