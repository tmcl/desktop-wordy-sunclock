{-#LANGUAGE ScopedTypeVariables, ForeignFunctionInterface #-}

module Swrapper(sunrise, sunset) where

import Data.Time
import Foreign.C.Types
import Coordinates

foreign import ccall "calcSunriseUTC"
	c_calcSunriseUTC :: CDouble -> CDouble -> CDouble -> CDouble

foreign import ccall "calcSunsetUTC"
	c_calcSunsetUTC :: CDouble -> CDouble -> CDouble -> CDouble

srWrap = stWrap c_calcSunriseUTC
ssWrap = stWrap c_calcSunsetUTC

stWrap f julianDate latitude longitude = realToFrac $ f jdC latC lonC
	where
		jdC  = realToFrac julianDate
		latC = realToFrac latitude
		lonC = realToFrac (-longitude)

sunrise :: Day -> Location -> UTCTime
sunrise = suntime srWrap
sunset :: Day -> Location -> UTCTime
sunset  = suntime ssWrap

suntime f jd place = utcified $ f (toJulianDay jd) lat lon
	where
		utcified t = UTCTime jd (secondsToDiffTime $ minutesToSeconds t)
		lat = decimalLatitude place
		lon = decimalLongitude place


minutesToSeconds :: Double -> Integer
minutesToSeconds m = floor $ m*60
toJulianDay d = 2400000.5 + (fromInteger $ toModifiedJulianDay d)

