{-#LANGUAGE ScopedTypeVariables, ForeignFunctionInterface #-}

module Swrapper(sunrise, sunset, dusk) where

import Data.Time
import Foreign.C.Types
import Coordinates (decimalLongitude, decimalLatitude, Location)

foreign import ccall "calcSunriseUTC"
	c_calcSunriseUTC :: CDouble -> CDouble -> CDouble -> CDouble

foreign import ccall "calcSunsetUTC"
	c_calcSunsetUTC :: CDouble -> CDouble -> CDouble -> CDouble

foreign import ccall "calcDuskUTC"
	c_calcDuskUTC :: CDouble -> CDouble -> CDouble -> CDouble

type SuntimeFunction  = Double -> Double -> Double -> Double
type CSuntimeFunction = CDouble -> CDouble -> CDouble -> CDouble

srWrap :: SuntimeFunction
srWrap = stWrap c_calcSunriseUTC
ssWrap :: SuntimeFunction
ssWrap = stWrap c_calcSunsetUTC
duskWrap :: SuntimeFunction
duskWrap = stWrap c_calcDuskUTC

stWrap :: Fractional b => CSuntimeFunction -> Double -> Double -> Double -> b
stWrap f julianDate latitude longitude = realToFrac $ f jdC latC (-lonC)
	where
		jdC  = realToFrac julianDate
		latC = realToFrac latitude
		lonC = realToFrac longitude

sunrise :: Day -> Location -> UTCTime
sunrise = suntime srWrap
sunset  :: Day -> Location -> UTCTime
sunset  = suntime ssWrap
dusk    :: Day -> Location -> UTCTime
dusk    = suntime duskWrap

suntime :: SuntimeFunction -> Day -> Location -> UTCTime
suntime f jd place = utcified $ f (toJulianDay jd) lat lon
	where
		utcified t = UTCTime jd (secondsToDiffTime $ minutesToSeconds t)
		lat = decimalLatitude place
		lon = decimalLongitude place


minutesToSeconds :: Double -> Integer
minutesToSeconds m = floor $ m*60

toJulianDay :: Day -> Double
toJulianDay d = 2400000.5 + (fromInteger $ toModifiedJulianDay d)

