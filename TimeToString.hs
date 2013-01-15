{-# LANGUAGE OverloadedStrings #-}
module TimeToString where

import qualified Data.Text as T
import NumbersToWords
import Data.Time
import System.Locale
import Utils


timeToDigits :: TimeOfDay -> String
timeToDigits = formatTime defaultTimeLocale "%l.%M %P"

wordyTime :: Integer -> Integer -> T.Text
wordyTime  0 00 = hour 0
wordyTime 12 00 = hour 12
wordyTime  h 00 = hour h `T.append` " o'clock"
wordyTime  h  5 = "five past " `T.append` hour h
wordyTime  h 10 = "ten past " `T.append` hour h
wordyTime  h 15 = "quarter past " `T.append` hour h
wordyTime  h 20 = "twenty past " `T.append` hour h
wordyTime  h 25 = "twenty-five past " `T.append` hour h
wordyTime  h 30 = "half past " `T.append` hour h
wordyTime  h 35 = "twenty-five to " `T.append` hour (nexthour h)
wordyTime  h 40 = "twenty to " `T.append` hour (nexthour h)
wordyTime  h 45 = "quarter to " `T.append` hour (nexthour h)
wordyTime  h 50 = "ten to " `T.append` hour (nexthour h)
wordyTime  h 55 = "five to " `T.append` hour (nexthour h)
wordyTime  h  m
	-- | m `elem` [  3,  4, 13, 23, 33, 48, 49 ] = wordyTime h (m-2)
	-- | m `elem` [ 57, 56, 48, 37, 27, 12, 11 ] = wordyTime h (m+2)
	| otherwise = offsetmessage `T.append` shorttime h roundedminutes
	where	
		roundedminutes = roundToNearest5 m
		offsetmessage = roundedminutes - m > 0  ? "nearly " $ "just after "

shorttime :: Integer -> Integer -> T.Text
shorttime h 00 = hour h
shorttime h 60 = hour $ (h+1) `mod` 24
shorttime h  m = wordyTime h m

roundToNearest5 :: Integral a => a -> a
roundToNearest5 m = m - remainder + offset
	where 
		remainder = m `mod` 5
		offset = remainder < 3 ? 0 $ 5

hour :: Integer -> T.Text
hour  0 = "midnight"
hour 12 = "midday"
hour x | 12 < x && x < 24 = hour (x-12)
       | otherwise = (T.pack . wordnum) x

nexthour :: Integer -> Integer
nexthour h 
	| h < 23    = h + 1
	| otherwise = 0
