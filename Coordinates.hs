module Coordinates where

data Location = Location Locationpart Locationpart

data Locationpart = LocationpartDecimal Double
                  | LocationpartDMS DMS Cardinal

data DMS = DMS Integer Integer Integer
data Cardinal = North | South | East | West
        deriving (Eq, Show)


longitude :: Location -> Locationpart
longitude (Location _ l) = l
latitude :: Location -> Locationpart
latitude (Location l _) = l

decimalLocationpart :: Locationpart -> Double
decimalLocationpart (LocationpartDecimal d) = d
decimalLocationpart (LocationpartDMS dms cardinal) = sign (decimaldms dms)
        where
                sign 
                        | cardinal == North || cardinal == East = id
                        | otherwise                             = ((-1) *) 

decimaldms :: Fractional a => DMS -> a
decimaldms (DMS degree minute second) =
        fromInteger degree + ((fromInteger minute+(fromInteger second/60)) / 60)

decimalLongitude :: Location -> Double
decimalLongitude  = decimalLocationpart . longitude 
decimalLatitude :: Location -> Double
decimalLatitude   = decimalLocationpart . latitude
