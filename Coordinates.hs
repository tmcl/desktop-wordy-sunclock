module Coordinates where

data Location = Location Locationpart Locationpart

data Locationpart = LocationpartDecimal Double
                  | LocationpartDMS DMS Cardinal

data DMS = DMS Integer Integer Integer
data Cardinal = North | South | East | West
	deriving (Eq, Show)


longitude (Location _ l) = l
latitude (Location l _) = l

decimalLocationpart (LocationpartDecimal d) = d
decimalLocationpart (LocationpartDMS dms cardinal) = sign (decimaldms dms)
	where
		sign 
			| cardinal == North || cardinal == East = id
			| otherwise                             = ((-1) *) 

decimaldms (DMS degree minute second) =
	(fromInteger degree) + (((fromInteger minute)+((fromInteger second)/60)) / 60)

decimalLongitude  = decimalLocationpart . longitude 
decimalLatitude   = decimalLocationpart . latitude
