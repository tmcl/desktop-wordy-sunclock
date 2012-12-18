{-#LANGUAGE ScopedTypeVariables, ForeignFunctionInterface #-}

--import Data.Time.Clock
import Data.Time.LocalTime
import qualified Data.Text as T
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events
import System.Environment
import TimeToString
import TimeToHoratime
import Coordinates

import Foreign
import Foreign.C.Types
import Unsafe.Coerce(unsafeCoerce)

foreign import ccall "set_strut_properties"
    c_set_strut_properties :: Ptr Window -> CLong -> CLong -> CLong -> CLong
                                            -> CLong -> CLong
                                            -> CLong -> CLong
                                            -> CLong -> CLong
                                            -> CLong -> CLong
                                            -> ()

setStrutProperties :: Window -> (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) -> IO ()
setStrutProperties gtkWindow (left, right, top, bottom,
                                left_start_y, left_end_y,
                                right_start_y, right_end_y,
                                top_start_x, top_end_x,
                                bottom_start_x, bottom_end_x) = do
    let ptrWin = unsafeCoerce gtkWindow :: ForeignPtr Window
    let fi = fromIntegral
    withForeignPtr ptrWin $ \realPointer -> 
        return $ c_set_strut_properties realPointer (fi left) (fi right) (fi top) (fi bottom)
                                                        (fi left_start_y) (fi left_end_y)
                                                        (fi right_start_y) (fi right_end_y)
                                                        (fi top_start_x) (fi top_end_x)
                                                        (fi bottom_start_x) (fi bottom_end_x)


main :: IO ()
main = do
   _ <- initGUI
   mainWindow
   mainGUI


mainWindow :: IO ()
mainWindow = do
   window <- windowNew
   oclockLabel  <- labelNew Nothing
   horaLabel <- labelNew Nothing
   hbox <- hBoxNew True 10
   boxPackStart hbox oclockLabel PackGrow 0
   boxPackEnd   hbox horaLabel PackGrow 0
   _  <- timeoutAdd (timehandler oclockLabel horaLabel) 1000
   set window [ 
                containerChild := hbox 
              --, containerBorderWidth := 10
              --, windowAllowGrow := False
              --, windowResizable := False
              , windowOpacity := 0.75
              , windowSkipTaskbarHint := True
              , windowTypeHint := WindowTypeHintDock
              , windowSkipPagerHint := True
              , windowFocusOnMap := False
              , windowAcceptFocus := False
              , windowDecorated := False
              ]
   app_dir <- getEnv "APP_DIR"
   rcParse $ app_dir ++ "/theme.gtkrc"
   getTimestring >>= labelSetText oclockLabel
   windowSetKeepAbove window True
   windowStick window
   (width, height) <- getDimensions oclockLabel
   windowSetDefaultSize window width (height+10)
   _ <- onDestroy window mainQuit
   _ <- onButtonPress window quitter
   _ <- onRealize window $
      setStrutProperties window (0, 0, 100, 0,
                                 0, 0,
                                 0, 0,
                                 100, 100,
                                 0, 0)

   widgetShowAll window
   where
      quitter (Button  _ TripleClick _ _ _ _ MiddleButton  _ _) = do mainQuit; return True 
      quitter _ = return True

getDimensions :: (WidgetClass s) => s -> IO (Int, Int)
getDimensions widget = do
   (Requisition _ height) <- widgetSizeRequest widget
   width <- screenWidth
   return ( (width < 2000 ? width $ width `div` 2)-46, height ) 

timehandler :: (LabelClass s1, LabelClass s2) => s1 -> s2 -> IO Bool
timehandler oclockLabel horaLabel = do
   getTimestring >>= labelSetText oclockLabel
   getHorastring >>= labelSetText horaLabel
   return True
   
getTimestring :: IO String
getTimestring = do
   getZonedTime >>= (return . localTimeOfDay . zonedTimeToLocalTime) >>= (return . (((++) "it's ") . (T.unpack . wordyTime'))) 
      where wordyTime' t =  wordyTime (toInteger $ todHour t) (toInteger $ todMin t)

getHorastring :: IO String
getHorastring = getZonedTime >>= (return . (((++) "it's ") . (horaTime melbourne)))
	where
		melbourne = Location (LocationpartDMS (DMS 37 48 49) South) (LocationpartDMS (DMS 144 57 47) East)

