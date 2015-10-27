module Main (main)
where

import Data.Time.LocalTime
import Graphics.UI.Gtk
import System.Environment
import TimeToString
import TimeToHoratime
import Coordinates
import Control.Monad.Trans (liftIO)
import Utils

import qualified Dock as D


main :: IO ()
main = do
   _ <- initGUI
   applyTheme
   mainWindow
   mainGUI
   where
      applyTheme = getEnv "APP_DIR" >>= \appdir -> rcParse (appdir ++ "/theme.gtkrc")

mainWindow :: IO ()
mainWindow = do
   window <- windowNew
   _ <- addButtons melbourne window
   set window [ windowOpacity         := 0.75,
                windowSkipTaskbarHint := True,
                windowTypeHint        := WindowTypeHintDock,
                windowSkipPagerHint   := True,
                windowFocusOnMap      := False,
                windowAcceptFocus     := False,
                windowDecorated       := False ]
   windowSetKeepAbove window True
   windowStick window
   _ <- window `on` destroyEvent $ tryEvent $ liftIO mainQuit
   _ <- window `on` buttonPressEvent $ quitter
   _ <- onRealize window $
      D.setStrutProperties window (D.Box 0 0 100 0) (D.Line 0 0) (D.Line 0 0) (D.Line 100 100) (D.Line 0 0)
   widgetShowAll window

data WindowProperties = WindowProperties [ButtonProperties]
data ButtonProperties = ButtonProperties { 
   propsLabels :: [(Label, HandlerId)], 
   propsButton :: Button
}

addButtons :: Location -> Window -> IO WindowProperties
addButtons l window = do
   oclockPair  <- makeLabelledButton [ fnMaker $ getTimestring timeToWords ,
                                       fnMaker $ getTimestring timeToDigits ]
   datePair    <- makeLabelledButton [ fnMaker $ getDatestring l dayOfWeek ]
   horaPair    <- makeLabelledButton [ fnMaker $ getHorastring l horaFormatted, 
                                       fnMaker $ getHorastring l horaRaw ]
   setWindowSizeFromLabelSize window (propsButton oclockPair)
   addToDock window [oclockPair, datePair, horaPair]
   return $ WindowProperties [oclockPair, datePair, horaPair]
   where
      fnMaker fn = \label -> do fn >>= labelSetText label; return True

setWindowSizeFromLabelSize :: (WidgetClass s ) => Window -> s -> IO()
setWindowSizeFromLabelSize window label = do
   (width, height) <- getDimensions label
   windowSetDefaultSize window width (height+10)

makeLabelledButton :: [(Label -> IO Bool)] -> IO ButtonProperties
makeLabelledButton labelFunctions = do
   labels <- mapM makeTimerButton labelFunctions
   button <- buttonNew
   addFirstLabelToButton labels button
   let props = ButtonProperties labels button
   _ <- button `on` buttonPressEvent $ quitter
   _ <- button `on` buttonPressEvent $ buttonHandler props
   return props
   where
      addFirstLabelToButton (label:_) button = containerAdd button (fst label)
      addFirstLabelToButton _         _      = return ()

makeTimerButton :: (Label -> IO Bool) -> IO (Label, HandlerId)
makeTimerButton fn = do
   label     <- (labelNew::Maybe String -> IO Label) Nothing
   _         <- fn label
   handlerId <- timeoutAdd (fn label) 3000
   return (label, handlerId)

buttonHandler :: ButtonProperties -> EventM EButton Bool
buttonHandler buttonProperties = tryEvent $ do
   LeftButton  <- eventButton
   SingleClick <- eventClick
   _ <- liftIO $ timeoutAdd (resetOriginalLabel buttonProperties) (250 * 1000)
   liftIO $ toggleVisible (propsButton buttonProperties) (map (toWidget . fst) $ propsLabels buttonProperties)
   where
      resetOriginalLabel (ButtonProperties (label:_) button) = do
         containerForeach button (containerRemove button)
         set button [containerChild := fst label]
         widgetShowAll button
         return False
      resetOriginalLabel _ = return False



toggleVisible :: Button -> [Widget] -> IO ()
toggleVisible button labels = do
   containees <- containerGetChildren button
   containerForeach button (containerRemove button)
   mapM_ (\c -> set button [containerChild := c]) $ newContainee labels containees
   widgetShowAll button
   where
      newContainee []           c = c 
      newContainee ll@(first:_) c = [newContainee' first ll c]
      newContainee' firstLabel []                     _        = firstLabel
      newContainee' firstLabel (_:[])                 _        = firstLabel
      newContainee' firstLabel (topLabel:newLabel:ll) oldLabel
         | topLabel `elem` oldLabel = newLabel
         | otherwise                = newContainee' firstLabel (newLabel:ll) oldLabel

quitter :: EventM EButton Bool
quitter = tryEvent $ do
   MiddleButton <- eventButton
   TripleClick  <- eventClick
   liftIO $ mainQuit
   
addToDock window items = do
   hbox <- hBoxNew True 10
   boxPack hbox (map propsButton items)
   set window [ containerChild := hbox ]
   where
      boxPack _    []     = return ()
      boxPack hbox (a:[]) = do boxPackEnd hbox a PackGrow 0; return ()
      boxPack hbox (a:as) = do boxPackStart hbox a PackGrow 0; boxPack hbox as

getDimensions :: (WidgetClass s) => s -> IO (Int, Int)
getDimensions widget = do
   (Requisition _ height) <- widgetSizeRequest widget
   width                  <- screenWidth
   return ( (width < 2000 ? width - 46 $ width `div` 2), height ) 

getTimestring :: (TimeOfDay -> String) -> IO String
getTimestring fn = getZonedTime >>= (return . localTimeOfDay . zonedTimeToLocalTime) >>= (return . fn) 

timeToWords :: TimeOfDay -> String
timeToWords = (((++) "it's ") . wordyTime')
     where wordyTime' t =  wordyTime (toInteger $ todHour t) (toInteger $ todMin t)

getHorastring :: Location -> DayFormatter -> IO String
getHorastring l fn = getZonedTime >>= (return . (fn l))

horaFormatted :: DayFormatter
horaFormatted l = ((++) "it's ") . (horaTime l)

melbourne :: Location
melbourne = Location (LocationpartDMS (DMS 37 48 49) South) (LocationpartDMS (DMS 144 57 47) East)

getDatestring :: Location -> DayFormatter -> IO String
getDatestring l fn = getZonedTime >>= (return . fn l)

type DayFormatter = Location -> ZonedTime -> String
