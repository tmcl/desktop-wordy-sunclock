{-#LANGUAGE ForeignFunctionInterface #-}

module Dock
where

import Foreign
import Foreign.C.Types
import Unsafe.Coerce(unsafeCoerce)
import Graphics.UI.Gtk(Window)

foreign import ccall "set_strut_properties"
   c_set_strut_properties :: Ptr Window -> CLong -> CLong -> CLong -> CLong
                                 -> CLong -> CLong
                                 -> CLong -> CLong
                                 -> CLong -> CLong
                                 -> CLong -> CLong
                                 -> ()

setStrutProperties :: Window -> Box -> Line -> Line -> Line -> Line -> IO()
setStrutProperties gtkWindow box left right top bottom = do
   let ptrWin = unsafeCoerce gtkWindow :: ForeignPtr Window
   let fi = fromIntegral
   withForeignPtr ptrWin $ \realPointer -> 
      return $ c_set_strut_properties realPointer 
         (fi$boxLeft box) (fi $boxRight box) (fi$boxTop box) (fi$boxBottom box)
         (fi$start left)   (fi$end left)
         (fi$start right)  (fi$end right)
         (fi$start top)    (fi$end top)
         (fi$start bottom) (fi$end bottom)

data Box  = Box  { boxLeft :: Int, boxRight :: Int, boxTop :: Int, boxBottom :: Int }
data Line = Line { start   :: Int, end :: Int }
