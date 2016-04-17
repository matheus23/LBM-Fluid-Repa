{-# LANGUAGE ExistentialQuantification #-}
module Screen where

import qualified SDL

data Screen
  = forall s. Screen
  { screenState :: s
  , updateScreen :: s -> [SDL.EventPayload] -> IO (Maybe Screen)
  , renderScreen :: SDL.Window -> SDL.Renderer -> s -> IO () }
