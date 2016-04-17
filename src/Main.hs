{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified SDL
import qualified Graphics.Rendering.OpenGL as GL
import Data.StateVar
import Data.IORef
import qualified Data.Vector as Vector
import Control.Concurrent (threadDelay)
import Control.Monad
import Linear
import Linear.Affine

import Screen
import SDLUtils
import ShaderUtils

data FrameMeasure = FrameMeasure { timeOfLastPrint :: Double, framesCounted :: Int }

main :: IO ()
main = runSDL $
    withWindow "Tiny World" windowConfig $ \ window ->
      withRenderer SDL.defaultRenderer window $ \ renderer ->
        mainLoop window renderer
  where
    windowConfig = SDL.defaultWindow

fps :: Double
fps = 60

secPerFrame :: Double
secPerFrame = 1 / fps

toMicroseconds :: Double -> Int
toMicroseconds seconds = floor (seconds * 1000000)

mainLoop :: SDL.Window -> SDL.Renderer -> IO ()
mainLoop window renderer = do
    time <- SDL.time
    frameMeasure <- newIORef (FrameMeasure time 0)

    iMainScreen <- initializeMainScreen

    runLoop iMainScreen (time + secPerFrame) frameMeasure
  where
    runLoop :: Screen -> Double -> IORef FrameMeasure -> IO ()
    runLoop (Screen state updateScreen _) endingSchedule frameMeasure = do
      events <- SDL.pollEvents
      maybeScreen <- updateScreen state (map SDL.eventPayload events)

      case maybeScreen of
        Nothing -> return ()
        Just newScreen@(Screen newState _ renderScreen) -> do
          renderScreen window renderer newState

          measureFramerate frameMeasure

          nextEndingSchedule <- handleDelay endingSchedule

          runLoop newScreen nextEndingSchedule frameMeasure

    handleDelay endingSchedule = do
      time <- SDL.time
      let timeLeftToSleep = endingSchedule - time
      if timeLeftToSleep < -0.1 -- over 100 ms behind schedule
        then do
          putStrLn $ "Far behind the schedule (" ++ show timeLeftToSleep ++ ")"
          return (time + secPerFrame) -- scrap the schedule, go new one
        else do
          delayWithAccuracy timeLeftToSleep 0.0005 -- accuracy of half a millisec
          return (endingSchedule + secPerFrame)

    delayWithAccuracy delayTime accuracy
      | delayTime < accuracy = return ()
      | otherwise = do
        timeBefore <- SDL.time
        threadDelay $ toMicroseconds (delayTime * 0.9)
        timeAfter <- SDL.time
        let timeSlept = timeAfter - timeBefore
        delayWithAccuracy (delayTime - timeSlept) accuracy

measureFramerate :: IORef FrameMeasure -> IO ()
measureFramerate framerateRef = do
    (FrameMeasure lastPrintTime countedFrames) <- get framerateRef
    time <- SDL.time
    if time - lastPrintTime > 1 -- second passed
      then do
        putStrLn $ "Fps: " ++ show countedFrames ++ " (" ++ show (1000 / fromIntegral countedFrames) ++ " ms)"
        framerateRef $= FrameMeasure time 0
      else
        framerateRef $= FrameMeasure lastPrintTime (countedFrames + 1)

data MainScreen
  = MainScreen
  { joystick :: SDL.Joystick
  , shaderProgram :: GL.Program
  , joystickPos :: (Double, Double) }

mainScreen :: MainScreen -> Screen
mainScreen state
  = Screen
  { screenState = state
  , updateScreen = updateMainScreen
  , renderScreen = renderMainScreen }

initializeMainScreen :: IO Screen
initializeMainScreen = do
    joysticks <- SDL.availableJoysticks
    joystick <- SDL.openJoystick $ Vector.head joysticks
    vert <- readFile "vert.glsl"
    frag <- readFile "frag.glsl"
    program <- loadShaderProgram vert frag
    return $ mainScreen $ MainScreen joystick program (0, 0)

updateMainScreen :: MainScreen -> [SDL.EventPayload] -> IO (Maybe Screen)
updateMainScreen state events
  | any isCloseEvent events = return Nothing
  | otherwise = do
    x <- SDL.axisPosition (joystick state) 0
    y <- SDL.axisPosition (joystick state) 1
    return $ Just $ mainScreen $ state { joystickPos = (fromIntegral x / 32768, fromIntegral y / 32768) }
  where
    isCloseEvent (SDL.WindowClosedEvent _) = True
    isCloseEvent _ = False

renderMainScreen :: SDL.Window -> SDL.Renderer -> MainScreen -> IO ()
renderMainScreen window renderer state = do
    let (x, y) = joystickPos state

    SDL.rendererDrawColor renderer $= V4 0 0 0 255
    SDL.clear renderer

    SDL.rendererDrawColor renderer $= V4 255 0 0 255
    SDL.drawLine renderer (P $ V2 400 400) (P $ V2 (400 + floor (x * 100)) (400 + floor (y * 100)))

    SDL.present renderer
