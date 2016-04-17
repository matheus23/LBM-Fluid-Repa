{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified SDL
import qualified Data.Array.Repa as Repa
import qualified Data.Array.Repa.Specialised.Dim2 as Repa
import Data.Array.Repa (Z(..), (:.)(..), DIM0, DIM1, DIM2, DIM3, U, D, (!))
import Data.StateVar
import Data.IORef
import qualified Data.Vector as Vector
import Control.Concurrent (threadDelay)
import Control.Monad
import Linear
import Linear.Affine

import Screen
import SDLUtils
import LatticeBoltzmann
import ColorMap

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

data Rendering = Lines | Rects

data MainScreen
  = MainScreen
  { fluid :: Repa.Array U DIM3 Double
  , velocities :: Repa.Array U DIM2 (Double, Double)
  , densities :: Repa.Array U DIM2 Double
  , rendering :: Rendering }

mainScreen :: MainScreen -> Screen
mainScreen state
  = Screen
  { screenState = state
  , updateScreen = updateMainScreen
  , renderScreen = renderMainScreen }

initialVel :: DIM2 -> (Double, Double)
initialVel (Z :. x :. y) = gravityVel
--  | 20 < x && x < 25 && 20 < y && y < 25 = (0.2, 0.2)
--  | otherwise = (0, 0)

initialDensity :: DIM3 -> Double
initialDensity (Z :. x :. y :. a) = 0.4 * weights Vector.! a
--  | 20 < x && x < 25 && 20 < y && y < 25 = 1.2 * weights Vector.! a
--  | otherwise = weights Vector.! a

initializeMainScreen :: IO Screen
initializeMainScreen = do
    fluid <- Repa.computeP $ Repa.fromFunction (size :. numGridVectors) initialDensity
    densts <- macroDensities fluid
    --vels <- Repa.computeP $ macroVelocities fluid densts
    vels <- Repa.computeP $ Repa.fromFunction size initialVel
    return $ mainScreen $ MainScreen fluid vels densts Rects

changeInVelocity :: (Int, Int) -> DIM2 -> (Double, Double)
changeInVelocity (cellx, celly) (Z :. x :. y) = scaleVec 0.1 gravityVel --smoothCircle 7 d (0.1, 0) --hardCircle 7 d (0.1, 0) --fan 13 1 d
  where
    d = (fromIntegral cellx - fromIntegral x, fromIntegral celly - fromIntegral y)

smoothCircle :: Double -> (Double, Double) -> (Double, Double) -> (Double, Double)
smoothCircle size d u = scaleVec (max 0 factor) u
  where factor = (size - magnitude d) / size

hardCircle :: Double -> (Double, Double) -> (Double, Double) -> (Double, Double)
hardCircle size d u = if magnitude d < size then u else (0, 0)

fan :: Double -> Double -> (Double, Double) -> (Double, Double)
fan size width d = scaleVec factor (rightAngle $ normalizeVec d)
  where
    factor = maxVel * max 0 (width - abs (magnitude d - size)) / width

gravityVel :: (Double, Double)
gravityVel = (0.1, 0)

updateMainScreen :: MainScreen -> [SDL.EventPayload] -> IO (Maybe Screen)
updateMainScreen state events
  | any isCloseEvent events = return Nothing
  | otherwise = do
    vels <- case lastButtonPress events of
      Just eventData -> do
        let (P (V2 mx my)) = SDL.mouseButtonEventPos eventData
        let cell = getCellFromPos (fromIntegral mx) (fromIntegral my)
        Repa.computeP $ Repa.traverse (velocities state) id
          $ \ beforeAt pos -> plusVec (changeInVelocity cell pos) (beforeAt pos)
      Nothing -> return (velocities state)

    equilibriumFluid <- Repa.computeP $ equilibrium (densities state) vels
    collidedFluid <- Repa.computeP $ collide (fluid state) equilibriumFluid
    streamedFluid <- Repa.computeP $ stream collidedFluid

    isDown <- SDL.getKeyboardState
    let rendering = if isDown SDL.ScancodeL then Lines else Rects

    Just . mainScreen <$> produceNewState streamedFluid rendering
  where
    isCloseEvent (SDL.WindowClosedEvent _) = True
    isCloseEvent _ = False

    takeRight a Nothing = a
    takeRight a (Just x) = Just x

    lastButtonPress events = foldl takeRight Nothing $ map filterMousePress events

    filterMousePress (SDL.MouseButtonEvent d) = Just d
    filterMousePress _ = Nothing

produceNewState :: Fluid U -> Rendering -> IO MainScreen
produceNewState fluid renderingState = do
    densities' <- macroDensities fluid
    velocities' <- Repa.computeP $ macroVelocities fluid densities'
    return $ MainScreen fluid velocities' densities' renderingState

res :: Int
res = 8

off :: Int
off = 10

maxVel :: Double
maxVel = 0.2

renderMainScreen :: SDL.Window -> SDL.Renderer -> MainScreen -> IO ()
renderMainScreen window renderer state = do
    SDL.rendererDrawColor renderer $= V4 0 0 0 255
    SDL.clear renderer

    (P (V2 mx my)) <- SDL.getAbsoluteMouseLocation

    let (cellx, celly) = getCellFromPos (fromIntegral mx) (fromIntegral my)
    {-
    when (Repa.isInside2 size (Z :. cellx :. celly)) $ do
      let velocity = velocities state ! (Z :. cellx :. celly)
      let density = densities state ! (Z :. cellx :. celly)

      putStrLn $ "Velocity: " ++ show velocity ++ ", density: " ++ show density
    -}

    forM_ [0..w-1] $ \ x ->
      forM_ [0..h-1] $ \ y -> do
        let (ux, uy) = velocities state ! (Z :. x :. y)
        let x0 = off + x * res
        let y0 = off + y * res
        let x1 = x0 + floor (fromIntegral res * (1 / maxVel) * ux)
        let y1 = y0 + floor (fromIntegral res * (1 / maxVel) * uy)

        let (r, g, b) = colorForValueBetween jet 0 maxVel (magnitude (ux, uy))

        if isSolidAt (Z :. x :. y)
          then do
            SDL.rendererDrawColor renderer $= V4 255 255 255 255
            let rect = SDL.Rectangle (P $ V2 (fromIntegral x0) (fromIntegral y0)) (V2 (fromIntegral res) (fromIntegral res))
            SDL.fillRect renderer (Just rect)
          else do
            SDL.rendererDrawColor renderer $= V4 (floor (255 * r)) (floor (255 * g)) (floor (255 * b)) 255

            case rendering state of
              Lines -> SDL.drawLine renderer (P $ V2 (fromIntegral x0) (fromIntegral y0)) (P $ V2 (fromIntegral x1) (fromIntegral y1))
              Rects ->
                let rect = SDL.Rectangle (P $ V2 (fromIntegral x0) (fromIntegral y0)) (V2 (fromIntegral res) (fromIntegral res))
                 in SDL.fillRect renderer (Just rect)

    SDL.present renderer

getCellFromPos :: Int -> Int -> (Int, Int)
getCellFromPos mx my = ((mx - off) `div` res, (my - off) `div` res)

magnitude :: (Double, Double) -> Double
magnitude (x, y) = sqrt (x * x + y * y)

normalizeVec :: (Double, Double) -> (Double, Double)
normalizeVec (0, 0) = (0, 0)
normalizeVec vec = scaleVec (1 / magnitude vec) vec

rightAngle :: (Double, Double) -> (Double, Double)
rightAngle (x, y) = (y, -x)
