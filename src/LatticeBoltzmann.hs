module LatticeBoltzmann where

import qualified Data.Array.Repa as Repa
import Data.Array.Repa (Z(..), (:.)(..), DIM0, DIM1, DIM2, DIM3, U, D, (!))
import qualified Data.Vector as V

w, h :: Int
w = 90
h = 50

size :: DIM2
size = Z :. w :. h

c :: Double
c = 1

tau :: Double
tau = 0.6

cSq :: Double
cSq = c * c

gridVectors :: V.Vector (Int, Int)
gridVectors = V.fromList
  [ (0, 0)
  , (1, 0)
  , (0, 1)
  , (-1, 0)
  , (0, -1)
  , (1, 1)
  , (-1, 1)
  , (-1, -1)
  , (1, -1) ]

fGridVectors :: V.Vector (Double, Double)
fGridVectors = V.map toFloat2 gridVectors

weights :: V.Vector Double
weights = V.fromList [ 4/9, 1/9, 1/9, 1/9, 1/9, 1/36, 1/36, 1/36, 1/36 ]

opposites :: V.Vector Int
opposites = V.fromList [ 0, 3, 4, 1, 2, 7, 8, 5, 6 ]

opposite :: Int -> Int
opposite a = opposites V.! a

-- V.length weights == V.length gridVectors == V.length opposites == numGridVectors
numGridVectors :: Int
numGridVectors
  | allEqual [V.length gridVectors, V.length weights, V.length opposites] = V.length weights
  | otherwise = error "You fucked up man."
  where allEqual ls = all (== head ls) ls

toFloat2 :: (Int, Int) -> (Double, Double)
toFloat2 (x, y) = (fromIntegral x, fromIntegral y)

scaleVec :: Num n => n -> (n, n) -> (n, n)
scaleVec factor (x, y) = (factor * x, factor * y)

plusVec :: Num n => (n, n) -> (n, n) -> (n, n)
plusVec (a, b) (c, d) = (a + c, b + d)

negVec :: Num n => (n, n) -> (n, n)
negVec (x, y) = (-x, -y)

subVec :: Num n => (n, n) -> (n, n) -> (n, n)
subVec vecA vecB = plusVec vecA (negVec vecB)

dot :: Num n => (n, n) -> (n, n) -> n
dot (x, y) (a, b) = x * a + y * b

type Fluid r = Repa.Array r DIM3 Double
type Densities r = Repa.Array r DIM2 Double
type Velocities r = Repa.Array r DIM2 (Double, Double)

macroDensities :: Fluid U -> IO (Densities U)
macroDensities = Repa.sumP

macroVelocities :: Fluid U -> Densities U -> Velocities D
macroVelocities fluid densities = Repa.traverse2 fluid densities (\_ s -> s) computeVelocity
  where
    computeVelocity fluidAt densityAt (Z :. x :. y) =
        scaleVec (1 / densityAt (Z :. x :. y))
          $ foldl plusVec (0, 0) (map getFluidVec [0..8])
      where
        getFluidVec n = scaleVec (fluidAt (Z :. x :. y :. n)) (fGridVectors V.! n)

equilibrium :: Densities U -> Velocities U -> Fluid D
equilibrium densities velocities = Repa.traverse2 densities velocities (\s _ -> s :. numGridVectors) computeEquilibrium
  where
    computeEquilibrium densityAt velocityAt (Z :. x :. y :. a) =
      equilibriumFormula a (densityAt (Z :. x :. y)) (velocityAt (Z :. x :. y))
        --densityAt (Z :. x :. y) * equilibriumFormula (fGridVectors V.! a) (velocityAt (Z :. x :. y))

equilibriumFormula :: Int -> Double -> (Double, Double) -> Double
equilibriumFormula a density u = weights V.! a * density *
    ( 1
    + 3 * eDotU / cSq
    + (9/2) * eDotU * eDotU / (cSq * cSq)
    - (3/2) * u `dot` u / cSq )
  where eDotU = (fGridVectors V.! a) `dot` u

collide :: Fluid U -> Fluid U -> Fluid D
collide fluid equilibrium = Repa.traverse2 fluid equilibrium const computeCollision
  where
    computeCollision fluidAt equilibriumAt pos@(pos2 :. a)
      | isSolidAt pos2 = fluidAt (pos2 :. opposite a)
      | otherwise = fluidAt pos - (fluidAt pos - equilibriumAt pos) / tau

isSolidAt :: DIM2 -> Bool
isSolidAt (Z :. x :. y) = y == 0 || y == h-1 || inSquare
  where
    c = (w `div` 5, h `div` 2)
    radius = h `div` 10 + 1
    (dx, dy) = subVec (x, y) c
    inSphere = dx * dx + dy * dy < radius * radius
    inSquare = abs dx < 2 && abs dy < radius

stream :: Fluid U -> Fluid D
stream fluid = Repa.traverse fluid id streamStep
  where
    (Z :. width :. height :. _) = Repa.extent fluid
    streamStep fluidAt (Z :. x :. y :. a) = fluidAt (Z :. rotate width refX :. rotate height refY :. a)
      where
        (refX, refY) = plusVec (x, y) (gridVectors V.! opposite a)
        rotate size pos = (pos + size) `mod` size
