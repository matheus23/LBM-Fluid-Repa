module ColorMap where

import Data.List (genericLength)

type Color = (Double, Double, Double)
type ColorMap = [Color]

jet :: ColorMap
jet =
  [ (0.00000, 0.00000, 0.50000)
  , (0.00000, 0.00000, 0.94444)
  , (0.00000, 0.38889, 1.00000)
  , (0.00000, 0.83333, 1.00000)
  , (0.27778, 1.00000, 0.72222)
  , (0.72222, 1.00000, 0.27778)
  , (1.00000, 0.83333, 0.00000)
  , (1.00000, 0.38889, 0.00000)
  , (0.94444, 0.00000, 0.00000)
  , (0.50000, 0.00000, 0.00000) ]

colorForValueBetween :: ColorMap -> Double -> Double -> Double -> Color
colorForValueBetween colorMap minValue maxValue value = colorAtValue colorMap t
  where t = (value - minValue) / (maxValue - minValue)

colorAtValue :: ColorMap -> Double -> Color
colorAtValue colorMap value
  | value >= 1 = last colorMap
  | value <= 0 = head colorMap
  | otherwise = interpolateColors t (colorMap !! index) (colorMap !! (index + 1))
  where
    findex = value * (genericLength colorMap - 1)
    index = floor findex
    t = findex - fromIntegral index

interpolateColors :: Double -> Color -> Color -> Color
interpolateColors t (r0, g0, b0) (r1, g1, b1) =
  (interpolate t r0 r1, interpolate t g0 g1, interpolate t b0 b1)

interpolate :: Double -> Double -> Double -> Double
interpolate t v0 v1 = (1 - t) * v0 + t * v1
