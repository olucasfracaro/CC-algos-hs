module LMSDaltonization where

import Data.Matrix (Matrix, fromList, mapPos)

type SRGB = Matrix Double
type RGBl = Matrix Double

sRGB :: Double -> Double -> Double -> SRGB
sRGB r g b = fromList 3 1 [r, g, b]

sRGB_RGBl :: SRGB -> RGBl
sRGB_RGBl = mapPos (\_ x -> x / 255)

