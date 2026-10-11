module LMSDaltonization where

import Data.Matrix (Matrix, fromList, mapPos)

type SRGB = Matrix Double
type RGBl = Matrix Double

-- Construtor do sRGB (normalizado)
_sRGB :: Double -> Double -> Double -> SRGB
_sRGB r g b = norm_sRGB(fromList 3 1 [r, g, b])

-- Construtor do RGBl
_RGBl :: SRGB -> RGBl
_RGBl s = transf_sRGB_inv s

-- Normaliza sRGB para ∈ [0.0; 1.0]
norm_sRGB :: SRGB -> SRGB
norm_sRGB = mapPos (\_ x -> x / 255)

-- Função de Transferência sRGB Inversa (IEC 61966-2-1:1999)
cLinear :: Double -> Double
cLinear c 
    | c <= 0.04045 = c / 12.92
    | otherwise = ((c + 0.055)/1.055) ** 2.4 -- > c > 0.04045

-- Aplica a FTsRGBI para a matriz inteira
transf_sRGB_inv :: SRGB -> RGBl
transf_sRGB_inv = mapPos (\_ c -> cLinear c)

