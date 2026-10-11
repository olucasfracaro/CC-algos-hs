module LMSDaltonization where

import Data.Word (Word8)

data SRGB = SRGB {
    sr :: Word8,
    sg :: Word8,
    sb :: Word8
} deriving (Show)

normalizarFloat :: Word8 -> Double
normalizarFloat x = fromIntegral x / 255

