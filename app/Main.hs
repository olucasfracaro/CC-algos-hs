module Main (main) where

import LMSDaltonization

s :: SRGB
s = _sRGB 128 64 5

l :: RGBl
l = _RGBl s

main :: IO ()
main = do
    putStrLn "\nUsando LMS Daltonization...\n\n"
    print s 
    print l 

