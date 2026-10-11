module Main (main) where

import LMSDaltonization

x :: SRGB
x = sRGB 128 64 150

main :: IO ()
main = do
    putStrLn "\nUsando LMS Daltonization...\n\n"
    print(sRGB_RGBl x)

