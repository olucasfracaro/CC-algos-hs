module Main (main) where

import LMSDaltonization

x :: SRGB
x = SRGB 50 100 150

main :: IO ()
main = do
    putStrLn "\nUsando LMS Daltonization...\n\n"
    print(normalizarFloat (sr x))

