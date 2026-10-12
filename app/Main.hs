module Main (main) where

import LMSDaltonization

s :: SRGB
s = _sRGB 128 64 5

l :: RGBl
l = _RGBl s

lms :: LMS
lms = _LMS l

-- LMSsim protan
lms_s :: LMS
lms_s = _LMSsim l 1

main :: IO ()
main = do
    putStrLn "\nUsando LMS Daltonization...\n\n"

    putStrLn "=====Amostra sRGB====="
    print s

    putStrLn "\n===Conversão para RGBl==="
    print l

    putStrLn "\n===Conversão para LMS==="
    print lms

    putStrLn "\n===Cálculo LMSsim (Protan)==="
    print lms_s

