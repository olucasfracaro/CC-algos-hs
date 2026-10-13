module Main (main) where

import LMSDaltonization

rgb :: [Integer]
rgb = [128, 64, 5]

s :: SRGB
s = _sRGB rgb

l :: RGBl
l = _RGBl s

lms :: LMS
lms = _LMS l

-- LMSsim Protan
lms_s :: LMS
lms_s = _LMSsim lms 1 1.0

lms_erro :: LMS
lms_erro = _LMSerro lms_s lms

rgb_comp :: RGBl
rgb_comp = redist_erro lms_erro 1


main :: IO ()
main = do
    putStrLn "\nUsando LMS Daltonization...\n\n"

    putStrLn "=Amostra RGB="
    print rgb

    putStrLn "\n===Conversão sRGB==="
    print s

    putStrLn "\n===Conversão para RGBl==="
    print l

    putStrLn "\n===Conversão para LMS==="
    print lms

    putStrLn "\n===Cálculo LMSsim (Protanopia)==="
    print lms_s

    putStrLn "\n===Cálculo LMSsim (Deuteranomalia, α = 0.8)==="
    print (_LMSsim lms 2 0.8)

    putStrLn "\n====Cálculo do erro===="
    print lms_erro

    putStrLn "\n===Redistribuição do erro (RGB_comp)==="
    print rgb_comp
