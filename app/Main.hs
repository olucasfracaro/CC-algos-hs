module Main (main) where

import LMSDaltonization

rgb :: [Integer]
rgb = [128, 64, 5]

main :: IO ()
main = do
    let s = _sRGB rgb
        l = _RGBl s
        lms = _LMS l

    putStrLn "\nUsando LMS Daltonization...\n\n"

    putStrLn "=Amostra RGB="
    print rgb

    putStrLn "\n===Conversão sRGB==="
    print s

    putStrLn "\n===Conversão para RGBl==="
    print l

    putStrLn "\n===Conversão para LMS==="
    print lms

    case _LMSsim lms 1 1.0 of
        Left erro -> print erro

        Right lms_s -> do
            putStrLn "\n===Cálculo LMSsim (Protanopia)==="
            print lms_s

            putStrLn "\n===Cálculo LMSsim (Deuteranomalia, α = 0.8)==="
            either print print (_LMSsim lms 2 0.8)

            let lms_erro = _LMSerro lms_s lms

            putStrLn "\n====Cálculo do erro===="
            print lms_erro

            case redist_erro lms_erro 1 of
                Left erro -> print erro

                Right rgb_comp -> do
                    putStrLn "\n===Redistribuição do erro (RGB_comp)==="
                    print rgb_comp

