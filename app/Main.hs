module Main (main) where

import LMSDaltonization

main :: IO ()
main =
    case _sRGB [50, 100, 150] of
        Left erro ->
            print erro

        Right srgb -> do
            let l = _LMS (_RGBl srgb)

            case _LMSsim l 2 1.0 of
                Left erro ->
                    print erro

                Right lmsSim -> do
                    let erro = _LMSerro l lmsSim

                    case redist_erro erro 2 of
                        Left e ->
                            print e

                        Right rgbComp -> do
                            let rgblOriginal = _RGBl srgb
                                rgblFinal = truncar_rgbl (rgblOriginal + rgbComp)
                                srgbFinal = transf_RGBl_sRGB rgblFinal
                                resultado = int_sRGB srgbFinal

                            print resultado

