module Main (main) where

import LMSDaltonization

rgb :: [Integer]
rgb = [128, 64, 5]

main :: IO ()
main = do
    let s = _sRGB rgb
        l = _RGBl s
        lms = _LMS l

    putStrLn "=== LMS Daltonization ==="

    putStrLn "=== RGB original (8 bits) ==="
    print rgb

    putStrLn "=== sRGB normalizado ==="
    print s

    putStrLn "=== RGB linear ==="
    print l

    putStrLn "=== LMS original ==="
    print lms

    -- tipo 1 = Protan; intensidade 1.0 = simulação completa.
    case _LMSsim lms 1 1.0 of
        Left erro ->
            print erro

        Right lms_sim -> do
            putStrLn "=== LMS simulado (Protanopia) ==="
            print lms_sim

            let erro = _LMSerro lms lms_sim

            putStrLn "=== Erro em RGB linear ==="
            print erro

            case redist_erro erro 1 of
                Left erroRedist ->
                    print erroRedist

                Right rgb_comp -> do
                    putStrLn "=== Erro redistribuído ==="
                    print rgb_comp

                    let rgbl_final = truncar_rgbl (l + rgb_comp)
                        srgb_final = transf_RGBl_sRGB rgbl_final
                        rgb8_final = int_sRGB srgb_final

                    putStrLn "=== RGB linear final (com clipping) ==="
                    print rgbl_final

                    putStrLn "=== sRGB final normalizado ==="
                    print srgb_final

                    putStrLn "=== RGB final (8 bits) ==="
                    print rgb8_final

