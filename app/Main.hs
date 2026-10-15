module Main (main) where

import LMSDaltonization
import Data.Matrix (toLists)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)


main :: IO ()
main = do
    putStrLn "\n=== CC-algos-hs: LMS Daltonization ===\n"

    -- Entrada da cor original
    putStrLn "Digite os valores RGB da cor original (0-255):"

    r <- lerInteiroEntre "R: " 0 255
    g <- lerInteiroEntre "G: " 0 255
    b <- lerInteiroEntre "B: " 0 255

    -- Seleção do tipo de daltonismo
    putStrLn "\nTipo de daltonismo:"
    putStrLn "1 - Protanopia"
    putStrLn "2 - Deuteranopia"
    putStrLn "3 - Tritanopia"

    tipo <- lerInteiroEntre "Escolha: " 1 3

    -- Intensidade da simulação
    putStrLn "\nIntensidade da simulação (0-100%):"

    intensidadePct <- lerInteiroEntre "Intensidade: " 0 100

    let intensidade = fromIntegral intensidadePct / 100

    -- Construção da cor original
    case _sRGB [fromIntegral r, fromIntegral g, fromIntegral b] of
        Left erro ->
            print erro

        Right srgbOriginal -> do
            let rgblOriginal = _RGBl srgbOriginal
                lmsOriginal = _LMS rgblOriginal

            -- Simulação da deficiência visual
            case _LMSsim lmsOriginal tipo intensidade of
                Left erro ->
                    print erro

                Right lmsSimulado -> do
                    let erro = _LMSerro lmsOriginal lmsSimulado

                    -- Redistribuição do erro
                    case redist_erro erro tipo of
                        Left e ->
                            print e

                        Right rgbComp -> do
                            let rgblFinal =
                                    truncar_rgbl
                                        (rgblOriginal + rgbComp)

                                srgbFinal =
                                    transf_RGBl_sRGB rgblFinal

                                rgbOriginal8 =
                                    int_sRGB srgbOriginal

                                rgbFinal8 =
                                    int_sRGB srgbFinal

                            -- Resultado
                            putStrLn "\n=== Resultado ==="
                            putStrLn "RGB original => RGB modificado"

                            putStrLn $
                                show (toLists rgbOriginal8)
                                ++ " => "
                                ++ show (toLists rgbFinal8)


-- Leitura de inteiros com validação de intervalo
lerInteiroEntre :: String -> Int -> Int -> IO Int
lerInteiroEntre mensagem minimo maximo = do
    putStr mensagem
    hFlush stdout

    entrada <- getLine

    case readMaybe entrada of
        Just n
            | n >= minimo && n <= maximo ->
                return n

        _ -> do
            putStrLn $
                "Entrada inválida. Digite um inteiro entre "
                ++ show minimo
                ++ " e "
                ++ show maximo
                ++ "."

            lerInteiroEntre mensagem minimo maximo

