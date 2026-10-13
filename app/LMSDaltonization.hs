module LMSDaltonization where

import Data.Matrix (Matrix, identity, fromList, fromLists, mapPos, scaleMatrix)

-- Espaços de cor
type SRGB = Matrix Double
type RGBl = Matrix Double
type LMS = Matrix Double

-- Construtor do sRGB (normalizado)
_sRGB :: [Integer] -> SRGB
_sRGB rgb = norm_sRGB (fromList 3 1 (map fromIntegral rgb))

-- Construtor do RGBl
_RGBl :: SRGB -> RGBl
_RGBl s = transf_sRGB_inv s

-- Construtor do LMS
_LMS :: RGBl -> LMS
_LMS l = transf_RGBl_LMS l

-- Construtor do LMSsim
_LMSsim :: LMS -> Int -> Float -> LMS
_LMSsim l t i = calc_LMSsim l t i

-- Normaliza sRGB para ∈ [0.0; 1.0]
norm_sRGB :: SRGB -> SRGB
norm_sRGB = mapPos (\_ x -> x / 255)

-- Função de Transferência sRGB Inversa (IEC 61966-2-1:1999)
cLinear :: Double -> Double
cLinear c 
    | c <= 0.04045 = c / 12.92
    | otherwise = ((c + 0.055)/1.055) ** 2.4 -- > c > 0.04045

-- Aplica a FTsRGBI para a matriz inteira
transf_sRGB_inv :: SRGB -> RGBl
transf_sRGB_inv = mapPos (\_ c -> cLinear c)



-- Matriz Hunt-Pointer-Estevez
mHPE :: Matrix Double
mHPE = fromLists  [ [17.8824, 43.5161, 4.11935],
                    [3.45565, 27.1554, 3.86714],
                    [0.0299566, 0.184309, 1.46709]]

-- Função de Transformação do RGBlinear para o espaço LMS (CIE 1931 XYZ)
-- usando mLMS = mHPE
transf_RGBl_LMS :: RGBl -> LMS
transf_RGBl_LMS rgbl = mHPE * rgbl


-- Matrizes de Simulação da Dicromacia (Brettel, Viénot, Mollon. 1997)
s_Protan :: Matrix Double
s_Protan = fromLists [[0, 2.02344, -2.52581],
                      [0,    1,        0],
                      [0,    0,        1]]

s_Deutan :: Matrix Double
s_Deutan = fromLists [[   1,     0,    0],
                      [0.494207, 0, 1.24827],
                      [   0,     0,    1]]

s_Tritan :: Matrix Double
s_Tritan = fromLists [[    1,        0,     0],
                      [    0,        1,     0],
                      [-0.395913, 0.801109, 0]]

-- Cálculo da Simulação LMS por grupo (LMS_sim)
calc_LMSsim :: LMS -> Int -> Float -> LMS
calc_LMSsim lms tipo intensidade = do
    if tipo == 1 && intensidade == 1.0       then s_Protan * lms
    else if tipo == 2 && intensidade == 1.0  then s_Deutan * lms
    else if tipo == 3 && intensidade == 1.0  then s_Tritan * lms
    else if (intensidade > 0.0 && intensidade < 1.0) then
        (interpolacao_anom tipo intensidade) * lms
    else 0

-- Interpolação para Tricromacia Anômala
interpolacao_anom :: Int -> Float -> LMS
interpolacao_anom tipo alfa =
    let a = realToFrac alfa
        identidade = identity 3
    in
        if tipo == 1 then
            scaleMatrix (1 - a) identidade + scaleMatrix a s_Protan
        else if tipo == 2 then
            scaleMatrix (1 - a) identidade + scaleMatrix a s_Deutan
        else if tipo == 3 then
            scaleMatrix (1 - a) identidade + scaleMatrix a s_Tritan
        else
            identidade


