module LMSDaltonization where

import Data.Matrix (Matrix, fromList, fromLists, mapPos)

-- Espaços de cor
type SRGB = Matrix Double
type RGBl = Matrix Double
type LMS = Matrix Double

-- Construtor do sRGB (normalizado)
_sRGB :: Double -> Double -> Double -> SRGB
_sRGB r g b = norm_sRGB(fromList 3 1 [r, g, b])

-- Construtor do RGBl
_RGBl :: SRGB -> RGBl
_RGBl s = transf_sRGB_inv s

-- Construtor do LMS
_LMS :: RGBl -> LMS
_LMS l = transf_RGBl_LMS l

-- Construtor do LMSsim
_LMSsim :: LMS -> Integer -> LMS
_LMSsim l t = calc_LMSsim l t

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
                    [0.02299566, 0.184309, 1.46709]]

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
calc_LMSsim :: LMS -> Integer -> LMS
calc_LMSsim lms tipo
    | tipo == 1 = s_Protan * lms
    | tipo == 2 = s_Deutan * lms
    | tipo == 3 = s_Tritan * lms
    | otherwise = 0


