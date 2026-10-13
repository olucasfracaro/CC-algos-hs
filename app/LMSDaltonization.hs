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
_LMSsim :: LMS -> Int -> Float -> Either ErroDaltonizacao LMS
_LMSsim l t i = calc_LMSsim l t i

-- Construtor do LMSerro
_LMSerro :: LMS -> LMS -> LMS
_LMSerro lms lms_sim = calc_LMSerro lms lms_sim


-- Classe de erro
data ErroDaltonizacao
    = TipoDaltonismoInvalido Int
    | IntensidadeInvalida Float
    deriving (Show, Eq)



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

s_Tipo :: [Matrix Double]
s_Tipo = [s_Protan, s_Deutan, s_Tritan]

-- Cálculo da Simulação LMS por grupo (LMS_sim)
calc_LMSsim :: LMS -> Int -> Float -> Either ErroDaltonizacao LMS
calc_LMSsim lms tipo intensidade
    | tipo < 1 || tipo > 3 =
        Left (TipoDaltonismoInvalido tipo)

    | not (intensidade >= 0.0 && intensidade <= 1.0) =
        Left (IntensidadeInvalida intensidade)

    | intensidade == 1.0 =
        Right ((s_Tipo !! (tipo - 1)) * lms)
    
    -- Interpolação para Tricromacia Anômala
    | otherwise =
        let a = realToFrac intensidade
            identidade = identity 3
            simulacao = s_Tipo !! (tipo - 1)
            matriz = scaleMatrix (1 - a) identidade
                   + scaleMatrix a simulacao
        in Right (matriz * lms)



-- Cálculo do Erro Perceptivo (ΔLMS) -> RGBl
calc_LMSerro :: LMS -> LMS -> RGBl
calc_LMSerro lms lms_sim =
    transf_LMS_RGBl lms - transf_LMS_RGBl lms_sim


-- Matriz Hunt-Pointer-Estevez Inversa
mHPE_inv :: Matrix Double
mHPE_inv = fromLists   [[0.08051, -0.13051, 0.11672],
                        [-0.01024, 0.05402, -0.11361],
                        [-0.00036, -0.00412, 0.69351]]

-- Função de Transformação do espaço LMS para RGBlinear
-- usando mLMS = mHPE⁻¹
transf_LMS_RGBl :: LMS -> RGBl
transf_LMS_RGBl lms = mHPE_inv * lms


-- Matrizes E (Fidaner et al.)
e_Protan :: Matrix Double
e_Protan = fromLists  [ [ 0,  0, 0], -- Canal L
                        [0.7, 1, 0],
                        [0.7, 0, 1]]

e_Deutan :: Matrix Double
e_Deutan = fromLists  [ [ 1, 0.7, 0],
                        [ 0,  0,  0], -- Canal M
                        [ 0, 0.7, 1]]

e_Tritan :: Matrix Double
e_Tritan = fromLists  [ [ 1,  0, 0.7],
                        [ 0,  1, 0.7],
                        [ 0,  0,  0]] -- Canal S

e_Tipo :: [Matrix Double]
e_Tipo = [e_Protan, e_Deutan, e_Tritan]

-- Redistribuição do erro usando as matrizes E
redist_erro :: RGBl -> Int -> Either ErroDaltonizacao RGBl -- RGBcomp
redist_erro erro tipo
    | tipo < 1 || tipo > 3 =
        Left (TipoDaltonismoInvalido tipo)

    | otherwise =
        Right ((e_Tipo !! (tipo - 1)) * erro)

