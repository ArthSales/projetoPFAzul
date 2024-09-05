module Jogo where

import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.Pure.Game
import Data1

loja :: Picture
loja = color (makeColorI 110 53 7 255) (circleSolid 80)

centro :: Picture
centro = circle 120

-- função que transforma o tipo Cor para um quadrado da respectiva cor
corParaAzulejo :: Cor -> Picture
corParaAzulejo Amarelo = color yellow $ polygon [(0,0),(25,0),(25,25),(0,25)]
corParaAzulejo Azul = color blue $ polygon [(0,0),(25,0),(25,25),(0,25)]
corParaAzulejo Branco = color white $ polygon [(0,0),(25,0),(25,25),(0,25)]
corParaAzulejo Vermelho = color red $ polygon [(0,0),(25,0),(25,25),(0,25)]
corParaAzulejo Preto = color black $ polygon [(0,0),(25,0),(25,25),(0,25)]

-- representa um azulejo vazio, para ser usado em lojas que foram compradas
azulejoVazio :: Picture
azulejoVazio = polygon [(0,0)]

-- aplica as duas funções acima para transformar uma lista de expositores em uma lista de quadrado (representação dos azulejos)
corParaAzulejoList :: [[Cor]] -> [[Picture]]
corParaAzulejoList [] = []
corParaAzulejoList ([]:xs) = [azulejoVazio,azulejoVazio,azulejoVazio,azulejoVazio] : corParaAzulejoList xs
corParaAzulejoList (xs:xss) = map corParaAzulejo xs : corParaAzulejoList xss

-- recebe uma lista com as posições de cada azulejo dentro dos expositores e uma lista com os quadrados da cor de cada
-- azulejo dentro dos expositores, transformando em uma lista de triplas com as posções x e y e os quadrados
-- (foi a unica forma que pensamos de deixar a compreensão de lista mais facil na hora de gerar a imagem)
juntaPosicoesCores :: [[(Float, Float)]] -> [[Picture]]  -> [(Float, Float, Picture)]
juntaPosicoesCores _ [] = []
juntaPosicoesCores [] _ = []
juntaPosicoesCores (xs:xss) (cs:css) = tripla xs cs ++ juntaPosicoesCores xss css
    where
        tripla :: [(Float, Float)] -> [Picture] -> [(Float, Float, Picture)]
        tripla [] _ = []
        tripla _ [] = []
        tripla ((x,y):yss) (z:zss) = (x,y,z) : tripla yss zss

-- função que move uma lista de posições inteira em relação a x e/ou y
transladar :: [(Float, Float)] -> Float -> Float -> [(Float, Float)]
transladar [] _ _ = []
transladar ((x,y):xs) x1 y1 = (x+x1,y+y1) : transladar xs x1 y1

-- recebendo os textos escritos na tela
textos :: [String]
textos = ["1", "2", "3", "4", "5", "6", "Jogador 1", "Jogador 2"]

teclasBMP :: IO Picture
teclasBMP = loadBMP "src/assets/teclas.bmp"

-- as proximas funções definem as posições dos objetos
posicoesLojas :: [(Float, Float)]
posicoesLojas = [(0,100),(-350,-150),(-50,-150),(-400,100),(-200,220)]

posicoesTextos :: [(Float, Float)]
posicoesTextos = [(-250, 300), (-70, 165), (-20, -70), (-400, -70), 
                  (-470, 165), (-210,90), (-540, -260), (-200, -260)]

posicaoAzulejoInicial :: [(Float, Float)]
posicaoAzulejoInicial = [(-235,235),(-190,235),(-235,185),(-190,185)]

posicoesAzulejos :: [[(Float, Float)]]
posicoesAzulejos = [posicaoAzulejoInicial,
                    transladar posicaoAzulejoInicial 200 (-120),
                    transladar posicaoAzulejoInicial 150 (-370),
                    transladar posicaoAzulejoInicial (-150) (-370),
                    transladar posicaoAzulejoInicial (-200) (-120)
                    ]

posicoesAzulejosCentro :: [(Float, Float)]
posicoesAzulejosCentro = [(-270,60),(-230,60),(-190,60),(-150,60),
                    (-270,10),(-230,10),(-190,10),(-150,10),
                    (-270,-40),(-230,-40),(-190,-40),(-150,-40),
                    (-255,-90),(-215,-90),(-175,-90)]

posicoesJ1 :: [(Float, Float)]

posicoesJ1 = [(-540,-300),(-505,-300),(-470,-300),(-435,-300),(-400,-300),(-365,-300),(-330,-300),(-295,-300),
              (-540,-340),(-505,-340),(-470,-340),(-435,-340),(-400,-340),(-365,-340),(-330,-340)]

posicoesJ2 :: [(Float, Float)]
posicoesJ2 = transladar posicoesJ1 340 0

-- essa função será usada, a cada rodada, para receber as cores presentes nos expositores, no centro, e nas 'mãos' de cada jogador
-- a função também posiciona os elementos para representar o estado do jogo a cada rodada.
tabuleiroLojas :: Picture -> [[Cor]] -> [Cor] -> [Cor] -> [Cor] -> Picture
tabuleiroLojas img expo cent j1 j2 = 
    pictures ([translate x y (scale 0.2 0.2 (text num)) | ((x , y) , num) <- zip posicoesTextos textos]
    ++ [translate x y loja | (x,y) <- posicoesLojas]
    ++ [translate (-200) 0 centro] 
    ++ [translate x y azulejo | (x, y, azulejo) <- juntaPosicoesCores posicoesAzulejos (corParaAzulejoList expo)] 
    ++ [translate x y azulejo | ((x, y), azulejo) <- zip posicoesAzulejosCentro $ map corParaAzulejo cent]
    ++ [translate x y azulejo | ((x, y), azulejo) <- zip posicoesJ1 $ map corParaAzulejo j1]
    ++ [translate x y azulejo | ((x, y), azulejo) <- zip posicoesJ2 $ map corParaAzulejo j2]
    ++ [translate 350 0 (scale 0.8 0.8 img)])