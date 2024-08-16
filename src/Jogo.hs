module Jogo where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data1

window :: Display
window = InWindow "Bad Window" (700 , 700) (100, 140)

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

-- as proximas funções definem as posições dos objetos
posicoesLojas :: [(Float, Float)]
posicoesLojas = [(200,100),(-150,-150),(150,-150),(-200,100),(0,220)]

posicoesTextos :: [(Float, Float)]
posicoesTextos = [(-50, 300), (130, 165), (180, -70), (-200, -70), 
                  (-270, 165), (-10,90), (-340, -260), (0, -260)]

posicaoAzulejoInicial :: [(Float, Float)]
posicaoAzulejoInicial = [(-35,235),(10,235),(-35,185),(10,185)]

posicoesAzulejos :: [[(Float, Float)]]
posicoesAzulejos = [posicaoAzulejoInicial,
                    transladar posicaoAzulejoInicial 200 (-120),
                    transladar posicaoAzulejoInicial 150 (-370),
                    transladar posicaoAzulejoInicial (-150) (-370),
                    transladar posicaoAzulejoInicial (-200) (-120)
                    ]

posicoesAzulejosCentro :: [(Float, Float)]
posicoesAzulejosCentro = [(-70,60),(-30,60),(10,60),(50,60),
                    (-70,10),(-30,10),(10,10),(50,10),
                    (-70,-40),(-30,-40),(10,-40),(50,-40),
                    (-55,-90),(-15,-90),(25,-90)]

posicoesJ1 :: [(Float, Float)]
posicoesJ1 = [(-340,-300),(-305,-300),(-270,-300),(-235,-300),(-200,-300),(-165,-300),(-130,-300),(-95,-300),
              (-340,-340),(-305,-340),(-270,-340),(-235,-340),(-200,-340),(-165,-340),(-130,-340)]

posicoesJ2 :: [(Float, Float)]
posicoesJ2 = transladar posicoesJ1 340 0

-- essa função será usada, a cada rodada, para receber as cores presentes nos expositores, no centro, e nas 'mãos' de cada jogador
-- a função também posiciona os elementos para representar o estado do jogo a cada rodada.
tabuleiroLojas :: [[Cor]] -> [Cor] -> [Cor] -> [Cor] -> Picture
tabuleiroLojas expo cent j1 j2 = 
    pictures ([translate x y (scale 0.2 0.2 (text num)) | ((x , y) , num) <- zip posicoesTextos textos]
    ++ [translate x y loja | (x,y) <- posicoesLojas]
    ++ [translate 0 0 centro] 
    ++ [translate x y azulejo | (x, y, azulejo) <- juntaPosicoesCores posicoesAzulejos (corParaAzulejoList expo)] 
    ++ [translate x y azulejo | ((x, y), azulejo) <- zip posicoesAzulejosCentro $ map corParaAzulejo cent]
    ++ [translate x y azulejo | ((x, y), azulejo) <- zip posicoesJ1 $ map corParaAzulejo j1]
    ++ [translate x y azulejo | ((x, y), azulejo) <- zip posicoesJ2 $ map corParaAzulejo j2])