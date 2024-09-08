module Jogo where
import Data.Maybe
import Data.Char
import Parede
import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.Pure.Game
import Data1
import SacoDeAzulejos (azulejosParaNum)

loja :: Picture
loja = color (makeColorI 110 53 7 255) (circleSolid 80)

centro :: Picture
centro = circle 120

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
posicoesLojas = [(0,100),(-350,-150),(-50,-150),(-400,100),(-200,220)]

posicoesTextos :: [(Float, Float)]
posicoesTextos = [(-250, 300), (-70, 165), (-20, -70), (-400, -70),
                  (-470, 165), (-210,90), (-540, -260), (-200, -260),
                  (542,325), (542,-25)]

posicaoAzulejoInicial :: [(Float, Float)]
posicaoAzulejoInicial = [(-225,245),(-170,245),(-225,195),(-170,195)]

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

posicoesAzulejosPT1 :: [(Float, Float)]
posicoesAzulejosPT1 = [(295,289),
                       (295,244),(250,244),
                       (295,199),(250,199),(206,199),
                       (295,154),(250,154),(206,154),(161,154),
                       (295,107),(250,107),(206,107),(161,107),(116,107)]

posicoesAzulejosPT2 :: [(Float,Float)]
posicoesAzulejosPT2 = transladar posicoesAzulejosPT1 0 (-350)

posicaoAzulejoInicialP :: [(Float, Float)]
posicaoAzulejoInicialP = [(362,289),(408,289),(452,289),(495,289),(539,289)]

posicoesAzulejosP1 :: [(Float, Float)]
posicoesAzulejosP1 = posicaoAzulejoInicialP ++
                    transladar posicaoAzulejoInicialP 0 (-44) ++
                    transladar posicaoAzulejoInicialP 0 (-90) ++
                    transladar posicaoAzulejoInicialP 0 (-137) ++
                    transladar posicaoAzulejoInicialP 0 (-182)

posicoesAzulejosP2 :: [(Float, Float)]
posicoesAzulejosP2 = transladar posicoesAzulejosP1 0 (-350)

posicoesJ1 :: [(Float, Float)]

posicoesJ1 = [(-540,-300),(-505,-300),(-470,-300),(-435,-300),(-400,-300),(-365,-300),(-330,-300),(-295,-300),
              (-540,-340),(-505,-340),(-470,-340),(-435,-340),(-400,-340),(-365,-340),(-330,-340)]

posicoesJ2 :: [(Float, Float)]
posicoesJ2 = transladar posicoesJ1 340 0

-- Função que retorna a imagem da cor correspondente
obtemImagem :: [(Cor, Picture)] -> Cor -> Picture
obtemImagem imagens cor = case lookup cor imagens of
  Just img -> scale 0.08 0.08 img  -- Aplica um redimensionamento de 50% em ambas as direções
  Nothing -> Blank  -- Caso não encontre a imagem, retorna uma imagem em branco
  
tabuleiroLojas :: [(Cor, Picture)] -> Picture -> [[Cor]] -> [Cor] -> [[Maybe Cor]] -> [[Maybe Cor]] -> [LinhaParede] -> [LinhaParede] -> (Int, Int) -> Picture
tabuleiroLojas tuplas tabuleiros expo cent pt1 pt2 p1 p2 (pont1, pont2)=
  let azulejosExpo = map (map (obtemImagem tuplas)) expo
      azulejosCent = map (obtemImagem tuplas) cent
      azulejosPl1 =  concatMap (picturesPattern tuplas) pt1
      azulejosPl2 = concatMap (picturesPattern tuplas) pt2
      azulejosP1 = concatMap (picturesParede tuplas) p1
      azulejosP2 = concatMap (picturesParede tuplas) p2

      -- azulejosJ1 = map (obtemImagem tuplas) azj1
      -- azulejosJ2 = map (obtemImagem tuplas) azj2

      -- Combina as transformações
      textosCompl = textos ++ [show pont1] ++ [show pont2]
      textosTraduzidos = [translate x y (scale 0.2 0.2 (text num)) | ((x , y) , num) <- zip posicoesTextos textosCompl]
      lojasTraduzidas = [translate x y loja | (x, y) <- posicoesLojas]
      centroTraduzido = translate (-200) 0 centro
      tabuleiro1 = translate 330 200 (scale 0.4 0.4 tabuleiros)
      tabuleiro2 = translate 330 (-150) (scale 0.4 0.4 tabuleiros)
      azulejosTraduzidosExpo = [translate x y azulejo | (x, y, azulejo) <- juntaPosicoesCores posicoesAzulejos azulejosExpo]
      azulejosTraduzidosCent = [translate x y azulejo | ((x, y), azulejo) <- zip posicoesAzulejosCentro azulejosCent]
      azulejosTraduzidosPt1 = [translate x y azulejo | ((x, y), azulejo) <- zip posicoesAzulejosPT1 azulejosPl1]
      azulejosTraduzidosPt2 = [translate x y azulejo | ((x, y), azulejo) <- zip posicoesAzulejosPT2 azulejosPl2]
      azulejosTraduzidosP1 = [translate x y (scale 0.95 0.95 azulejo) | ((x, y), azulejo) <- zip posicoesAzulejosP1 azulejosP1]
      azulejosTraduzidosP2 = [translate x y (scale 0.95 0.95 azulejo) | ((x, y), azulejo) <- zip posicoesAzulejosP2 azulejosP2]

      -- azulejosTraduzidosJ1 = [translate x y azulejo | ((x, y), azulejo) <- zip posicoesJ1 azulejosJ1]
      -- azulejosTraduzidosJ2 = [translate x y azulejo | ((x, y), azulejo) <- zip posicoesJ2 azulejosJ2]

  -- in pictures (textosTraduzidos ++ lojasTraduzidas ++ [centroTraduzido] ++ azulejosTraduzidosExpo ++ azulejosTraduzidosCent ++ azulejosTraduzidosJ1 ++ azulejosTraduzidosJ2 ++ [tabuleiro1] ++ [tabuleiro2])
   in pictures (textosTraduzidos ++ lojasTraduzidas ++ [centroTraduzido] ++ azulejosTraduzidosExpo ++ azulejosTraduzidosCent ++ [tabuleiro1] ++ [tabuleiro2] ++ azulejosTraduzidosPt1 ++ azulejosTraduzidosPt2 ++ azulejosTraduzidosP1 ++ azulejosTraduzidosP2)

listaParede :: [(Cor,Picture)] -> (Cor,Bool) -> Picture
listaParede [] _ = Blank
listaParede _ (_,False) = Blank
listaParede imagens (c,True) = case lookup c imagens of
  Just img -> scale 0.08 0.08 img 
  Nothing -> Blank


listaPattern :: [(Cor, Picture)] -> Maybe Cor -> Picture
listaPattern imagens cor = case lookup cor imagensContexto of
  Just img -> scale 0.08 0.08 img
  Nothing -> Blank 
  where
    imagensContexto = maybeCor imagens
    maybeCor :: [(Cor, Picture)] -> [(Maybe Cor, Picture)]
    maybeCor [] = []
    maybeCor ((x,y):xs) = (Just x, y) : maybeCor xs 

patterns :: [[Maybe Cor]]
patterns = [[Nothing], 
            [Just Azul, Just Azul],
            [Just Azul, Just Azul, Just Azul],
            [Just Azul, Just Azul, Just Azul, Just Azul],
            [Just Azul, Just Azul, Just Azul, Just Azul, Just Azul]]

picturesPattern :: [(Cor, Picture)] -> [Maybe Cor] -> [Picture]
picturesPattern _ [] = []
picturesPattern imagens (x:xs) = listaPattern imagens x : picturesPattern imagens xs

picturesParede :: [(Cor, Picture)] -> LinhaParede -> [Picture]
picturesParede _ [] = []
picturesParede imagens (x:xs) = listaParede imagens x : picturesParede imagens xs
