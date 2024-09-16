module Jogo where
import Graphics.Gloss
import Data1
import Data.Maybe (isNothing)
loja :: Picture
loja = color (makeColorI 110 53 7 255) (circleSolid 80)

lojaC :: Picture
lojaC = color (makeColorI 228 221 207 255) (circleSolid 70)

centro :: Picture
centro = circle 125

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
posicoesLojas = [(-70,70),(-420,-180),(-120,-180),(-470,70),(-270,190)]

posicoesTextos :: [(Float, Float)]
posicoesTextos = [(-320, 270), (-140, 135), (-90, -100), (-470, -100), (-540, 135), (-280,60), --numeros das lojas
                  (100, 325), (100, -25), --jogadores
                  (380,325), (380,-25), --pontuação
                  (-600,350)] --vez

posicaoAzulejoInicial :: [(Float, Float)]
posicaoAzulejoInicial = [(-295,215),(-240,215),(-295,165),(-240,165)]

posicoesAzulejos :: [[(Float, Float)]]
posicoesAzulejos = [posicaoAzulejoInicial,
                    transladar posicaoAzulejoInicial 200 (-120),
                    transladar posicaoAzulejoInicial 150 (-370),
                    transladar posicaoAzulejoInicial (-150) (-370),
                    transladar posicaoAzulejoInicial (-200) (-120)
                    ]

posicoesAzulejosCentro :: [(Float, Float)]
posicoesAzulejosCentro = [(-335,30)  ,(-290,30)  ,(-245,30)  ,(-200,30) ,
                          (-360,-20) ,(-315,-20) ,(-270,-20) ,(-225,-20), (-180,-20),
                          (-335,-70) ,(-290,-70) ,(-245,-70) ,(-200,-70),
                          (-290,-120),(-245,-120)]

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

posicoesChao1 :: [(Float, Float)]
posicoesChao1 = [(116,55),(161,55),(206,55),(250,55),(295,55),(340,55),(385,55),(430,55)]

posicoesChao2 :: [(Float, Float)]
posicoesChao2 = transladar posicoesChao1 0 (-350)

obtemImagem :: [(Cor, Picture)] -> Cor -> Picture
obtemImagem imagens cor = case lookup cor imagens of
  Just img -> scale 0.08 0.08 img
  Nothing -> Blank

chaoImagem :: Picture -> [Chao] -> [Picture]
chaoImagem _ [] = []
chaoImagem img (_:xs) = img : chaoImagem img xs

tabuleiroLojas :: [(Cor, Picture)] -> Picture -> Picture -> Picture -> [[Cor]] -> [Cor] -> [Chao] -> [Chao] -> [[Maybe Cor]] -> [[Maybe Cor]] -> [LinhaParede] -> [LinhaParede] -> (Int, Int) -> Int -> Picture
tabuleiroLojas tuplas tabuleiros azulejoQuebradoImg inst expo cent ch1 ch2 pt1 pt2 p1 p2 (pont1, pont2) vez=
  let azulejosExpo = map (map (obtemImagem tuplas)) expo
      azulejosCent = map (obtemImagem tuplas) cent
      azulejosPl1 =  concatMap (picturesPattern tuplas) pt1
      azulejosPl2 = concatMap (picturesPattern tuplas) pt2
      azulejosP1 = concatMap (picturesParede tuplas) p1
      azulejosP2 = concatMap (picturesParede tuplas) p2
      chao1Img = chaoImagem azulejoQuebradoImg ch1
      chao2Img = chaoImagem azulejoQuebradoImg ch2

      textoVez = "Vez: Jogador " ++ show  (vez+1)
      textoPont = "Pontuacao: "
      textosCompl = textos ++ [textoPont ++ show pont1] ++ [textoPont ++ show pont2] ++ [textoVez]
      textosTraduzidos = [translate x y (scale 0.2 0.2 (text num)) | ((x , y) , num) <- zip posicoesTextos textosCompl]
      lojasTraduzidas = [translate x y loja | (x, y) <- posicoesLojas]
      lojasCTraduzidas = [translate x y lojaC | (x, y) <- posicoesLojas]
      centroTraduzido = translate (-270) (-30) centro
      tabuleiro1 = translate 319 200 (scale 0.4 0.4 tabuleiros)
      tabuleiro2 = translate 319 (-150) (scale 0.4 0.4 tabuleiros)
      instrucoes = translate (-500) 250 (scale 0.5 0.5 inst)
      azulejosTraduzidosExpo = [translate x y azulejo | (x, y, azulejo) <- juntaPosicoesCores posicoesAzulejos azulejosExpo]
      azulejosTraduzidosCent = [translate x y azulejo | ((x, y), azulejo) <- zip posicoesAzulejosCentro azulejosCent]
      azulejosTraduzidosPt1 = [translate x y azulejo | ((x, y), azulejo) <- zip posicoesAzulejosPT1 azulejosPl1]
      azulejosTraduzidosPt2 = [translate x y azulejo | ((x, y), azulejo) <- zip posicoesAzulejosPT2 azulejosPl2]
      azulejosTraduzidosP1 = [translate x y (scale 0.95 0.95 azulejo) | ((x, y), azulejo) <- zip posicoesAzulejosP1 azulejosP1]
      azulejosTraduzidosP2 = [translate x y (scale 0.95 0.95 azulejo) | ((x, y), azulejo) <- zip posicoesAzulejosP2 azulejosP2]
      azulejosTraduzidosC1 = [translate x y (scale 0.08 0.08 azulejo) | ((x, y), azulejo) <- zip posicoesChao1 chao1Img]
      azulejosTraduzidosC2 = [translate x y (scale 0.08 0.08 azulejo) | ((x, y), azulejo) <- zip posicoesChao2 chao2Img]

   in pictures (textosTraduzidos ++ lojasTraduzidas ++ lojasCTraduzidas ++ [centroTraduzido] ++ azulejosTraduzidosExpo ++
                azulejosTraduzidosCent ++ [tabuleiro1] ++ [tabuleiro2] ++ azulejosTraduzidosPt1 ++
                azulejosTraduzidosPt2 ++ azulejosTraduzidosP1 ++ azulejosTraduzidosP2 ++ [instrucoes] ++
                azulejosTraduzidosC1 ++  azulejosTraduzidosC2)

listaParede :: [(Cor,Picture)] -> (Cor,Bool) -> Picture
listaParede [] _ = Blank
listaParede  _ (_,False) = Blank
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

--REGRAS DE FUNCIONAMENTO DO JOGO

jogadaPossivel :: [Cor] -> [Maybe Cor] -> Bool
jogadaPossivel [] _ = False
jogadaPossivel _ [] = False
jogadaPossivel l@(x:xs) l2@(Just y:ys) = (x == y && any isNothing l2) || jogadaPossivel xs l2
jogadaPossivel _ ys = any isNothing ys

naoHaJogadasPossiveis :: [[Cor]] -> [[Maybe Cor]] -> Bool
naoHaJogadasPossiveis [] _  = False
naoHaJogadasPossiveis _ [] = False
naoHaJogadasPossiveis (x:xs) l2@(y:ys) = any (jogadaPossivel x) l2 || naoHaJogadasPossiveis xs l2

adicionaLista :: [a] -> [[a]] -> [[a]]
adicionaLista novaLista listaDeListas = novaLista : listaDeListas

jogadaPossivelParede :: Cor -> [(Cor,Bool)] -> Bool
jogadaPossivelParede x [] = False
jogadaPossivelParede x ((c,b):cs) | x == c && not b = True
                                  | otherwise = jogadaPossivelParede x cs