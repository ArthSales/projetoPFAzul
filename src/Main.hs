{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Main (main, Azulejos) where

import SacoDeAzulejos ( azulejosParaNum, sacoAzulejos )
import Expositores
import Parede
import Jogador1
import Data1
import Jogo
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
    ( Key(Char), KeyState(Down), Event(EventKey) )
import System.Random (RandomGen(next))
import Data.Char (digitToInt)


trataEvento :: Event -> State1 -> State1
trataEvento (EventKey (Char c) Down _ _) st@(State1 sa2 expo cm2 v j12 j22 i)
  | length newInputs == 2 = processarInputs newInputs st { inputs = [] }
  | otherwise = st { inputs = newInputs }
  where
    newInputs = i ++ [c]

    -- Função para processar a lista de dois inputs
    processarInputs :: [Char] -> State1 -> State1
    processarInputs [] estado = estado
    processarInputs [_] estado  = estado
    processarInputs (_:_:_:_) estado = estado
    processarInputs [str1, str2] estado = estado {
      expositores = novoExpo,
      cm = novoCoresCentro,
      deQuemEAVez = novaVez,
      j1 = novoJ12,
      j2 = novoJ22,
      inputs = []
    }
      where
        nloja = digitToInt str1 - 1
        cor 
          | str2 == 'a' = Amarelo
          | str2 == 'p' = Preto
          | str2 == 'b' = Branco
          | str2 == 'v' = Vermelho
          | str2 == 'z' = Azul
          | otherwise = error "Cor não identificada"

        novoExpo
          | nloja == 5 = expo
          | otherwise = dropaExpositor expo nloja
        
        novoJ12
          | nloja == 5 && v == 0 = incrementaCores j12 (compraCentroDaMesa cor cm2)
          | v == 0 = incrementaCores j12 (compraExpositor cor nloja expo)
          | otherwise = j12 -- Se não se encaixa em nenhum caso, mantém o valor antigo

        novoCoresCentro
          | nloja == 5 = dropaCorDeLsCores cor cm2
          | otherwise = if v == 0 
                        then incrementaCores (restoExpositor (last novoJ12) nloja expo) cm2
                        else incrementaCores (restoExpositor (last novoJ22) nloja expo) cm2
        novoJ22
          | nloja == 5 && v == 1 = incrementaCores j22 (compraCentroDaMesa cor cm2)
          | v == 1 = incrementaCores j22 (compraExpositor cor nloja expo)
          | otherwise = j22 -- Se não se encaixa em nenhum caso, mantém o valor antigo

        novaVez = trocaVez v

trataEvento _ state = state

trocaVez :: Int -> Int
trocaVez x | x == 0 = 1
           | otherwise = 0

-- render  :: Picture -> State1 -> Picture
-- render img (State1 saco exp1 cm1 v j11 j21) = tabuleiroLojas img exp1 cm1 j11 j21
render :: [(Cor, Picture)] -> Picture -> State1 -> Picture
render imagens tabuleiros (State1 _ exp1 cm1 _ j11 j21 i) = tabuleiroLojas imagens tabuleiros exp1 cm1 j11 j21

update :: Float -> State1 -> State1
update _ state = state

main :: IO ()
main = do
  --  let sacoInicial = sacoAzulejos []
  --      expoInicial = geraExpositores (azulejosParaNum sacoInicial 0) 20
  --      vez = 0
  --      estadoInicial = State1 sacoInicial expoInicial [] vez [] []
  --  img <- teclasBMP
  --  play
  --     (InWindow "Azul in Haskell" (1280, 720) (10, 10))  -- Cria uma janela
  --     (makeColorI 105 105 105 255)                       -- Cor de fundo
  --     30                                                 -- Número de frames por segundo
  --     estadoInicial                                      -- Estado inicial com a imagem
  --     (render img)                                       -- Função para desenhar o estado
  --     trataEvento                                        -- Função para lidar com eventos
  --     update


-- initialState :: State2
-- initialState = State2 (sacoAzulejos []) (geraExpositores (azulejosParaNum (sacoAzulejos []) 0) 20) [] 0 [] [] 0 []

-- -- data State2 = State2 {
-- --   sa1 :: Azulejos
-- --   ,expositores1 :: [[Cor]]
-- --   ,cm1 :: [Cor]
-- --   ,deQuemEAVez1 :: Int
-- --   ,j11 :: [Cor]
-- --   ,j21 :: [Cor]
-- --   ,pontj11 :: Int
-- --  -- ,picture :: Picture
-- -- } deriving Show

-- attChao :: [Chao] -> State2 -> State2
-- attChao c s@(State2 sa exp cm v j1 j2 pontj111 _) = State2 sa exp cm v j1 j2 pontj111 c

-- nextPont :: [Chao] -> State2 -> (Int,State2)
-- nextPont [] s@(State2 sa exp cm v j1 j2 pontj111 chao1) = (pontj111,s)
-- nextPont chao@(c:cs) s@(State2 sa exp cm v j1 j2 pontj111 chao1) | c == AzulejoQuebrado = (newPont , State2 sa exp cm v j1 j2 newPont chao)
--                                                 | otherwise = nextPont cs (State2 sa exp cm v j1 j2 newPont chao)
--   where
--     newPont = pontj111-1

-- -- >>>nextPont [AzulejoQuebrado,AzulejoQuebrado,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio] initialState
-- -- (-1,State2 {sa1 = [(20,Azul),(20,Amarelo),(20,Vermelho),(20,Preto),(20,Branco)], expositores1 = [[Branco,Preto,Amarelo,Vermelho],[Branco,Preto,Amarelo,Vermelho],[Branco,Branco,Amarelo,Vermelho],[Azul,Branco,Amarelo,Vermelho],[Amarelo,Branco,Amarelo,Vermelho]], cm1 = [], deQuemEAVez1 = 0, j11 = [], j21 = [], pontj11 = -1, chao1 = [AzulejoQuebrado,AzulejoQuebrado,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]})

-- -- atualizaPontChao :: [Chao] -> (Int, State2)
-- -- atualizaPontChao [] = (0, initialState)
-- -- atualizaPontChao (c:cs) | c == AzulejoQuebrado = ((- 1) pontj11 , initialState)
-- --                               | otherwise = atualizaPontChao cs

-- -- atualizaPontChaoST :: (Int,State2)
-- -- atualizaPontChaoST = state atualizaPontChao
  let sacoInicial = sacoAzulejos []
      expoInicial = geraExpositores (azulejosParaNum sacoInicial 0) 20
      vez = 0
  imagens <- carregaImagens
  tabuleiro <- loadBMP "src/assets/tabuleiro.bmp"
  let estadoInicial = State1 sacoInicial expoInicial [] vez [] [] []
  play
    (InWindow "Azul in Haskell" (1280, 720) (10, 10))  -- Cria uma janela
    (makeColorI 105 105 105 255)                       -- Cor de fundo
    30                                                 -- Número de frames por segundo
    estadoInicial                                      -- Estado inicial
    (render imagens tabuleiro)                                            -- Função para desenhar o estado
    trataEvento                                        -- Função para lidar com eventos
    update

-- Carrega as imagens baseadas nas cores
carregaImagens :: IO [(Cor, Picture)]
carregaImagens = do
  amarelo <- loadBMP "src/assets/azulejo_amarelo.bmp"
  azul <- loadBMP "src/assets/azulejo_azul.bmp"
  branco <- loadBMP "src/assets/azulejo_branco.bmp"
  vermelho <- loadBMP "src/assets/azulejo_vermelho.bmp"
  preto <- loadBMP "src/assets/azulejo_preto.bmp"
  return [(Amarelo, amarelo), (Azul, azul), (Branco, branco), (Vermelho, vermelho), (Preto, preto)]
