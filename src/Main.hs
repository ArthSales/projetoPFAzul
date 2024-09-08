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
