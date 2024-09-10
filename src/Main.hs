{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Main (main, Azulejos) where

import SacoDeAzulejos ( azulejosParaNum, sacoAzulejos )
import Expositores
import Parede
import Rodada ( nextRound )
import Data1
import Jogo
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
    ( Key(Char), KeyState(Down), Event(EventKey) )
import Data.Char (digitToInt)


trataEvento :: Event -> State2 -> State2
trataEvento (EventKey (Char c) Down _ _) st@(State2 _ expo cm v _ _ pl1 pl2 _ _ _ i)
  | length newInputs == 3 = processarInputs newInputs st { inputs = [] }
  | otherwise = st { inputs = newInputs }
  where
    newInputs = i ++ [c]

    -- Função para processar a lista de dois inputs
    processarInputs :: [Char] -> State2 -> State2
    processarInputs [] estado = estado
    processarInputs [_] estado  = estado
    processarInputs [_,_] estado = estado
    processarInputs (_:_:_:_:_) estado = estado
    processarInputs [str1, str2, str3] estado = novoEstado
      where
        nloja = digitToInt str1 - 1
        npl = digitToInt str3 - 1
        cor
          | str2 == 'a' = Amarelo
          | str2 == 'p' = Preto
          | str2 == 'b' = Branco
          | str2 == 'v' = Vermelho
          | str2 == 'z' = Azul
          | otherwise = error "Cor não identificada"
        novoEstado | v == 0 && not (naoHaJogadasPossiveis (adicionaLista cm expo) pl1) = nextRound (quebraUmAzulejo estado)
                   | v == 1 && not (naoHaJogadasPossiveis (adicionaLista cm expo) pl2) = nextRound (quebraUmAzulejo estado)
                   | nloja == 5 = nextRound (compraPraPatternLine (compraNoContexto $ compraCentroDaMesa cor cm) nloja npl estado)
                   | otherwise = nextRound (compraPraPatternLine (compraNoContexto $ compraExpositor cor nloja expo) nloja npl estado)
      --   novoExpo
      --     | nloja == 5 = expo
      --     | otherwise = dropaExpositor expo nloja

      --   novoCoresCentro
      --     | nloja == 5 = dropaCorDeLsCores cor cm2
      --     | otherwise = if v == 0 
      --                   then incrementaCores (restoExpositor (last novoJ12) nloja expo) cm2
      --                   else incrementaCores (restoExpositor (last novoJ22) nloja expo) cm2

      --   novaVez = trocaVez v

      --  novoC1 = if v == 0 then atualizaChao (restoExpositor (last $ compraExpositor cor nloja expo) nloja expo) c1 else c1

      --  novoC2 = if v == 1 then atualizaChao (restoExpositor (last $ compraExpositor cor nloja expo) nloja expo) c2 else c2

      --   novoC1 = compraPraPatternLIne

      --  novoJ12
      --    | nloja == 5 && v == 0 = incrementaCores j12 (compraCentroDaMesa cor cm2)
      --    | v == 0 = incrementaCores j12 (compraExpositor cor nloja expo)
      --    | otherwise = j12 --Se não se encaixa em nenhum caso, mantém o valor antigo


      --  novoJ22
      --    | nloja == 5 && v == 1 = incrementaCores j22 (compraCentroDaMesa cor cm2)
      --    | v == 1 = incrementaCores j22 (compraExpositor cor nloja expo)
      --    | otherwise = j22 --Se não se encaixa em nenhum caso, mantém o valor antigo

trataEvento _ state = state

-- trocaVez :: Int -> Int
-- trocaVez x | x == 0 = 1
--            | otherwise = 0

-- render  :: Picture -> State1 -> Picture
-- render img (State1 saco exp1 cm1 v j11 j21) = tabuleiroLojas img exp1 cm1 j11 j21
render :: [(Cor, Picture)] -> Picture -> Picture -> State2 -> Picture
render imagens tabuleiros azulejoQuebrado (State2 _ exp1 cm1 vez c1 c2 pt1 pt2 p1 p2 p _) = tabuleiroLojas imagens tabuleiros azulejoQuebrado exp1 cm1 c1 c2 pt1 pt2 p1 p2 p vez

update :: Float -> State2 -> State2
update _ state = state

main :: IO ()
main = do

  let sacoInicial = sacoAzulejos []
      expoInicial = geraExpositores (azulejosParaNum sacoInicial 0) 20
      vez = 0

  imagens <- carregaImagens
  tabuleiro <- loadBMP "src/assets/tabuleiro.bmp"
  azulejoQuebrado <- loadBMP "src/assets/azulejoquebrado.bmp"

  let initState = State2 sacoInicial expoInicial [] vez [] [] (criarPatternLines 5) (criarPatternLines 5) criarParede criarParede (0, 0) []
  play
    (InWindow "Azul in Haskell" (1280, 720) (10, 10))  -- Cria uma janela
    (makeColorI 105 105 105 255)                       -- Cor de fundo
    30                                                 -- Número de frames por segundo
    initState                                      -- Estado inicial
    (render imagens tabuleiro azulejoQuebrado)                                            -- Função para desenhar o estado
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
