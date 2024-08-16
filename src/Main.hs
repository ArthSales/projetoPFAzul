module Main (main, Azulejos) where

import SacoDeAzulejos
import Expositores
import Parede
import Jogador1
import Jogador2
import Data1
import Jogo
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Char (digitToInt)
import Data.Bits (Bits(xor))
-- Define the state of the game

data State = State Picture deriving (Show, Eq)

data GameState = GameState { input1 :: Maybe Char
                           , input2 :: Maybe Char
                           , picture :: State
                           } deriving Show

main :: IO ()
main = do
   let sacoInicial = sacoAzulejos []
       expoInicial = geraExpositores (azulejosParaNum sacoInicial 0) 20
       imagem = tabuleiroLojas expoInicial [] [] []
       estadoInicial = GameState Nothing Nothing (State imagem)

   play
      (InWindow "Bad Window" (700, 700) (100, 140))  -- Cria uma janela
      white                                          -- Cor de fundo
      30                                             -- Número de frames por segundo
      estadoInicial                                 -- Estado inicial com a imagem
      desenha                                  -- Função para desenhar o estado
      (trataEvento imagem expoInicial)                          -- Função para lidar com eventos
      atualiza

desenha :: GameState -> Picture
desenha (GameState Nothing Nothing (State img)) = img
desenha (GameState (Just _) Nothing (State img)) = img
desenha (GameState Nothing (Just _) (State img)) = img
desenha (GameState (Just _) (Just _) (State img)) = img

-- trataEvento :: Picture -> [[Cor]] ->   Event -> GameState -> GameState
-- trataEvento img1 expo (EventKey (Char c) Down _ _) (State img)
--     | isNothing (input1 state) = state { input1 = Just c }
--     | isNothing (input2 state) = state { input2 = Just c }
--     | img == img1 = State (criaImagem input1 input2 expo)
--     | otherwise = State img1
-- trataEvento _ _ _  estado = estado

trataEvento :: Picture -> [[Cor]] ->   Event -> GameState -> GameState
trataEvento img1 expo (EventKey (Char c) Down _ _) (GameState _ _ img)
    | img == State img1 = GameState Nothing Nothing (State (criaImagem (Just c)  (Just c) expo))
    | otherwise = GameState Nothing Nothing (State img1)
   -- where
   --    | (input1 state) == Nothing = state { input1 = Just c }
   --    | (input2 state) == Nothing = state { input2 = Just c }
trataEvento _ _ _  estado = estado



atualiza :: Float -> GameState -> GameState
atualiza _ estado = estado

tira :: Maybe Char -> Char
tira Nothing = 'm'
tira (Just x) = x

criaImagem :: Maybe Char -> Maybe Char -> [[Cor]] -> Picture
criaImagem str1 str2 expo = tabuleiroLojas novoExpo coresCentro maoJ1 maoJ2
    where
    novoExpo
      | str1 ==  (Just 'p') || str2 == Just 'r' = [[],[]]
      | otherwise = replicate 5 (replicate 4 Amarelo)

    coresCentro = replicate 15 Preto
    maoJ1 = replicate 15 Preto
    maoJ2 = replicate 15 Preto


