{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Main (main, Azulejos) where

import SacoDeAzulejos
import Expositores
import Parede
import Jogador1
import Data1
import Jogo
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Char (digitToInt)
import System.Posix.Internals (statGetType)
-- Define the state of the game

data State = State Picture deriving (Show, Eq)



-- data GameState = GameState { input1 :: Maybe Char
--                            , input2 :: Maybe Char
--                            , picture :: State
--                            } deriving Show

-- main :: IO ()
-- main = do
--    let sacoInicial = sacoAzulejos []
--        expoInicial = geraExpositores (azulejosParaNum sacoInicial 0) 20
--        imagem = tabuleiroLojas expoInicial [] [] []
--        cm = []
--        j1 = []
--        j2 = []
--        vez = 0
--        estadoInicial = State1 sacoInicial expoInicial cm vez j1 j2 imagem

--    play
--       (InWindow "Azul in Haskell" (700, 700) (100, 140))  -- Cria uma janela
--       white                                          -- Cor de fundo
--       30                                             -- Número de frames por segundo
--       estadoInicial                                 -- Estado inicial com a imagem
--       desenha                                  -- Função para desenhar o estado
--       (trataEvento imagem expoInicial)                          -- Função para lidar com eventos
--       atualiza

-- desenha :: State1 -> Picture
-- desenha (State1 sa expo cm v j1 j2 img) = img

-- -- trataEvento :: Picture -> [[Cor]] ->   Event -> GameState -> GameState
-- -- trataEvento img1 expo (EventKey (Char c) Down _ _) (State img)
-- --     | isNothing (input1 state) = state { input1 = Just c }
-- --     | isNothing (input2 state) = state { input2 = Just c }
-- --     | img == img1 = State (criaImagem input1 input2 expo)
-- --     | otherwise = State img1
-- -- trataEvento _ _ _  estado = estado

-- -- trataEvento :: Picture -> [[Cor]] -> Event -> State1 -> State1
-- -- trataEvento img1 expo (EventKey (Char c) Down _ _) estado@(State1 sa expo1 cm v j1 j2 img)
-- --     | img == img1 =  State1 sa expo1 cm v j1 j2 (criaImagem c expo)
-- --     | otherwise = State1 sa expo1 cm v j1 j2 img1
-- --    -- where
-- --    --    | (input1 state) == Nothing = state { input1 = Just c }
-- --    --    | (input2 state) == Nothing = state { input2 = Just c }
-- -- trataEvento _ _ _  estado = estado
-- trataEvento :: Event -> State1 -> State1
-- trataEvento (EventKey (Char c) Down _ _) estado@(State1 sa expo1 cm v j1 j2 img)
--     | img == img1 =  State1 sa expo1 cm v j1 j2 (criaImagem c expo)
--     | otherwise = State1 sa expo1 cm v j1 j2 img1
--    -- where
--    --    | (input1 state) == Nothing = state { input1 = Just c }
--    --    | (input2 state) == Nothing = state { input2 = Just c }
-- trataEvento _ _ _  estado = estado


-- atualiza :: Float -> State1 -> State1
-- atualiza _ estado@(State1 sa expo cm v j1 j2 picture) = estado


-- criaImagem :: Char -> [[Cor]] -> Picture
-- criaImagem str1 expo | maoJ1 == [Azul,Azul,Azul,Azul,Azul] = tabuleiroLojas expo [] [] maoJ2
--                      | otherwise = tabuleiroLojas novoExpo coresCentro maoJ1 maoJ2
--     where
--     novoExpo
--       | str1 == 'q' = dropaExpositor expo 0
--       | str1 == 'w' = dropaExpositor expo 0
--       | str1 == 'e' = dropaExpositor expo 0
--       | str1 == 'r' = dropaExpositor expo 0
--       | str1 == 't' = dropaExpositor expo 0
--       | str1 == 'a' = dropaExpositor expo 1
--       | str1 == 's' = dropaExpositor expo 1
--       | str1 == 'd' = dropaExpositor expo 1
--       | str1 == 'f' = dropaExpositor expo 1
--       | str1 == 'g' = dropaExpositor expo 1
--       | str1 == 'z' = dropaExpositor expo 2
--       | str1 == 'x' = dropaExpositor expo 2
--       | str1 == 'c' = dropaExpositor expo 2
--       | str1 == 'v' = dropaExpositor expo 2
--       | str1 == 'b' = dropaExpositor expo 2
--       | str1 == 'y' = dropaExpositor expo 3
--       | str1 == 'u' = dropaExpositor expo 3
--       | str1 == 'i' = dropaExpositor expo 3
--       | str1 == 'o' = dropaExpositor expo 3
--       | str1 == 'p' = dropaExpositor expo 3
--       | str1 == 'h' = dropaExpositor expo 4
--       | str1 == 'j' = dropaExpositor expo 4
--       | str1 == 'k' = dropaExpositor expo 4
--       | str1 == 'l' = dropaExpositor expo 4
--       | str1 == 'n' = dropaExpositor expo 4
--       | otherwise = replicate 5 (replicate 4 Amarelo)
--     maoJ1
--       | str1 == 'q' = compraExpositor Amarelo 0 expo
--       | str1 == 'w' = compraExpositor Azul 0 expo
--       | str1 == 'e' = compraExpositor Branco 0 expo
--       | str1 == 'r' = compraExpositor Vermelho 0 expo
--       | str1 == 't' = compraExpositor Preto 0 expo
--       | str1 == 'a' = compraExpositor Amarelo 1 expo
--       | str1 == 's' = compraExpositor Azul 1 expo
--       | str1 == 'd' = compraExpositor Branco 1 expo
--       | str1 == 'f' = compraExpositor Vermelho 1 expo
--       | str1 == 'g' = compraExpositor Preto 1 expo
--       | str1 == 'z' = compraExpositor Amarelo 2 expo
--       | str1 == 'x' = compraExpositor Azul 2 expo
--       | str1 == 'c' = compraExpositor Branco 2 expo
--       | str1 == 'v' = compraExpositor Vermelho 2 expo
--       | str1 == 'b' = compraExpositor Preto 2 expo
--       | str1 == 'y' = compraExpositor Amarelo 3 expo
--       | str1 == 'u' = compraExpositor Azul 3 expo
--       | str1 == 'i' = compraExpositor Branco 3 expo
--       | str1 == 'o' = compraExpositor Vermelho 3 expo
--       | str1 == 'p' = compraExpositor Preto 3 expo
--       | str1 == 'h' = compraExpositor Amarelo 4 expo
--       | str1 == 'j' = compraExpositor Azul 4 expo
--       | str1 == 'k' = compraExpositor Branco 4 expo
--       | str1 == 'l' = compraExpositor Vermelho 4 expo
--       | str1 == 'n' = compraExpositor Preto 4 expo
--       | otherwise = []
--     coresCentro = restoExpositor (head maoJ1) 0 expo
--     maoJ2 = []
--     -- maoJ1
--     --   | str1 == 'p' = compraExpositor Preto 0 expo
--     --   | otherwise = []
--     -- maoJ2
--     --   | str1 == 'p' = compraExpositor Preto 0 expo
--     --   | otherwise = []
--     -- coresCentro = restoExpositor (head maoJ1) 0 expo
data State1 = State1 {
  sa :: Azulejos
  ,expositores :: [[Cor]]
  ,cm :: [Cor]
  ,deQuemEAVez :: Int
  ,j1 :: [Cor]
  ,j2 :: [Cor]
 -- ,picture :: Picture
}

--  maoJ1 == [Azul,Azul,Azul,Azul,Azul] = State1 sa2 expo cm2 v j12 j22
--                                                                                    | otherwise 
trataEvento :: Event -> State1 -> State1
trataEvento (EventKey (Char str1) Down _ _) estado@(State1 sa2 expo cm2 v j12 j22) 
  | take 5 (reverse maoJ1) == [Azul,Azul,Azul,Azul,Azul] && v == 0 = State1 sa2 expo cm2 v j12 maoJ2
  | take 5 (reverse maoJ2) == [Azul,Azul,Azul,Azul,Azul] && v == 1 = State1 sa2 expo cm2 v maoJ1 j22
  | otherwise = State1 sa2 novoExpo coresCentro vez maoJ1 maoJ2
  where
    novoExpo
      | str1 == 'q' = dropaExpositor expo 0
      | str1 == 'w' = dropaExpositor expo 0
      | str1 == 'e' = dropaExpositor expo 0
      | str1 == 'r' = dropaExpositor expo 0
      | str1 == 't' = dropaExpositor expo 0
      | str1 == 'a' = dropaExpositor expo 1
      | str1 == 's' = dropaExpositor expo 1
      | str1 == 'd' = dropaExpositor expo 1
      | str1 == 'f' = dropaExpositor expo 1
      | str1 == 'g' = dropaExpositor expo 1
      | str1 == 'z' = dropaExpositor expo 2
      | str1 == 'x' = dropaExpositor expo 2
      | str1 == 'c' = dropaExpositor expo 2
      | str1 == 'v' = dropaExpositor expo 2
      | str1 == 'b' = dropaExpositor expo 2
      | str1 == 'y' = dropaExpositor expo 3
      | str1 == 'u' = dropaExpositor expo 3
      | str1 == 'i' = dropaExpositor expo 3
      | str1 == 'o' = dropaExpositor expo 3
      | str1 == 'p' = dropaExpositor expo 3
      | str1 == 'h' = dropaExpositor expo 4
      | str1 == 'j' = dropaExpositor expo 4
      | str1 == 'k' = dropaExpositor expo 4
      | str1 == 'l' = dropaExpositor expo 4
      | str1 == 'n' = dropaExpositor expo 4
      | otherwise = expo
    maoJ1
      | str1 == 'q' && v == 0 = incrementaCores j12 (compraExpositor Amarelo 0 expo)
      | str1 == 'w' && v == 0 = incrementaCores j12 (compraExpositor Azul 0 expo)
      | str1 == 'e' && v == 0 = incrementaCores j12 (compraExpositor Branco 0 expo)
      | str1 == 'r' && v == 0 = incrementaCores j12 (compraExpositor Vermelho 0 expo)
      | str1 == 't' && v == 0 = incrementaCores j12 (compraExpositor Preto 0 expo)
      | str1 == 'a' && v == 0 = incrementaCores j12 (compraExpositor Amarelo 1 expo)
      | str1 == 's' && v == 0 = incrementaCores j12 (compraExpositor Azul 1 expo)
      | str1 == 'd' && v == 0 = incrementaCores j12 (compraExpositor Branco 1 expo)
      | str1 == 'f' && v == 0 = incrementaCores j12 (compraExpositor Vermelho 1 expo)
      | str1 == 'g' && v == 0 = incrementaCores j12 (compraExpositor Preto 1 expo)
      | str1 == 'x' && v == 0 = incrementaCores j12 (compraExpositor Azul 2 expo)
      | str1 == 'c' && v == 0 = incrementaCores j12 (compraExpositor Branco 2 expo)
      | str1 == 'z' && v == 0 = incrementaCores j12 (compraExpositor Amarelo 2 expo)
      | str1 == 'v' && v == 0 = incrementaCores j12 (compraExpositor Vermelho 2 expo)
      | str1 == 'b' && v == 0 = incrementaCores j12 (compraExpositor Preto 2 expo)
      | str1 == 'y' && v == 0 = incrementaCores j12 (compraExpositor Amarelo 3 expo)
      | str1 == 'u' && v == 0 = incrementaCores j12 (compraExpositor Azul 3 expo)
      | str1 == 'i' && v == 0 = incrementaCores j12 (compraExpositor Branco 3 expo)
      | str1 == 'o' && v == 0 = incrementaCores j12 (compraExpositor Vermelho 3 expo)
      | str1 == 'p' && v == 0 = incrementaCores j12 (compraExpositor Preto 3 expo)
      | str1 == 'h' && v == 0 = incrementaCores j12 (compraExpositor Amarelo 4 expo)
      | str1 == 'j' && v == 0 = incrementaCores j12 (compraExpositor Azul 4 expo)
      | str1 == 'k' && v == 0 = incrementaCores j12 (compraExpositor Branco 4 expo)
      | str1 == 'l' && v == 0 = incrementaCores j12 (compraExpositor Vermelho 4 expo)
      | str1 == 'n' && v == 0 = incrementaCores j12 (compraExpositor Preto 4 expo)
      | str1 == '1' && v == 0 = incrementaCores j12 (compraCentroDaMesa Amarelo cm2)
      | str1 == '2' && v == 0 = incrementaCores j12 (compraCentroDaMesa Azul cm2)
      | str1 == '3' && v == 0 = incrementaCores j12 (compraCentroDaMesa Branco cm2)
      | str1 == '4' && v == 0 = incrementaCores j12 (compraCentroDaMesa Vermelho cm2)
      | str1 == '5' && v == 0 = incrementaCores j12 (compraCentroDaMesa Preto cm2)
      | otherwise = j12
    coresCentro
      | str1 == '1' = dropaCorDeLsCores Amarelo cm2
      | str1 == '2' = dropaCorDeLsCores Azul cm2
      | str1 == '3' = dropaCorDeLsCores Branco cm2
      | str1 == '4' = dropaCorDeLsCores Vermelho cm2
      | str1 == '5' = dropaCorDeLsCores Preto cm2
      | otherwise = if v == 0 then incrementaCores cm2 (restoExpositor (last maoJ1) 0 expo) else incrementaCores cm2 (restoExpositor (last maoJ2) 0 expo)
    maoJ2
      | str1 == 'q' && v == 1 = incrementaCores j22 (compraExpositor Amarelo 0 expo)
      | str1 == 'w' && v == 1 = incrementaCores j22 (compraExpositor Azul 0 expo)
      | str1 == 'e' && v == 1 = incrementaCores j22 (compraExpositor Branco 0 expo)
      | str1 == 'r' && v == 1 = incrementaCores j22 (compraExpositor Vermelho 0 expo)
      | str1 == 't' && v == 1 = incrementaCores j22 (compraExpositor Preto 0 expo)
      | str1 == 'a' && v == 1 = incrementaCores j22 (compraExpositor Amarelo 1 expo)
      | str1 == 's' && v == 1 = incrementaCores j22 (compraExpositor Azul 1 expo)
      | str1 == 'd' && v == 1 = incrementaCores j22 (compraExpositor Branco 1 expo)
      | str1 == 'f' && v == 1 = incrementaCores j22 (compraExpositor Vermelho 1 expo)
      | str1 == 'g' && v == 1 = incrementaCores j22 (compraExpositor Preto 1 expo)
      | str1 == 'x' && v == 1 = incrementaCores j22 (compraExpositor Azul 2 expo)
      | str1 == 'c' && v == 1 = incrementaCores j22 (compraExpositor Branco 2 expo)
      | str1 == 'z' && v == 1 = incrementaCores j22 (compraExpositor Amarelo 2 expo)
      | str1 == 'v' && v == 1 = incrementaCores j22 (compraExpositor Vermelho 2 expo)
      | str1 == 'b' && v == 1 = incrementaCores j22 (compraExpositor Preto 2 expo)
      | str1 == 'y' && v == 1 = incrementaCores j22 (compraExpositor Amarelo 3 expo)
      | str1 == 'u' && v == 1 = incrementaCores j22 (compraExpositor Azul 3 expo)
      | str1 == 'i' && v == 1 = incrementaCores j22 (compraExpositor Branco 3 expo)
      | str1 == 'o' && v == 1 = incrementaCores j22 (compraExpositor Vermelho 3 expo)
      | str1 == 'p' && v == 1 = incrementaCores j22 (compraExpositor Preto 3 expo)
      | str1 == 'h' && v == 1 = incrementaCores j22 (compraExpositor Amarelo 4 expo)
      | str1 == 'j' && v == 1 = incrementaCores j22 (compraExpositor Azul 4 expo)
      | str1 == 'k' && v == 1 = incrementaCores j22 (compraExpositor Branco 4 expo)
      | str1 == 'l' && v == 1 = incrementaCores j22 (compraExpositor Vermelho 4 expo)
      | str1 == 'n' && v == 1 = incrementaCores j22 (compraExpositor Preto 4 expo)
      | str1 == '1' && v == 1 = incrementaCores j22 (compraCentroDaMesa Amarelo cm2)
      | str1 == '2' && v == 1 = incrementaCores j22 (compraCentroDaMesa Azul cm2)
      | str1 == '3' && v == 1 = incrementaCores j22 (compraCentroDaMesa Branco cm2)
      | str1 == '4' && v == 1 = incrementaCores j22 (compraCentroDaMesa Vermelho cm2)
      | str1 == '5' && v == 1 = incrementaCores j22 (compraCentroDaMesa Preto cm2)
      | otherwise = j22
    vez = trocaVez v
  -- in State1 sa2 novoExpo coresCentro v maoJ1 maoJ2
trataEvento _ state = state

trocaVez :: Int -> Int
trocaVez x | x == 0 = 1
           | x >= 1 = 0

render  :: State1 -> Picture
render (State1 saco exp1 cm1 v j11 j21) = tabuleiroLojas exp1 cm1 j11 j21

update :: Float -> State1 -> State1
update _ state = state

main :: IO ()
main = do
   let sacoInicial = sacoAzulejos []
       expoInicial = geraExpositores (azulejosParaNum sacoInicial 0) 20
      --  imagem = tabuleiroLojas expoInicial [] [] []
      --  cm = []
      --  j1 = []
      --  j2 = []
       vez = 0
       estadoInicial = State1 sacoInicial expoInicial [] vez [] []

   play
      (InWindow "Azul in Haskell" (700, 700) (100, 140))  -- Cria uma janela
      white                                          -- Cor de fundo
      30                                             -- Número de frames por segundo
      estadoInicial                                 -- Estado inicial com a imagem
      render                                  -- Função para desenhar o estado
      trataEvento                          -- Função para lidar com eventos
      update
