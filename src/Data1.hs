

module Data1 where

import Graphics.Gloss


-- Definindo as cores
data Cor = Amarelo | Azul | Branco | Vermelho | Preto deriving (Show, Enum, Bounded, Eq)
data Chao = Vazio | AzulejoQuebrado deriving (Show, Enum, Bounded, Eq)

-- Definindo Azulejos
type Azulejos = [(Int,Cor)] --Saco de azulejos que mantém a quantidade de cada cor ainda restante
type AzulejosSeparados = (Int,Cor) --Separa azulejo pra que tenha valor numérico atrelado
type LinhaParede = [(Cor, Bool)]

-- Define o estado do jogo
--newtype State = State Picture deriving (Show, Eq)
newtype State s a = State (s -> (a, s))

data State1 = State1 {
  sa :: Azulejos
  ,expositores :: [[Cor]]
  ,cm :: [Cor]
  ,deQuemEAVez :: Int
  ,j1 :: [Cor]
  ,j2 :: [Cor]
 -- ,picture :: Picture
}

data State2 = State2 {
  sa1 :: Azulejos
  ,expositores1 :: [[Cor]]
  ,cm1 :: [Cor]
  ,deQuemEAVez1 :: Int
  ,j11 :: [Cor]
  ,j21 :: [Cor]
  ,pontj11 :: Int
  ,chao1 :: [Chao]
 -- ,picture :: Picture
} deriving Show

type GameState = State1
type GameST a = State GameState a