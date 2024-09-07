module Data1 where

import Graphics.Gloss
import Control.Monad


-- Definindo as cores
data Cor = Amarelo | Azul | Branco | Vermelho | Preto deriving (Show, Enum, Bounded, Eq)
data Chao = Vazio | AzulejoQuebrado deriving (Show, Enum, Bounded, Eq)

-- Definindo Azulejos
type Azulejos = [(Int,Cor)] --Saco de azulejos que mantém a quantidade de cada cor ainda restante
type AzulejosSeparados = (Int,Cor) --Separa azulejo pra que tenha valor numérico atrelado
type LinhaParede = [(Cor, Bool)]
type Pontuacao = Int
-- Define o estado do jogo
--newtype State = State Picture deriving (Show, Eq)
--newtype State s = State (s -> s)
-- newtype ST s = ST (s -> s)

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
  ,chao1 :: [Chao]
  ,chao2 :: [Chao]
  ,pl1 :: [[Maybe Cor]]
  ,pl2 :: [[Maybe Cor]]
  ,parede1 :: [LinhaParede]
  ,parede2 :: [LinhaParede]
  ,pontuacoes :: (Pontuacao,Pontuacao)
 -- ,picture :: Picture
} deriving Show

