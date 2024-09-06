module Data1 where

import Graphics.Gloss


-- Definindo as cores
data Cor = Amarelo | Azul | Branco | Vermelho | Preto deriving (Show, Enum, Bounded, Eq)
data Chao = Vazio | AzulejoQuebrado deriving (Show, Enum, Bounded, Eq)

-- Definindo Azulejos
type Azulejos = [(Int,Cor)] --Saco de azulejos que mantém a quantidade de cada cor ainda restante
type AzulejosSeparados = (Int,Cor) --Separa azulejo pra que tenha valor numérico atrelado

-- Define o estado do jogo
newtype State = State Picture deriving (Show, Eq)

data State1 = State1 {
  sa :: Azulejos
  ,expositores :: [[Cor]]
  ,cm :: [Cor]
  ,deQuemEAVez :: Int
  ,j1 :: [Cor]
  ,j2 :: [Cor]
 -- ,picture :: Picture
}