module Data where

-- Definindo as cores
data Cor = Amarelo | Azul | Branco | Vermelho | Preto deriving (Show, Enum, Bounded, Eq)