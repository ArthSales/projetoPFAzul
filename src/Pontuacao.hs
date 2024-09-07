module Pontuacao where

import Data1
import Parede

attSTPontuacao :: StateST -> [Chao] -> (Int,StateST)
attSTPontuacao = state contaAzulejosQuebrados

contaAzulejosQuebrados :: [Chao] -> Int
contaAzulejosQuebrados [] = 0
contaAzulejosQuebrados (c:cs) | c == AzulejoQuebrado = (- 1) + contaAzulejosQuebrados cs
                              | otherwise = contaAzulejosQuebrados cs
