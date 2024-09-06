module Pontuacao where

import Data1
import Parede

atualizaPontuacao :: [Chao] -> Int
atualizaPontuacao cs = contaAzulejosQuebrados cs

contaAzulejosQuebrados :: [Chao] -> Int
contaAzulejosQuebrados [] = 0
contaAzulejosQuebrados (c:cs) | c == AzulejoQuebrado = (- 1) + contaAzulejosQuebrados cs
                              | otherwise = contaAzulejosQuebrados cs


