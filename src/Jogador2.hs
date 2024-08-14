module Jogador2 where

import Data1

--Define a lista dos azulejos comprados pelo jogador 2
jogador2 :: [Cor] -> [Cor] -> [Cor]
jogador2 cs [] = cs
jogador2 [] cm = cm
jogador2 cs cm = cs ++ cm