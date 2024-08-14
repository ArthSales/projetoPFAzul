module Jogador1 where

import Data1

--Define a lista dos azulejos comprados pelo jogador 1
jogador1 :: [Cor] -> [Cor] -> [Cor]
jogador1 cs [] = cs
jogador1 [] cm = cm
jogador1 cs cm = cs ++ cm