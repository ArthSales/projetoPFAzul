module Jogador1 where

import Data1

--Define a lista dos azulejos comprados pelo jogador 1
incrementaCores :: [Cor] -> [Cor] -> [Cor]
incrementaCores cs [] = cs
incrementaCores [] cm = cm
incrementaCores cs cm = cs ++ cm