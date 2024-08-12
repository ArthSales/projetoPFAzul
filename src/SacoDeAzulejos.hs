module SacoDeAzulejos where

import Parede
import Data

-- Definindo as cores
type Azulejos = [(Int,Cor)]
type AzulejosSeparados = (Int,Cor)
--type Expositores = [[String]]

--sacoAzulejos: define o saco de azulejos do jogo Azul, que contém 20 peças de cada uma das cinco cores e garante que caso
--a soma das peças seja menor que 20, o saco volta a ter as 100 peças iniciais
sacoAzulejos :: Azulejos -> Azulejos
sacoAzulejos [] = [(20,Azul),(20,Amarelo),(20,Vermelho),(20,Preto),(20,Branco)]
sacoAzulejos xs | totalAzulejos < 20 = [(20,Azul),(20,Amarelo),(20,Vermelho),(20,Preto),(20,Branco)]
                | otherwise = xs
                  where totalAzulejos = sum (map fst xs)

somaAzulejo :: Cor -> Azulejos -> Azulejos
somaAzulejo _ [] = [(20,Azul),(20,Amarelo),(20,Vermelho),(20,Preto),(20,Branco)]
somaAzulejo cor ((q,c):azulejos) | cor == c = (q+1, c): azulejos
                                     | otherwise = (q,c) : somaAzulejo cor azulejos

azulejosParaNum :: Azulejos -> Int -> [AzulejosSeparados]
azulejosParaNum [] _ = []
azulejosParaNum ((0, _):xs) acc = azulejosParaNum xs acc
azulejosParaNum (x:xs) acc = (acc,snd x): azulejosParaNum ((fst x - 1, snd x):xs) (acc+1)

numParaAzulejos :: [AzulejosSeparados] -> Azulejos -> Azulejos
numParaAzulejos [] azulejos = azulejos
numParaAzulejos ((_,c):azulejosSeparados) azulejos = numParaAzulejos azulejosSeparados (somaAzulejo c azulejos)