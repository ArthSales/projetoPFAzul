module SacoDeAzulejos where

import Parede
import Data
import System.Random

--soma o total de azulejos no Saco de Azulejos
totalAzulejos :: Azulejos -> Int
totalAzulejos xs = sum (map fst xs)

--sacoAzulejos: define o saco de azulejos do jogo Azul, que contém 20 peças de cada uma das cinco cores e garante que caso
--a soma das peças seja menor que 20, o saco volta a ter as 100 peças iniciais
sacoAzulejos :: Azulejos -> Azulejos
sacoAzulejos [] = [(20,Azul),(20,Amarelo),(20,Vermelho),(20,Preto),(20,Branco)]
sacoAzulejos xs | totalAzulejos xs < 20 = [(20,Azul),(20,Amarelo),(20,Vermelho),(20,Preto),(20,Branco)]
                | otherwise = xs

--Adiciona um azulejo de uma cor específica no saco
somaAzulejo :: Cor -> Azulejos -> Azulejos
somaAzulejo _ [] = [(20,Azul),(20,Amarelo),(20,Vermelho),(20,Preto),(20,Branco)]
somaAzulejo cor ((q,c):azulejos) | cor == c = (q+1, c): azulejos
                                 | otherwise = (q,c) : somaAzulejo cor azulejos

--Subtrai um azulejo de uma cor específica no saco
subAzulejo :: Cor -> Azulejos -> Azulejos
subAzulejo _ [] = [(20,Azul),(20,Amarelo),(20,Vermelho),(20,Preto),(20,Branco)]
subAzulejo cor ((q,c):azulejos) | cor == c = (q-1, c): azulejos
                                | otherwise = (q,c) : subAzulejo cor azulejos

--Função que transforma os azulejos numa lista sequencial de azulejos numerados
azulejosParaNum :: Azulejos -> Int -> [AzulejosSeparados]
azulejosParaNum [] _ = []
azulejosParaNum ((0, _):xs) acc = azulejosParaNum xs acc
azulejosParaNum (x:xs) acc = (acc,snd x): azulejosParaNum ((fst x - 1, snd x):xs) (acc+1)

--transforma uma lista de azulejos sequências no saco de azulejos
numParaAzulejos :: [AzulejosSeparados] -> Azulejos -> Azulejos
numParaAzulejos [] azulejos = azulejos
numParaAzulejos ((_,c):azulejosSeparados) azulejos = numParaAzulejos azulejosSeparados (somaAzulejo c azulejos)


--A lógica para o uso da biblioteca veio da documentação, e uma ajuda do GPT para lidar com o sistema de monadas, vide link https://hackage.haskell.org/package/random-1.2.1.2/docs/System-Random.html
geraExpositores :: [AzulejosSeparados] -> Int -> [Cor]
geraExpositores [] _ = []
geraExpositores _ 0 = []
geraExpositores lsazulejos n = corSorteada ++ geraExpositores lsazulejos (n-1)
  where
    gen = mkStdGen n 
    (randomNumber, _) = randomR (0, totalAzulejos(numParaAzulejos lsazulejos [(0,Azul),(0,Amarelo),(0,Vermelho),(0,Preto),(0,Branco)])) gen 
    corSorteada = map snd $ filter (puxaAzulejo randomNumber) lsazulejos
    puxaAzulejo :: Int -> AzulejosSeparados -> Bool
    puxaAzulejo n (x, _) = n == x
