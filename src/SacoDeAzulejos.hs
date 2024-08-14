{-# OPTIONS_GHC -Wno-name-shadowing #-}
module SacoDeAzulejos where

import Parede
import Data
import System.Random
import Prelude hiding (exp)

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

--Define a regra pra filtrar os azulejos pelo número na lista de azulejos separados, ignorando qual a cor no primeiro momento    
puxaAzulejo :: Int -> AzulejosSeparados -> Bool
puxaAzulejo p (x, _) = p == x


-- Função para sortear uma lista de cores a partir dos azulejos
expositor :: [AzulejosSeparados] -> Int -> [Cor]
expositor [] _ = []
expositor _ 0 = []
expositor as n = snd (head (corSorteada as)) : expositor novoSaco (n-1)
  where
    sacoAs = numParaAzulejos as [(0,Azul),(0,Amarelo),(0,Vermelho),(0,Preto),(0,Branco)]
    total = totalAzulejos sacoAs
    gen = mkStdGen n
    (randomNumber, _) = randomR (0, total-1) gen
    corSorteada =  filter (puxaAzulejo randomNumber)
    novoSaco = azulejosParaNum (subAzulejo (snd (head (corSorteada as))) (numParaAzulejos sacoAs [(0,Azul),(0,Amarelo),(0,Vermelho),(0,Preto),(0,Branco)])) 0

tiraExpositorDoSaco :: [Cor] -> Azulejos -> [AzulejosSeparados]
tiraExpositorDoSaco cs as
  = azulejosParaNum (foldl
      (\ as c
         -> map
              (\ (n, cor) -> if cor == c then (n - 1, cor) else (n, cor)) as)
      as cs) 0

-- Função para gerar os expositores
geraExpositores :: [AzulejosSeparados] -> Int -> [[Cor]]
geraExpositores [] _ = []
geraExpositores _ n | n < 4 = []
geraExpositores as n = expo : geraExpositores novoSaco (n-4)
  where
    expo = expositor as 4
    converte = numParaAzulejos as [(0,Azul),(0,Amarelo),(0,Vermelho),(0,Preto),(0,Branco)]
    novoSaco = tiraExpositorDoSaco expo converte