module Expositores where

import Data1
import SacoDeAzulejos
import System.Random

-- Função para sortear uma lista de cores a partir de lista de azulejos numerada onde cada elemento é do tipo (i,Cor), tal que 0 < i < 19 
expositor :: [AzulejosSeparados] -> Int -> [Cor]
expositor [] _ = []
expositor _ 0 = []
expositor as n | length as > 4 = c : expositor as (n-1)
               | otherwise = snd (head as) : expositor (tail as) (n-1)
  where
    sacoAs = numParaAzulejos as [(0,Azul),(0,Amarelo),(0,Vermelho),(0,Preto),(0,Branco)]
    total = totalAzulejos sacoAs
    gen = mkStdGen (n+2)
    (randomNumber, _) = randomR (0, total-1) gen
    corSorteada =  filter (puxaAzulejo randomNumber)
    c = snd (head (corSorteada as))

-- Função que retira de uma lista de azulejos os azulejos que foram sorteados para o expositor 
tiraExpositorDoSaco :: [Cor] -> Azulejos -> [AzulejosSeparados]
tiraExpositorDoSaco cs azs
  = azulejosParaNum (foldl
      (\ azs c
         -> map
              (\ (n, cor) -> if cor == c then (n - 1, cor) else (n, cor)) azs)
      azs cs) 0

-- Função para gerar os expositores das fábricas
geraExpositores :: [AzulejosSeparados] -> Int -> [[Cor]]
geraExpositores [] _ = []
geraExpositores _ n | n < 4 = []
geraExpositores as n = expo : geraExpositores novoSaco (n-4)
  where
    expo = expositor as 4
    converte = numParaAzulejos as [(0,Azul),(0,Amarelo),(0,Vermelho),(0,Preto),(0,Branco)]
    novoSaco = tiraExpositorDoSaco expo converte

dropaExpositor :: [[Cor]] -> Int -> [[Cor]]
dropaExpositor [] _ = []
dropaExpositor _ n | n > 4 = error "Não devia ser passado um valor maior que 4 nas opções de expositores"
dropaExpositor cs n | n == 0 = tail cs
                    | 1 <= n && n <= 3 = take n cs ++ drop (n+1) cs
                    | otherwise = take n cs

--Função pra passar as infos pro novo saco
retiraExpositores :: [[Cor]] -> Azulejos -> Azulejos
retiraExpositores [] as = as
retiraExpositores [expo] as = numParaAzulejos (tiraExpositorDoSaco expo as) [(0,Azul),(0,Amarelo),(0,Vermelho),(0,Preto),(0,Branco)]
retiraExpositores (e:es) as = retiraExpositores es novoSaco
  where
    novoSaco = numParaAzulejos (tiraExpositorDoSaco e as) [(0,Azul),(0,Amarelo),(0,Vermelho),(0,Preto),(0,Branco)]

-- Função para fazer a soma de todos os elementos dos expositores dentro de uma lista de expositores
somaExpositores :: [[Cor]] -> Int
somaExpositores = foldl (\acc xs -> acc + length xs) 0

garanteCompraSafe :: [Cor] -> Bool
garanteCompraSafe [] = True
garanteCompraSafe cs = cs /= [Azul,Azul,Azul,Azul,Azul]

-- Função para que o jogador faça a compra de um expositor
compraExpositor :: Cor -> Int -> [[Cor]] -> [Cor]
compraExpositor c i es | i > (length es - 1) || null filtrada = [Azul,Azul,Azul,Azul,Azul]
                       | otherwise = filtrada
  where
    corBate :: Cor -> Cor -> Bool
    corBate cs cexp = cs == cexp
    filtrada = filter (corBate c) (head (drop i es))


dropaCorDeLsCores :: Cor -> [Cor] -> [Cor]
dropaCorDeLsCores _ [] = []
dropaCorDeLsCores c (cor:cs) | corBate c cor = dropaCorDeLsCores c cs
                             | otherwise = cor: dropaCorDeLsCores c cs
  where
    corBate :: Cor -> Cor -> Bool
    corBate cs1 cexp = cs1 == cexp


compraCentroDaMesa :: Cor -> [Cor] -> [Cor]
compraCentroDaMesa c es = filtrada
  where
    corBate :: Cor -> Cor -> Bool
    corBate cs cexp = cs == cexp
    filtrada = filter (corBate c) es

-- Função que manda o resto pro centro da mesa
restoExpositor :: Cor -> Int -> [[Cor]] -> [Cor]
restoExpositor c i es = filter (corBate c) (head (drop i es))
  where
    corBate :: Cor -> Cor -> Bool
    corBate cs cexp = cs /= cexp
