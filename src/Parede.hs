
module Parede (linhaInicial, Cor, criarParede, atualizarMatriz) where

import Data.Maybe
import Data1

-- PAREDE
linhaInicial :: LinhaParede
linhaInicial = [(Azul, False), (Amarelo, False), (Vermelho, False), (Preto, False), (Branco, False)]

proximaLinha :: Int -> [(a, Bool)] -> [(a, Bool)]
proximaLinha n xs = drop k xs ++ take k xs
    where k = length xs - (n `mod` length xs)

criarParede :: [LinhaParede]
criarParede = [proximaLinha n linhaInicial | n <- [0..4]]

-- PATTERN LINES
-- essa função não devia retornar um monte de nothing?
criarPatternLines :: Int -> [[Maybe Cor]]
criarPatternLines n = map criarSubPattern [1..n]
    where
        criarSubPattern :: Int -> [Maybe Cor]
        criarSubPattern size = take size (map Just cores ++ repeat Nothing)

        cores :: [Cor]
        cores = [minBound .. maxBound]

todosIguais :: Eq a => [Maybe a] -> Bool
todosIguais [] = True
todosIguais (x:xs) = all (== x) xs

atualizarLinha :: [Maybe Cor] -> LinhaParede -> LinhaParede
atualizarLinha pat linha
  | todosIguais pat && not (null pat) = map atualizarElemento linha
  | otherwise = linha
  where
    elementoComum = fromMaybe (head (Data.Maybe.catMaybes pat)) (head pat)

    atualizarElemento :: (Cor, Bool) -> (Cor, Bool)
    atualizarElemento (cor, b)
      | cor == elementoComum = (cor, True)
      | otherwise = (cor, b)

atualizarMatriz :: [LinhaParede] -> [[Maybe Cor]] -> ([LinhaParede], [[Maybe Cor]])
atualizarMatriz parede patternLines =
    let paredeAtualizada = zipWith atualizarLinha patternLines parede
        patternLinesAtualizadas = map atualizarPattern patternLines
    in (paredeAtualizada, patternLinesAtualizadas)

atualizarPattern :: [Maybe Cor] -> [Maybe Cor]
atualizarPattern pat
    | todosIguais pat && not (null pat) = map (const Nothing) pat
    | otherwise = pat

--Linhas de chão

-- Define o Chao inicial
chao :: [Chao]
chao = [Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]

-- Função que quebra um azulejo
quebraAzulejo :: Chao -> Chao
quebraAzulejo _ = AzulejoQuebrado

--Função que atualiza o chão sempre que é recebe a lista de sobra
atualizaChao :: [Cor] -> [Chao] -> [Chao]
atualizaChao r ci = map quebraAzulejo (take (length r) ci) ++ drop (length r) ci


