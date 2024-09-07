
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
-- >>> criarParede
-- [[(Azul,False),(Amarelo,False),(Vermelho,False),(Preto,False),(Branco,False)],[(Branco,False),(Azul,False),(Amarelo,False),(Vermelho,False),(Preto,False)],[(Preto,False),(Branco,False),(Azul,False),(Amarelo,False),(Vermelho,False)],[(Vermelho,False),(Preto,False),(Branco,False),(Azul,False),(Amarelo,False)],[(Amarelo,False),(Vermelho,False),(Preto,False),(Branco,False),(Azul,False)]]

-- PATTERN LINES
criarPatternLines :: Int -> [[Maybe Cor]]
criarPatternLines n = map criarSubPattern [1..n]
    where
        criarSubPattern :: Int -> [Maybe Cor]
        criarSubPattern size = replicate size Nothing

-- >>> criarPatternLines 5
-- [[Nothing],[Nothing,Nothing],[Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Nothing]]

-- Verifica se todos os elementos de uma lista são iguais
todosIguais :: Eq a => [Maybe a] -> Bool
todosIguais [] = True
todosIguais (x:xs) = all (== x) xs

atualizarLinha :: [Maybe Cor] -> [(Cor, Bool)] -> [(Cor, Bool)]
atualizarLinha pat linha
  | todosIguais pat && not (null pat) = map atualizarElemento linha -- se os azulejos forem iguais e se nao tiver vazia
  | otherwise = linha
  where
    corPattern = head (Data.Maybe.catMaybes pat) --pega primeiro elemento (cor)
    -- catMaybes: remove todos os nothing e retorna só os valores just fora do contexto
    atualizarElemento :: (Cor, Bool) -> (Cor, Bool)
    atualizarElemento (corParede, b)
      | corParede == corPattern = (corParede, True)
      | otherwise = (corParede, b)

atualizarMatriz :: [LinhaParede] -> [[Maybe Cor]] -> ([LinhaParede], [[Maybe Cor]])
atualizarMatriz parede patternLines =
    let paredeAtualizada = zipWith atualizarLinha patternLines parede
        patternLinesAtualizadas = map atualizarPattern patternLines
    in (paredeAtualizada, patternLinesAtualizadas)

atualizarPattern :: [Maybe Cor] -> [Maybe Cor] 
-- se a parede estiver cheia com azulejo iguais, atualiza tudo para nothing 
atualizarPattern pat
    | todosIguais pat && not (null pat) = map (const Nothing) pat
    | otherwise = pat

-- >>> atualizarPattern [Just Amarelo,Just Amarelo]
-- [Nothing,Nothing]

-- >>> atualizarLinha [Just Azul,Just Azul] [(Azul, False), (Amarelo, False)]
-- [(Azul,True),(Amarelo,False)]

-- >>> atualizarMatriz [[Just Azul,Just Azul], [Just Amarelo,Just Amarelo]] [[(Azul, False), (Amarelo, False)], [(Azul, False), (Amarelo, False)]] 
-- ([[(Azul,True),(Amarelo,False)],[(Azul,False),(Amarelo,True)]],[[Nothing,Nothing],[Nothing,Nothing]])



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

