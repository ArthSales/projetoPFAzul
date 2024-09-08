
module Parede (linhaInicial, Cor, criarParede, atualizarMatriz) where

import Data.Maybe
import Data1
import Text.ParserCombinators.ReadP (get)
import SacoDeAzulejos (sacoAzulejos)

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



-- data State2 = State2 {
--   sa1 :: Azulejos
--   ,expositores1 :: [[Cor]]
--   ,cm1 :: [Cor]
--   ,deQuemEAVez1 :: Int
--   ,chao1 :: [Chao]
--   ,chao2 :: [Chao]
--   ,pl1 :: [Maybe Cor]
--   ,pl2 :: [Maybe Cor]
--   ,parede1 :: [LinhaParede]
--   ,parede2 :: [LinhaParede]
--  -- ,picture :: Picture
-- } deriving Show
--Linhas de chão

-- Define o Chao inicial
chao :: [Chao]
chao = [Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]

-- Função que quebra um azulejo
quebraAzulejo :: a -> Chao
quebraAzulejo _ = AzulejoQuebrado

--Função que atualiza o chão sempre que é recebe a lista de sobra
atualizaChao :: [Cor] -> [Chao] -> [Chao]
atualizaChao r ci = map quebraAzulejo (take (length r) ci) ++ drop (length r) ci

--atualiza PatternLine
compraPraPatternLine :: [Maybe Cor] -> Int -> State2 -> State2
compraPraPatternLine [] i s = s
compraPraPatternLine compra@(c:cs) i s0@(State2 s e m v c1 c2 pl1 pl2 p1 p2 p inp) 
  | v == 0 = State2 s e m v (sobra ++ c1) c2 novoPl pl2 p1 p2 p inp
  | otherwise = State2 s e m v c1 (sobra ++ c2) pl1 novoPl p1 p2 p inp
  where
    tamanhoEscolhida = length (pl1 !! i)
    novoPl | v == 0 = take i pl1 ++ [subPl compra (pl1 !! i)] ++ drop (i+1) pl1
           | otherwise = pl1
    subPl :: [Maybe Cor] -> [Maybe Cor] -> [Maybe Cor]
    subPl [] ps = ps
    subPl _ [] = []
    subPl (Just c:cs) (Nothing:ps) = Just c : subPl cs ps
    subPl (Nothing:cs) (Nothing:ps) = Nothing : subPl cs ps
    subPl _ (p:ps) = p : subPl [] ps
    sobra = fmap quebraAzulejo (drop tamanhoEscolhida compra)

estadoInicial :: State2
estadoInicial = State2 (sacoAzulejos []) [] [] 0 [] [] (criarPatternLines 5) [] [] [] (0,0) []

-- >>> compraPraPatternLine [Just Amarelo, Just Amarelo, Just Amarelo] 1 estadoInicial
-- State2 {sa1 = [(20,Azul),(20,Amarelo),(20,Vermelho),(20,Preto),(20,Branco)], expositores1 = [], cm1 = [], deQuemEAVez1 = 0, chao1 = [AzulejoQuebrado], chao2 = [], pl1 = [[Nothing],[Just Amarelo,Just Amarelo],[Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Nothing]], pl2 = [], parede1 = [], parede2 = []}
