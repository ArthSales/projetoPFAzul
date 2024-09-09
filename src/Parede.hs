module Parede where

import Data.Maybe
import Data1
import Text.ParserCombinators.ReadP (get)
import SacoDeAzulejos (sacoAzulejos)
import Expositores

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
atualizarLinha (Nothing:xs) linha = linha
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

-- >>> atualizaChao [Amarelo,Amarelo] [Vazio,Vazio]
-- [AzulejoQuebrado,AzulejoQuebrado]
compraPraPatternLine :: [Maybe Cor] -> Int -> Int -> State2 -> State2
compraPraPatternLine [] _ _ s = s
compraPraPatternLine compra l i s0@(State2 s e m v c1 c2 pl1 pl2 p1 p2 p inp)
  | v == 0 && verificaChaoInsta e pl1 = State2 s novoExpo novoCentro 1 (sobra ++ c1) c2 novoPl pl2 p1 p2 p inp
  | v == 0 && podeJogadaOuReseta compra linhaSelecionada = State2 s novoExpo novoCentro 1 (sobra ++ c1) c2 novoPl pl2 p1 p2 p inp
  | v == 1 && podeJogadaOuReseta compra linhaSelecionada = State2 s novoExpo novoCentro 0 c1 (sobra ++ c2) pl1 novoPl p1 p2 p inp
  | otherwise = s0
  where
    cor = tiraCor $ head compra --pega a cor selecionada na compra e tira do contexto do Maybe
    linhaSelecionada = pl1 !! i
    tamanhoEscolhida = length (pl1 !! i) --pega tamanho da patternLine escolhida da lista de patternLines
    novoPl | v == 0 = take i pl1 ++ [preencheLista compra (pl1 !! i)] ++ drop (i+1) pl1 --compõe a nova pl com as informações da pl anterior
           | otherwise = take i pl2 ++ [preencheLista compra (pl2 !! i)] ++ drop (i+1) pl2

    preencheLista :: Eq a => [Maybe a] -> [Maybe a] -> [Maybe a]
    preencheLista [] ys = ys  -- Caso a primeira lista esteja vazia, retorna a segunda lista como está.
    preencheLista _ [] = []   -- Caso a segunda lista esteja vazia, retorna uma lista vazia.
    preencheLista (Just x:xs) (Nothing:ys) = Just x : preencheLista xs ys
    preencheLista x1@(Just x:xs) (Just y:ys)
        | x == y    = Just y : preencheLista x1 ys -- Só substitui os próximos valores se o primeiro Just for igual em ambas as listas.
        | otherwise = Just y : ys                  -- Retorna a segunda lista sem alterações se os valores iniciais forem diferentes.
    preencheLista _ ys = ys  -- Para qualquer outro caso, retorna a segunda lista como está.
    sobra = atualizaChao compra (preencheLista compra (pl2 !! i))
    novoExpo | l == 5 = e
             | otherwise = dropaExpositor e l
    novoCentro | l == 5 = dropaCorDeLsCores cor m
               | otherwise = incrementaCores (restoExpositor cor l e) m
    podeJogadaOuReseta :: Eq a => [Maybe a] -> [Maybe a] -> Bool -- verifica se o jogador pode realizar a jogada desejada
    podeJogadaOuReseta [] _ = True
    podeJogadaOuReseta _ [] = True
    podeJogadaOuReseta (Nothing:_) _ = True
    podeJogadaOuReseta l@(Just x:xs) (Nothing:ys) | isNothing (last ys) = True
                                                  | otherwise = podeJogadaOuReseta xs ys
    podeJogadaOuReseta (Just x:xs) (Just y:ys) = x == y

    verificaChaoInsta :: Eq a => [[a]] -> [[Maybe a]] -> Bool --verifica se pode jogar um azulejo no chao do jogador direto, pois não há mais jogadas
    verificaChaoInsta [] _ = True
    verificaChaoInsta _ [] = False
    verificaChaoInsta (xs:xss) yss =
        case xs of
        [] -> False  -- Caso a lista de `a` esteja vazia
        (x:_) -> any (verificaPrimeiro x) yss && verificaChaoInsta xss yss
        where
        verificaPrimeiro :: Eq a => a -> [Maybe a] -> Bool
        verificaPrimeiro _ [] = False
        verificaPrimeiro _ (Nothing:_) = False
        verificaPrimeiro x (Just y:_) = x == y

atualizaChao :: [Maybe Cor] -> [Maybe Cor] -> [Chao]
atualizaChao [] _ = []
atualizaChao _ [] = []
atualizaChao (Nothing:_) _ = []
atualizaChao c@(Just _:_) p@(Nothing:_) | length c > length p = fmap quebraAzulejo (drop (length p) c)
                                        | otherwise = fmap quebraAzulejo c
atualizaChao c@(Just x:xs) p@(Just y:ys) | podeJogar1 && length c > length p = fmap quebraAzulejo (drop (length p - 1) c)
                                         | otherwise = []
  where
    podeJogar :: Eq a => [Maybe a] -> [Maybe a] -> Bool
    podeJogar [] _ = True
    podeJogar _ [] = True
    podeJogar (Nothing:_) _ = True
    podeJogar (Just x:xs) (Nothing:ys) = True
    podeJogar (Just x:xs) (Just y:ys) = x == y || podeJogar xs ys
    podeJogar1 = podeJogar c p

tiraCor :: Maybe Cor -> Cor
tiraCor (Just c) = c

estadoInicial :: State2
estadoInicial = State2 (sacoAzulejos []) [] [] 0 [] [] (criarPatternLines 5) [] [] [] (0,0) []

-- >>> compraPraPatternLine [Just Amarelo, Just Amarelo, Just Amarelo] 1 estadoInicial
-- State2 {sa1 = [(20,Azul),(20,Amarelo),(20,Vermelho),(20,Preto),(20,Branco)], expositores1 = [], cm1 = [], deQuemEAVez1 = 0, chao1 = [AzulejoQuebrado], chao2 = [], pl1 = [[Nothing],[Just Amarelo,Just Amarelo],[Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Nothing]], pl2 = [], parede1 = [], parede2 = []}

-- >>> criarParede 
-- [[(Azul,False),(Amarelo,False),(Vermelho,False),(Preto,False),(Branco,False)],[(Branco,False),(Azul,False),(Amarelo,False),(Vermelho,False),(Preto,False)],[(Preto,False),(Branco,False),(Azul,False),(Amarelo,False),(Vermelho,False)],[(Vermelho,False),(Preto,False),(Branco,False),(Azul,False),(Amarelo,False)],[(Amarelo,False),(Vermelho,False),(Preto,False),(Branco,False),(Azul,False)]]
