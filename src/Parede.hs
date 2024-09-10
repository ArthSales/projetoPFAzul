module Parede where

import Data.Maybe
import Data1
import Text.ParserCombinators.ReadP (get)
import SacoDeAzulejos (sacoAzulejos)
import Expositores
import Jogo (jogadaPossivel, jogadaPossivelParede)

-- PAREDE
linhaInicial :: LinhaParede -- linha parede inicial
linhaInicial = [(Azul, False), (Amarelo, False), (Vermelho, False), (Preto, False), (Branco, False)]

proximaLinha :: Int -> [(a, Bool)] -> [(a, Bool)] -- gera proximas linhas trocando ordem
proximaLinha n xs = drop k xs ++ take k xs
    where k = length xs - (n `mod` length xs)

criarParede :: [LinhaParede] -- cria a parede completa
criarParede = [proximaLinha n linhaInicial | n <- [0..4]]

-- PATTERN LINES
criarPatternLines :: Int -> [[Maybe Cor]] 
criarPatternLines n = map criarSubPattern [1..n]
    where
        criarSubPattern :: Int -> [Maybe Cor]
        criarSubPattern size = replicate size Nothing

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

--CHAO
chao :: [Chao]
chao = [Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]

-- Função que quebra um azulejo pra jogar no Chao
quebraAzulejo :: a -> Chao
quebraAzulejo _ = AzulejoQuebrado

-- Função que atualiza o chao caso a compra tenha sido excessiva pra pattern line
atualizaChao :: [Maybe Cor] -> [Maybe Cor] -> [Chao]
atualizaChao [] _ = []
atualizaChao _ [] = []
atualizaChao c@(x:xs) p@(y:ys) = replicate (length cConcatP - length p) AzulejoQuebrado
  where
    novaP = filter isJust p
    cConcatP = c ++ novaP

-- Retorna o estado passando a vez e quebrando um azulejo do centro da mesa
quebraUmAzulejo :: State2 -> State2
quebraUmAzulejo (State2 s e m v c1 c2 pl1 pl2 p1 p2 p inp) | v == 0 = State2 s e (tail m) 1 (AzulejoQuebrado:c1) c2 pl1 pl2 p1 p2 p inp
                                                           | otherwise = State2 s e (tail m) 0 c1 (AzulejoQuebrado:c2) pl1 pl2 p1 p2 p inp

--Função auxiliar que tira Cor do contexto Maybe sem tratar Nothing:
tiraCor :: Maybe Cor -> Cor
tiraCor (Just c) = c

trocaVez :: Int -> Int
trocaVez 0 = 1
trocaVez 1 = 0

--Função que gerencia a compra do jogador pra pattern line e devolve o proximo estado pós-compra
compraPraPatternLine :: [Maybe Cor] -> Int -> Int -> State2 -> State2
compraPraPatternLine [] _ _ s = s
compraPraPatternLine compra l i s0@(State2 s e m v c1 c2 pl11 pl22 p1 p2 p inp)
  | v == 0 && jogadaPossivel (fmap tiraCor compra) linhaSelecionada1 && jogadaPossivelParede cor linhaParedeSelecionada1 = State2 s novoExpo novoCentro 1 (sobra ++ c1) c2 novoPl pl22 p1 p2 p inp
  | v == 1 && jogadaPossivel (fmap tiraCor compra) linhaSelecionada2 && jogadaPossivelParede cor linhaParedeSelecionada2 = State2 s novoExpo novoCentro 0 c1 (sobra ++ c2) pl11 novoPl p1 p2 p inp
  | jogadaPossivelParede cor linhaParedeSelecionada1 || jogadaPossivelParede cor linhaParedeSelecionada2 = State2 s e m (trocaVez v) c1 c2 pl11 pl22 p1 p2 p inp
  | otherwise = s0
  where
    cor = tiraCor $ head compra --pega a cor selecionada na compra e tira do contexto do Maybe && jogadaPossivelParede cor linhaParedeSelecionada2
    linhaSelecionada1 = pl11 !! i -- linha da patternline selecionada se vez do jogador 1
    linhaSelecionada2 = pl22 !! i -- linha da patternline selecionada se vez do jogador 2
    linhaParedeSelecionada1 = p1 !! i -- linha da parede selecionada se vez do jogador 1
    linhaParedeSelecionada2 = p2 !! i -- linha da parede selecionada se vez do jogador 2
    tamanhoEscolhida = length (pl11 !! i) --pega tamanho da patternLine escolhida da lista de patternLines
    novoPl | v == 0 = take i pl11 ++ [preencheLista compra (pl11 !! i)] ++ drop (i+1) pl11 --compõe a nova pl com as informações da pl anterior
           | otherwise = take i pl22 ++ [preencheLista compra (pl22 !! i)] ++ drop (i+1) pl22
    preencheLista :: Eq a => [Maybe a] -> [Maybe a] -> [Maybe a]
    preencheLista [] ys = ys  -- Caso a primeira lista esteja vazia, retorna a segunda lista como está.
    preencheLista _ [] = []   -- Caso a segunda lista esteja vazia, retorna uma lista vazia.
    preencheLista (Just x:xs) (Nothing:ys) = Just x : preencheLista xs ys
    preencheLista x1@(Just x:xs) (Just y:ys)
        | x == y    = Just y : preencheLista x1 ys -- Só substitui os próximos valores se o primeiro Just for igual em ambas as listas.
        | otherwise = Just y : ys                  -- Retorna a segunda lista sem alterações se os valores iniciais forem diferentes.
    preencheLista _ ys = ys  -- Para qualquer outro caso, retorna a segunda lista como está.
    sobra | v == 0 = atualizaChao compra (pl11 !! i)
          | otherwise = atualizaChao compra (pl22 !! i)
    novoExpo | l == 5 = e
             | otherwise = dropaExpositor e l
    novoCentro | l == 5 = dropaCorDeLsCores cor m
               | otherwise = incrementaCores (restoExpositor cor l e) m


