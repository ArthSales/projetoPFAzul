module Parede (linhaInicial, Cor, criarParede, atualizarMatriz) where

import Data.Maybe
import Data1

-- PAREDE
linhaInicial :: [(Cor, Bool)] 
linhaInicial = [(Azul, False), (Amarelo, False), (Vermelho, False), (Preto, False), (Branco, False)]

proximaLinha :: Int -> [(a, Bool)] -> [(a, Bool)]
proximaLinha n xs = drop k xs ++ take k xs
    where k = length xs - (n `mod` length xs)
    
criarParede :: [[(Cor, Bool)]] 
criarParede = [proximaLinha n linhaInicial | n <- [0..4]]

-- PATTERN LINES
criarPatternLines :: Int -> [[Maybe Cor]]
criarPatternLines n = map criarSubPattern [1..n]
    where
        criarSubPattern :: Int -> [Maybe Cor]
        criarSubPattern size = take size (map Just cores ++ repeat Nothing)

        cores :: [Cor]
        cores = [minBound .. maxBound]

-- Verifica se todos os elementos de uma lista são iguais
todosIguais :: Eq a => [Maybe a] -> Bool
todosIguais [] = True
todosIguais (x:xs) = all (== x) xs


atualizarLinha :: [Maybe Cor] -> [(Cor, Bool)] -> [(Cor, Bool)]
atualizarLinha pat linha
  | todosIguais pat && not (null pat) = map atualizarElemento linha
  | otherwise = linha
  where
    elementoComum = fromMaybe (head (Data.Maybe.catMaybes pat)) (head pat)
    
    atualizarElemento :: (Cor, Bool) -> (Cor, Bool)
    atualizarElemento (cor, b)
      | cor == elementoComum = (cor, True)
      | otherwise = (cor, b)

atualizarMatriz :: [[(Cor, Bool)]] -> [[Maybe Cor]] -> ([[(Cor, Bool)]], [[Maybe Cor]])
atualizarMatriz parede patternLines = 
    let paredeAtualizada = zipWith atualizarLinha patternLines parede
        patternLinesAtualizadas = map atualizarPattern patternLines
    in (paredeAtualizada, patternLinesAtualizadas)

atualizarPattern :: [Maybe Cor] -> [Maybe Cor]
atualizarPattern pat
    | todosIguais pat && not (null pat) = map (const Nothing) pat
    | otherwise = pat

-- main :: IO ()
-- main = do
--     let parede = criarParede
--     --let patternlines = (criarPatternLines 5)
--     let patternlines = [ [Just Amarelo, Just Amarelo, Nothing]
--                        , [Just Azul, Just Azul]
--                        , [Just Preto, Just Preto]
--                        , [Just Vermelho, Just Vermelho]
--                        , [Just Amarelo, Just Amarelo]
--                        ]
--     mapM_ print parede
--     mapM_ print patternlines

--     let (paredeAtualizada, patternLinesAtualizadas) = atualizarMatriz parede patternlines
    
--     putStrLn "\nParede atualizada:"
--     mapM_ print paredeAtualizada
--     putStrLn "\nPattern Lines atualizadas:"
--     mapM_ print patternLinesAtualizadas
