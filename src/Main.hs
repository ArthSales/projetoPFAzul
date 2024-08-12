module Main (main, Azulejos) where

import SacoDeAzulejos
import Parede
import Graphics.Gloss (display, Display (..), white, Picture (..))
import Data

main :: IO ()

main = do
   putStrLn "Entre com a lista de azulejos::"
   print (sacoAzulejos [(4,Azul),(10,Amarelo),(10,Vermelho),(6,Preto),(10,Branco)])
   print (sacoAzulejos [(0,Azul),(2,Amarelo),(3,Vermelho),(10,Preto),(5,Branco)])
   print (azulejosParaNum (sacoAzulejos [(0,Azul),(2,Amarelo),(3,Vermelho),(10,Preto),(5,Branco)]) 0 )

   let parede = criarParede
     --let patternlines = (criarPatternLines 5)
   let patternlines = [ [Just Amarelo, Just Amarelo, Nothing]
                        , [Just Azul, Just Azul]
                        , [Just Preto, Just Preto]
                        , [Just Vermelho, Just Vermelho]
                        , [Just Amarelo, Just Amarelo]
                        ]
   mapM_ print parede
   mapM_ print patternlines

   let (paredeAtualizada, patternLinesAtualizadas) = atualizarMatriz parede patternlines
    
   putStrLn "\nParede atualizada:"
   mapM_ print paredeAtualizada
   putStrLn "\nPattern Lines atualizadas:"
   mapM_ print patternLinesAtualizadas

-- --Renomeia a lista de tuplas [(Int,String)] para definir a lista de azulejos
-- type Azulejos = [(Int,Cor)]
-- type AzulejosSeparados = (Int,Cor)
-- --type Expositores = [[String]]

-- --sacoAzulejos: define o saco de azulejos do jogo Azul, que contém 20 peças de cada uma das cinco cores e garante que caso
-- --a soma das peças seja menor que 20, o saco volta a ter as 100 peças iniciais
-- sacoAzulejos :: Azulejos -> Azulejos
-- sacoAzulejos [] = [(20,"Azul"),(20,"Amarelo"),(20,"Vermelho"),(20,"Preto"),(20,"Branco")]
-- sacoAzulejos xs | totalAzulejos < 20 = [(20,"Azul"),(20,"Amarelo"),(20,"Vermelho"),(20,"Preto"),(20,"Branco")]
--                 | otherwise = xs
--                   where totalAzulejos = sum (map fst xs)

-- somaAzulejo :: Cor -> Azulejos -> Azulejos
-- somaAzulejo _ [] = [(20,"Azul"),(20,"Amarelo"),(20,"Vermelho"),(20,"Preto"),(20,"Branco")]
-- somaAzulejo cor ((q,c):azulejos) | cor == c = (q+1, c): azulejos
--                                      | otherwise = (q,c) : somaAzulejo cor azulejos

-- azulejosParaNum :: Azulejos -> Int -> [AzulejosSeparados]
-- azulejosParaNum [] _ = []
-- azulejosParaNum ((0, _):xs) acc = azulejosParaNum xs acc
-- azulejosParaNum (x:xs) acc = (acc,snd x): azulejosParaNum ((fst x - 1, snd x):xs) (acc+1)

-- numParaAzulejos :: [AzulejosSeparados] -> Azulejos -> Azulejos
-- numParaAzulejos [] azulejos = azulejos
-- numParaAzulejos ((_,c):azulejosSeparados) azulejos = numParaAzulejos azulejosSeparados (somaAzulejo c azulejos)

-- -- 
-- --expositores :: [Azulejos] -> Expositores
-- --expositores [] = []
-- --expositores azulejos = 


