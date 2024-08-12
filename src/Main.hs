module Main (main, Azulejos) where

import SacoDeAzulejos
import Parede
import Data

main :: IO ()

main = do
   putStrLn "Entre com a lista de azulejos::"
   print (sacoAzulejos [(4,Azul),(10,Amarelo),(10,Vermelho),(6,Preto),(10,Branco)])
   print (sacoAzulejos [(0,Azul),(2,Amarelo),(3,Vermelho),(10,Preto),(5,Branco)])
   let azulejosTeste = azulejosParaNum (sacoAzulejos [(0,Azul),(2,Amarelo),(3,Vermelho),(10,Preto),(5,Branco)]) 0
   print azulejosTeste
   print (geraExpositores azulejosTeste 4)


  --  let parede = criarParede
  --    --let patternlines = (criarPatternLines 5)
  --  let patternlines = [ [Just Amarelo, Just Amarelo, Nothing]
  --                       , [Just Azul, Just Azul]
  --                       , [Just Preto, Just Preto]
  --                       , [Just Vermelho, Just Vermelho]
  --                       , [Just Amarelo, Just Amarelo]
  --                       ]
  --  mapM_ print parede
  --  mapM_ print patternlines

  --  let (paredeAtualizada, patternLinesAtualizadas) = atualizarMatriz parede patternlines

  --  putStrLn "\nParede atualizada:"
  --  mapM_ print paredeAtualizada
  --  putStrLn "\nPattern Lines atualizadas:"
  --  mapM_ print patternLinesAtualizadas


