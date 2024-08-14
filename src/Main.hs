module Main (main, Azulejos) where

import SacoDeAzulejos
import Parede
import Data
import GHC.Float (Floating(expm1))

main :: IO ()

main = do
   let azulejosTeste = azulejosParaNum (sacoAzulejos [(5,Azul),(2,Amarelo),(3,Vermelho),(5,Preto),(5,Branco)]) 0
   let volta = numParaAzulejos azulejosTeste [(0,Azul),(0,Amarelo),(0,Vermelho),(0,Preto),(0,Branco)]
   print volta
   print azulejosTeste
   let expo = expositor azulejosTeste 4
   print expo
   let sub = azulejosParaNum (subAzulejo Preto (numParaAzulejos azulejosTeste [(0,Azul),(0,Amarelo),(0,Vermelho),(0,Preto),(0,Branco)])) 0
   print sub
   let expositores = geraExpositores azulejosTeste 10
   print expositores

   -- let tira = tiraExpositorDoSaco expo [(5,Azul),(2,Amarelo),(3,Vermelho),(5,Preto),(5,Branco)]
   -- print tira
   -- let exp1 = geraExpositores azulejosTeste 8
   -- print exp1
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


