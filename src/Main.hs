module Main (main, Azulejos) where

import SacoDeAzulejos
import Expositores
import Parede
import Jogador1
import Jogador2
import Data1
import GHC.Float (Floating(expm1))


main :: IO ()

main = do
   let jogador10 = []
   let jogador20 = []
   let saco0 = sacoAzulejos []
   print saco0

   let expositores0 = geraExpositores (azulejosParaNum saco0 0) 20
   print expositores0

   let saco1 = retiraExpositores expositores0 saco0
   print saco1
 
   let comp1 = compraExpositor Amarelo 0 expositores0
   print comp1

   let jogador11 = jogador1 comp1 jogador10
   print jogador11

   let comp2 = compraExpositor Amarelo 1 expositores0
   let jogador21 = jogador2 comp2 jogador20
   print jogador21
   -- let tira2 = tiraExpositorDoSaco expo2 (numParaAzulejos [(0,Azul),(1,Azul),(2,Azul),(3,Azul),(4,Azul),(5,Vermelho),(6,Preto),(7,Preto)] [(0,Azul),(0,Amarelo),(0,Vermelho),(0,Preto),(0,Branco)])
   -- print tira2
   -- let expo3 = expositor tira2 4
   -- print expo3
   -- let sub = azulejosParaNum (subAzulejo Preto (numParaAzulejos azulejosTeste [(0,Azul),(0,Amarelo),(0,Vermelho),(0,Preto),(0,Branco)])) 0
   -- print sub
   -- let expositores = geraExpositores azulejosTeste 20
   -- print expositores
   

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


