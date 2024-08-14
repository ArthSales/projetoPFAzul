module Main (main, Azulejos) where

import SacoDeAzulejos
import Parede
import Data1
import GHC.Float (Floating(expm1))

main :: IO ()

main = do
   let azulejosTeste = azulejosParaNum (sacoAzulejos [(5,Azul),(2,Amarelo),(3,Vermelho),(5,Preto),(5,Branco)]) 0
   let volta = numParaAzulejos azulejosTeste [(0,Azul),(0,Amarelo),(0,Vermelho),(0,Preto),(0,Branco)]
   print volta
   print azulejosTeste
   let teste = [(0,Azul),(1,Azul),(2,Azul),(3,Azul),(4,Azul),(5,Amarelo),(6,Amarelo),(7,Vermelho),(8,Vermelho),(9,Vermelho),(10,Preto),(11,Preto),(12,Preto),(13,Preto),(14,Preto),(15,Branco),(16,Branco),(17,Branco),(18,Branco),(19,Branco)]
   let expo = expositor teste 4
   print expo
   let tira = tiraExpositorDoSaco expo (numParaAzulejos teste [(0,Azul),(0,Amarelo),(0,Vermelho),(0,Preto),(0,Branco)])
   print tira
   let expo1 = expositor tira 4
   print expo1
   let tira1 = tiraExpositorDoSaco expo1 (numParaAzulejos tira [(0,Azul),(0,Amarelo),(0,Vermelho),(0,Preto),(0,Branco)])
   print tira1
   let expo2 = expositor tira1 4
   print expo2
   let tira2 = tiraExpositorDoSaco expo2 (numParaAzulejos tira1 [(0,Azul),(0,Amarelo),(0,Vermelho),(0,Preto),(0,Branco)])
   print tira2
   let expo3 = expositor tira2 4
   print expo3
   let tira3 = tiraExpositorDoSaco expo3 (numParaAzulejos tira2 [(0,Azul),(0,Amarelo),(0,Vermelho),(0,Preto),(0,Branco)])
   print tira3
   let expo4 = expositor tira3 4
   print expo4

   print "--------------------"
   let gerado = geraExpositores teste 20
   print gerado
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


