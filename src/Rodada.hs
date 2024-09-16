{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
module Rodada where

import Data1
import Parede
import SacoDeAzulejos (sacoAzulejos,azulejosParaNum)
import Expositores (retiraExpositores, geraExpositores, attPontuacao)
import Control.Monad.State

-- >>>:t

-- nextRound :: State2 -> State2
-- nextRound s0@(State2 s e m v ch1 ch2 pl1 pl2 p1 p2 p i) 
--   | null e && null m = attPontuacao (State2 { sa = novoSaco, 
--                                 expositores = novosExpositores, 
--                                 cm = m, 
--                                 deQuemEAVez = 0, 
--                                 chao1 = ch1, 
--                                 chao2 = ch2, 
--                                 pl1 = novaPattern1, 
--                                 pl2 = novaPattern2,
--                                 parede1 = novaParede1,
--                                 parede2 = novaParede2,
--                                 pontuacoes = p,
--                                 inputs = [] })
--   | null s = resetaSaco
--   | otherwise = s0
--   where
--     resetaSaco = State2 (sacoAzulejos []) novoExpoReset m 0 ch1 ch2 pl1 pl2 p1 p2 p []
--     novoExpoReset = geraExpositores (azulejosParaNum (sacoAzulejos []) 0) 20
--     novosExpositores = geraExpositores (azulejosParaNum (sacoAzulejos s) 0) 20
--     novoSaco = retiraExpositores novosExpositores s
--     (novaParede1, novaPattern1) = atualizarMatriz p1 pl1
--     (novaParede2, novaPattern2) = atualizarMatriz p2 pl2

nextRound :: State State2 ()
nextRound = do
    sac <- gets sa
    e <- gets expositores
    m <- gets cm
    p1 <- gets parede1
    p2 <- gets parede2
    pl1 <- gets pl1
    pl2 <- gets pl2
    p <- gets pontuacoes
    if sac == [(0,Azul),(0,Amarelo),(0,Vermelho),(0,Preto),(0,Branco)] 
      then modify $ \rs -> rs{sa = sacoAzulejos [], expositores = geraExpositores (azulejosParaNum (sacoAzulejos []) 0) 20}
      else if null m && null e 
        then modify $ \nr -> nr{ sa = retiraExpositores (geraExpositores (azulejosParaNum (sacoAzulejos sac) 0) 20) sac,
                                 expositores = geraExpositores (azulejosParaNum (sacoAzulejos sac) 0) 20,
                                 deQuemEAVez = 0,
                                 pl1 = snd $ atualizarMatriz p1 pl1,
                                 pl2 = snd $ atualizarMatriz p2 pl2,
                                 parede1 = fst $ atualizarMatriz p1 pl1,
                                 parede2 = fst $ atualizarMatriz p2 pl2,
                                 pontuacoes = p}
        else return ()
    
-- >>> execState nextRound (attPontuacao (State2 [(20,Azul),(20,Amarelo),(20,Vermelho),(10,Preto),(10,Branco)]  [] [] 0 [] [] [] [] [[(Azul,False),(Azul,True),(Azul,True)],[(Azul,False),(Azul,True),(Azul,True)],[(Azul,True),(Azul,True),(Azul,True)]] [] (20,0) []))
-- State2 {sa = [(18,Azul),(9,Amarelo),(15,Vermelho),(10,Preto),(8,Branco)], expositores = [[Azul,Branco,Amarelo,Vermelho],[Amarelo,Branco,Amarelo,Vermelho],[Amarelo,Amarelo,Amarelo,Vermelho],[Amarelo,Amarelo,Amarelo,Vermelho],[Amarelo,Azul,Amarelo,Vermelho]], cm = [], deQuemEAVez = 0, chao1 = [], chao2 = [], pl1 = [], pl2 = [], parede1 = [], parede2 = [], pontuacoes = (27,0), inputs = ""}

-- gameOver :: State2 -> State2
-- gameOver s0@(State2 s e m v ch1 ch2 pl1 pl2 p1 p2 p i) | verificaGameOver p1 || verificaGameOver p2 = State2 [] [] [] 0 [] [] [] [] [] [] (0,0) []
--                                                        | otherwise = s0

gameOver :: State State2 ()
gameOver = do
  state <- get
  p1 <- gets parede1
  p2 <- gets parede2
  p <- gets pontuacoes
  if verificaGameOver p1 || verificaGameOver p2
    then modify $ \ne -> ne{
      sa = [],
      expositores = [],
      cm = [],
      deQuemEAVez = 0,
      chao1 = [],
      chao2 = [],
      pl1 = [],
      pl2 = [],
      parede1 = [],
      parede2 = [],
      pontuacoes = evalState pontuacaoFinal state,
      inputs = []
    }
    else return ()

pontuacaoFinal :: State State2 (Int,Int)
pontuacaoFinal = do
  p <- gets pontuacoes
  let novop = (fst p + 1, snd p)
  return novop

-- >>> exec (gameOver (State2 [] [] [] 0 [] [] [] [] [[(Azul,False),(Azul,True),(Azul,True)],[(Azul,False),(Azul,True),(Azul,True)],[(Azul,True),(Azul,True),(Azul,True)]] [] (0,0) []))
-- Variable not in scope: exec :: t0_a1j6T[tau:1] -> t_a1j6V[sk:1]
    
verificaGameOver :: [[(Cor,Bool)]] -> Bool
verificaGameOver [] = False
verificaGameOver (x:xs) = all true x || verificaGameOver xs
      where
        true :: (Cor,Bool) -> Bool
        true (_,b) = b

-- >>> verificaGameOver [[(Azul,False),(Azul,True),(Azul,True)],[(Azul,False),(Azul,True),(Azul,True)],[(Azul,True),(Azul,True),(Azul,True)]]
-- True
