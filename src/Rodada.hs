{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
module Rodada where

import Data1
import Parede
import SacoDeAzulejos (sacoAzulejos,azulejosParaNum)
import Expositores (retiraExpositores, geraExpositores, attPontuacao)
import Control.Monad.State

-- Função que verifica se trocou a rodada ou se o saco de azulejos acabou antes do jogo, e modela o estado da nova rodada, ou modela o estado com um novo saco sendo gerado.
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
  
-- Função que verifica se o jogo acabou e modela o estado final do jogo
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

-- função que altera a pontuação final quando o jogo acaba
pontuacaoFinal :: State State2 (Int,Int)
pontuacaoFinal = do
  p <- gets pontuacoes
  let novop = (fst p + 1, snd p)
  return novop


--Função auxiliar que verifica a regra do Game Over
verificaGameOver :: [[(Cor,Bool)]] -> Bool
verificaGameOver [] = False
verificaGameOver (x:xs) = all true x || verificaGameOver xs
      where
        true :: (Cor,Bool) -> Bool
        true (_,b) = b
