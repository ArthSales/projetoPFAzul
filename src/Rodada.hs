{-# LANGUAGE TupleSections #-}
module Rodada where

import Data1
import Parede
import SacoDeAzulejos (sacoAzulejos,azulejosParaNum)
import Expositores (retiraExpositores, geraExpositores, attPontuacao)

-- Transformador de estados
-- dado um estado inicial, devolve um novo estado e um valor
newtype ST s a = ST (s -> (a, s))

-- Roda o state transformer com o estado (ambos recebidos por
-- parâmetro) e devolve uma tupla com o resultado da execução e o novo
-- estado
rodaCom :: ST s a -> s -> (a, s)
rodaCom (ST f) = f

-- Veja slides da aula para maiores explicações
instance Functor (ST s) where
  -- fmap :: (a -> b) -> ST a -> ST b
  -- x :: a
  fmap g st = ST stb
    where
      stb s = (g x, s')
        where
          (x, s') = rodaCom st s

-- Veja slides da aula para maiores explicações
instance Applicative (ST s) where
  -- <*> :: ST (a -> b) -> ST a -> ST b
  stf <*> stx = ST stb
    where
      stb s = (f x, s'')
        where
          (f, s')  = rodaCom stf s
          (x, s'') = rodaCom stx s'

  pure x = ST (x,)

-- Veja slides da aula para maiores explicações
instance Monad (ST s) where
  -- (>>=) :: ST a -> (a -> ST b) -> ST b
  st >>= f = ST stb
    where
      stb s = rodaCom (f x) s'
        where (x, s') = rodaCom st s

nextRound :: State2 -> State2
nextRound s0@(State2 s e m v ch1 ch2 pl1 pl2 p1 p2 p i) 
  | null e && null m = attPontuacao (State2 { sa = novoSaco, 
                                expositores = novosExpositores, 
                                cm = m, 
                                deQuemEAVez = 0, 
                                chao1 = ch1, 
                                chao2 = ch2, 
                                pl1 = novaPattern1, 
                                pl2 = novaPattern2, 
                                parede1 = novaParede1,
                                parede2 = novaParede2,
                                pontuacoes = p,
                                inputs = [] })
  | otherwise = s0
  where
    novosExpositores = geraExpositores (azulejosParaNum (sacoAzulejos s) 0) 20
    novoSaco = retiraExpositores novosExpositores s
    (novaParede1, novaPattern1) = atualizarMatriz p1 pl1
    (novaParede2, novaPattern2) = atualizarMatriz p2 pl2

estadoInicial :: State2
estadoInicial = State2 (sacoAzulejos []) [] [] 0 [] [] [] [] [] [] (0,0) []
-- >>> v = State2 [(19,Azul),(14,Amarelo),(15,Vermelho),(18,Preto),(14,Branco)] [[Branco,Preto,Amarelo,Vermelho],[Branco,Preto,Amarelo,Vermelho],[Branco,Branco,Amarelo,Vermelho],[Azul,Branco,Amarelo,Vermelho],[Amarelo,Branco,Amarelo,Vermelho]] [] 0 [] [] [] [] [] [] (0,0)
-- >>> nextRound v
-- State2 {sa1 = [(19,Azul),(14,Amarelo),(15,Vermelho),(18,Preto),(14,Branco)], expositores1 = [[Branco,Preto,Amarelo,Vermelho],[Branco,Preto,Amarelo,Vermelho],[Branco,Branco,Amarelo,Vermelho],[Azul,Branco,Amarelo,Vermelho],[Amarelo,Branco,Amarelo,Vermelho]], cm1 = [], deQuemEAVez1 = 0, chao1 = [], chao2 = [], pl1 = [], pl2 = [], parede1 = [], parede2 = [], pontuacoes = (0,0)}
