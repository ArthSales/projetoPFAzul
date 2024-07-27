{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main (main, Azulejos) where

main :: IO ()
main = do
  putStrLn "Entre com a lista de azulejos::"
  print (sacoAzulejos [(4,"Azul"),(10,"Amarelo"),(10,"Vermelho"),(6,"Preto"),(10,"Branco")])
  print (sacoAzulejos [(0,"Azul"),(2,"Amarelo"),(3,"Vermelho"),(10,"Preto"),(5,"Branco")])

--Renomeia a lista de tuplas [(Int,String)] para definir a lista de azulejos
type Azulejos = [(Int,String)]

--sacoAzulejos: define o saco de azulejos do jogo Azul, que contém 20 peças de cada uma das cinco cores e garante que caso
--a soma das peças seja menor que 20, o saco volta a ter as 100 peças iniciais
sacoAzulejos :: Azulejos -> Azulejos
sacoAzulejos [] = [(20,"Azul"),(20,"Amarelo"),(20,"Vermelho"),(20,"Preto"),(20,"Branco")]
sacoAzulejos xs | sum (map fst xs) <= 20 = [(20,"Azul"),(20,"Amarelo"),(20,"Vermelho"),(20,"Preto"),(20,"Branco")]
                | otherwise = xs