module Main (main, Azulejos) where

main :: IO ()
main = do
  putStrLn "Entre com a lista de azulejos::"
  line <- getLine
  print (sacoAzulejos [(20,line)])

type Azulejos = [(Int,String)] 

sacoAzulejos :: Azulejos -> Azulejos
sacoAzulejos [] = [(20,"Azul"),(20,"Amarelo"),(20,"Vermelho"),(20,"Preto"),(20,"Branco")]
sacoAzulejos ((x,y):zs) =  x + sacoAzulejos zs