module Main (main, Azulejos) where

main :: IO ()
main = do
  putStrLn "Entre com a lista de azulejos::"
  print (sacoAzulejos [(4,"Azul"),(10,"Amarelo"),(10,"Vermelho"),(6,"Preto"),(10,"Branco")])
  print (sacoAzulejos [(0,"Azul"),(2,"Amarelo"),(3,"Vermelho"),(10,"Preto"),(5,"Branco")])
  print (azulejosParaNum (sacoAzulejos [(0,"Azul"),(2,"Amarelo"),(3,"Vermelho"),(10,"Preto"),(5,"Branco")]) 0 ) 

--Renomeia a lista de tuplas [(Int,String)] para definir a lista de azulejos
type Azulejos = (Int,String)
type Azulejo = (Int,String)
--type Expositores = [[String]]

--sacoAzulejos: define o saco de azulejos do jogo Azul, que contém 20 peças de cada uma das cinco cores e garante que caso
--a soma das peças seja menor que 20, o saco volta a ter as 100 peças iniciais
sacoAzulejos :: [Azulejos] -> [Azulejos]
sacoAzulejos [] = [(20,"Azul"),(20,"Amarelo"),(20,"Vermelho"),(20,"Preto"),(20,"Branco")]
sacoAzulejos xs | totalAzulejos <= 20 = [(20,"Azul"),(20,"Amarelo"),(20,"Vermelho"),(20,"Preto"),(20,"Branco")]
                | otherwise = xs
                  where totalAzulejos = sum (map fst xs)

azulejosParaNum :: [Azulejos] -> Int -> [Azulejo]
azulejosParaNum [] _ = []
azulejosParaNum ((0, _):xs) acc = azulejosParaNum xs acc
azulejosParaNum (x:xs) acc = (acc,snd x): azulejosParaNum ((fst x - 1, snd x):xs) (acc+1)
--azulejosParaNum (v:w:x:y:z:xss) acc | fst v /= 0 = (acc,"Azul"): azulejosParaNum ((fst v - 1, snd v):w:x:y:z:xss) (acc+1)
--                                   | fst w >= 0 = (acc,"Amarelo"): azulejosParaNum ((fst w - 1, snd w):x:y:z:xss) (acc+1)
--                                    | fst x >= 0 = (acc,"Vermelho"): azulejosParaNum ((fst x - 1, snd x):y:z:xss) (acc+1)
--                                    | fst y >= 0 = (acc,"Preto"): azulejosParaNum ((fst y - 1, snd y):z:xss) (acc+1)
--                                   | fst z >= 0 = (acc,"Branco"): azulejosParaNum ((fst z - 1, snd z):xss) (acc+1)


--expositores :: [Azulejos] -> Expositores
--expositores [] = []
--expositores azulejos = 


