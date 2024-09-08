module SacoDeAzulejos where

import Data1

-- Soma o total de azulejos no Saco de Azulejos
totalAzulejos :: Azulejos -> Int
totalAzulejos xs = sum (map fst xs)

-- SacoAzulejos: define o saco de azulejos do jogo Azul, que contém 20 peças de cada uma das cinco cores e garante que caso
-- a soma das peças seja menor que 20, o saco volta a ter as 100 peças iniciais
sacoAzulejos :: Azulejos -> Azulejos
sacoAzulejos [] = [(20,Azul),(20,Amarelo),(20,Vermelho),(20,Preto),(20,Branco)]
sacoAzulejos xs | totalAzulejos xs < 20 = [(20,Azul),(20,Amarelo),(20,Vermelho),(20,Preto),(20,Branco)]
                | otherwise = xs

-- Adiciona um azulejo de uma cor específica no saco
somaAzulejo :: Cor -> Azulejos -> Azulejos
somaAzulejo _ [] = [(20,Azul),(20,Amarelo),(20,Vermelho),(20,Preto),(20,Branco)]
somaAzulejo cor ((q,c):azulejos) | cor == c = (q+1, c): azulejos
                                 | otherwise = (q,c) : somaAzulejo cor azulejos

-- Subtrai um azulejo de uma cor específica no saco
subAzulejo :: Cor -> Azulejos -> Azulejos
subAzulejo _ [] = [(20,Azul),(20,Amarelo),(20,Vermelho),(20,Preto),(20,Branco)]
subAzulejo cor ((q,c):azulejos) | cor == c = (q-1, c): azulejos
                                | otherwise = (q,c) : subAzulejo cor azulejos

-- Função que transforma os azulejos numa lista sequencial de azulejos numerados
azulejosParaNum :: Azulejos -> Int -> [AzulejosSeparados]
azulejosParaNum [] _ = []
azulejosParaNum ((0, _):xs) acc = azulejosParaNum xs acc
azulejosParaNum (x:xs) acc = (acc,snd x): azulejosParaNum ((fst x - 1, snd x):xs) (acc+1)

-- >>> azulejosParaNum (sacoAzulejos []) 0
-- [(0,Azul),(1,Azul),(2,Azul),(3,Azul),(4,Azul),(5,Azul),(6,Azul),(7,Azul),(8,Azul),(9,Azul),(10,Azul),(11,Azul),(12,Azul),(13,Azul),(14,Azul),(15,Azul),(16,Azul),(17,Azul),(18,Azul),(19,Azul),(20,Amarelo),(21,Amarelo),(22,Amarelo),(23,Amarelo),(24,Amarelo),(25,Amarelo),(26,Amarelo),(27,Amarelo),(28,Amarelo),(29,Amarelo),(30,Amarelo),(31,Amarelo),(32,Amarelo),(33,Amarelo),(34,Amarelo),(35,Amarelo),(36,Amarelo),(37,Amarelo),(38,Amarelo),(39,Amarelo),(40,Vermelho),(41,Vermelho),(42,Vermelho),(43,Vermelho),(44,Vermelho),(45,Vermelho),(46,Vermelho),(47,Vermelho),(48,Vermelho),(49,Vermelho),(50,Vermelho),(51,Vermelho),(52,Vermelho),(53,Vermelho),(54,Vermelho),(55,Vermelho),(56,Vermelho),(57,Vermelho),(58,Vermelho),(59,Vermelho),(60,Preto),(61,Preto),(62,Preto),(63,Preto),(64,Preto),(65,Preto),(66,Preto),(67,Preto),(68,Preto),(69,Preto),(70,Preto),(71,Preto),(72,Preto),(73,Preto),(74,Preto),(75,Preto),(76,Preto),(77,Preto),(78,Preto),(79,Preto),(80,Branco),(81,Branco),(82,Branco),(83,Branco),(84,Branco),(85,Branco),(86,Branco),(87,Branco),(88,Branco),(89,Branco),(90,Branco),(91,Branco),(92,Branco),(93,Branco),(94,Branco),(95,Branco),(96,Branco),(97,Branco),(98,Branco),(99,Branco)]

-- Transforma uma lista de azulejos sequenciais no saco de azulejos
numParaAzulejos :: [AzulejosSeparados] -> Azulejos -> Azulejos
numParaAzulejos [] azulejos = azulejos
numParaAzulejos ((_,c):azulejosSeparados) azulejos = numParaAzulejos azulejosSeparados (somaAzulejo c azulejos)

--Função que define a regra pra filtrar os azulejos pelo número na lista de azulejos separados, ignorando qual a cor no primeiro momento    
puxaAzulejo :: Int -> AzulejosSeparados -> Bool
puxaAzulejo p (x, _) = p == x


