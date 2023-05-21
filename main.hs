-- Função para calcular o fatorial de um número usando recursão
fatorial :: Integer -> Integer
fatorial 0 = 1
fatorial n = n * fatorial (n - 1)

-- Função para verificar se um número é par
ehPar :: Integer -> Bool
ehPar n = n `mod` 2 == 0

-- Função para elevar um número ao quadrado
quadrado :: Integer -> Integer
quadrado n = n * n

-- Função de alta ordem que aplica uma função a todos os elementos de uma lista
meuMap :: (a -> b) -> [a] -> [b]
meuMap _ [] = []
meuMap f (x:xs) = f x : meuMap f xs

-- Função para filtrar os elementos de uma lista que satisfazem uma condição
meuFilter :: (a -> Bool) -> [a] -> [a]
meuFilter _ [] = []
meuFilter p (x:xs)
  | p x = x : meuFilter p xs
  | otherwise = meuFilter p xs

-- Função principal que utiliza as funções acima
main :: IO ()
main = do
  putStrLn "Digite um número:"
  input <- getLine
  let num = read input :: Integer
  let fat = fatorial num
  let par = ehPar num
  let quadrados = meuMap quadrado [1..10]
  let paresQuadrados = meuFilter ehPar quadrados
  putStrLn ("O fatorial de " ++ show num ++ " é: " ++ show fat)
  putStrLn ("O número " ++ show num ++ " é par? " ++ show par)
  putStrLn ("Os quadrados de 1 a 10 são: " ++ show quadrados)
  putStrLn ("Os números pares dentre os quadrados são: " ++ show paresQuadrados)
