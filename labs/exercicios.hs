import Data.Char

--Anna Letycia Fernandes Reis
--12011BSI235

--Exercícios AS30_09

--1-a)
primeiros :: [(a,b)] -> [a]
primeiros [] = []
primeiros (x:xs) = fst x: primeiros xs

primeirosFOS :: [(a,b)] -> [a]
primeirosFOS (x:xs) = fst x: map fst xs

--b)
maiusculas :: String -> String
maiusculas [] = []
maiusculas (x:xs)
      |(isLower x) = (toUpper x): maiusculas xs
      |otherwise = x: maiusculas xs

maiusculasFOS :: String -> String
maiusculasFOS (x:xs) = toUpper x: map toUpper xs

--c)
dobra :: Num a => [a] -> [a]
dobra [] = []
dobra (x:xs) = (2*x): dobra xs

dobraFOS :: Num a => [a] -> [a]
dobraFOS (x:xs) = 2*x: map (2*) xs

--d)
hora_em_seg :: [Float] -> [Float]
hora_em_seg [] = []
hora_em_seg (x:xs) = (3600*x): hora_em_seg xs

hora_em_segFOS :: [Float] -> [Float]
hora_em_segFOS (x:xs) = (3600*x): map (3600*) xs

--7-a)
pares :: [Int] -> [Int]
pares [] = []
pares (x:xs)
      |even x = x: pares xs
      |otherwise = pares xs

paresFOS :: [Int] -> [Int]
paresFOS (x:xs) = filter even (x:xs)

--b)
alfa :: String -> String
alfa [] = []
alfa (x:xs)
      |isAlpha x = x: alfa xs
      |otherwise = alfa xs

alfaFOS :: String -> String
alfaFOS (x:xs) = filter isAlpha (x:xs)

--c)
rmChar :: Char -> String -> String
rmChar a[] = []
rmChar a (x:xs)
      |(a == x) = rmChar a xs
      |otherwise = x: rmChar a xs

rmCharFOS :: Char -> String -> String
rmCharFOS a (x:xs) = filter (/= a) (x:xs)

--d)
acima :: Int -> [Int] -> [Int]
acima a [] = []
acima a (x:xs)
      |(x <= a) = acima a xs
      |otherwise = x: acima a xs

acimaFOS :: Int -> [Int] -> [Int]
acimaFOS a (x:xs) =  filter (> a) (x:xs)

--e)
desiguais :: Eq t => [(t,t)] -> [(t,t)]
desiguais [] = []
desiguais (x:xs)
      |(fst x == snd x) = desiguais xs
      |otherwise = x: desiguais xs
 
aIguais :: Eq t => (t,t) -> Bool
aIguais (a,b)
      |(a /= b) = True
      |otherwise = False

desiguaisFOS :: Eq t => [(t,t)] -> [(t,t)]
desiguaisFOS (x:xs) = filter (aIguais) (x:xs)

--8-a)
produto :: Num a => [a] -> a
produto [] = 1
produto (x:xs) = x * produto xs

produtoFOS :: Num a => [a] -> a
produtoFOS (x:xs) = foldr (*) x xs

--b)
e_logico :: [Bool] -> Bool
e_logico [] = True
e_logico (x:xs)
      |(x == True) = True && e_logico xs
      |otherwise = False

e_logicoFOS :: [Bool] -> Bool
e_logicoFOS (x:xs) = foldr (&&) x xs

--c)
concatena :: [String] -> String
concatena [] = []
concatena (x:xs) = x ++ concatena xs

concatenaFOS :: [String] -> String
concatenaFOS (x:xs) = foldr (++) ("") (x:xs)

--d)
maior :: Int -> [Int] -> Int
maior a [] = a
maior a (x:xs)
      |(a > x) = maior a xs
      |otherwise = maior x xs

maiorDeDois :: Int -> Int -> Int
maiorDeDois a b
      |(a>b) = a
      |otherwise = b

maiorFOS :: Int -> [Int] -> Int
maiorFOS a (x:xs) = foldr (maiorDeDois) a (x:xs)
