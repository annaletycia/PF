import Data.Char

--Anna Letycia Fernandes Reis
-- 12011BSI235
-- T5

--AS_08_09:
--Exercícios: 8, 10 e 11.

--8

conta_digitos ::  Int -> Int
conta_digitos n
     |(n<10) = 1
     |otherwise = 1 + conta_digitos (div n 10)
 
--10.
 
potencia :: (Int, Int) -> Int
potencia (b,1) = b
potencia (b, e) = b* (potencia(b,(e-1)))

--11.

ackerman :: (Int, Int) -> Int
ackerman (m,n)
     |(m==0) = n+1
     |(m>0) && (n==0) = ackerman (m-1,1)
     |(m>0) && (n>0) = ackerman (m,n-1) 
 
 
--AS_15_09
--Exercícios: 2(h),2(i),2(k),2(n),3(b),3(c),4(c),7 e 8.

--2(h)

ultimo :: [Int] -> Int
ultimo [cab] = cab
ultimo (cab:cau) = ultimo cau
{-
exemplo:
ultimo [3,14,1,5,9] =>
=> ultimo [14,1,5,9] =>
=> ultimo [1,5,9] =>
=> ultimo [5,9] =>
=> ultimo [9] =>
=> 9 
-}

--2(i)

duplica :: [Int]->[Int]
duplica [cab] = cab:[cab]
duplica (cab:cau) = (cab:cab:duplica cau)
{-
exemplo:
duplica [3,14,1,5,9] =>
=> (3:3: duplica [14,1,5,9]) =>
=> (3:3:14:14: duplica [1,5,9]) =>
=> (3:3:14:14:1:1: duplica [5,9]) =>
=> (3:3:14:14:1:1:5:5: duplica [9]) =>
=> (3:3:14:14:1:1:5:5:9:[9]) =>
=> [3,3,14,14,1,1,5,5,9,9] 
-}

--2 (k)

substituir_todos :: Int->Int->[Int]->[Int]
substituir_todos x y [] = []
substituir_todos x y (cab:cau)
     |(x == cab) = y: substituir_todos x y cau
     |otherwise = cab: substituir_todos x y cau
 {-
exemplo:
substituir_todos 1 2 [3,14,1,5,1] =>
=> 3: substituir_todos 1 2 [14,1,5,1] =>
=> 3:14: substituir_todos 1 2 [1,5,1] =>
=> 3:14:2: substituir_todos 1 2 [5,1] =>
=> 3:14:2:5: substituir_todos 1 2 [1] =>
=> 3:14:2:5:2: substituir_todos 1 2 [] =>
=> 3:14:2:5:2:[] =>
=> [3,14,2,5,2]
-}

--2 (n) 

maior :: [Int] -> Int
maior [cab] = cab
maior (cab:cau)
     |cab > maior cau = cab
     |otherwise = maior cau
{-
exemplo:
maior [3,14,1,5,9] => 3>14>1>5>9 =>
=> maior [14,1,5,9] => 14>1>5>9 =>
=> 14
-}

--3 (b)

uniao :: [Int] -> [Int] -> [Int]
uniao [] a = a
uniao a [] = a
uniao (a:x) (b:xs)
     | a < b = a: uniao x (b:xs)
     | a == b = a: uniao x xs
     | otherwise = b: uniao (a:x) xs
 
--3(c)

inter :: [Int] -> [Int] -> [Int]
inter a [] = []
inter [] a = []
inter (cab1:cau1) (cab2:cau2)
     |(cab1 == cab2) = cab1: inter cau1 cau2
     |(cab1 < cab2) = inter cau1 (cab2:cau2)
     |(cab1 > cab2) = inter (cab1:cau1) cau2
 
--4(c)

--para auxilia a função em_maiusculo
maiuscula :: String -> String
maiuscula [] = []
maiuscula (x:xs) 
     |isLower x = (toUpper x): maiuscula xs
     |otherwise = x: maiuscula xs
em_maiusculo :: String -> (String,String)
em_maiusculo x = (x, maiuscula x)

--7
conta_posiçao :: [Int] -> Int
conta_posiçao [] = 0
conta_posiçao (x:xs) = 1 + conta_posiçao xs

converte :: [Int] -> Int
converte [] = 0
converte (x:xs) = x*(2^(conta_posiçao(x:xs)-1))+converte xs 

--8
digitos :: Int -> [Int]
digitos 0 = []
digitos x = (digitos (div x 10)) ++ [mod x 10] 