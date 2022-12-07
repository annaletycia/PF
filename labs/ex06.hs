--LISTA DE EXERCICIOS 4
--1
mdc :: (Int, Int) -> Int
mdc (m,n) 
  |n==0 = m 
  |otherwise = mdc(n, m `mod` n)
  
mmc :: (Int, Int) -> Int
mmc (m,n) = div (m*n) (mdc (m,n))

--2
mdc3 :: (Int, Int, Int) -> Int
mdc3 (a, b, c) = mdc ( mdc (a, b), c)

--3
soma :: Int -> Int
soma n 
  |n==1 = n
  |otherwise = n+(soma (n-1))
  
--4
soma2limites :: (Int, Int) -> Int
soma2limites (x, y)
  |x==y = y
  |otherwise = x + soma2limites (x+1, y)

soma2SemLimites :: (Int, Int)-> Int
soma2SemLimites (x, y)
  |x + 1 == y - 1 = y - 1
  |otherwise = x + 1 + soma2SemLimites (x + 1, y)
  
--5
lista = []

multiplo :: Int -> Int -> Int -> [Int]
multiplo n1 n2 n3
  |n1 > n2 = aux n1 n3
  |otherwise = multiplo (n1+1) n2 n3

aux n1 n3 
  |mod n3 n1 == 0 = n1:lista
