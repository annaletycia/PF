---menor :: (Int,Int) -> Int
---menor (x,y) = if x<y then x else y
---menordetres :: (Int,Int,Int) -> Int
---menordetres (a,b,c) =  if a<b && a<c then a else if b<c then b else c

menor :: (Int,Int) -> Int
menor (x,y) 
   |x<y = x
   |y<x = y
   
menordetres :: (Int, Int, Int) -> Int
menordetres (a,b,c) 
   |a<b && a<c = a
   |b<c = b
   |c<b = c
   
not_logico :: Bool->Bool
not_logico x
 |x == True = False
 |x == not_logico False = True 

and_logico :: (Bool, Bool)->Bool
and_logico x
 |x == (True,True) = True
 |otherwise = False

or_logico :: (Bool,Bool)->Bool
or_logico x
 |(fst x) == True  = True
 |(snd x) == True = True
 |otherwise = False